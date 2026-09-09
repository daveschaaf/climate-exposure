# 02_event_study_plots.R
# Event study visualizations for report.Rmd
# Requires: 00_load_data.R, 05_hedonic sourced first

library(tidyverse)
library(scales)
library(broom)
library(fixest)
library(ggplot2)
library(dplyr)

here::i_am("paper/R/06_hedonic_plots.R")
source(here::here("paper", "R", "theme.R"))

run_hedonic_plots_legacy <- function(output_dir, include_extras = FALSE) {
  
  has_spatial <- requireNamespace("tigris", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)
  has_binsreg <- requireNamespace("binsreg", quietly = TRUE)
  if (has_spatial) options(tigris_use_cache = TRUE)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  hed_inline <- readRDS(here::here("paper", "output", "regression", "hedonic_inline.rds"))
  mA <- hed_inline$mA; mB <- hed_inline$mB; mC <- hed_inline$mC
  mD <- hed_inline$mD; mE <- hed_inline$mE; mF <- hed_inline$mF
  df <- hed_inline$rucc_df
  hed_df <- hed_inline$hed_df
  
  # Coefficient Plot (horizontal forest)
  coef_df <- data.frame(
    term      = names(coef(mC)),
    estimate  = coef(mC),
    std.error = se(mC)
  ) %>%
    mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value   = 2 * (1 - pnorm(abs(estimate / std.error))),
      label = case_when(
        term == "log_eal"         ~ "Expected Annual Loss (log)",
        term == "resl_value_z"    ~ "Resilience (1-SD)",
        term == "sovi_score_z"    ~ "Social Vulnerability (1-SD)",
        term == "sovi_score_z:cdc_svi_regime" ~ "SoVI × CDC regime",
        term == "log_income"      ~ "Median income (log)",
        term == "log_pop_density" ~ "Pop density (log)",
        term == "coastal"         ~ "Coastal county",
        TRUE                      ~ term
      ),
      sig = ifelse(p.value < 0.05, "Significant", "Not significant")
    )
  
  coefficient_plot <- ggplot(coef_df, aes(x = estimate, y = reorder(label, estimate), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.6) +
    scale_color_manual(values = c("Significant" = "#1a5490", "Not significant" = "grey60")) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(
      title    = stringr::str_wrap("Hedonic Regression Coefficients on log(ZHVI)", width = 50),
      subtitle = stringr::str_wrap("County-month panel, Nov 2020 – Dec 2025; state + month FE; county-clustered SEs", width = 60),
      x = "Coefficient (% change in ZHVI)", y = NULL, color = NULL
    ) + theme_paper

  
  # Income Effect
  get_coefs <- function(model, model_label) {
    data.frame(
      term      = names(coef(model)),
      estimate  = coef(model),
      std.error = se(model),
      model     = model_label
    ) %>%
      mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error)
  }
  
  coef_compare <- bind_rows(
    get_coefs(mE, "Without income"),
    get_coefs(mB, "With income")
  ) %>%
    filter(term %in% c("log_eal", "resl_value_z", "sovi_score_z", "coastal"))
  
  income_comparison_plot <- ggplot(coef_compare, aes(x = estimate, y = term, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(0.4), size = 0.5) +
    labs(
      title    = stringr::str_wrap("Coefficients shift dramatically when income is removed", width = 50),
      subtitle = "Income effects absorb much of the apparent NRI variance",
      x = "Coefficient", y = NULL, color = NULL
    ) + theme_paper
  
  # Urban/Ruralness ruled out
  rucc_summary <- df %>%
    distinct(stcofips, .keep_all = TRUE) %>%
    group_by(rucc_2023) %>%
    summarise(
      Resilience      = mean(resl_value, na.rm = TRUE),
      `Median ZHVI`   = mean(zhvi, na.rm = TRUE),
      `Median Income` = mean(median_hh_income, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-rucc_2023, names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    mutate(indexed = 100 * value / value[rucc_2023 == 1]) %>%
    ungroup()
  
  urban_rural_plot <- ggplot(rucc_summary, aes(x = rucc_2023, y = indexed, color = metric)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = 1:9) +
    scale_color_manual(values = c(
      "Resilience" = "#1a5490", "Median ZHVI" = "#b2182b", "Median Income" = "#4d9221"
    )) +
    labs(
      title    = stringr::str_wrap("Resilience barely moves across the urban-rural continuum; income and prices halve", width = 50),
      subtitle = "All series indexed to RUCC 1 (large metro) = 100",
      x = "RUCC code  (1 = large metro, 9 = most rural)", y = "Index (RUCC 1 = 100)", color = NULL
    ) + theme_paper
  
  # Scatter/Partial-residual plot (binned) — not used in report.qmd; skip by default
  if (include_extras) {
    df$zhvi_resid <- residuals(feols(log_zhvi ~ log_income + log_pop_density + coastal | state + month_id, data = df))
    df$eal_resid  <- residuals(feols(log_eal  ~ log_income + log_pop_density + coastal | state + month_id, data = df))
    
    if (has_binsreg) {
      partial_residual_bins_plot <- binsreg::binsreg(y = df$zhvi_resid, x = df$eal_resid, nbins = 30, line = c(3, 3))
    } else {
      message("06_hedonic_plots.R: binsreg not installed - skipping partial residual bin plot.")
      partial_residual_bins_plot <- NULL
    }
  } else {
    partial_residual_bins_plot <- NULL
  }
  
  # Maps of Residuals — not used in report.qmd; skip by default
  if (include_extras && has_spatial) {
    ... # unchanged map-building code
  } else {
    if (include_extras) message("06_hedonic_plots.R: tigris/sf not available - skipping residual maps.")
    hedonic_residual_state_map    <- NULL
    hedonic_residual_national_map <- NULL
  }
  
  # ---- Save PDFs too (still useful standalone) ----
  ggsave(file.path(output_dir, "hedonic_coefficient_plot.pdf"), coefficient_plot, width = 6.5, height = 3.25)
  ggsave(file.path(output_dir, "hedonic_income_comparison.pdf"), income_comparison_plot, width = 6.5, height = 3.25)
  ggsave(file.path(output_dir, "hedonic_urban_rural.pdf"), urban_rural_plot, width = 6.5, height = 3.75)
  if (!is.null(partial_residual_bins_plot)) {
    ggsave(file.path(output_dir, "hedonic_residual_bins.pdf"), partial_residual_bins_plot$bins_plot, width = 8, height = 5)
  }
  if (!is.null(hedonic_residual_state_map)) {
    ggsave(file.path(output_dir, "hedonic_residual_state_map.pdf"), hedonic_residual_state_map, width = 10, height = 7)
  }
  if (!is.null(hedonic_residual_national_map)) {
    ggsave(file.path(output_dir, "hedonic_residual_national_map.pdf"), hedonic_residual_national_map, width = 10, height = 7)
  }
  
  invisible(list(
    coefficient_plot               = coefficient_plot,
    income_comparison_plot         = income_comparison_plot,
    urban_rural_plot                = urban_rural_plot,
    partial_residual_bins_plot     = partial_residual_bins_plot,
    hedonic_residual_state_map     = hedonic_residual_state_map,
    hedonic_residual_national_map  = hedonic_residual_national_map
  ))
}