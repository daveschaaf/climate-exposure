# balance.R
# Section 3.3: Sample construction and covariate balance
# Per-event difference histograms: target - baseline mean

library(tidyverse)
library(scales)
library(tidycensus)
library(here)

source(here::here("paper", "R", "theme.R"))

run_balance <- function(baselines, storms, output_dir) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Load NRI for all counties (monthly smoothed) ----
  nri <- read_csv(here::here("data", "processed", "nri_panel_smooth.csv"),
                  col_types = cols(stcofips = col_character()),
                  show_col_types = FALSE) |>
    select(stcofips, storm_year, month, resl_value, eal_valt)
  
  # ---- Pull ACS income (cross-sectional) ----
  acs <- get_acs(
    geography = "county",
    variables = c(median_hh_income = "B19013_001"),
    year      = 2023,
    survey    = "acs5",
    output    = "wide",
    cache_table = TRUE
  ) |>
    transmute(
      stcofips         = GEOID,
      median_hh_income = median_hh_incomeE
    )
  
  cat(sprintf("ACS: %s counties, NRI: %s rows\n",
              format(nrow(acs), big.mark = ","),
              format(nrow(nri), big.mark = ",")))
  
  # ---- Attrition table ----
  total_storms <- nrow(storms)
  
  attrition_rows <- list()
  for (key in names(baselines)) {
    bl     <- baselines[[key]]
    df_mid <- bl$df |> filter(tier == "mid")
    
    attrition_rows[[key]] <- tibble(
      baseline         = bl$baseline,
      total_storms     = total_storms,
      covered_events   = nrow(df_mid),
      covered_counties = n_distinct(df_mid$stcofips),
      complete_tiers   = bl$df |>
        group_by(stcofips, year, month) |>
        filter(n() == 3) |>
        ungroup() |>
        distinct(stcofips, year, month) |>
        nrow()
    )
  }
  
  attrition_df <- bind_rows(attrition_rows) |>
    mutate(baseline = factor(baseline, levels = c("Regional", "Second Ring (R2)", "First Ring (R1)")))
  
  write_csv(attrition_df, file.path(output_dir, "attrition_table.csv"))
  cat("\nAttrition:\n")
  print(attrition_df)
  
  # Target ZHVI is in event_neighbors
  diff_list <- list()
  
  for (key in names(baselines)) {
    bl <- baselines[[key]]
    
    en <- read_csv(
      file.path(here::here(bl$path), "event_neighbors_mid.csv"),
      col_types = cols(target_fips = col_character()),
      show_col_types = FALSE
    )
    
    # Expand and compute baseline NRI/income means
    en_long <- en |>
      select(target_fips, storm_year, storm_month, target_zhvi_t0, baseline_mean_zhvi_t0, neighbor_fips) |>
      separate_rows(neighbor_fips, sep = ",") |>
      mutate(neighbor_fips = str_trim(neighbor_fips)) |>
      left_join(
        nri |> rename(neighbor_fips = stcofips, nbr_resl = resl_value, nbr_eal = eal_valt),
        by = c("neighbor_fips", "storm_year", "storm_month" = "month")
      ) |>
      left_join(
        acs |> rename(neighbor_fips = stcofips, nbr_income = median_hh_income),
        by = "neighbor_fips"
      )
    
    baseline_means <- en_long |>
      group_by(target_fips, storm_year, storm_month, target_zhvi_t0, baseline_mean_zhvi_t0) |>
      summarise(
        baseline_mean_resl   = mean(nbr_resl, na.rm = TRUE),
        baseline_mean_eal    = mean(nbr_eal, na.rm = TRUE),
        baseline_mean_income = mean(nbr_income, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Get target NRI at event time
    target_nri <- bl$df |>
      filter(tier == "mid") |>
      select(stcofips, year, month, resl_value, eal_valt) |>
      rename(target_fips = stcofips, storm_year = year, storm_month = month)
    
    # Get target income
    target_income <- acs |> rename(target_fips = stcofips, target_income = median_hh_income)
    
    event_diffs <- baseline_means |>
      inner_join(target_nri, by = c("target_fips", "storm_year", "storm_month")) |>
      left_join(target_income, by = "target_fips") |>
      mutate(
        diff_zhvi   = log1p(target_zhvi_t0) - log1p(baseline_mean_zhvi_t0),
        diff_resl   = resl_value - baseline_mean_resl,
        diff_eal    = log1p(eal_valt) - log1p(baseline_mean_eal),
        diff_income = target_income - baseline_mean_income,
        baseline    = bl$baseline
      )
    
    diff_list[[key]] <- event_diffs
  }
  
  diff_all <- bind_rows(diff_list)
  
  # ---- Balance summary ----
  balance_summary <- diff_all |>
    mutate(baseline = factor(baseline, levels = c("Regional", "Second Ring (R2)", "First Ring (R1)"))) |>
    group_by(baseline) |>
    summarise(
      mean_zhvi   = round(mean(diff_zhvi, na.rm = TRUE), 3),
      sd_zhvi     = round(sd(diff_zhvi, na.rm = TRUE), 3),
      mean_income = round(mean(diff_income, na.rm = TRUE)),
      sd_income   = round(sd(diff_income, na.rm = TRUE)),
      mean_resl   = round(mean(diff_resl, na.rm = TRUE), 3),
      sd_resl     = round(sd(diff_resl, na.rm = TRUE), 3),
      mean_eal    = round(mean(diff_eal, na.rm = TRUE), 3),
      sd_eal      = round(sd(diff_eal, na.rm = TRUE), 3),
      n_events    = n(),
      .groups     = "drop"
    )
  
  write_csv(balance_summary, file.path(output_dir, "balance_summary.csv"))
  cat("\nBalance summary (mean = bias, SD = spread):\n")
  print(balance_summary)
  
  # ---- Pivot for plotting ----
  diff_plot <- diff_all |>
    select(baseline, diff_zhvi, diff_resl, diff_eal, diff_income) |>
    pivot_longer(
      cols      = starts_with("diff_"),
      names_to  = "metric",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    mutate(
      metric = factor(metric,
                      levels = c("diff_zhvi", "diff_income", "diff_resl", "diff_eal"),
                      labels = c("ZHVI at T=0 (log $)", "Median Income ($)", "Resilience Value", "Expected Annual Loss (log $)")
      ),
      baseline = factor(baseline, levels = c("Regional", "Second Ring (R2)", "First Ring (R1)"))
    )
  
  # ---- Grid plot: rows = covariates, columns = baselines ----
  library(patchwork)
  
  make_row <- function(data, metric_name, x_label) {
    ggplot(data, aes(x = value)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "white",
                     linewidth = 0.2, alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
      facet_wrap(~ baseline, ncol = 3) +
      labs(x = x_label, y = NULL, subtitle = metric_name) +
      theme_minimal() +
      theme(
        strip.text       = element_text(size = 8, face = "bold"),
        axis.text.x      = element_text(size = 7),
        panel.grid.minor = element_blank()
      )
  }
  
  p_zhvi   <- make_row(diff_plot |> filter(metric == "ZHVI at T=0 (log $)"),
                       "ZHVI at T=0 (log $)", "Difference")
  p_income <- make_row(diff_plot |> filter(metric == "Median Income ($)"),
                       "Median Income ($)", "Difference ($)")
  p_resl   <- make_row(diff_plot |> filter(metric == "Resilience Value"),
                       "Resilience Value", "Difference")
  p_eal    <- make_row(diff_plot |> filter(metric == "Expected Annual Loss (log $)"),
                       "Expected Annual Loss (log $)", "Difference")
  
  # Only show baseline labels on top row
  p_income <- p_income + theme(strip.text = element_blank())
  p_resl   <- p_resl   + theme(strip.text = element_blank())
  p_eal    <- p_eal    + theme(strip.text = element_blank())
  
  fig_balance <- p_zhvi / p_income / p_resl / p_eal +
    plot_annotation(
      title    = "Covariate Balance: Target - Baseline Difference",
      subtitle = "Per-event difference. Red dashed line = zero (perfect balance)."
    )
  
  # ---- Attrition: percentage survival ----
  fig_attrition <- attrition_df |>
    ggplot(aes(x = baseline, y = covered_events / total_storms * 100, fill = baseline)) +
    geom_col(width = 0.6) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_fill_manual(values = BASELINE_COLORS) +
    labs(
      title    = "Sample Survival Rate by Baseline Method",
      subtitle = sprintf("Percentage of %s total storm events retained.", format(total_storms, big.mark = ",")),
      x        = NULL,
      y        = "Survival Rate"
    ) +
    theme_paper +
    theme(legend.position = "none")
  
  # ---- Save ----
  ggsave(file.path(output_dir, "fig_balance.pdf"),
         fig_balance, width = 6.5, height = 8.5)
  ggsave(file.path(output_dir, "fig_attrition.pdf"),
         fig_attrition, width = 7, height = 5)
  
  write_csv(diff_all, file.path(output_dir, "covariate_balance.csv"))
  
  cat(sprintf("\nBalance analysis saved to %s\n", output_dir))
  
  list(
    fig_balance   = fig_balance,
    fig_attrition = fig_attrition,
    attrition_df  = attrition_df,
    diff_all      = diff_all
  )
}