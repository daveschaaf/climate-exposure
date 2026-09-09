# lp_did.R
# Section 4.3: Local Projections
# LP 1: Dynamic damage effect by tier (L2 baseline)
# LP 2: Dynamic damage effect by baseline (mid tier)

library(tidyverse)
library(fixest)
library(broom)
library(here)

source(here::here("paper", "R", "theme.R"))
PLACEBO_FILL   <- "#fee0d2"   # light salmon
POSTSTORM_FILL <- "#deebf7"   # light blue-grey

run_lp <- function(baselines, output_dir) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  PRE  <- PRE_EVENT_MONTHS
  POST <- POST_EVENT_MONTHS
  horizons <- c(-PRE:-1, 1:POST)
  x_breaks <- c(-PRE:-1, 0, 1:POST)
  x_labels <- c(paste0("T", -PRE:-1), "T0", paste0("T+", 1:POST))
  
  ref_row <- function(term_name, group_val, group_name) {
    tibble(term = term_name, estimate = 0, conf.low = 0, conf.high = 0,
           horizon = 0, std.error = NA, statistic = NA, p.value = NA) |>
      mutate(!!group_name := group_val)
  }
  
  # ---- Helper: build md_lp from a baseline ----
  build_md_lp <- function(bl) {
    bl$monthly_dev |>
      filter(month_t != 0) |>
      left_join(
        bl$df |> select(stcofips, year, month, tier, log_damage,
                        pre_trend_annual, month_year, coastal,
                        log_income, log_pop_density, log_risk_value),
        by = c("stcofips", "year", "month", "tier")
      ) |>
      filter(!is.na(pre_trend_annual), !is.na(log_income), !is.na(log_pop_density))
  }
  
  # ---- Helper: run LP loop ----
  run_lp_loop <- function(data, horizons) {
    results <- list()
    for (h in horizons) {
      data_h <- data |> filter(month_t == h)
      model <- tryCatch({
        feols(
          deviation ~ pre_trend_annual + coastal + log_income +
            log_pop_density + log_damage + log_risk_value | month_year,
          cluster = ~stcofips + month_year,
          data    = data_h
        )
      }, error = function(e) {
        cat(sprintf("  LP failed at h=%d: %s\n", h, e$message))
        NULL
      })
      if (!is.null(model)) {
        results[[as.character(h)]] <- broom::tidy(model, conf.int = TRUE) |>
          mutate(horizon = h)
      }
    }
    bind_rows(results)
  }
  
  # ---- LP 1: Damage by tier (L2 baseline) ----
  md_l2 <- build_md_lp(baselines$level2)
  
  tier_lp_list <- list()
  
  for (t in c("mid", "bottom", "top")) {
    tier_label <- case_when(
      t == "mid"    ~ "Mid",
      t == "bottom" ~ "Bottom",
      t == "top"    ~ "Top"
    )
    
    lp_result <- run_lp_loop(md_l2 |> filter(tier == t), horizons)
    
    tier_lp_list[[tier_label]] <- lp_result |>
      filter(term == "log_damage") |>
      mutate(tier = tier_label) |>
      bind_rows(ref_row("log_damage", tier_label, "tier"))
  }
  
  lp_by_tier <- bind_rows(tier_lp_list) |>
    mutate(tier = factor(tier, levels = c("Mid", "Bottom", "Top")))

  
  fig_lp_tier <- ggplot(lp_by_tier,
                        aes(x = horizon, y = estimate, color = tier, fill = tier)) +
    annotate("rect", xmin = -PRE - 0.5, xmax = -0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.05, fill = PLACEBO_FILL) +
    annotate("rect", xmin = 0.5, xmax = POST + 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.05, fill = POSTSTORM_FILL) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_color_manual(values = TIER_COLORS) +
    scale_fill_manual(values  = TIER_COLORS) +
    labs(
      title    = "Dynamic Effect of Log Damage by Market Tier",
      subtitle = "Second Ring (R2) baseline. 95% CI.",
      x        = "Months Relative to Storm",
      y        = "Coefficient on Log Property Damage",
      color    = "Tier",
      fill     = "Tier"
    ) +
    theme_paper
  
  # ---- LP 2: Damage by baseline (mid tier) ----
  baseline_lp_list <- list()
  
  for (key in names(baselines)) {
    bl <- baselines[[key]]
    md <- build_md_lp(bl) |> filter(tier == "bottom")
    
    lp_result <- run_lp_loop(md, horizons)
    
    baseline_lp_list[[bl$baseline]] <- lp_result |>
      filter(term == "log_damage") |>
      mutate(baseline = bl$baseline) |>
      bind_rows(ref_row("log_damage", bl$baseline, "baseline"))
  }
  
  lp_by_baseline <- bind_rows(baseline_lp_list) |>
    mutate(baseline = factor(baseline, 
                             levels = c("Regional", "Second Ring (R2)", "First Ring (R1)")))
  
  fig_lp_baseline <- ggplot(lp_by_baseline,
                            aes(x = horizon, y = estimate, color = baseline, fill = baseline)) +
    annotate("rect", xmin = -PRE - 0.5, xmax = -0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.05, fill = PLACEBO_FILL) +
    annotate("rect", xmin = 0.5, xmax = POST + 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.05, fill = POSTSTORM_FILL) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_color_manual(values = BASELINE_COLORS) +
    scale_fill_manual(values  = BASELINE_COLORS) +
    labs(
      title    = "Dynamic Effect of Log Damage by Baseline Method",
      subtitle = "Bottom tier only. Red = placebo. Blue = post-storm. 95% CI.",
      x        = "Months Relative to Storm",
      y        = "Coefficient on Log Property Damage",
      color    = "Baseline",
      fill     = "Baseline"
    ) +
    theme_paper
  
  # ---- Save ----
  ggsave(file.path(output_dir, "fig_lp_tier.pdf"),
         fig_lp_tier, width = 6.5, height = 4.0)
  ggsave(file.path(output_dir, "fig_lp_baseline.pdf"),
         fig_lp_baseline, width = 6.5, height = 4.0)
  
  cat(sprintf("LP figures saved to %s\n", output_dir))
  
  list(
    fig_lp_tier     = fig_lp_tier,
    fig_lp_baseline = fig_lp_baseline,
    lp_by_tier      = lp_by_tier,
    lp_by_baseline  = lp_by_baseline
  )
}