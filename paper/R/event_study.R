# event_study.R
# Section 4.1: Event study trajectory charts
# One chart per tier, three baseline lines per chart

library(tidyverse)
library(scales)
library(here)

source(here::here("paper", "R", "theme.R"))

run_event_study <- function(baselines, output_dir) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  PRE  <- PRE_EVENT_MONTHS
  POST <- POST_EVENT_MONTHS
  x_breaks <- c(-PRE:-1, 0, 1:POST)
  x_labels <- c(paste0("T", -PRE:-1), "T0", paste0("T+", 1:POST))
  
  # Combine monthly deviations across all baselines
  monthly_all <- bind_rows(lapply(baselines, function(bl) {
    bl$monthly_dev |>
      mutate(baseline = bl$baseline)
  })) |>
    filter(!is.na(baseline))
  
  # ---- Chart function per tier ----
  make_tier_chart <- function(data, tier_name, tier_label) {
    data |>
      filter(tier == tier_name) |>
      group_by(baseline, month_t) |>
      summarise(
        mean_dev = mean(deviation, na.rm = TRUE),
        se_dev   = sd(deviation, na.rm = TRUE) / sqrt(n()),
        .groups  = "drop"
      ) |>
      mutate(baseline = factor(baseline, 
                               levels = c("Regional", "Second Ring (R2)", "First Ring (R1)"))) |>
      ggplot(aes(x = month_t, y = mean_dev, color = baseline, fill = baseline)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.5) +
      geom_ribbon(aes(ymin = mean_dev - 1.96 * se_dev,
                      ymax = mean_dev + 1.96 * se_dev),
                  alpha = 0.15, color = NA) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels) +
      scale_y_continuous(labels = number_format(accuracy = 0.01)) +
      scale_color_manual(values = BASELINE_COLORS) +
      scale_fill_manual(values  = BASELINE_COLORS) +
      annotate("text", x = -0.3, y = Inf, label = "Storm",
               hjust = 1, vjust = 1.5, size = 3, color = "grey40", fontface = "italic") +
      labs(
        title    = sprintf("Post-Storm ZHVI Trajectory - %s Tier", tier_label),
        subtitle = "Mean indexed deviation by baseline method. Shaded = 95% CI.",
        x        = "Months Post-Storm",
        y        = "Mean Indexed Deviation (index points)",
        color    = "Baseline",
        fill     = "Baseline"
      ) +
      theme_paper
  }
  
  fig_es_mid    <- make_tier_chart(monthly_all, "mid",    "Mid")
  fig_es_bottom <- make_tier_chart(monthly_all, "bottom", "Bottom")
  fig_es_top    <- make_tier_chart(monthly_all, "top",    "Top")
  
  # ---- Save ----
  ggsave(file.path(output_dir, "fig_es_mid.pdf"),    fig_es_mid,    width = 6.5, height = 3.85)
  ggsave(file.path(output_dir, "fig_es_bottom.pdf"), fig_es_bottom, width = 6.5, height = 3.85)
  ggsave(file.path(output_dir, "fig_es_top.pdf"),    fig_es_top,    width = 6.5, height = 3.85)
  
  cat(sprintf("Event study figures saved to %s\n", output_dir))
  
  list(
    fig_es_mid    = fig_es_mid,
    fig_es_bottom = fig_es_bottom,
    fig_es_top    = fig_es_top
  )
}