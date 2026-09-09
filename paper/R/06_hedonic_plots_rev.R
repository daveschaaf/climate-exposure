# 06_hedonic_plots.R
# Additional reporting visuals for the cross-sectional hedonic study.
# Source of truth for the analysis: analysis/hedonic_xsec_zhvi_window_full.Rmd
#
# This module mirrors the run_*(args, output_dir) pattern used by event_study.R
# and balance.R: it takes the fitted models + data already built in the Rmd and
# returns a named list of ggplot objects, while also writing PDFs to output_dir.
#
# Usage from analysis/hedonic_xsec_zhvi_window_full.Rmd (after the fit chunks):
# source(here::here("paper", "R", "06_hedonic_plots_rev.R"))
# hed_plots <- run_hedonic_plots(
#   df       = df,        # panel analytic frame
#   xsec_df  = xsec1_df,   # vintage-collapsed cross-section
#   models   = list(
#     mC      = mC,                       # panel main spec (Spec C)
#     mE      = mE,                       # panel, income removed
#     xs_B    = xs_B,                     # cross-section main (analog of Spec C)
#     xs_2020 = xs_2020, xs_2021 = xs_2021,
#     xs_2023 = xs_2023, xs_2025 = xs_2025
#   ),
#   output_dir = here::here("paper", "output", "hedonic")
# )
#
# Requires: dplyr, tidyr, ggplot2, scales, fixest, here. tigris + sf only for the
# optional residual map (guarded so the rest renders without them).

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(fixest)
library(here)

# ---- Shared visual constants -------------------------------------------------
# Reuse the project theme
source(here::here("paper", "R", "theme.R"))


HED_COL_SIG    <- "#1a5490"   # significant (deep blue)
HED_COL_NULL   <- "grey60"    # not significant
HED_COL_NEG    <- "#b2182b"   # price discount / negative
HED_COL_POS    <- "#2166ac"   # premium / positive

# Human-readable labels for model terms (single source of truth)
HED_TERM_LABELS <- c(
  log_eal         = "EAL (log)",
  log_crf         = "Community risk factor (log)",
  resl_value_z    = "Resilience (1-SD)",
  sovi_score_z    = "SoVI (1-SD)",
  log_income      = "Median income (log)",
  log_pop_density = "Pop. density (log)",
  coastal         = "Coastal county"
)

# Tidy a fixest model into a coefficient frame with 95% CIs and significance flag
hed_tidy <- function(model, model_label = NA_character_) {
  est <- coef(model)
  ses <- se(model)
  data.frame(
    term      = names(est),
    estimate  = as.numeric(est),
    std.error = as.numeric(ses[names(est)]),
    model     = model_label,
    row.names = NULL,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value   = 2 * (1 - pnorm(abs(estimate / std.error))),
      sig       = ifelse(p.value < 0.05, "Significant", "Not significant"),
      label     = ifelse(term %in% names(HED_TERM_LABELS),
                         HED_TERM_LABELS[term], term)
    )
}

# ==============================================================================
run_hedonic_plots <- function(df, xsec_df, models, output_dir) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  figs <- list()

  # ---------------------------------------------------------------------------
  # FIGURE 1 - Cross section coefficient forest plot (headline)
  # Uses the main # cross-sectional spec xs_B (analog of panel Spec C). 
  # ---------------------------------------------------------------------------
  coef_df <- hed_tidy(models$xs_B) %>%
    filter(term %in% names(HED_TERM_LABELS))

  figs$coefficient_xsec_plot <- ggplot(
    coef_df, aes(x = estimate, y = reorder(label, estimate), color = sig)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.6) +
    scale_color_manual(values = c("Significant" = HED_COL_SIG,
                                  "Not significant" = HED_COL_NULL)) +
    scale_x_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title    = "What the Property Market Prices: Hedonic Coefficients on log(ZHVI)",
      subtitle = "Vintage-collapsed cross-section (Spec xs_B); state + vintage FE; county-clustered SEs. Bars = 95% CI.",
      x        = "Coefficient  (≈ % change in ZHVI)",
      y        = NULL, color = NULL
    ) +
    theme_paper

  # ---------------------------------------------------------------------------
  # FIGURE 2 - Minimum detectable effect vs. estimate
  # Plots |coefficient| against its 80% / 90% MDE
  # threshold for the three climate terms. EAL falls short of detectability while
  # resilience and SoVI clear the bar.
  # MDE_80 = 2.8 * SE, MDE_90 = 3.24 * SE (alpha = 0.05 two-sided).
  # ---------------------------------------------------------------------------
  mde_terms <- c("log_eal", "resl_value_z", "sovi_score_z")
  mde_src   <- models$xs_B
  mde_df <- hed_tidy(mde_src) %>%
    filter(term %in% mde_terms) %>%
    transmute(
      label    = factor(label, levels = HED_TERM_LABELS[mde_terms]),
      abs_beta = abs(estimate),
      mde_80   = 2.8  * std.error,
      mde_90   = 3.24 * std.error,
      detectable = ifelse(abs_beta >= mde_80, "Detectable", "Below MDE")
    )

  figs$mde_plot <- ggplot(mde_df, aes(y = label)) +
    # MDE thresholds as a span from 80% to 90% power
    geom_segment(aes(x = mde_80, xend = mde_90, yend = label),
                 color = "grey70", linewidth = 6, alpha = 0.5) +
    geom_point(aes(x = mde_80), shape = "|", size = 6, color = "grey45") +
    geom_point(aes(x = mde_90), shape = "|", size = 6, color = "grey45") +
    geom_point(aes(x = abs_beta, color = detectable), size = 4) +
    scale_color_manual(values = c("Detectable" = HED_COL_SIG,
                                  "Below MDE"  = HED_COL_NEG)) +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    labs(
      title    = "Estimate vs. Minimum Detectable Effect",
      subtitle = stringr::str_wrap("Grey band = 80%–90% power MDE (2.8–3.24 × SE). Dot = |estimate|. EAL falls short of detectability", width = 60),
      x = "Absolute coefficient magnitude", y = NULL, color = NULL
    ) + theme_paper

  # ---------------------------------------------------------------------------
  # FIGURE 3 - Panel vs. cross-section robustness
  # Shows the three climate terms hold sign and rough magnitude when moving
  # from the ~189k county-month panel (Spec C) to the ~9k vintage cross-section
  # (xs_B). Stable estimates => panel findings aren't artifacts of
  # time-aggregation. Wider CIs on the cross-section are expected (smaller n).
  # ---------------------------------------------------------------------------
  cmp_terms <- c("log_eal", "resl_value_z", "sovi_score_z")
  cmp_df <- bind_rows(
    hed_tidy(models$mC,   "Panel (Spec C, ~189k obs)"),
    hed_tidy(models$xs_B, "Cross-section (xs_B, ~9k obs)")
  ) %>%
    filter(term %in% cmp_terms) %>%
    mutate(
      label = factor(label, levels = HED_TERM_LABELS[cmp_terms]),
      model = factor(model, levels = c("Panel (Spec C, ~189k obs)",
                                       "Cross-section (xs_B, ~9k obs)"))
    )

  figs$panel_vs_xsec_plot <- ggplot(
    cmp_df, aes(x = estimate, y = label, color = model)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(0.5), size = 0.5) +
    scale_color_manual(values = c("Panel (Spec C, ~189k obs)" = HED_COL_SIG,
                                  "Cross-section (xs_B, ~9k obs)" = "#e08214")) +
    scale_x_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title    = "Panel vs Cross Section Consistency",
      subtitle = stringr::str_wrap("Same sign and rough magnitude across specifications; cross-section CIs widen as n drops. Bars = 95% CI.", width = 60),
      x = "Coefficient  (≈ % change in ZHVI)", y = NULL, color = NULL
    ) + theme_paper

  # ---------------------------------------------------------------------------
  # FIGURE 4 - log(EAL) coefficient by single NRI vintage
  # The pooled EAL coefficient is a null, but the
  # single-vintage cuts reveal a negative, significant EAL price discount in the
  # earlier vintages (2020/2021) that fades to noise by 2023/2025. The dashed
  # line is the ~ -0.017 implied-discount-rate benchmark (5% perpetuity
  # capitalization of modeled EAL) from the Rmd's calibration section.
  # ---------------------------------------------------------------------------
  vint_specs <- list(
    "2020" = models$xs_2020, "2021" = models$xs_2021,
    "2023" = models$xs_2023, "2025" = models$xs_2025
  )
  vint_specs <- vint_specs[!vapply(vint_specs, is.null, logical(1))]

  eal_by_vintage <- bind_rows(lapply(names(vint_specs), function(v) {
    m <- vint_specs[[v]]
    if (!("log_eal" %in% names(coef(m)))) return(NULL)
    hed_tidy(m, v) %>% filter(term == "log_eal") %>% mutate(vintage = v)
  }))

  if (nrow(eal_by_vintage) > 0) {
    IMPLIED_BENCHMARK <- -0.017  # 5% perpetuity capitalization reference (see Rmd §9)
    figs$eal_by_vintage_plot <- ggplot(
      eal_by_vintage, aes(x = vintage, y = estimate, color = sig)
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_hline(yintercept = IMPLIED_BENCHMARK, linetype = "dotted",
                 color = HED_COL_NEG) +
      annotate("text", x = Inf, y = IMPLIED_BENCHMARK, hjust = 1.05, vjust = -0.6,
               size = 3, color = HED_COL_NEG, fontface = "italic",
               label = "Implied full-capitalization benchmark (≈ -0.017)") +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.7) +
      scale_color_manual(values = c("Significant" = HED_COL_NEG,
                                    "Not significant" = HED_COL_NULL)) +
      labs(
        title    = "The EAL discount was real before 2023, then faded to noise",
        subtitle = "log(EAL) coefficient by single NRI vintage cross-section (state FE). Bars = 95% CI.",
        x        = "NRI vintage", y = "log(EAL) coefficient  (≈ % change in ZHVI)",
        color    = NULL
      ) +
      theme_paper
  }

  # ---------------------------------------------------------------------------
  # FIGURE 5 - Partial-relationship binscatter: EAL vs. ZHVI net of controls
  # Residualize both log(ZHVI) and log(EAL) on the controls + FE, then bin. A
  # near-flat cloud is the visual counterpart of the EAL null - it shows the
  # reader *why* the coefficient is ~0 rather than asking them to trust a table.
  # Uses the cross-section so one dot is one county-vintage.
  # ---------------------------------------------------------------------------
  resid_ok <- all(c("log_zhvi", "log_eal", "log_income", "log_pop_density",
                    "coastal", "state", "nri_vintage") %in% names(xsec_df))
  if (resid_ok) {
    rx <- xsec_df %>% filter(is.finite(log_zhvi), is.finite(log_eal))
    rx$.zhvi_resid <- residuals(
      feols(log_zhvi ~ log_income + log_pop_density + coastal | state + nri_vintage, data = rx)
    )
    rx$.eal_resid <- residuals(
      feols(log_eal ~ log_income + log_pop_density + coastal | state + nri_vintage, data = rx)
    )
    nbin <- 30
    bin_df <- rx %>%
      mutate(bin = ntile(.eal_resid, nbin)) %>%
      group_by(bin) %>%
      summarise(x = mean(.eal_resid), y = mean(.zhvi_resid), .groups = "drop")

    figs$partial_residual_plot <- ggplot(bin_df, aes(x = x, y = y)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_point(color = HED_COL_SIG, size = 2) +
      geom_smooth(data = rx, aes(x = .eal_resid, y = .zhvi_resid),
                  method = "lm", se = TRUE, color = HED_COL_NEG,
                  linewidth = 0.8, inherit.aes = FALSE) +
      labs(
        title    = "Net of income, density, coastal and fixed effects, EAL barely tilts ZHVI",
        subtitle = "Binned partial-residual scatter (30 bins) in the vintage cross-section. Flat slope = the EAL null, visualized.",
        x        = "log(EAL), residualized on controls + FE",
        y        = "log(ZHVI), residualized on controls + FE"
      ) +
      theme_paper
  }

  # ---------------------------------------------------------------------------
  # FIGURE 6 - County residual map
  # Where the model over-/under-predicts ZHVI relative to its state. tigris/sf 
  # dependency.
  # ---------------------------------------------------------------------------
  has_spatial <- requireNamespace("tigris", quietly = TRUE) &&
                 requireNamespace("sf", quietly = TRUE)
  if (has_spatial && all(c("stcofips") %in% names(xsec_df))) {
    options(tigris_use_cache = TRUE)
    mC <- models$mC
    df_used <- df[fixest::obs(mC), ]
    df_used$.resid <- residuals(mC)
    county_resid <- df_used %>%
      group_by(stcofips) %>%
      summarise(mean_resid = mean(.resid, na.rm = TRUE), n_obs = n(), .groups = "drop") %>%
      filter(n_obs >= 12)

    counties_sf <- tigris::counties(cb = TRUE, resolution = "20m", year = 2021)
    counties_sf <- counties_sf[!counties_sf$STATEFP %in%
                                 c("02", "15", "60", "66", "69", "72", "78"), ]
    county_resid$stcofips <- trimws(as.character(county_resid$stcofips))
    counties_sf$GEOID     <- trimws(as.character(counties_sf$GEOID))

    map_df <- dplyr::left_join(counties_sf, county_resid,
                               by = c("GEOID" = "stcofips"))

    figs$residual_map <- ggplot(map_df) +
      geom_sf(aes(fill = mean_resid), color = NA) +
      scale_fill_gradient2(
        low = HED_COL_NEG, mid = "white", high = HED_COL_POS,
        midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish,
        labels = scales::percent_format(), na.value = "grey85",
        name = "Mean residual"
      ) +
      labs(
        title    = "Where the hedonic over- and under-predicts home values (vs. state average)",
        subtitle = "Blue = market price exceeds the NRI-and-controls prediction; red = falls short."
      ) +
      theme_void() +
      theme(legend.position = "right")
  } else {
    message("06_hedonic_plots.R: tigris/sf not available - skipping residual_map.")
  }

  # ---------------------------------------------------------------------------
  # Save all figures present
  # ---------------------------------------------------------------------------
  sizes <- list(
    coefficient_xsec_plot     = c(8, 5),
    mde_plot             = c(6.5, 3.25),
    panel_vs_xsec_plot   = c(6.5, 3.25),
    eal_by_vintage_plot  = c(8, 5),
    partial_residual_plot= c(8, 5),
    residual_map         = c(10, 7)
  )
  for (nm in names(figs)) {
    wh <- sizes[[nm]]; if (is.null(wh)) wh <- c(8, 5)
    ggsave(file.path(output_dir, paste0("hedonic_", nm, ".pdf")),
           figs[[nm]], width = wh[1], height = wh[2])
  }
  cat(sprintf("Hedonic figures saved to %s (%d figures)\n",
              output_dir, length(figs)))

  invisible(figs)
}
# ==============================================================================
# REPORTING HEDONIC ALONGSIDE THE EVENT STUDY- Update this with ES data
# ==============================================================================
# The two analyses answer different questions and the report is strongest when
# they're shown together: the hedonic asks "does the cross-section of prices
# *level* reflect modeled risk?" and the event study asks "do prices *update*
# when risk is realized via a storm?" Both land on the same headline - NRI/EAL
# carries little to no pricing signal - which is far more convincing as a
# convergence of two independent designs than either result alone.
#
# SUGGESTED JOINT VISUALS (highest value first):
#
#   1. "Two tests, one verdict" side-by-side null panel.  [BUILT BELOW]
#      Left: hedonic log(EAL) coefficient on price level (this module).
#      Right: event-study post-storm cumulative ZHVI deviation for the high-EAL
#      tier (from event_study.R). Shared zero line. Visually states that NRI
#      neither prices into the level nor moves prices after an event.
#
#   2. Methods-comparison forest plot.  [BUILT BELOW]
#      One row per climate construct (EAL, resilience, SoVI), two estimates per
#      row: hedonic level effect vs. event-study post-storm effect, on a shared
#      standardized axis. Makes "significant in the cross-section, noise in the
#      event study" legible at a glance (resilience/SoVI are the clean example).
#
#   3. Design-tradeoff schematic (static, build in slides/Quarto, not ggplot).
#      A 2x2 of amenity-confounded vs. amenity-purged x level vs. update. Places
#      the hedonic (amenity-confounded, level) and the event study
#      (amenity-purged, update) in complementary quadrants; motivates why both
#      are needed. Better as a drawn figure than a data plot.
#
#   4. Sample/precision context bar.  Annotate the n collapse (~189k panel
#      -> ~9k cross-section -> ~3k single vintage) next to the event-study
#      effective n, so readers don't over-read widening CIs as disagreement.
#
# The functions below build #1 and #2. They take already-summarized inputs so
# this module stays decoupled from the event-study data pipeline; pass the
# event-study summary frame produced by run_event_study()/lp_did.R.

# ---- Joint Figure A: "Two tests, one verdict" --------------------------------
# es_summary: data.frame with at least columns
#   month_t (int, event time), mean_dev (num), se_dev (num)
#   for the high-EAL / high-risk tier you want to feature.
make_two_tests_panel <- function(models, es_summary, output_dir = NULL) {
  stopifnot(all(c("month_t", "mean_dev", "se_dev") %in% names(es_summary)))

  # Left: hedonic EAL coefficient (cross-section main spec)
  eal <- hed_tidy(models$xs_B) %>% filter(term == "log_eal")
  p_left <- ggplot(eal, aes(x = "log(EAL)", y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.8) +
    scale_color_manual(values = c("Significant" = HED_COL_NEG,
                                  "Not significant" = HED_COL_NULL)) +
    labs(title = "Hedonic: price level", subtitle = "Does EAL discount the cross-section of prices?",
         x = NULL, y = "Coefficient on log(ZHVI)", color = NULL) +
    theme_paper

  # Right: event-study post-storm trajectory
  p_right <- ggplot(es_summary, aes(x = month_t, y = mean_dev)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    geom_ribbon(aes(ymin = mean_dev - 1.96 * se_dev,
                    ymax = mean_dev + 1.96 * se_dev),
                alpha = 0.15, fill = HED_COL_SIG) +
    geom_line(color = HED_COL_SIG, linewidth = 0.9) +
    geom_point(color = HED_COL_SIG, size = 2) +
    labs(title = "Event study: price update", subtitle = "Do prices move after a storm hits a high-risk county?",
         x = "Months relative to storm", y = "Mean indexed ZHVI deviation") +
    theme_paper

  # Combine if patchwork is available; otherwise return the pieces.
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- patchwork::wrap_plots(p_left, p_right, widths = c(1, 2)) +
      patchwork::plot_annotation(
        title = "Two independent tests, one verdict: NRI carries little property-market signal"
      )
    if (!is.null(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      ggsave(file.path(output_dir, "joint_two_tests_one_verdict.pdf"),
             combined, width = 11, height = 5)
    }
    return(invisible(combined))
  }
  message("make_two_tests_panel: patchwork not installed; returning list(left, right).")
  invisible(list(left = p_left, right = p_right))
}

# ---- Joint Figure B: methods-comparison forest ------------------------------
# es_effects: data.frame with columns term, estimate, conf.low, conf.high for
#   the event-study estimates of the same constructs (standardized to compare).
make_methods_comparison <- function(models, es_effects, output_dir = NULL) {
  keep <- c("log_eal", "resl_value_z", "sovi_score_z")
  hed_part <- hed_tidy(models$xs_B) %>%
    filter(term %in% keep) %>%
    transmute(term, label, estimate, conf.low, conf.high, method = "Hedonic (price level)")
  es_part <- es_effects %>%
    filter(term %in% keep) %>%
    mutate(label = HED_TERM_LABELS[term], method = "Event study (post-storm)") %>%
    select(term, label, estimate, conf.low, conf.high, method)

  comp <- bind_rows(hed_part, es_part) %>%
    mutate(label = factor(label, levels = HED_TERM_LABELS[keep]))

  p <- ggplot(comp, aes(x = estimate, y = label, color = method)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(0.5), size = 0.5) +
    scale_color_manual(values = c("Hedonic (price level)" = HED_COL_SIG,
                                  "Event study (post-storm)" = "#e08214")) +
    labs(
      title    = "Same constructs, two methods: where the cross-section and the event study agree",
      subtitle = "Standardized effects. Terms significant in the level test often turn to noise post-storm. Bars = 95% CI.",
      x        = "Standardized coefficient", y = NULL, color = NULL
    ) +
    theme_paper

  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(output_dir, "joint_methods_comparison.pdf"), p,
           width = 9, height = 5)
  }
  invisible(p)
}
# Edit this to incorporate Event Study Dataframes
# source(here::here("paper", "R", "06_hedonic_plots_rev.R"))
# joint_plot_A <- make_two_tests_panel(
#   df       = df, # panel analytic frame
#   # es_summary =
#   xsec_df  = xsec_df,   # vintage-collapsed cross-section
#   models   = list(
#     mC      = mC,                       # panel main spec (Spec C)
#     mE      = mE,                       # panel, income removed
#     xs_B    = xs_B,                     # cross-section main (analog of Spec C)
#     xs_2020 = xs_2020, xs_2021 = xs_2021,
#     xs_2023 = xs_2023, xs_2025 = xs_2025
#   ),
#   output_dir = here::here("paper", "output", "hedonic")
# )
