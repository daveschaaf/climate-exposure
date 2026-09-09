# theme.R
# Shared visual constants for all charts

# Event months
PRE_EVENT_MONTHS  <- 3
POST_EVENT_MONTHS <- 9

BASELINE_LEVEL1_PATH   <- paste0("data/processed/nlevel1_nmin1_pre", PRE_EVENT_MONTHS, "_post", POST_EVENT_MONTHS)
BASELINE_LEVEL2_PATH   <- paste0("data/processed/nlevel2_nmin2_pre", PRE_EVENT_MONTHS, "_post", POST_EVENT_MONTHS)
BASELINE_REGIONAL_PATH <- paste0("data/processed/nlevelr_nmin2_pre", PRE_EVENT_MONTHS, "_post", POST_EVENT_MONTHS)

library(ggplot2)

# ---- Color palettes ----
TIER_COLORS <- c(
  "Bottom" = "#e41a1c",
  "Mid"    = "#4daf4a",
  "Top"    = "#984ea3"
)

TIER_COLORS_LOWER <- c(
  "bottom" = "#e41a1c",
  "mid"    = "#4daf4a",
  "top"    = "#984ea3"
)

TIER_LABELS <- c(
  "bottom" = "Bottom",
  "mid"    = "Mid",
  "top"    = "Top"
)

BASELINE_COLORS <- c(
  "Regional"         = "#fc8d62",
  "Second Ring (R2)" = "#8da0cb",
  "First Ring (R1)"    = "#66c2a5"
)

# ---- Shared theme ----
theme_paper <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(size = 14, face = "bold"),
    plot.subtitle    = element_text(size = 11,  color = "grey40",
                                    margin = margin(b = 6)),
    plot.caption     = element_text(size = 9,  color = "grey40", hjust = 0),
    # Align title/subtitle to the panel, not the whole plot: keeps them out
    # of the rotated y-axis title's column so long y titles can't overlap them.
    plot.title.position   = "panel",
    plot.caption.position = "plot",
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    axis.text.x      = element_text(size = 9),
    strip.text       = element_text(size = 10, face = "bold"),
    legend.title     = element_text(size = 9),
    legend.text      = element_text(size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )
