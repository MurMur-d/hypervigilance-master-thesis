# ============================================================
# File: R/plotting/mechanism_env/03_plot_mechanism_risk_autocorr.R
#
# Purpose: Helpers for the mechanism risk (SSR) and autocorrelation figures (Fig4D).
# Notes:
#   - Builds the mechanism-colored tiles for SSR and autocorrelation.
#   - Provides legend helpers + labeling utilities used across the panel scripts.
# ============================================================

# ---- Dependencies ----------------------------------------------------------
# These are *plot-facing* dependencies: we expect the upstream pipeline to have
# produced â€œmechanism-readyâ€ data frames (e.g., with HV totals, preventative share,
# spillover share, etc.), and we only transform them into a consistent plot layout.
source("R/core/plot_utils.R")              # theme_vigilance(), scale_fill_grey01(), save_graphs(), etc.
source("R/helpers/utils_model_scenarios.R")# default_env_model_scenarios, label conventions
source("R/plotting/_shared/utils_subtitles.R")     # subtitle helpers (h0 notes, reward notes, etc.)
source("R/plotting/mechanism_env/03_plot_mechanism_env.R")      # prepare_mechanism_df(), mechanism_color_map(), etc.

suppressPackageStartupMessages({
  library(dplyr)      # data munging (mutate, filter, summarise)
  library(ggplot2)    # plotting
  library(patchwork)  # layout composition (plot1 / plot2, wrap_plots)
})

# -----------------------------------------------------------------------------
# safe_seq_step()
# -----------------------------------------------------------------------------
# Helper for robust axis tick spacing when K/SSR/autocorr values are "almost"
# evenly spaced but may contain:
#   - a single unique value (no diffs)
#   - non-finite entries
#   - floating point jitter (diffs extremely small)
#
# We compute the *minimum meaningful positive* step size in `values`.
# If we canâ€™t, we fall back to a sensible default (e.g. 0.1).
safe_seq_step <- function(values, fallback = 0.1) {
  unique_vals <- sort(unique(values))                    # de-duplicate and sort
  unique_vals <- unique_vals[is.finite(unique_vals)]     # drop NA/Inf/-Inf
  if (length(unique_vals) <= 1) return(fallback)         # no spacing if only one value

  diffs <- diff(unique_vals)                             # successive differences
  # Drop â€œnumerical noiseâ€ differences (tiny diffs caused by float rounding)
  diffs <- diffs[diffs > .Machine$double.eps * 100]
  if (length(diffs) == 0) return(fallback)               # nothing meaningful left

  step <- min(diffs, na.rm = TRUE)                       # smallest actual step
  if (!is.finite(step) || step <= 0) return(fallback)    # guard
  step
}

# -----------------------------------------------------------------------------
# build_mechanism_legend()
# -----------------------------------------------------------------------------
# We create a *separate* plot that acts as the legend for â€œmechanism tilesâ€.
#
# Mechanism tiles encode two quantities into a single colour:
#   1) hv_value      : total hypervigilance magnitude (vertical axis of the key)
#   2) prevent_share : composition of that HV (horizontal axis of the key)
#        - prevent_share ~ 0  => mostly spillover (blue-ish)
#        - prevent_share ~ 1  => mostly preventative (red-ish)
#
# The colour mapping itself lives in `mechanism_color_map()` (from plot_mechanism.R).
# This legend is drawn using scale_fill_identity() because `mechanism_color_map()`
# returns final hex colours; we are not asking ggplot to compute a gradient.
#' Build the color key explaining the mechanism gradient used by SSR/autocorr tiles.
build_mechanism_legend <- function() {
  # Build a dense grid of â€œ(prevent_share, hv_value)â€ pairs so the key looks smooth.
  legend_grid <- expand.grid(
    prevent_share  = seq(0, 1, length.out = 60),   # x-axis: composition
    hv_value       = seq(0, 1, length.out = 60),   # y-axis: magnitude
    KEEP.OUT.ATTRS = FALSE
  )

  # Map each (prevent_share, hv_value) pair to a final display colour.
  legend_grid$color <- mechanism_color_map(
    legend_grid$prevent_share,
    legend_grid$hv_value
  )

  ggplot(legend_grid, aes(x = prevent_share, y = hv_value, fill = color)) +
    geom_tile() +
    scale_fill_identity() +                          # interpret `fill` values as colours directly
    labs(
      x = "proportion preventative\n(blue = spillover, red = preventative)",
      y = "total hypervigilance",
      title = "Mechanism key"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.title = element_text(face = "bold", size = 8),
      axis.text = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 9),
      panel.grid = element_blank(),
      plot.margin = margin(t = 0, r = 2, b = 2, l = 2)
    )
}

# -----------------------------------------------------------------------------
# wrap_model_labels()
# -----------------------------------------------------------------------------
# Facet strip labels often become too wide (especially with parameter notes
# like â€œ(Î² = 0)â€ or â€œ(Ï„ = 30)â€). This helper inserts a newline before the first
# parenthesis so strips wrap nicely:
#   "Health (Î² = 0)" -> "Health\n(Î² = 0)"
wrap_model_labels <- function(lbl) {
  gsub(" \\(", "\n(", as.character(lbl), fixed = FALSE)
}

# -----------------------------------------------------------------------------
# build_model_variant_title()
# -----------------------------------------------------------------------------
# In several figures you want a *left-side title/gutter* that reads â€œmodel variantâ€.
# Instead of hacking facet strip settings, you build a tiny stand-alone plot
# that contains only a centered label, then stack it above the actual panels.
#
# This keeps typography consistent across patchwork layouts.
build_model_variant_title <- function(label = "model variant") {
  ggplot() +
    annotate(
      "text", x = 0.5, y = 0.5,
      label = label,
      fontface = "bold",
      size = 3.2
    ) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 1, l = 0))
}

# -----------------------------------------------------------------------------
# plot_K_vs_SSR_mechanism_by_model()
# -----------------------------------------------------------------------------
# Main â€œSSR vs costâ€ plot:
#   - X axis: vigilance cost K
#   - Y axis: SSR risk measure
#   - Fill:  mechanism colour (encodes magnitude + composition)
#   - Facets: model variants (columns)
#
# Inputs:
#   df_models should already include per-tile HV metrics and mechanism shares.
#   We call prepare_mechanism_df() to standardize column names and compute:
#     - hypervigilance (total)
#     - preventative/spillover shares
#     - mechanism_fill (final colour)
#
# Layout:
#   A vertical patchwork stack:
#     1) â€œmodel variantâ€ header line
#     2) tile panels (faceted)
#     3) mechanism legend key (optional)
#' Plot SSR mechanism tiles faceted by model variant.
#'
#' @param df_models Data frame produced by `prepare_mechanism_df()`.
#' @param subtitle_context Optional subtitle string.
#' @param show_legend Logical, include mechanism legend below the panels.
#' @return Patchwork layout combining the tiles + legend.
plot_K_vs_SSR_mechanism_by_model <- function(
    df_models,
    subtitle_context = NULL,
    show_legend = TRUE
) {
  if (nrow(df_models) == 0) stop("df_models is empty")

  # Standardize and enrich the incoming data for mechanism plotting.
  # prepare_mechanism_df() is the *central contract*:
  # it ensures we end up with a `mechanism_fill` colour per row.
  df_plot <- prepare_mechanism_df(df_models)

  # Make facet strips compact and readable.
  df_plot$model_label <- wrap_model_labels(df_plot$model_label)

  # Defensive: older inputs might not carry SSR; if missing, plot a degenerate row.
  if (!"SSR" %in% names(df_plot)) df_plot$SSR <- 0

  # Determine axis tick breaks from observed values.
  # We explicitly compute min/max and step size so the grid feels â€œalignedâ€
  # across models, and to avoid ggplot choosing unhelpful breaks.
  x_vals <- sort(unique(df_plot$K))
  x_vals <- x_vals[is.finite(x_vals)]
  if (length(x_vals) == 0) x_vals <- c(0, 1)

  y_vals <- sort(unique(df_plot$SSR))
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) y_vals <- c(0, 0.1)

  x_step <- if (length(x_vals) > 1) min(diff(x_vals)) else 1
  y_step <- if (length(y_vals) > 1) min(diff(y_vals)) else 0.1

  xmin <- min(x_vals, na.rm = TRUE)
  xmax <- max(x_vals, na.rm = TRUE)
  ymin <- min(y_vals, na.rm = TRUE)
  ymax <- max(y_vals, na.rm = TRUE)

  # Build the main tile plot.
  # Note: scale_fill_identity(guide="none") because `mechanism_fill` already stores colours.
  base <- ggplot(df_plot, aes(x = K, y = SSR, fill = mechanism_fill)) +
    geom_tile() +
    scale_fill_identity(guide = "none") +
    facet_grid(cols = vars(model_label)) +
    labs(
      x = "vigilance cost (K)",
      y = expression(risk~(SSR))
    ) +
    theme_vigilance(base_size = 11, strip_size = 9) +
    scale_x_continuous(breaks = seq(xmin, xmax, by = x_step), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(ymin, ymax, by = y_step), expand = c(0, 0)) +
    theme(
      strip.text.x = element_text(face = "bold", size = 8),
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 4)),
      panel.spacing = grid::unit(0.2, "lines"),
      aspect.ratio = 1,
      plot.margin = margin(t = 2, r = 4, b = 0, l = 4),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )

  # A small header strip that labels the facet dimension.
  model_title <- build_model_variant_title("model variant")

  # Compose final layout: header + base + (optional) legend.
  combined <- if (show_legend) {
    patchwork::wrap_plots(
      list(model_title, base, build_mechanism_legend()),
      ncol = 1,
      heights = c(0.05, 0.70, 0.25)  # allocate space to the legend key
    )
  } else {
    patchwork::wrap_plots(
      list(model_title, base),
      ncol = 1,
      heights = c(0.05, 0.95)
    )
  }

  # Add an overall title/subtitle (shared across the patchwork object).
  combined + plot_annotation(
    title = "Proportion of hypervigilance across risk (SSR)",
    subtitle = subtitle_context,
    theme = theme_vigilance(base_size = 11) +
      theme(
        plot.margin = margin(t = 2, r = 4, b = 2, l = 4),
        plot.title = element_text(size = 14, face = "bold", margin = margin(b = 1)),
        plot.subtitle = element_text(size = 10, margin = margin(b = 1))
      )
  )
}

# -----------------------------------------------------------------------------
# plot_K_vs_SSR_split_mechanism_by_model()
# -----------------------------------------------------------------------------
# Decomposes the mechanism into *two separate grey-scale heatmaps*:
#   - preventative_rate panel
#   - spillover_rate panel
#
# This is useful when you want to show *how much* of HV comes from each component,
# rather than compressing both components into a single colour.
#
# `orientation` controls whether panels are stacked (vertical) or side-by-side.
#' Plot SSR split bars (preventative/spillover) faceted by model variant.
plot_K_vs_SSR_split_mechanism_by_model <- function(
    df_models,
    orientation = c("vertical", "horizontal"),
    subtitle_context = NULL,
    show_legend = TRUE
) {
  orientation <- match.arg(orientation)
  stopifnot(nrow(df_models) > 0)

  # Normalize columns + compute the preventative/spillover decomposition.
  df_plot <- prepare_mechanism_df(df_models)
  df_plot$model_label <- wrap_model_labels(df_plot$model_label)
  if (!"SSR" %in% names(df_plot)) df_plot$SSR <- 0

  # Robust axis ranges and step sizes.
  x_vals <- sort(unique(df_plot$K))
  x_vals <- x_vals[is.finite(x_vals)]
  if (length(x_vals) == 0) x_vals <- c(0, 1)

  y_vals <- sort(unique(df_plot$SSR))
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) y_vals <- c(0, 0.1)
  x_step <- if (length(x_vals) > 1) min(diff(x_vals)) else 1
  y_step <- if (length(y_vals) > 1) min(diff(y_vals)) else 0.1

  xmin <- min(x_vals, na.rm = TRUE)
  xmax <- max(x_vals, na.rm = TRUE)
  ymin <- min(y_vals, na.rm = TRUE)
  ymax <- max(y_vals, na.rm = TRUE)

  # Local helper to keep preventative/spillover panels visually identical
  # except for the `value` they map to fill.
  build_panel <- function(df, title_suffix) {
    ggplot(df, aes(x = K, y = SSR, fill = value)) +
      geom_tile() +
      scale_fill_grey01(title = "hypervigilance") +   # shared grey scale (from plot_utils)
      guides(fill = if (show_legend) "legend" else "none") +
      facet_grid(cols = vars(model_label)) +
      labs(
        x = "vigilance cost (K)",
        y = expression(risk~(SSR)),
        title = title_suffix
      ) +
      theme_vigilance(base_size = 10, strip_size = 9) +
      scale_x_continuous(breaks = seq(xmin, xmax, by = x_step), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(ymin, ymax, by = y_step), expand = c(0, 0)) +
      theme(
        strip.text.x = element_text(face = "bold", size = 8),
        axis.text = element_text(size = 7),
        axis.title.y = element_text(margin = margin(r = 4)),
        plot.title = element_text(size = 10, face = "bold"),
        panel.spacing = grid::unit(0.2, "lines"),
        aspect.ratio = 1,
        plot.margin = margin(2, 4, 1, 4)
      )
  }

  # Create two copies of the same df, each exposing a different fill value.
  prev_df <- df_plot %>% mutate(value = preventative_rate)
  spill_df <- df_plot %>% mutate(value = spillover_rate)

  p_prev <- build_panel(prev_df, "preventative")
  p_spill <- build_panel(spill_df, "spillover")

  panels <- if (orientation == "vertical") {
    p_prev / p_spill
  } else {
    p_prev | p_spill
  }

  model_title <- build_model_variant_title("model variant")
  combined <- patchwork::wrap_plots(
    list(model_title, panels),
    ncol = 1,
    heights = c(0.07, 0.93)
  )

  combined + plot_annotation(
    title = "Mechanism decomposition across risk (SSR)",
    subtitle = subtitle_context,
    theme = theme_vigilance(base_size = 11) +
      theme(
        plot.margin = margin(3, 4, 2, 4),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, margin = margin(b = 2))
      )
  )
}

# -----------------------------------------------------------------------------
# plot_K_vs_autocorr_mechanism_by_model()
# -----------------------------------------------------------------------------
# Same visual logic as the SSR plot, but the y-axis is autocorrelation:
#   autocorr â‰ˆ 1 - (Î»A + Î»L) in your environment parameterization.
#
# Uses safe_seq_step() because autocorr values can be irregular and may contain
# floating point noise, especially if derived quantities are stored.
#' Plot autocorrelation mechanism heatmaps faceted by model variant.
plot_K_vs_autocorr_mechanism_by_model <- function(
    df_models,
    subtitle_context = NULL,
    show_legend = TRUE
) {
  if (nrow(df_models) == 0) stop("df_models is empty")

  df_plot <- prepare_mechanism_df(df_models)
  df_plot$model_label <- wrap_model_labels(df_plot$model_label)

  # Axis ranges
  x_vals <- sort(unique(df_plot$K))
  x_vals <- x_vals[is.finite(x_vals)]
  if (length(x_vals) == 0) x_vals <- c(0, 1)

  y_vals <- sort(unique(df_plot$autocorr))
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) y_vals <- c(0, 0.1)

  # Use robust step size selection for ticks.
  x_step <- safe_seq_step(x_vals, fallback = 1)
  y_step <- safe_seq_step(y_vals, fallback = 0.1)

  xmin <- min(x_vals, na.rm = TRUE)
  xmax <- max(x_vals, na.rm = TRUE)
  ymin <- min(y_vals, na.rm = TRUE)
  ymax <- max(y_vals, na.rm = TRUE)

  base <- ggplot(df_plot, aes(x = K, y = autocorr, fill = mechanism_fill)) +
    geom_tile() +
    scale_fill_identity(guide = "none") +
    facet_grid(cols = vars(model_label)) +
    labs(
      x = "vigilance cost (K)",
      y = "autocorrelation"
    ) +
    theme_vigilance(base_size = 11, strip_size = 9) +
    scale_x_continuous(breaks = seq(xmin, xmax, by = x_step), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(ymin, ymax, by = y_step), expand = c(0, 0)) +
    theme(
      strip.text.x = element_text(face = "bold", size = 8),
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 4)),
      panel.spacing = grid::unit(0.2, "lines"),
      aspect.ratio = 1,
      plot.margin = margin(2, 4, 2, 4),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )

  model_title <- build_model_variant_title("model variant")
  combined <- if (show_legend) {
    patchwork::wrap_plots(
      list(model_title, base, build_mechanism_legend()),
      ncol = 1,
      heights = c(0.07, 0.63, 0.30)
    )
  } else {
    patchwork::wrap_plots(
      list(model_title, base),
      ncol = 1,
      heights = c(0.07, 0.93)
    )
  }

  combined + plot_annotation(
    title = "Hypervigilance amount + mechanism across autocorrelation",
    subtitle = subtitle_context,
    theme = theme_vigilance(base_size = 11) +
      theme(
        plot.margin = margin(3, 4, 2, 4),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, margin = margin(b = 2))
      )
  )
}

# -----------------------------------------------------------------------------
# plot_K_vs_autocorr_split_mechanism_by_model()
# -----------------------------------------------------------------------------
# Grey-scale split version (preventative vs spillover), but with y-axis = autocorr.
plot_K_vs_autocorr_split_mechanism_by_model <- function(
    df_models,
    orientation = c("vertical", "horizontal"),
    subtitle_context = NULL,
    show_legend = TRUE
) {
  orientation <- match.arg(orientation)
  stopifnot(nrow(df_models) > 0)

  df_plot <- prepare_mechanism_df(df_models)
  df_plot$model_label <- wrap_model_labels(df_plot$model_label)

  x_vals <- sort(unique(df_plot$K))
  x_vals <- x_vals[is.finite(x_vals)]
  y_vals <- sort(unique(df_plot$autocorr))
  y_vals <- y_vals[is.finite(y_vals)]
  x_step <- safe_seq_step(x_vals, fallback = 1)
  y_step <- safe_seq_step(y_vals, fallback = 0.1)

  xmin <- min(x_vals, na.rm = TRUE)
  xmax <- max(x_vals, na.rm = TRUE)
  ymin <- min(y_vals, na.rm = TRUE)
  ymax <- max(y_vals, na.rm = TRUE)

  build_panel <- function(df, title_suffix) {
    ggplot(df, aes(x = K, y = autocorr, fill = value)) +
      geom_tile() +
      scale_fill_grey01(title = "hypervigilance") +
      guides(fill = if (show_legend) "legend" else "none") +
      facet_grid(cols = vars(model_label)) +
      labs(
        x = "vigilance cost (K)",
        y = "autocorrelation",
        title = title_suffix
      ) +
      theme_vigilance(base_size = 10, strip_size = 9) +
      scale_x_continuous(breaks = seq(xmin, xmax, by = x_step), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(ymin, ymax, by = y_step), expand = c(0, 0)) +
      theme(
        strip.text.x = element_text(face = "bold", size = 8),
        axis.text = element_text(size = 7),
        axis.title.y = element_text(margin = margin(r = 4)),
        plot.title = element_text(size = 10, face = "bold"),
        panel.spacing = grid::unit(0.2, "lines"),
        aspect.ratio = 1,
        plot.margin = margin(2, 4, 1, 4)
      )
  }

  prev_df <- df_plot %>% mutate(value = preventative_rate)
  spill_df <- df_plot %>% mutate(value = spillover_rate)

  p_prev <- build_panel(prev_df, "preventative")
  p_spill <- build_panel(spill_df, "spillover")

  panels <- if (orientation == "vertical") {
    p_prev / p_spill
  } else {
    p_prev | p_spill
  }

  model_title <- build_model_variant_title("model variant")
  combined <- patchwork::wrap_plots(
    list(model_title, panels),
    ncol = 1,
    heights = c(0.07, 0.93)
  )

  combined + plot_annotation(
    title = "Mechanism decomposition across autocorrelation",
    subtitle = subtitle_context,
    theme = theme_vigilance(base_size = 11) +
      theme(
        plot.margin = margin(3, 4, 2, 4),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, margin = margin(b = 2))
      )
  )
}

# -----------------------------------------------------------------------------
# prepare_autocorr_mechanism_df()
# -----------------------------------------------------------------------------
# Convenience transformation for older code paths where the mechanism fields
# werenâ€™t yet standardized.
#
# The idea:
#   - HypervigilanceRate_all      = total HV
#   - HypervigilanceRate_filtered = â€œspillover HVâ€ proxy (HV in stressed state, etc.)
#   - preventative_rate           = remainder, clipped at 0 to avoid negative noise
#
# Note: this function is currently not used in the plotting calls above because
# they rely on prepare_mechanism_df(); keep it as a compatibility helper.
prepare_autocorr_mechanism_df <- function(df) {
  df %>%
    mutate(
      hypervigilance = HypervigilanceRate_all,
      spillover_rate = HypervigilanceRate_filtered,
      preventative_rate = pmax(0, HypervigilanceRate_all - HypervigilanceRate_filtered)
    )
}

#' Transformation notes:
#' - `prepare_mechanism_df()` ensures every tile uses the same hv_total + colour mapping.
#' - The split panels rely on consistent ordering of preventative/spillover so facet strips stay aligned.

