# ============================================================
# File: R/plotting/symmetry_risk/03_plot_symmetry_risk.R
#
# Purpose: Plotting helpers for the symmetry risk (SSR) and SSC sweeps (Fig6Dâ€“F).
# Notes:
#   - Pure helper module that expects tidy risk/SSR tables from `prep_symmetry_risk_autocorr.R`.
# ============================================================

# ---- Repo-local dependencies ------------------------------------------------
# plot_utils.R:
#   - provides the projectâ€™s ggplot theme(s), typography, and standard spacing rules
# utils_hv_rate_grid.R:
#   - typically includes shared grid utilities (consistent breaks/labels, grid builders)
# utils_subtitles.R:
#   - convenience helpers for consistent subtitle strings across figure panels
# utils_plot_layout.R:
#   - gtable/patchwork helpers that add row-title gutters and clean repeated axes
source("R/core/plot_utils.R")
source("R/plotting/_shared/utils_hv_rate_grid.R")
source("R/plotting/_shared/utils_subtitles.R")
source("R/plotting/_shared/utils_plot_layout.R")

suppressPackageStartupMessages({
  library(dplyr)   # factor ordering, mutate, and other small table operations
  library(ggplot2) # plotting
  library(scales)  # label_number(), breaks/formatting helpers
})

# =============================================================================
# 1) plot_K_vs_SSR_heatmap()
# =============================================================================
# Draw a 2D heatmap where:
#   x-axis: vigilance cost (K)
#   y-axis: stationary stressor rate (SSR)  ~ â€œriskâ€
#   fill:  HypervigilanceRate_filtered in [0,1]
#
# Optional features:
#   - row facets (e.g., by model variant): row_facet + row_labels
#   - optional row-title gutter: add_row_title triggers a gtable wrapper
#   - customizable fill scale: pass fill_scale to override default grayscale
#
# This is designed to be a â€œpure plotterâ€:
#   - it expects df is already tidy (one row per KÃ—SSRÃ—[model] cell)
#   - it does not run simulations or build the grid (that happens upstream)
#
# @param df Tidy risk table with columns: K, SSR, HypervigilanceRate_filtered
#          plus optional row_facet column (e.g., model_label).
# @param subtitle_context Optional subtitle string. If NULL and df has attr(df,"meta"),
#        the function builds a standard subtitle from meta.
# @param row_facet Optional column name for row facets (e.g., "model_label").
# @param row_labels Optional explicit ordering of row facet levels.
# @param add_row_title If TRUE and row_facet is used, wrap plot with left gutter titles.
# @param row_title Text for the left gutter (e.g., "model variant").
# @param gutter_row_title Horizontal spacing reserved for the rotated gutter title.
# @param gutter_row_labels Horizontal spacing reserved for the per-row facet labels.
# @param row_label_width Wrap width for long row labels in facet strips.
# @param fill_scale Optional ggplot2 scale for fill. If NULL, uses a standard 0â€“1 grayscale.
# @export
plot_K_vs_SSR_heatmap <- function(
    df,
    subtitle_context = NULL,
    row_facet = NULL,
    row_labels = NULL,
    add_row_title = FALSE,
    row_title = "model scenario",
    gutter_row_title = grid::unit(1.6, "lines"),
    gutter_row_labels = grid::unit(1.4, "lines"),
    row_label_width = 18,
    fill_scale = NULL,
    show_column_header = TRUE
) {
  # ---- Validate required inputs --------------------------------------------
  # These three columns are the minimum needed to define the heatmap.
  stopifnot(all(c("K", "SSR", "HypervigilanceRate_filtered") %in% names(df)))

  # ---- Determine whether we facet by row -----------------------------------
  # Row faceting is optional; when present, we convert the facet column to a factor
  # with a controlled level order so facet rows stay consistent across panels.
  has_row_facet <- !is.null(row_facet) && row_facet %in% names(df)
  if (has_row_facet) {
    # Choose levels:
    #   - if row_labels was provided, use that order
    #   - otherwise, use the order the labels appear in df
    levels <- if (is.null(row_labels)) unique(as.character(df[[row_facet]])) else as.character(row_labels)
    levels <- levels[!is.na(levels)]

    # Fallback if everything was NA or empty after filtering:
    if (length(levels) == 0) levels <- unique(as.character(df[[row_facet]]))

    # Store the facet variable in a dedicated column, so later code can facet
    # without touching the original label column.
    df$facet_row <- factor(as.character(df[[row_facet]]), levels = levels)
  }

  # ---- Infer axis steps and limits from the data ---------------------------
  # The grids are typically evenly spaced (e.g., K = 1,3,5,... ; SSR = 0.05 steps).
  # We compute a step so tick marks align exactly with the tile grid.
  x_vals <- sort(unique(df$K))
  y_vals <- sort(unique(df$SSR))
  x_step <- if (length(x_vals) > 1) min(diff(x_vals)) else 1
  y_step <- if (length(y_vals) > 1) min(diff(y_vals)) else 0.1
  xmin <- min(x_vals); xmax <- max(x_vals)
  ymin <- min(y_vals); ymax <- max(y_vals)

  # ---- Auto subtitle from metadata (if available) --------------------------
  # Upstream preprocessors often attach a small list of fixed parameters as an
  # attribute so plotting functions can create standardized subtitles.
  meta <- attr(df, "meta")
  if (is.null(subtitle_context) && !is.null(meta)) {
    subtitle_context <- sprintf(
      "C = %s | D = %s | d = %s | T = %s | N = %s | LA+LL = %.2f (autocorr = %.2f)",
      as.character(meta$C), as.character(meta$D), as.character(meta$d),
      as.character(meta$T_steps), as.character(meta$N_agents),
      as.numeric(meta$total_rate), 1 - as.numeric(meta$total_rate)
    )
  }

  # ---- Facet strip styling logic -------------------------------------------
  # When we add an external row-title gutter (add_row_title), we usually want to:
  #   - hide the default facet strip labels on the y-side
  #   - because we will re-draw them manually in the gutter wrapper
  strip_y_text <- if (has_row_facet && add_row_title) element_blank() else element_text(size = 12, face = "bold")

  # ---- Fill scale configuration --------------------------------------------
  # Default: grayscale gradient where:
  #   0 â†’ white, 1 â†’ black
  # With breaks chosen to show structure in low-HV regimes (0.02, 0.05, 0.1, ...).
  #
  # NOTE: If you pass a custom scale via fill_scale, it should already be a valid
  # ggplot2 scale object (e.g., scale_fill_viridis_c(...), scale_fill_gradientn(...)).
  fill_layer <- if (is.null(fill_scale)) {
    scale_fill_gradientn(
      colours = c("#ffffff", "#bbbbdd", "#7756a4", "#2c1544"),
      limits = c(0, 1),
      na.value = "transparent",
      name = "hyper-\nvigilance",
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = label_number(accuracy = 0.01),
      guide = guide_colourbar(
        frame.colour = "black",
        frame.linewidth = 0.5,
        ticks.colour = "black",
        barheight = grid::unit(100, "pt"),
        barwidth = grid::unit(28, "pt"),
        ticks.linewidth = 0.5
      )
    )
  } else {
    fill_scale
  }

  # ---- Core plot ------------------------------------------------------------
  # geom_tile() expects a â€œgrid-likeâ€ df where each (K, SSR) has one cell.
  # coord_fixed(ratio=1) makes tiles square: visually comparable along both axes.
  p <- ggplot(df, aes(x = K, y = SSR, fill = HypervigilanceRate_filtered)) +
    geom_tile() +
    fill_layer +
    labs(
      title = "Proportion of hypervigilance across risk (SSR)",
      subtitle = subtitle_context,
      x = "vigilance cost (K)",
      y = "stationary stressor rate (risk)"
    ) +
    theme_vigilance(base_size = 13) +
    coord_fixed(ratio = 1) +
    # Use explicit breaks aligned to the grid step so tick marks match tiles.
    scale_x_continuous(breaks = seq(xmin, xmax, by = x_step), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(ymin, ymax, by = y_step), expand = c(0, 0)) +
    theme(
      # Keep each facet panel square.
      aspect.ratio = 1,

      # Title/subtitle hierarchy.
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = if (is.null(subtitle_context) || identical(subtitle_context, "")) element_blank() else element_text(size = 11, margin = margin(b = 6)),

      # Axis formatting for figure readability.
      axis.title = element_text(size = 12, face = "plain"),
      axis.text = element_text(size = 11),
      axis.title.x = element_text(hjust = 0, margin = margin(t = 8)),
      axis.title.y = element_text(hjust = 0, vjust = 0, margin = margin(r = 10)),

      # Legend formatting (compact but readable).
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.key.height = grid::unit(80, "pt"),
      legend.key.width = grid::unit(22, "pt"),

      # Facet strip behavior:
      #   - if we will add an external gutter title, strip.text.y becomes blank
      strip.text.y = strip_y_text,
      strip.background.y = if (has_row_facet) element_blank() else element_blank(),

      # Place facet strips outside to mimic paper-style multi-row panels.
      strip.placement = if (has_row_facet) "outside" else "inside",

      # Spacing between row panels.
      panel.spacing.y = unit(0.45, "lines"),

      # More compact margins for thesis export.
      plot.margin = margin(t = 8, r = 6, b = 8, l = 70)
    )

  # ---- Apply row faceting if requested -------------------------------------
  if (has_row_facet) {
    p <- p + facet_grid(
      rows = vars(facet_row),
      switch = "y", # put strip labels on the left side (paper-style)
      labeller = labeller(facet_row = label_wrap_gen(row_label_width))
    )
    # Store row order on the plot object so wrappers can re-use it later.
    attr(p, "row_levels") <- levels(df$facet_row)
  }

  # ---- Optional column header (single-row scenario) ------------------------
  # If there is no row facet, we sometimes annotate a small â€œheaderâ€ above the grid
  # to clarify what x represents, especially when the figure is used in composites.
  if (isTRUE(show_column_header) && !has_row_facet && length(x_vals) > 0) {
    col_title_df <- data.frame(K = x_vals[ceiling(length(x_vals) / 2)], SSR = ymax + 1e-6)
    p <- p + geom_text(
      data = col_title_df,
      aes(x = K, y = Inf, label = "vigilance cost (K)"),
      inherit.aes = FALSE,
      fontface = "plain",
      vjust = -1.8,
      size = 5
    ) + coord_cartesian(clip = "off") # allow text outside plotting region
  }

  # ---- Optional gtable wrapper: row-title gutter ---------------------------
  # When plotting multiple model rows, you often want:
  #   - a single rotated label (â€œmodel variantâ€) in the left margin
  #   - and cleaner row labels aligned with panels
  # wrap_autocorr_heatmap_with_row_title() (despite the name) is a generic wrapper
  # in utils_plot_layout.R that likely:
  #   - converts ggplot â†’ gtable
  #   - inserts extra columns for title + row labels
  #   - suppresses repeated axes and adjusts strip placement
  if (has_row_facet && add_row_title) {
    p <- wrap_autocorr_heatmap_with_row_title(
      p,
      row_title = row_title,
      gutter_row_title = gutter_row_title,
      gutter_row_labels = gutter_row_labels,
      row_levels = attr(p, "row_levels"),
      row_label_width = row_label_width
    )
  }

  # Return either:
  #   - ggplot object (if no wrapper), OR
  #   - patchwork/gtable-wrapped object (if wrapper applied)
  p
}

# =============================================================================
# 2) plot_K_vs_SSR_heatmap_by_model()
# =============================================================================
# Convenience wrapper that:
#   1) builds the SSR grid across model scenarios (risk_grid_K_vs_SSR_by_model)
#   2) chooses a consistent row order (row_levels)
#   3) constructs a standardized subtitle (unless overridden)
#   4) calls plot_K_vs_SSR_heatmap() with row facets enabled
#
# Return:
#   list(
#     df_risk_models = <the tidy stacked grid>,
#     p_faceted      = <the plot object>
#   )
#
# This function is meant for *scripts* that produce figures:
#   - scripts call one function and get both the underlying data + final plot
plot_K_vs_SSR_heatmap_by_model <- function(
    model_scenarios = default_risk_model_scenarios,
    C, d, deltaD, K_values,
    T_steps, states, N_agents,
    ssr_step = 0.05,
    total_rate = 0.5,
    base_policy_args = list(),
    base_sim_args = list(),
    subtitle_context = NULL,
    add_row_title = TRUE,
    row_title = "model variant",
    heatmap_fill_scale = NULL
) {
  # ---- Build the stacked grid across models --------------------------------
  # risk_grid_K_vs_SSR_by_model() is expected to:
  #   - loop over model_scenarios
  #   - for each model, sweep K and SSR values
  #   - compute HypervigilanceRate_filtered at each grid cell
  #   - attach attributes like row_levels and meta (for subtitles)
  df_models <- risk_grid_K_vs_SSR_by_model(
    model_scenarios = model_scenarios,
    C = C, d = d, deltaD = deltaD, K_values = K_values,
    T_steps = T_steps, states = states, N_agents = N_agents,
    ssr_step = ssr_step, total_rate = total_rate,
    base_policy_args = base_policy_args, base_sim_args = base_sim_args
  )
  if (nrow(df_models) == 0) stop("No data generated for the provided model_scenarios")

  # ---- Determine row order for facets --------------------------------------
  # Prefer explicit row_levels stored on df_models by the builder.
  # Fallback: whatever factor levels currently exist.
  row_levels <- attr(df_models, "row_levels")
  if (is.null(row_levels)) row_levels <- levels(df_models$model_label)

  # ---- Subtitle handling ----------------------------------------------------
  # If caller didnâ€™t specify a subtitle, build a standardized one that includes:
  #   - cost parameters (C, D, d)
  #   - time horizon and simulation size
  #   - the fixed total_rate constraint (LA+LL) and implied autocorrelation
  subtitle_text <- if (is.null(subtitle_context)) {
    sprintf(
      "C = %s | D = %s | d = %s | T = %s | N = %s | LA+LL = %.2f (autocorr = %.2f)",
      C, deltaD, d, T_steps, N_agents,
      total_rate, 1 - total_rate
    )
  } else {
    as.character(subtitle_context)
  }

  # ---- Build the final plot -------------------------------------------------
  p_grid <- plot_K_vs_SSR_heatmap(
    df_models,
    subtitle_context = subtitle_text,
    row_facet = "model_label",
    row_labels = row_levels,
    add_row_title = add_row_title,
    row_title = row_title,
    fill_scale = heatmap_fill_scale
  )

  # Return both data (for saving / diagnostics) and plot (for the figure panel).
  list(df_risk_models = df_models, p_faceted = p_grid)
}
