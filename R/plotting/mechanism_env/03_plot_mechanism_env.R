# ============================================================
# File: R/plotting/mechanism_env/03_plot_mechanism_env.R
#
# Purpose: Shared helpers for the mechanism hypervigilance figures (Fig4A).
# Notes: Pure helper module—scripts should source it before plotting.
# ============================================================

# ---- Imports / repo dependencies -------------------------------------------
# These sources pull in:
#   - shared ggplot themes and “house style” helpers (plot_utils.R)
#   - the same environment heatmap builders used for Fig2-style panels (plot_env_heatmaps.R)
#   - grid builders / hv-rate grid conventions (utils_hv_rate_grid.R)
#   - canonical model-scenario definitions and label conventions (utils_model_scenarios.R)
#   - the canonical health environment scenario table (utils_health_env_scenarios.R)
#   - the mechanism data prep that converts raw counts → tidy rates + mechanism colors (prep_mechanism_data.R)
source("R/core/plot_utils.R")
source("R/plotting/env_heatmaps/03_plot_env_heatmaps.R")
source("R/plotting/_shared/utils_hv_rate_grid.R")
source("R/helpers/utils_model_scenarios.R")
source("R/plotting/_shared/utils_health_env_scenarios.R")
source("R/plotting/mechanism_env/00_data_prep_mechanism_data.R")

suppressPackageStartupMessages({
  library(dplyr)     # filtering/mutating (model facets, rates, matching canonical envs)
  library(ggplot2)   # plotting
  library(patchwork) # stacking heatmap + legend/key panels
  library(scales)    # pretty labels (0.02 accuracy, percent/number formatters, etc.)
  library(grid)      # unit(), grobs, layout spacing (axes/strip placement tweaks)
})

# =============================================================================
# Global defaults used by multiple panels in this module
# =============================================================================
# These strings are kept here (rather than repeated inside functions) so:
#   - all mechanism plots share the same title/subtitle wording
#   - scripts can override them by passing subtitle_context if desired
env_full_grid_title <- "Hypervigilance across full LA-LL environment space"
env_full_grid_subtitle <- "Rows = model variants   |   Columns = vigilance cost (K)"

# -----------------------------------------------------------------------------
# Mechanism data convention (important):
# -----------------------------------------------------------------------------
# `prepare_mechanism_df()` (from prep_mechanism_data.R) is the *one place* where the
# raw simulation outputs are converted into standardized mechanism variables.
#
# In practice it:
#   - normalizes HV counts into rates (e.g., total HV, spillover HV, preventative HV)
#   - clamps rates/shares into [0, 1] to prevent plotting artifacts
#   - derives mechanism shares (preventative proportion) used for bivariate coloring
#   - creates `mechanism_fill`, a precomputed color string per tile
#
# This means downstream plotting functions can treat the input as “clean tidy tiles”
# without repeating sanitization logic (and without inconsistent definitions across panels).

# =============================================================================
# 1) plot_env_bivariate_hv_heatmap()
# =============================================================================
# Main Fig4A-style panel:
#   - base layer: grayscale HV intensity (0=white, 1=black)
#   - overlay layer: bivariate mechanism color (red↔blue gradient) with transparency
#     where hue encodes mechanism balance and brightness encodes total HV
#
# Why two layers?
#   - readers can perceive total HV from the grayscale ramp even if they ignore color
#   - mechanism overlay adds qualitative info (“preventative vs spillover”) without
#     needing a second figure
#
# Inputs:
#   df: environment grid output (must include LA, LL, K, and HV-related columns)
#   hv_column: name of total HV rate column in df (default HypervigilanceRate_filtered)
#   prevent_column / spill_column / safe_column:
#       raw mechanism decomposition fields used by prepare_mechanism_df()
#   row_facet: which column to use for model rows (typically model_label)
#   threshold_lines: optional analytic thresholds; converted to drawable segments
#   subtitle_context: optional override for subtitle
#   mechanism_overlay_alpha: transparency for the overlay (higher = more saturated)
plot_env_bivariate_hv_heatmap <- function(
    df,
    hv_column = "HypervigilanceRate_filtered",
    prevent_column = "HypervigilancePreventCount",
    spill_column = "HypervigilanceSpillCount",
    safe_column = "HypervigilanceSafeHV",
    row_facet = "model_label",
    threshold_lines = NULL,
    subtitle_context = NULL,
    mechanism_overlay_alpha = 0.72,
    title = "Mechanism composition and total hypervigilance across environments",
    legend_subtitle = NULL
) {
  # ---- Guardrails -----------------------------------------------------------
  # Empty inputs usually indicate the upstream grid builder failed or was filtered away.
  if (nrow(df) == 0) stop("df is empty")

  # Match the subtitle behavior used by the main HV heatmap panel when metadata exists.
  meta <- attr(df, "fixed_params")
  subtitle_text <- if (!is.null(subtitle_context)) {
    as.character(subtitle_context)
  } else if (!is.null(meta)) {
    hv_subtitle_with_params(meta, NULL)
  } else {
    NULL
  }

  # ---- Standardize mechanism variables -------------------------------------
  # Returns a df where *at minimum* we expect:
  #   - hv_total: total HV rate in [0,1]
  #   - preventative_rate / spillover_rate: decomposed rates
  #   - mechanism_fill: precomputed hex/RGB color string representing mechanism+HV
  #   - original coordinates (LA, LL) and facets (K, model_label)
  df_proc <- prepare_mechanism_df(
    df,
    hv_column = hv_column,
    prevent_column = prevent_column,
    spill_column = spill_column,
    safe_column = safe_column
  )
  if (row_facet %in% names(df_proc)) {
    df_proc[[row_facet]] <- enc2utf8(as.character(df_proc[[row_facet]]))
  }

  # ---- Threshold line preprocessing ----------------------------------------
  # Some pipelines store threshold specs in different formats; this converts them
  # into “segments” that add_threshold_layer() understands.
  threshold_segments <- prepare_threshold_segments(threshold_lines)

  # ---- Build base heatmap scaffold (same path as the main "proportion" plot) -
  # Using build_env_heatmap_base() keeps panel geometry/theme identical to
  # the canonical proportion-of-HV figure; we then add mechanism as an overlay.
  res <- build_env_heatmap_base(
    df = df_proc,
    fill_column = "hv_total",
    fill_type = "gradient",
    fill_label = "hypervigilance",
    facet_rows = row_facet,
    facet_cols = "K",
    title = title,
    subtitle = subtitle_text,
    x_label = "P(arrive)",
    y_label = "P(leave)",
    env_scenarios = NULL,
    caption_suffix = NULL,
    D = if ("D" %in% names(df_proc)) unique(df_proc$D)[1] else if (!is.null(meta)) meta$D else NULL,
    add_threshold_lines = FALSE,
    add_ssp_boundaries = FALSE,
    add_region_labels = FALSE,
    threshold_segments = threshold_segments
  )

  df_plot <- res$df_plot
  p <- res$plot + ggplot2::theme(legend.position = "none")

  # Infer tile size so the mechanism overlay aligns exactly with the base tiles.
  x_vals <- sort(unique(df_plot$LA[is.finite(df_plot$LA)]))
  y_vals <- sort(unique(df_plot$LL[is.finite(df_plot$LL)]))
  x_step <- if (length(x_vals) > 1) min(diff(x_vals)) else 0.1
  y_step <- if (length(y_vals) > 1) min(diff(y_vals)) else 0.1
  tile_step <- min(x_step, y_step)

  # ---- Overlay mechanism color ---------------------------------------------
  # If prepare_mechanism_df created `mechanism_fill`, overlay it as a second tile layer.
  # We set inherit.aes=FALSE because:
  #   - the base plot already has fill mapped; overlay uses a literal vector of colors.
  # We also set colour=NA to avoid re-drawing gridlines on top of the grayscale tiles.
  if ("mechanism_fill" %in% names(df_plot)) {
    p <- p +
      ggplot2::geom_tile(
        data = df_plot,
        mapping = ggplot2::aes(x = LA, y = LL),
        width = tile_step,
        height = tile_step,
        fill = df_plot$mechanism_fill,
        alpha = mechanism_overlay_alpha,
        colour = NA,
        inherit.aes = FALSE
      )
  }

  # Keep region labels out of the tiles; show them in a dedicated side key panel.

  # ---- Convert to a gtable and clean axes ----------------------------------
  # keep_left_bottom_axes():
  #   removes repeated axes in a facet grid so the figure is less cluttered.
  # preserve_bottom_axes = FALSE:
  #   often used when the bottom axis is added elsewhere in a composite layout.
  heatmap <- keep_left_bottom_axes(p, preserve_bottom_axes = FALSE)

  # Add a column header across facet columns (“vigilance cost (K)”).
  heatmap <- add_column_header_to_gtable(heatmap, header = "vigilance cost (K)")

  # This helper is repo-specific: it rearranges the y-axis label position relative to strips
  # so λL doesn’t collide with facet headers.
  heatmap <- move_lambdaL_after_strips(heatmap)

  # Wrap the gtable into patchwork so we can compose with side key + legend.
  heatmap_patch <- patchwork::wrap_elements(full = heatmap)

  # ---- Build the mechanism legend/key --------------------------------------
  # The legend is an explicit 2D tile showing:
  #   x = preventative share (blue→dark→orange)
  #   y = total HV level (brightness)
  # We compute a grid, map each (prevent_share, hv_value) to a color, then draw it.
  legend_grid <- expand.grid(
    prevent_share = seq(0, 1, length.out = 60),
    hv_value = seq(0, 1, length.out = 60)
  )
  legend_grid$color <- mechanism_color_map(legend_grid$prevent_share, legend_grid$hv_value)

  legend_plot <- ggplot2::ggplot(legend_grid, aes(x = prevent_share, y = hv_value, fill = color)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_identity() +  # colors already computed, no scale needed
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::label_number(accuracy = 0.01),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::label_number(accuracy = 0.01),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "proportion anticipatory",
      y = "total hypervigilance",
      title = "Mechanism key"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "plain", size = 14),
      axis.text = ggplot2::element_text(face = "plain", size = 12),
      plot.title = ggplot2::element_text(face = "plain", size = 13),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6)
    )

  # ---- Final assembly -------------------------------------------------------
  # Stack heatmap above a centered, compact legend (about half-width).
  heatmap_patch /
    ((patchwork::plot_spacer() | legend_plot | patchwork::plot_spacer()) +
       patchwork::plot_layout(widths = c(1, 2, 1))) +
    patchwork::plot_layout(heights = c(1, 0.18)) +
    patchwork::plot_annotation(
      subtitle = legend_subtitle
    ) &
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(t = 0, b = 0))
    )
}

# =============================================================================
# 2) plot_env_split_mechanism_heatmaps()
# =============================================================================
# Produces two panels that separate mechanism components:
#   - Preventative hypervigilance (rate)
#   - Spillover hypervigilance (rate)
#
# Both are shown using the same grayscale 0..1 fill scale so magnitude comparisons
# across panels remain meaningful.
#
# This is useful when:
#   - the bivariate overlay is “too much” for some readers
#   - you want to explicitly show that one component dominates in certain regions
plot_env_split_mechanism_heatmaps <- function(
    df,
    hv_column = "HypervigilanceRate_filtered",
    prevent_column = "HypervigilancePreventCount",
    spill_column = "HypervigilanceSpillCount",
    safe_column = "HypervigilanceSafeHV",
    row_facet = "model_label",
    threshold_lines = NULL,
    orientation = c("horizontal", "vertical"),
    subtitle_context = NULL,
    show_threshold_lines = TRUE,
    show_environment_boundaries = TRUE,
    tile_border_colour = "grey85",
    compact_layout = FALSE
) {
  orientation <- match.arg(orientation)

  # Default subtitle matches the full-grid convention used across mechanism plots.
  subtitle_text <- if (is.null(subtitle_context)) NULL else as.character(subtitle_context)

  # Standardize the mechanism variables once, then reuse for both panels.
  df_proc <- prepare_mechanism_df(
    df,
    hv_column = hv_column,
    prevent_column = prevent_column,
    spill_column = spill_column,
    safe_column = safe_column
  )
  if (row_facet %in% names(df_proc)) {
    df_proc[[row_facet]] <- enc2utf8(as.character(df_proc[[row_facet]]))
  }
  threshold_segments <- prepare_threshold_segments(threshold_lines)

  # Build the base facet grid structure via build_env_heatmap_base() so we can reuse its
  # processed df + facet_layer (ensures identical strip ordering and spacing).
  res <- build_env_heatmap_base(
    df_proc,
    fill_column = "hv_total",
    fill_type = "gradient",
    fill_label = "HV",
    facet_rows = row_facet,
    facet_cols = "K",
    title = NULL,
    subtitle = subtitle_text,
    env_scenarios = NULL,
    caption_suffix = NULL,
    add_threshold_lines = FALSE,
    add_ssp_boundaries = FALSE,
    add_region_labels = FALSE
  )

  df_plot <- res$df_plot
  facet_layer <- res$facet_layer

  compact_model_label <- function(x) {
    x <- enc2utf8(as.character(x))
    dplyr::case_when(
      grepl("basic", x, ignore.case = TRUE) ~ "Basic",
      grepl("no terminal reward|β\\s*=\\s*0|ω\\s*=\\s*0|health", x, ignore.case = TRUE) ~ "No terminal reward",
      grepl("linear", x, ignore.case = TRUE) ~ "Linear\n(w = 1)",
      grepl("power", x, ignore.case = TRUE) ~ "Power (α = 3)",
      grepl("threshold", x, ignore.case = TRUE) ~ "Threshold (τ=.6H0)",
      TRUE ~ x
    )
  }

  if (isTRUE(compact_layout) && row_facet %in% names(df_plot)) {
    compact_values <- compact_model_label(as.character(df_plot[[row_facet]]))
    compact_values <- sub("^No terminal reward$", "No terminal\nreward", compact_values)
    compact_values <- sub("^Power \\(.*3\\)$", "Power\n(a = 3)", compact_values)
    compact_values <- sub("^Threshold \\(.*H0\\)$", "Threshold\n(t = 0.6 H0)", compact_values)
    df_plot[[row_facet]] <- factor(compact_values, levels = unique(compact_values))
  }

  # Axis inference (same logic as in the bivariate plot).
  axis_x <- sort(unique(df_plot$LA))
  axis_y <- sort(unique(df_plot$LL))
  x_step <- if (length(axis_x) > 1) min(diff(axis_x)) else 0.1
  y_step <- if (length(axis_y) > 1) min(diff(axis_y)) else 0.1
  xmin <- min(axis_x, 0,   na.rm = TRUE)
  xmax <- max(axis_x, 0.5, na.rm = TRUE)
  ymin <- min(axis_y, 0,   na.rm = TRUE)
  ymax <- max(axis_y, 0.5, na.rm = TRUE)

  # Shared region boundaries/labels so both panels have identical overlays.
  boundaries <- build_ssp_predictability_boundaries(c(xmin, xmax), c(ymin, ymax))

  strip_position <- if (length(unique(na.omit(df_plot[[row_facet]]))) > 0) "outside" else "inside"

  # Helper that builds one grayscale tile panel from a given value column.
  build_panel <- function(data, panel_title, show_axis_titles = TRUE) {
    p <- ggplot(data, aes(x = LA, y = LL, fill = value)) +
      geom_tile(color = tile_border_colour) +
      facet_layer +
      scale_fill_gradient(
        limits = c(0, 1),
        low = "#f7f7f7",
        high = "#252525",
        name = "hypervigilance",
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = scales::label_number(accuracy = 0.01)
      ) +
      guides(
        fill = guide_colorbar(
          direction = "horizontal",
          barwidth = unit(10, "cm"),
          barheight = unit(0.6, "cm"),
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          frame.colour = "black",
          frame.linewidth = 0.8
        )
      ) +
      labs(
        title = panel_title,
        x = if (isTRUE(show_axis_titles)) "P(arrive)" else NULL,
        y = if (isTRUE(show_axis_titles)) "P(leave)" else NULL
      ) +
      theme_vigilance(base_size = 12) +
      coord_fixed(ratio = 1) +
      scale_x_continuous(
        breaks = seq(0, 0.5, by = 0.1),
        labels = sprintf("%.1f", seq(0, 0.5, by = 0.1)),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        breaks = seq(0, 0.5, by = 0.1),
        labels = sprintf("%.1f", seq(0, 0.5, by = 0.1)),
        expand = c(0, 0)
      ) +
      theme(
        strip.text.x = element_text(size = 18, face = "plain"),
        strip.text.y = element_text(size = 16, face = "plain", lineheight = 0.9),
        strip.placement = strip_position,
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),
        axis.text.y = element_text(angle = 0, size = 12),
        axis.title.x = element_text(hjust = 0, vjust = 0, margin = margin(t = 8), size = 20, face = "bold"),
        axis.title.y = element_text(angle = 90, hjust = 0, vjust = 0, margin = margin(r = 16), size = 20, face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.spacing = unit(0.45, "lines"),
        plot.margin = margin(t = 6, r = 6, b = 6, l = 6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    if (isTRUE(show_threshold_lines)) {
      p <- add_threshold_layer(p, threshold_segments)
    }
    if (isTRUE(show_environment_boundaries)) {
      p <- add_ssp_boundaries(p, boundaries)
    }

    if (isTRUE(compact_layout)) {
      p <- p +
        theme(
          strip.text.x = element_text(size = 15, face = "plain", margin = margin(b = 1)),
          strip.text.y = element_text(size = 14, face = "plain", lineheight = 0.85, margin = margin(r = 1)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 3)),
          axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 5)),
          plot.title = element_text(size = 17, face = "bold", margin = margin(b = 1)),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          panel.spacing.x = unit(0.6, "lines"),
          panel.spacing.y = unit(0.18, "lines"),
          plot.margin = margin(t = 1, r = 1, b = 1, l = 1)
        )
    }

    p
  }

  # Prepare the two views by copying df_plot and swapping the fill value.
  preventative_df <- df_plot %>% mutate(value = preventative_rate)
  spillover_df <- df_plot %>% mutate(value = spillover_rate)

  prev_title <- if (isTRUE(compact_layout)) "A) Anticipatory" else "A) Anticipatory hypervigilance"
  spill_title <- if (isTRUE(compact_layout)) "B) Reactive" else "B) Reactive hypervigilance"

  p_prev <- build_panel(preventative_df, prev_title, show_axis_titles = TRUE)
  p_spill <- build_panel(spillover_df, spill_title, show_axis_titles = TRUE)

  add_k_header <- function(p) {
    g <- ggplotGrob(p + theme(legend.position = "none"))
    g <- add_column_header_to_gtable(
      g,
      header = "cost of vigilance (K)",
      fontsize = if (isTRUE(compact_layout)) 15 else 16,
      fontface = "bold",
      row_height = if (isTRUE(compact_layout)) 1.35 else 1.65,
      y = 0.58
    )
    patchwork::wrap_elements(full = g)
  }

  legend_plot <- ggplot(
    data.frame(x = 1, y = 1, value = 0.5),
    aes(x = x, y = y, fill = value)
  ) +
    geom_tile(alpha = 0) +
    scale_fill_gradient(
      limits = c(0, 1),
      low = "#f7f7f7",
      high = "#252525",
      name = "hypervigilance",
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        barwidth = unit(10, "cm"),
        barheight = unit(0.6, "cm"),
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        frame.colour = "black",
        frame.linewidth = 0.8
      )
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = if (isTRUE(compact_layout)) 12 else 11),
      legend.text = element_text(size = if (isTRUE(compact_layout)) 11 else 10),
      legend.key.height = unit(0.14, "in"),
      legend.key.width = unit(0.75, "in"),
      legend.box.spacing = unit(0.01, "in"),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(0, 0, 0, 0)
    )

  p_prev <- add_k_header(p_prev)
  p_spill <- add_k_header(p_spill)

  # Assemble panels vertically (default) or horizontally.
  layout <- if (orientation == "vertical") {
    (p_prev / patchwork::plot_spacer() / p_spill) +
      plot_layout(ncol = 1, heights = c(1, 0.08, 1))
  } else {
    (p_prev | patchwork::plot_spacer() | p_spill) +
      plot_layout(nrow = 1, widths = c(1, 0.12, 1))
  }

  layout <- (layout / legend_plot) + plot_layout(heights = c(1, 0.02))

  if (!is.null(subtitle_text)) {
    layout <- layout + plot_annotation(subtitle = subtitle_text)
  }
  if (isTRUE(compact_layout)) {
    layout <- layout &
      theme(
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5, margin = margin(b = 4)),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key.height = unit(0.14, "in"),
        legend.key.width = unit(0.75, "in"),
        legend.box.spacing = unit(0.01, "in"),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(0, 0, 0, 0)
      )
  } else {
    layout <- layout &
      theme(
        plot.title = element_text(size = 16, face = "plain", hjust = 0.5, margin = margin(b = 6))
      )
  }
  layout
}

# =============================================================================
# 3) plot_env_stacked_hv_bars()
# =============================================================================
# Summarizes mechanism components on a *canonical set of environments* (your named
# environment codes), rather than showing the full LA/LL grid.
#
# Key idea:
#   - We first prepare the full grid with prepare_mechanism_df() so all component
#     definitions match the heatmaps.
#   - Then we “match” the canonical environments back onto the grid cells using
#     a tolerance (tol) so it works even if grid_step differs slightly.
#   - Finally we aggregate preventative/spillover for each model × K × environment,
#     pivot long, and plot side-by-side bars.
#
# This plot is useful as a compact companion to the heatmaps: it tells the reader
# “in the environments we actually highlight in text, how much is preventative vs spillover?”
plot_env_stacked_hv_bars <- function(
    df,
    env_scenarios = default_health_env_scenarios(),
    hv_column = "HypervigilanceRate_filtered",
    prevent_column = "HypervigilancePreventCount",
    spill_column = "HypervigilanceSpillCount",
    safe_column = "HypervigilanceSafeHV",
    row_facet = "model_label",
    tol = 1e-4,
    subtitle_context = NULL,
    value_mode = c("absolute_rate", "share_of_hv"),
    title = "Mechanism Composition Across Canonical Environments",
    show_subtitle = TRUE,
    show_k_header = TRUE,
    legend_position = "bottom"
) {
  value_mode <- match.arg(value_mode)
  clean_model_label <- function(x) {
    x <- as.character(x)
    dplyr::case_when(
      grepl("basic", x, ignore.case = TRUE) ~ "Basic",
      grepl("no terminal reward|w.*=.*0|omega.*=.*0|ω.*=.*0", x, ignore.case = TRUE) ~ "Health\n(w = 0)",
      grepl("linear", x, ignore.case = TRUE) ~ "Linear\n(w = 1)",
      grepl("power", x, ignore.case = TRUE) ~ "Power\n(a = 3)",
      grepl("threshold", x, ignore.case = TRUE) ~ "Threshold\n(τ=.6H0)",
      TRUE ~ "Other"
    )
  }

  # Ensure required columns for grouping/faceting exist.
  required <- c("K", row_facet)
  stopifnot(all(required %in% names(df)))

  env_axis_order <- c("L-P", "L-U", "M-P", "M-U", "H-P", "H-U")
  env_levels <- env_axis_order[env_axis_order %in% env_scenarios$env_label]
  env_levels <- c(env_levels, setdiff(as.character(env_scenarios$env_label), env_levels))

  # Prepare standardized mechanism variables.
  df_proc <- prepare_mechanism_df(
    df,
    hv_column = hv_column,
    prevent_column = prevent_column,
    spill_column = spill_column,
    safe_column = safe_column
  )

  # ---- Match canonical environments to the computed grid -------------------
  # The simulation grid may be 0.05 steps while the canonical table might be 0.1,
  # or floating point rounding may differ. We therefore match using abs(...) <= tol.
  la_step <- min(diff(sort(unique(df_proc$LA[is.finite(df_proc$LA)]))), na.rm = TRUE)
  ll_step <- min(diff(sort(unique(df_proc$LL[is.finite(df_proc$LL)]))), na.rm = TRUE)
  if (!is.finite(la_step)) la_step <- 0
  if (!is.finite(ll_step)) ll_step <- 0
  tol_eff <- max(tol, la_step / 2, ll_step / 2) + sqrt(.Machine$double.eps)

  matched <- do.call(
    rbind,
    lapply(seq_len(nrow(env_scenarios)), function(i) {
      env <- env_scenarios[i, ]
      rows <- df_proc %>%
        mutate(.env_distance = abs(LA - env$LA) + abs(LL - env$LL)) %>%
        group_by(.data[[row_facet]], K) %>%
        filter(.env_distance == min(.env_distance, na.rm = TRUE), .env_distance <= (2 * tol_eff)) %>%
        slice(1) %>%
        ungroup() %>%
        select(-.env_distance)
      if (nrow(rows) == 0) return(NULL)
      rows %>%
        mutate(
          env_label = env$env_label,  # short code used on the axis
          env_full  = env$env_full,   # verbose description (kept for possible captions)
          env_rank  = i               # preserves the canonical ordering
        )
    })
  )
  if (is.null(matched) || nrow(matched) == 0) stop("No matching canonical environments found.")

  # ---- Aggregate to bar-friendly summaries ---------------------------------
  # We sum rates across matching rows per model × K × env.
  # (If each env maps to exactly one tile, sum == that tile value.)
  summary_df <- matched %>%
    mutate(model_label = clean_model_label(.data[[row_facet]])) %>%
    mutate(env_label = factor(env_label, levels = env_levels)) %>%
    group_by(model_label, K, env_label) %>%
    summarise(
      preventative = sum(preventative_rate, na.rm = TRUE),
      spillover    = sum(spillover_rate, na.rm = TRUE),
      total_hv     = sum(preventative_rate + spillover_rate, na.rm = TRUE),
      .groups = "drop"
    )

  # Preserve the raw HV magnitude for annotations regardless of plotting mode.
  summary_df <- summary_df %>%
    mutate(total_hv_raw = total_hv)

  if (identical(value_mode, "share_of_hv")) {
    summary_df <- summary_df %>%
      mutate(
        preventative = ifelse(total_hv > 0, preventative / total_hv, 0),
        spillover = ifelse(total_hv > 0, spillover / total_hv, 0)
      )
  }

  summary_long <- summary_df %>%
    tidyr::pivot_longer(
      cols = c("preventative", "spillover"),
      names_to = "component",
      values_to = "value"
    ) %>%
    mutate(
      component   = factor(component, levels = c("preventative", "spillover")),
      env_label   = factor(env_label, levels = env_levels),
      model_label = factor(model_label, levels = c(
        "Basic",
        "Health\n(w = 0)",
        "Linear\n(w = 1)",
        "Power\n(a = 3)",
        "Threshold\n(τ=.6H0)"
      )),
      K           = factor(K, levels = sort(unique(K)))
    )

  zero_hv_df <- summary_df %>%
    mutate(
      env_label = factor(env_label, levels = env_levels),
      model_label = factor(model_label, levels = c(
        "Basic",
        "Health\n(w = 0)",
        "Linear\n(w = 1)",
        "Power\n(a = 3)",
        "Threshold\n(τ=.6H0)"
      )),
      K = factor(K, levels = sort(unique(K)))
    ) %>%
    filter(total_hv_raw <= 0)

  subtitle_final <- if (is.null(subtitle_context) || !nzchar(subtitle_context)) {
    if (identical(value_mode, "share_of_hv")) {
      "Anticipatory (red) and reactive (blue) shares of total hypervigilance across model variants and vigilance cost"
    } else {
      "Anticipatory (red) and reactive (blue) hypervigilance across model variants and vigilance cost"
    }
  } else {
    paste(
      if (identical(value_mode, "share_of_hv")) {
        "Anticipatory (red) and reactive (blue) shares of total hypervigilance across model variants and vigilance cost"
      } else {
        "Anticipatory (red) and reactive (blue) hypervigilance across model variants and vigilance cost"
      },
      subtitle_context,
      sep = " | "
    )
  }

  # ---- Plot: grouped columns ------------------------------------------------
  # position_dodge2(preserve="single") keeps bars aligned even when some groups are missing.
  p_bars <- ggplot(summary_long, aes(x = env_label, y = value, fill = component)) +
    geom_tile(
      data = zero_hv_df,
      aes(x = env_label, y = 0.5),
      inherit.aes = FALSE,
      fill = "grey95",
      width = 0.9,
      height = 1.0
    ) +
    geom_col(
      colour = "black",
      width = 0.70,
      position = position_dodge2(width = 0.80, preserve = "single")
    ) +
    scale_fill_manual(
      values = c(preventative = "#c62828", spillover = "#1565c0"),
      name   = "mechanism",
      labels = c("anticipatory", "reactive")
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = c(0, 0),
      labels = if (identical(value_mode, "share_of_hv")) scales::label_percent(accuracy = 1) else scales::label_number(accuracy = 0.01)
    ) +
    scale_x_discrete(
      limits = env_levels,
      drop = FALSE
    ) +
    facet_grid(
      rows = vars(model_label), 
      cols = vars(K),
      switch = NULL,
      drop = FALSE,
      labeller = label_value
    ) +
    labs(
      title = title,
      subtitle = if (isTRUE(show_subtitle)) subtitle_final else NULL,
      x = "environment",
      y = if (identical(value_mode, "share_of_hv")) "share of total hypervigilance" else "hypervigilance rate"
    ) +
    theme_vigilance(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 13),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 16), face = "bold"),
      axis.title.y = element_text(margin = margin(r = 12), face = "bold"),
      plot.title = element_text(size = 19, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 11, colour = "grey30", hjust = 0.5, margin = margin(t = 10)),
      legend.position = legend_position,
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      strip.text.y.right = element_text(angle = -90, size = 10, face = "plain", hjust = 0.5, vjust = 0.5, margin = margin(l = 8, r = 8)),
      strip.text.x = element_text(size = 13, face = "plain", margin = margin(b = 8)),
      strip.placement = "outside",
      plot.margin = margin(18, 56, 10, 10),
      panel.spacing.y = unit(1.2, "lines"),
      panel.spacing.x = unit(0.5, "lines")
    )
      g <- ggplotGrob(p_bars)
      if (isTRUE(show_k_header)) {
        g <- add_column_header_to_gtable(
          g,
          header = "cost of vigilance (K)",
          fontsize = 14,
          fontface = "bold",
          row_height = 1.7,
          y = 0.62
        )
      }
      patchwork::wrap_elements(full = g)
}

# -----------------------------------------------------------------------------
# Data transformation summary (module-level)
# -----------------------------------------------------------------------------
# - `prepare_mechanism_df()` is the single source of truth for:
#     hv_total, spillover_rate, preventative_rate, and mechanism_fill.
# - The bivariate and split heatmaps:
#     * reuse those prepared columns,
#     * keep LA/LL axis breaks derived from the actual grid,
#     * overlay the same thresholds + SSP/predictability boundaries + region labels.
# - The stacked bars:
#     * match named canonical environments back onto the prepared grid (tolerance-based),
#     * aggregate components per (model, K, environment),
#     * pivot long so ggplot can map mechanism to fill cleanly.
