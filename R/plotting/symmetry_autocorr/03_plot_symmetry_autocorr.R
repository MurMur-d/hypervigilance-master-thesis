# ============================================================
# File: R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R
#
# Purpose: Plotting helpers for the symmetry autocorrelation sweeps (Fig6Aâ€“C).
# Notes:
#   - Pure module: consumes tidy symmetry/autocorrelation tables and returns ggplots.
#   - Relies on `hv_subtitle_with_params()` + layout helpers housed in `R/plotting/utils_*`.
# ============================================================

# ---- Repo-local dependencies ------------------------------------------------
# plot_utils.R:
#   - project theme wrappers (theme_vigilance, theme_supervisor_grid, etc.)
#   - consistent fonts, strip sizing, and figure margins used across the paper
# utils_hv_rate_grid.R:
#   - shared grid-building / hv-rate helpers (often used by the data prep scripts)
# utils_subtitles.R:
#   - helpers for building consistent subtitles from meta + h0 arguments
# utils_plot_layout.R:
#   - gtable utilities to remove repeated axes and add paper-style row/column headers
source("R/core/plot_utils.R")
source("R/plotting/_shared/utils_hv_rate_grid.R")
source("R/plotting/_shared/utils_subtitles.R")
source("R/plotting/_shared/utils_plot_layout.R")

suppressPackageStartupMessages({
  library(dplyr)     # mutate/factor ordering and small summarise steps
  library(ggplot2)   # plotting
  library(patchwork) # used in the SSP vs autocorr panel wrapper at bottom
  library(scales)    # label_number(), percent, etc.
})

# =============================================================================
# Helper: keep_row_metadata()
# =============================================================================
# Some of the layout wrappers expect to find a stable, explicit ordering of
# facet rows. This helper:
#   - inspects df[[row_facet]] to get the facet label values
#   - optionally uses row_labels (caller-provided desired order)
#   - stores the resulting order as an attribute "facet_order" on df
#
# Why use an attribute?
#   - it avoids adding â€œhelper columnsâ€ to the userâ€™s df
#   - you can pass df around and recover the intended facet ordering later
#
# @param df A tidy table.
# @param row_facet Column name (string) that contains row facet labels.
# @param row_labels Optional explicit order to enforce.
# @return Either NULL (if no valid row_facet) or the same df with attr(df,"facet_order").
keep_row_metadata <- function(df, row_facet, row_labels) {
  # If there is no valid facet variable, nothing to store.
  if (is.null(row_facet) || !row_facet %in% names(df)) return(NULL)

  # Choose ordering:
  #   - if row_labels provided, use those
  #   - else: preserve appearance order from df
  levels <- if (is.null(row_labels)) unique(as.character(df[[row_facet]])) else as.character(row_labels)

  # Drop NA levels (facet strips should not include missing labels).
  levels <- levels[!is.na(levels)]

  # Fallback if everything was NA/empty:
  if (length(levels) == 0) levels <- unique(as.character(df[[row_facet]]))

  # Attach ordering to df for later reuse (e.g., wrappers that rebuild labels).
  attr(df, "facet_order") <- levels
  df
}

# =============================================================================
# 1) plot_K_vs_autocorr_heatmap()
# =============================================================================
# Draw a 2D heatmap where:
#   x-axis: vigilance cost (K)
#   y-axis: autocorrelation (typically 1 - (LA + LL) under the symmetry constraint)
#   fill:  HypervigilanceRate_filtered in [0,1]
#
# This function is the core plot builder; it is used both:
#   - directly for single-scenario plots, and
#   - indirectly by plot_K_vs_autocorr_heatmap_by_model() (multi-model rows).
#
# Key design choices:
#   - coord_fixed(ratio = 1) forces square tiles (easy comparison across axes)
#   - fill scale defaults to whiteâ†’black with extra breaks near 0 to show â€œlow HV structureâ€
#   - supports row facets (e.g., model variants) with a consistent row ordering
#   - can add a left gutter title via wrap_autocorr_heatmap_with_row_title()
#
# @param df Tidy symmetry table with at least: K, autocorr, HypervigilanceRate_filtered
# @param subtitle_context Optional subtitle. If NULL and df has attr(df,"meta"),
#        we create a standard subtitle with key parameters.
# @param subtitle_suffix Optional text appended to subtitle_context (e.g., health terminal reward notes).
# @param row_facet Optional column name used for row facets (e.g., "model_label").
# @param row_labels Optional explicit ordering of row facet levels.
# @param add_row_title If TRUE, wrap plot with a paper-style row-title gutter.
# @param row_title Text for the gutter title (e.g., "model variant").
# @param gutter_row_title Horizontal space for the rotated gutter title.
# @param gutter_row_labels Horizontal space for the per-row labels in the gutter.
# @param row_label_width Wrap width for long row facet labels.
# @param fill_scale Optional ggplot2 fill scale override.
# @export
plot_K_vs_autocorr_heatmap <- function(
    df,
    subtitle_context = NULL,
    subtitle_suffix = "",
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
  # ---- Validate input -------------------------------------------------------
  # These two are structurally required for this plot. (HV column is assumed later.)
  stopifnot("K" %in% names(df), "autocorr" %in% names(df))

  # ---- Row faceting logic ---------------------------------------------------
  # We optionally facet by a row_facet column (e.g., model_label).
  # If present, we:
  #   - compute stable ordering
  #   - create a factor column facet_row used solely for ggplot facets
  has_row_facet <- !is.null(row_facet) && row_facet %in% names(df)
  row_levels <- NULL
  if (has_row_facet) {
    df <- keep_row_metadata(df, row_facet, row_labels)
    row_levels <- attr(df, "facet_order")
    df$facet_row <- factor(as.character(df[[row_facet]]), levels = row_levels)
  }

  # ---- Axis breaks aligned to grid -----------------------------------------
  # Most symmetry sweeps are computed on regular grids, so matching ticks to
  # the grid step keeps the heatmap visually â€œsnappedâ€ to underlying cells.
  x_vals <- sort(unique(df$K))
  y_vals <- sort(unique(df$autocorr))
  x_step <- if (length(x_vals) > 1) min(diff(x_vals)) else 1
  y_step <- if (length(y_vals) > 1) min(diff(y_vals)) else 0.1
  xmin <- min(x_vals); xmax <- max(x_vals)
  ymin <- min(y_vals); ymax <- max(y_vals)

  # ---- Subtitle construction (meta-aware) -----------------------------------
  # Many upstream builders attach attr(df,"meta") with fixed parameters.
  # We use it to create consistent subtitles when the caller doesnâ€™t supply one.
  meta <- attr(df, "meta")
  subtitle_context <- if (is.null(subtitle_context) && !is.null(meta)) {
    mode_val <- if (is.null(meta$mode)) "basic" else as.character(meta$mode)
    sprintf(
      "C = %s | D = %s | d = %s | T = %s | N = %s | mode = %s",
      as.character(meta$C), as.character(meta$D), as.character(meta$d),
      as.character(meta$T_steps), as.character(meta$N_agents), mode_val
    )
  } else {
    subtitle_context
  }

  # Append h0 (or similar) if present in meta. This avoids each script having to
  # manually add â€œh0 = ...â€ when comparing variants.
  subtitle_context <- append_meta_h0_subtitle(subtitle_context, meta)

  # If this is a single-panel health plot and the caller did not provide a suffix,
  # we auto-add the terminal reward descriptor so â€œmode=healthâ€ plots are self-contained.
  if (!has_row_facet && !nzchar(subtitle_suffix) && !is.null(meta) && identical(meta$mode, "health")) {
    pa <- if (!is.null(meta$policy_args)) meta$policy_args else list()
    sa <- if (!is.null(meta$sim_args)) meta$sim_args else list()
    subtitle_suffix <- health_reward_subtitle(policy_args = pa, sim_args = sa)
  }

  # Finally, join suffix to main subtitle if both exist.
  if (!is.null(subtitle_context) && nzchar(subtitle_suffix)) {
    subtitle_context <- paste0(subtitle_context, subtitle_suffix)
  }

  # ---- Strip styling logic --------------------------------------------------
  # If we add an external gutter title (add_row_title), we typically hide the default
  # y-strip labels so we can re-draw them in the gutter wrapper.
  strip_y_text <- if (has_row_facet && add_row_title) element_blank() else element_text(size = 12, face = "bold")

  # ---- Fill scale configuration --------------------------------------------
  # Default to grayscale 0â€“1, with more tick marks near 0 to show low-HV structure.
  # Pass fill_scale to override (e.g., viridis, a nonlinear transform, etc.).
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
  # Note: HypervigilanceRate_filtered is assumed to be present (the data prep should ensure this).
  p <- ggplot(
    df,
    aes(x = K, y = autocorr, fill = HypervigilanceRate_filtered)
  ) +
    geom_tile() +
    fill_layer +
    labs(
      title = "Proportion of hypervigilance across autocorrelation",
      subtitle = subtitle_context,
      x = "vigilance cost (K)",
      y = "autocorrelation"
    ) +
    theme_vigilance(base_size = 13) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = seq(xmin, xmax, by = x_step), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(ymin, ymax, by = y_step), expand = c(0, 0)) +
    theme(
      # Keep panels square (important for paper-style grids).
      aspect.ratio = 1,

      # Title/subtitle formatting.
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = if (is.null(subtitle_context) || identical(subtitle_context, "")) element_blank() else element_text(size = 11, margin = margin(b = 6)),

      # Axes formatting.
      axis.title = element_text(size = 12, face = "plain"),
      axis.text = element_text(size = 11),
      axis.title.x = element_text(hjust = 0, margin = margin(t = 8)),
      axis.title.y = element_text(hjust = 0, vjust = 0, margin = margin(r = 10)),

      # Legend.
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      legend.key.height = grid::unit(80, "pt"),
      legend.key.width = grid::unit(22, "pt"),

      # Facet strips: hide y-strip text if we plan to re-draw labels in the wrapper.
      strip.text.y = strip_y_text,
      strip.background.y = if (has_row_facet) element_blank() else element_blank(),
      strip.placement = if (has_row_facet) "outside" else "inside",

      # Space between row panels.
      panel.spacing.y = unit(0.45, "lines"),

      # More compact margins for thesis export.
      plot.margin = margin(t = 8, r = 6, b = 8, l = 70)
    )

  # ---- Facet rows, if requested --------------------------------------------
  if (has_row_facet) {
    p <- p + ggplot2::facet_grid(
      rows = vars(facet_row),
      switch = "y", # show strip labels on left side (paper-style)
      labeller = labeller(facet_row = label_wrap_gen(row_label_width))
    )
    # Stash row levels so wrappers can align gutter labels to panel rows.
    attr(p, "row_levels") <- levels(df$facet_row)
  }

  # ---- Optional column header for single-row plots -------------------------
  # If there is no row facet, add a small â€œvigilance cost (K)â€ label above the panel.
  # This helps when embedding the plot into composites where axes may be cropped.
  if (isTRUE(show_column_header) && !has_row_facet && length(x_vals) > 0) {
    col_title_df <- data.frame(K = x_vals[ceiling(length(x_vals) / 2)], autocorr = ymax + 1e-6)
    p <- p + geom_text(
      data = col_title_df,
      aes(x = K, y = Inf, label = "vigilance cost (K)"),
      inherit.aes = FALSE,
      fontface = "plain",
      vjust = -1.8,
      size = 5
    ) + coord_cartesian(clip = "off")
  }

  # ---- Optional wrapper: add row-title gutter ------------------------------
  # wrap_autocorr_heatmap_with_row_title() is expected to:
  #   - turn the ggplot into a gtable
  #   - add a left gutter column with a single rotated title (row_title)
  #   - add per-row labels aligned with facet rows
  #   - optionally suppress repeated axes for a cleaner matrix look
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

  p
}

# =============================================================================
# 2) plot_K_vs_autocorr_heatmap_by_model()
# =============================================================================
# Convenience wrapper that:
#   1) builds a symmetry grid across model_scenarios using symmetry_grid_K_vs_autocorr_by_model()
#   2) selects a consistent row order (prefer attr(df_models,"row_levels"))
#   3) constructs a standardized subtitle if none provided
#   4) calls plot_K_vs_autocorr_heatmap() with row faceting enabled
#
# Returns a list:
#   - df_autocorr_models: the underlying tidy grid table
#   - p_faceted: the final heatmap (possibly wrapped with gutters)
plot_K_vs_autocorr_heatmap_by_model <- function(
    model_scenarios = default_symmetry_model_scenarios,
    C, d, deltaD, K_values,
    T_steps, states, N_agents,
    step = 0.025,
    base_policy_args = list(),
    base_sim_args = list(),
    subtitle_context = NULL,
    add_row_title = TRUE,
    row_title = "model variant",
    heatmap_fill_scale = NULL
) {
  # ---- Build the stacked grid ----------------------------------------------
  # symmetry_grid_K_vs_autocorr_by_model() is expected to:
  #   - loop over model scenarios
  #   - sweep K and autocorr values (subject to symmetry constraints)
  #   - compute HypervigilanceRate_filtered per cell
  #   - attach metadata such as row_levels and meta (for subtitles)
  df_models <- symmetry_grid_K_vs_autocorr_by_model(
    model_scenarios = model_scenarios,
    C = C, d = d, deltaD = deltaD, K_values = K_values,
    T_steps = T_steps, states = states, N_agents = N_agents,
    step = step,
    base_policy_args = base_policy_args, base_sim_args = base_sim_args
  )
  if (nrow(df_models) == 0) stop("No data returned for the provided model_scenarios")

  # ---- Determine row order --------------------------------------------------
  row_levels <- attr(df_models, "row_levels")
  if (is.null(row_levels)) row_levels <- levels(df_models$model_label)

  # ---- Determine mode label (for subtitle) ---------------------------------
  # Some symmetry grids may mix model modes; we try to summarize.
  mode_label <- unique(as.character(df_models$model))
  mode_label <- if (length(mode_label) == 1) mode_label else "mixed"

  # ---- Default subtitle -----------------------------------------------------
  subtitle_text <- if (is.null(subtitle_context)) {
    sprintf(
      "C = %s | D = %s | d = %s | T = %s | N = %s | mode = %s",
      C, deltaD, d, T_steps, N_agents, paste(mode_label, collapse = ", ")
    )
  } else {
    as.character(subtitle_context)
  }

  # ---- Build plot -----------------------------------------------------------
  p_grid <- plot_K_vs_autocorr_heatmap(
    df_models,
    subtitle_context = subtitle_text,
    subtitle_suffix = "",
    row_facet = "model_label",
    row_labels = row_levels,
    add_row_title = add_row_title,
    row_title = row_title,
    fill_scale = heatmap_fill_scale
  )

  list(df_autocorr_models = df_models, p_faceted = p_grid)
}

# =============================================================================
# 3) plot_ssp_vs_autocorr_by_model_K_hvstyle()
# =============================================================================
# Plot HV in SSPâ€“autocorrelation space, faceted by model (rows) and K (cols).
#
# This is a different â€œviewâ€ than the KÃ—autocorr heatmap:
#   - Here, x is SSP (stationary stressor probability = LA/(LA+LL))
#   - y is autocorrelation (= 1 - (LA + LL))
#   - each tile corresponds to a named environment point (or small grid around it)
#
# It also draws:
#   - environment bounding boxes per env_label (rectangles)
#   - environment label text placed at the midpoint of each envâ€™s region
#
# @param df Table with model_label, K, SSP, autocorr, env_label and hv columns.
# @param use_no_stressor If TRUE, prefer a no-stressor HV metric when available.
# @param subtitle_context Optional override; otherwise uses hv_subtitle_with_params(meta,...).
# @return A patchwork-wrapped gtable so axes/headers match paper layout.
plot_ssp_vs_autocorr_by_model_K_hvstyle <- function(
    df,
    use_no_stressor = TRUE,
    subtitle_context = NULL
) {
  stopifnot(nrow(df) > 0)

  # ---- Choose which HV column to visualize ---------------------------------
  # Priority:
  #   1) hv_no_stressor (if caller wants it and it exists)
  #   2) hv_rate_no_stressor (alternate naming)
  #   3) hv_rate (fallback)
  fill_column <- if (use_no_stressor && "hv_no_stressor" %in% names(df)) {
    "hv_no_stressor"
  } else if ("hv_rate_no_stressor" %in% names(df)) {
    "hv_rate_no_stressor"
  } else {
    "hv_rate"
  }

  # ---- Subtitle from meta ---------------------------------------------------
  meta <- attr(df, "meta")
  subtitle_final <- hv_subtitle_with_params(meta, subtitle_context)

  # ---- Build environment bounding boxes ------------------------------------
  # For each environment label, compute the min/max span in SSP-autocorr space.
  # Then pad slightly so the rectangles are readable and not tight to tiles.
  env_boxes <- df %>%
    group_by(env_label) %>%
    summarise(
      SSP_min = min(SSP, na.rm = TRUE),
      SSP_max = max(SSP, na.rm = TRUE),
      ac_min = min(autocorr, na.rm = TRUE),
      ac_max = max(autocorr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # small padding: 5% of each envâ€™s span, with a minimum absolute pad of 0.01
      SSP_pad = pmax(0.01, (SSP_max - SSP_min) * 0.05),
      ac_pad = pmax(0.01, (ac_max - ac_min) * 0.05),
      x_min = SSP_min - SSP_pad,
      x_max = SSP_max + SSP_pad,
      y_min = ac_min - ac_pad,
      y_max = ac_max + ac_pad,
      # midpoints used for label placement
      x_mid = (SSP_min + SSP_max) / 2,
      y_mid = (ac_min + ac_max) / 2
    )

  # ---- Factor ordering for stable facet layout ------------------------------
  df <- df %>%
    mutate(
      model_label = factor(model_label, levels = rev(unique(model_label))),
      K = factor(K, levels = sort(unique(K))),
      fill_value = .data[[fill_column]]
    )

  # ---- Caption: define SSP/autocorr -----------------------------------------
  caption_text <- paste(
    "SSP = LA / (LA + LL)",
    "autocorrelation = 1 - (LA + LL)",
    "Boxes and labels indicate named environments in SSP-autocorr space.",
    sep = "\n"
  )

  # ---- Core plot ------------------------------------------------------------
  p <- ggplot(df, aes(x = SSP, y = autocorr, fill = fill_value)) +
    # Tiles represent (SSP, autocorr) points; width/height tuned for a visually pleasing grid.
    geom_tile(color = "grey85", linewidth = 0.3, width = 0.085, height = 0.085) +

    # Draw an outline rectangle per environment cluster.
    geom_rect(
      data = env_boxes,
      aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
      inherit.aes = FALSE,
      fill = NA,
      color = "black",
      linewidth = 0.45
    ) +

    # Add environment labels at the midpoints.
    geom_text(
      data = env_boxes,
      aes(x = x_mid, y = y_mid, label = env_label),
      inherit.aes = FALSE,
      size = 3.2,
      fontface = "bold"
    ) +

    # Facet into a matrix: model rows Ã— K columns.
    facet_grid(model_label ~ K, switch = "y") +

    # Heatmap fill scale: grayscale HV, consistent with other figures.
    scale_fill_gradientn(
      colours = c("#ffffff", "#bbbbdd", "#7756a4", "#2c1544"),
      limits = c(0, 1),
      na.value = "transparent",
      name = "hyper-\nvigilance",
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = label_number(accuracy = 0.01)
    ) +

    # Put a compact vertical colorbar on the right.
    guides(
      fill = guide_colourbar(
        title = "hyper-\nvigilance",
        direction = "vertical",
        title.position = "top",
        barwidth = grid::unit(0.22, "in"),
        barheight = grid::unit(1.4, "in"),
        frame.colour = "black",
        frame.linewidth = 0.6,
        ticks.colour = "black",
        ticks.linewidth = 0.4
      )
    ) +

    # Titles and labels.
    labs(
      title = "Hypervigilance across SSP and autocorrelation (model comparison)",
      subtitle = subtitle_final,
      x = "SSP (stationary stressor probability)",
      y = "autocorrelation",
      caption = caption_text
    ) +

    # Theme: uses repo-consistent styling for strips and typography.
    theme_vigilance(base_size = 12, strip_size = 11) +
    theme(
      axis.text = element_text(size = 11),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      panel.spacing = grid::unit(0.3, "lines"),
      legend.position = "right",
      legend.direction = "vertical",
      legend.background = element_blank(),
      legend.key = element_rect(fill = "white", colour = "black", linewidth = 0.4),
      legend.spacing.y = grid::unit(2, "pt"),
      legend.margin = margin(t = 1, r = 2, b = 2, l = 2),
      plot.margin = margin(t = 1, r = 2, b = 12, l = 2, unit = "mm"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, margin = margin(t = 30, unit = "pt"))
    ) +
    coord_fixed(ratio = 1) # keep squares square

  # ---- Layout cleanup: keep only left/bottom axes + add column header --------
  # keep_left_bottom_axes() reduces visual clutter in dense facet matrices by
  # removing repeated axes. The preserve_bottom_axes flag determines whether
  # bottom axes are kept across columns.
  g <- keep_left_bottom_axes(p, preserve_bottom_axes = TRUE)

  # Add a spanning column header above the facet columns.
  g <- add_column_header_to_gtable(g, header = "vigilance cost ( K )")

  # Wrap the gtable as a patchwork element so callers can combine it in layouts.
  patchwork::wrap_elements(full = g)
}
