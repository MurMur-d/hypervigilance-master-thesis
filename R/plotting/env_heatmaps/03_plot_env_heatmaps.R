# ============================================================
# File: R/plotting/env_heatmaps/03_plot_env_heatmaps.R
# Purpose: Encapsulate the environment heatmap builders for the Fig2A pipeline.
# Author: Codex             Last updated: 2025-12-09
#
# High-level role in the project:
#   This module is the “plotting façade” for environment (λA, λL) heatmaps.
#   It sits between:
#     (1) data-generation code that produces LA/LL × K × model grids of HV, and
#     (2) figure scripts that assemble multi-panel manuscript figures.
#
# Design principles:
#   - Pure helper module: no file I/O, no saving plots, no global state mutation
#     beyond attaching metadata attributes to returned data frames.
#   - Reusable plotting logic: figure scripts source this file and call a small
#     number of stable entry points (build_* / plot_* functions).
#   - Inspectability: each helper is documented with its transformations so
#     reviewers (and future-you) can trace how raw simulation outputs become
#     the final faceted heatmaps.
# ============================================================

# -----------------------------------------------------------------------------
# Imports / upstream helpers
# -----------------------------------------------------------------------------
# Provides hypervigilance grid builders (data generation) or wrappers:
source("R/plotting/_shared/utils_hv_rate_grid.R")

# Provides shared themes and caption helpers used across plots:
source("R/core/plot_utils.R")

# Provides mechanism overlay helpers + threshold overlays used later:
# (e.g., add_threshold_layer, SSP boundary functions, region label builders)
source("R/plotting/mechanism_env/00_data_prep_mechanism_data.R")
source("R/plotting/_shared/utils_health_env_scenarios.R")

# Packages:
# - dplyr: tidy transforms and factor ordering
# - ggplot2: heatmap rendering
# - patchwork: wrapping gtables/ggplots in figure-friendly layouts
# - purrr: map_dfr for looping over model variants
# - scales: label helpers (e.g., formatted legend breaks)
# - rlang: tidy evaluation for dynamic column selection
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(purrr)
  library(scales)
  library(rlang)
})

# -----------------------------------------------------------------------------
# map_model_label_values(): label normalization for facet strips
# -----------------------------------------------------------------------------
# Why this exists:
#   Model labels used in code (e.g., “Health threshold terminal reward…”) can be
#   verbose and inconsistent across pipelines. Heatmap facet headers need short,
#   stable labels to:
#     - keep the figure compact,
#     - avoid line-wrapping artifacts in facet strips,
#     - and make comparisons across panels easier.
#
# What it does:
#   Takes a vector of raw model labels and maps them to a concise “strip label”
#   used in facet headers. Any label that does not match known patterns is
#   returned unchanged (fails open).
#
# NOTE:
#   This file defines its own mapper for concision, but your project also has
#   a more canonical mapper (`utils_model_labels.R`). If you want one source of
#   truth, this helper is the one to delete/redirect later; for now it keeps
#   the Fig2A facet strips stable even if upstream labels change slightly.
# -----------------------------------------------------------------------------
#' Normalize verbose model labels so the facet headers stay concise.
#'
#' @param values Character vector of raw model names.
#' @return Cleaned labels for the facet strips.
map_model_label_values <- function(values) {
  vapply(
    as.character(values),
    function(lbl) {
      # “Basic” model (no health state).
      if (grepl("Basic", lbl, ignore.case = TRUE)) {
        return("basic")
      }
      # Health model without terminal reward; the literal "?" is a placeholder in this
      # version (your canonical label elsewhere uses β).
      if (grepl("Health", lbl, ignore.case = TRUE)) {
        return("health (w = 0)")
      }
      # Linear terminal reward variant.
      if (grepl("Linear", lbl, ignore.case = TRUE)) {
        return("linear (w = 1)")
      }
      # Power terminal reward variant.
      if (grepl("Power", lbl, ignore.case = TRUE)) {
        return("power (a = 3)")
      }
      # Threshold terminal reward variant.
      if (grepl("Threshold", lbl, ignore.case = TRUE)) {
        return("threshold (t = 0.6*H0)")
      }
      # Fall-through: keep whatever string was supplied.
      lbl
    },
    character(1)
  )
}

# -----------------------------------------------------------------------------
# build_env_heatmaps_for_models(): generate HV grids for all model variants
# -----------------------------------------------------------------------------
# Intended use:
#   This is the “data constructor” for Fig2A’s environment heatmaps. It loops over
#   your canonical model specification table and generates a stacked grid that
#   contains:
#     - LA/LL coordinates,
#     - cost K (via RatioDK sweep),
#     - hypervigilance rate (hv),
#     - derived descriptors for captions and overlays (ssp, autocorr),
#     - model metadata (id/label/type),
#     - and a few fixed parameters stored as attributes.
#
# Key assumptions:
#   - `model_specs` comes from `default_model_specs()` and contains:
#       model_id, model_label, model_type, policy_args (list-col), sim_args (list-col)
#   - `hypervigilance_grid_by_Kratio()` returns a tidy table that includes
#       HypervigilanceRate_filtered plus the grid coordinates LA, LL, and K.
#
# Output:
#   - A single stacked data.frame/tibble with one row per cell per model variant.
#   - A `fixed_params` attribute used downstream for captions and figure metadata.
# -----------------------------------------------------------------------------
#' Build the LA/LL heatmaps for every registered model + vigilance cost.
#'
#' @param model_specs Data frame from `default_model_specs()` describing the
#'   policy and simulation arguments per variant.
#' @param C Baseline relaxed cost.
#' @param D Vigilant damage cost.
#' @param d Relaxed damage cost.
#' @param T_steps Number of DP time steps to evaluate.
#' @param states State labels included in the DP solver.
#' @param N_agents Agents simulated per grid cell.
#' @param grid_step LA/LL resolution.
#' @param ratio_step Step size for the K/D ratio sweep.
#' @return Data frame stacking every hypervigilance cell plus metadata for downstream plotting.
#' @details
#'   - Runs `hypervigilance_grid_by_Kratio()` once per model variant and K ratio.
#'   - Annotates the derived hv rate along with SSP/autocorrelation helpers for captions.
#'   - Records the policy/sim arguments so captions can include the configuration.
build_env_heatmaps_for_models <- function(
  model_specs = default_model_specs(h0 = 35),
  C = 0, D = 10, d = 0,
  T_steps = 10,
  states = c("K", "Kd", "C", "CD"),
  N_agents = 1000,
  grid_step = 0.05,
  ratio_step = 0.2
) {
  # Console message is useful in long figure pipelines: this is the expensive step.
  message("Building full LA/LL environment heatmaps for all model variants...")

  # Loop across model variants and row-bind the resulting grids.
  # map_dfr() ensures we end with one combined data frame rather than a list.
  result <- purrr::map_dfr(seq_len(nrow(model_specs)), function(i) {
    spec <- model_specs[i, ]

    # Compute the full LA/LL grid for this model variant across the K/D ratio sweep.
    # This is where DP + simulation work happens (upstream helper).
    df_env <- hypervigilance_grid_by_Kratio(
      D = D, C = C, d = d,
      T_steps = T_steps,
      states = states,
      N_agents = N_agents,
      grid_step = grid_step,
      ratio_step = ratio_step,
      model = spec$model_type,
      policy_args = spec$policy_args[[1]],
      sim_args    = spec$sim_args[[1]]
    )

    # Keep the plotting schema intentionally small and explicit:
    #   - avoids leaking extra columns into faceting logic,
    #   - keeps plotting stable if upstream returns additional fields later.
    df_env %>%
      transmute(
        # Model identifiers
        model_id   = spec$model_id,
        model      = spec$model_label,
        model_type = spec$model_type,

        # Cost sweep coordinates
        RatioDK,
        K,

        # Environment coordinates
        LA,
        LL,

        # Hypervigilance fill variable (rename to a stable "hv" field)
    hv = HypervigilanceRate_filtered,

        # Derived environment descriptors:
        # - ssp: steady-state probability of stressor presence (π_S) implied by LA/LL
        # - autocorr: persistence proxy = 1 - (LA+LL) in your short-memory environment
        ssp = ifelse(LA + LL > 0, LA / (LA + LL), NA_real_),
        autocorr = 1 - (LA + LL),

        # Repeat D into each row so downstream can compute analytic threshold lines
        # without needing to carry a separate meta object.
        D = D
      )
  })

  # Keep a structured record of the args used to build these grids.
  # This is used downstream to build subtitles/captions that are reproducible.
  subtitle_meta <- list(
    policy_args = if ("policy_args" %in% names(model_specs)) model_specs$policy_args else NULL,
    sim_args = if ("sim_args" %in% names(model_specs)) model_specs$sim_args else NULL
  )

  # Attach the pipeline configuration as an attribute rather than polluting the table.
  # Plotting functions can read this attribute to produce consistent subtitles.
  attr(result, "fixed_params") <- list(
    C = C,
    D = D,
    d = d,
    T_steps = T_steps,
    states = states,
    N_agents = N_agents,
    subtitle_meta = subtitle_meta
  )

  result
}

# -----------------------------------------------------------------------------
# build_env_key_panel(): reference panel for thresholds + region labels
# -----------------------------------------------------------------------------
# Uses the same heatmap builder as the main plot so tile size/limits/coord_fixed
# match a single facet panel exactly.
build_env_key_panel <- function(
  df,
  k_value,
  axis_max_breaks = 4,
  D_fallback = NULL,
  panel_title = "Environment\nKey",
  x_label = NULL,
  y_label = NULL
) {
  base_text_size <- 16
  axis_text_size <- base_text_size
  axis_title_size <- base_text_size + 2
  label_text_size <- base_text_size - 1

  df_key <- dplyr::filter(df, K == k_value)
  # Use an in-range constant fill so geom_tile doesn't drop rows with NA.
  df_key$key_fill <- 0
  df_key$hv_total <- 0                 # required for region label placement

  x_vals <- sort(unique(df_key$LA))
  y_vals <- sort(unique(df_key$LL))
  x_step <- if (length(x_vals) > 1) min(diff(x_vals)) else 0.1
  y_step <- if (length(y_vals) > 1) min(diff(y_vals)) else 0.1
  tile_step <- min(x_step, y_step)
  tile_pad <- tile_step / 2
  xmin <- min(x_vals, 0); xmax <- max(x_vals, 0.5)
  ymin <- min(y_vals, 0); ymax <- max(y_vals, 0.5)
  x_limits <- c(0, 0.5)
  y_limits <- c(0, 0.5)

  d_value <- if ("D" %in% names(df_key)) unique(df_key$D)[1] else D_fallback
  threshold_segments <- build_standard_threshold_lines(
    K_levels = k_value,
    D = d_value,
    x_range = x_limits,
    y_range = y_limits
  )
  boundaries <- build_ssp_predictability_boundaries(x_limits, y_limits)
  region_labels <- build_env_region_labels(df_key, x_limits, y_limits)
  if (!is.null(region_labels) && nrow(region_labels) > 0) {
    region_labels$label_colour <- "black" # force black labels for the key panel
  }

  base_plot <- plot_env_heatmap_faceted_by_K(
    df_key,
    rate_column = "key_fill",
    subtitle_context = NULL,
    axis_max_breaks = axis_max_breaks
  ) +
    ggplot2::scale_fill_gradient(limits = c(0, 1), low = "white", high = "white",
                                 na.value = "white", guide = "none") +
    ggplot2::labs(title = panel_title, subtitle = NULL, fill = NULL, x = x_label, y = y_label) +
    ggplot2::scale_x_continuous(limits = x_limits, breaks = seq(0, 0.5, 0.1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = y_limits, breaks = seq(0, 0.5, 0.1), expand = c(0, 0)) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_blank(), # keep strip height for facet-size parity
      strip.background = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = axis_text_size),
      axis.title = if (is.null(x_label) && is.null(y_label)) ggplot2::element_blank() else ggplot2::element_text(size = axis_title_size, face = "plain"),
      axis.ticks.length = grid::unit(3, "pt"),
      plot.title = ggplot2::element_text(size = base_text_size, face = "plain", hjust = 0.5, margin = ggplot2::margin(b = 4)),
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 6, unit = "pt"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8)
    )

  base_plot <- add_ssp_boundaries(base_plot, boundaries)
  if (!is.null(region_labels) && nrow(region_labels) > 0) {
    base_plot <- base_plot +
      ggplot2::geom_text(
        data = region_labels,
        mapping = ggplot2::aes(
          x = la_label, y = ll_label, label = region_code,
          hjust = hjust, vjust = vjust
        ),
        inherit.aes = FALSE,
        size = label_text_size / ggplot2::.pt,
        colour = "black",
        fontface = "plain",
        lineheight = 0.9,
        show.legend = FALSE,
        check_overlap = TRUE
      )
  }
  base_plot
}

# -----------------------------------------------------------------------------
# build_env_heatmap_base_key(): minimal ggplot builder (older/simple path)
# -----------------------------------------------------------------------------
# What it provides:
#   A compact, general-purpose heatmap builder returning:
#     - df_plot: the (optionally mutated) data frame used for plotting
#     - plot  : a ggplot object
#
# Why it still exists:
#   This helper is a “small surface area” builder: it is useful for quick checks
#   or for alternative layers where you want full control in a calling script.
#   The more feature-complete builder used by Fig2A is `build_env_heatmap_base()`.
#
# Important:
#   This helper supports two fill modes:
#     - gradient: compute a color scale (typical for hv)
#     - identity: use already-specified colors in the fill column (overlays)
# -----------------------------------------------------------------------------
#' Base ggplot builder for environment heatmaps.
#'
#' @param df Tidy data that includes LA/LL coordinates and the fill column.
#' @param fill_column Column to use for tile fill.
#' @param fill_type Either "gradient" (default) or "identity".
#' @param fill_label Legend title.
#' @param facet_rows Column mapped to facet rows.
#' @param facet_cols Column mapped to facet columns.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
build_env_heatmap_base_key <- function(
    df,
    fill_column = "hv_total",
    fill_type = "gradient",
    fill_label = "hypervigilance",
    facet_rows = "model_label",
    facet_cols = "K",
    title = NULL,
    subtitle = NULL,
    env_scenarios = NULL,
    D = NULL,
    add_threshold_lines = FALSE,
    add_ssp_boundaries = FALSE,
    add_region_labels = FALSE
) {
  # ---- Defensive checks -------------------------------------------------------
  stopifnot(is.data.frame(df))

  # Use tidy evaluation so the caller can pass fill_column as a string.
  fill_sym <- rlang::sym(fill_column)

  # ---- Base heatmap layer -----------------------------------------------------
  plot <- ggplot(df, aes(x = LA, y = LL, fill = !!fill_sym)) +
    geom_tile(color = "grey80") +
    labs(
      title = title,
      subtitle = subtitle,
      x = expression(lambda[A]),
      y = expression(lambda[L]),
      fill = fill_label
    ) +
    theme_vigilance(base_size = 11) +
    coord_fixed(ratio = 1)

  # ---- Faceting ---------------------------------------------------------------
  # Three cases:
  #   (1) rows + cols: a full facet_grid matrix
  #   (2) only rows or only cols: facet_wrap
  #   (3) none: no faceting
  if (!is.null(facet_rows) && facet_rows %in% names(df) &&
      !is.null(facet_cols) && facet_cols %in% names(df)) {
    plot <- plot + facet_grid(rows = vars(!!rlang::sym(facet_rows)), cols = vars(!!rlang::sym(facet_cols)))
  } else if (!is.null(facet_rows) && facet_rows %in% names(df)) {
    plot <- plot + facet_wrap(vars(!!rlang::sym(facet_rows)))
  } else if (!is.null(facet_cols) && facet_cols %in% names(df)) {
    plot <- plot + facet_wrap(vars(!!rlang::sym(facet_cols)))
  }

  # ---- Fill scale -------------------------------------------------------------
  # gradient = continuous HV color scale
  # identity = treat fill column as already-encoded colors
  if (fill_type == "gradient") {
    plot <- plot + scale_fill_gradientn(
      colours = c("#ffffff", "#bbbbdd", "#7756a4", "#2c1544"),
      limits = c(0, 1),
      na.value = "transparent"
    )
  } else {
    plot <- plot + scale_fill_identity()
  }

  list(df_plot = df, plot = plot)
}

# -----------------------------------------------------------------------------
# wrap_label_before_paren(): strip label line-breaking helper
# -----------------------------------------------------------------------------
# Purpose:
#   Facet strip labels often include parameter notes in parentheses, e.g.:
#     "Linear (β = 1)"
#   On narrow plots, these can overflow or produce ugly wrapping. This helper
#   inserts a newline before the first parenthesis so labels become:
#     "Linear\n(β = 1)"
#
# This keeps the strip height compact and consistent across rows.
# -----------------------------------------------------------------------------
#' Break row labels at parentheses to keep facet strips tidy.
#'
#' @param text Character vector with row labels that may contain parenthetical notes.
#' @return The same labels with newlines inserted before any parentheses.
wrap_label_before_paren <- function(text) {
  if (is.null(text)) return(text)
  vapply(
    text,
    function(x) {
      if (is.na(x)) return(NA_character_)
      if (grepl("\\(", x)) sub(" \\(", "\n(", x)
      else x
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# -----------------------------------------------------------------------------
# build_env_heatmap_base(): feature-complete builder used for Fig2A-style panels
# -----------------------------------------------------------------------------
# This is the main “plot constructor” in this module.
#
# What it does:
#   1) validates required columns exist
#   2) optionally filters K columns (K_filter)
#   3) normalizes and orders facet row/column factors (stable panel ordering)
#   4) sets axis limits/breaks so LA/LL panels are square and aligned
#   5) builds a shared HV fill scale with emphasis on low HV values
#   6) optionally adds:
#        - analytic threshold lines (vertical/horizontal) given D and K values
#        - SSP/predictability boundary overlays
#        - region labels (e.g., "L-P", "H-U") using helper functions
#   7) returns plot + the processed df_plot and the facet spec
#
# Output contract:
#   A list with:
#     - plot: the ggplot object
#     - df_plot: post-processed data used for plotting
#     - facet_layer: facet object used (for downstream gtable editing)
# -----------------------------------------------------------------------------
#' Build the ggplot2 base for the hypervigilance heatmap panel.
#'
#' @param df Tidy data from build_env_heatmaps_for_models().
#' @param fill_column Column used as the fill value (default hv).
#' @param fill_type Choose 'gradient' for hv or 'identity' for overlay layers.
#' @param fill_label Legend title for the fill scale.
#' @param facet_rows Column that maps to facet rows.
#' @param facet_cols Column that maps to facet columns.
#' @param env_scenarios Optional environment metadata for captions.
#' @param D Optional D parameter forwarded to threshold helpers.
#' @param add_threshold_lines Whether to display the analytic threshold lines.
#' @param add_ssp_boundaries Whether to overlay SSP boundaries.
#' @param add_region_labels Whether to annotate SSP region labels.
#' @param caption_suffix Optional suffix appended to auto-generated captions.
#' @param threshold_segments Precomputed threshold segments (optional).
#' @return List with the ggplot2 object, the processed data frame, and the facet layer.
build_env_heatmap_base <- function(
  df,
  fill_column = "hv",
  fill_type = c("gradient", "identity"),
  fill_label = "hypervigilance",
  facet_rows = "model",
  facet_cols = "K",
  K_filter = NULL,
  title = expression(paste("Proportion of hypervigilance across ", lambda[A], "-", lambda[L], " environments")),
  subtitle = "Rows = model variants   |   Columns = vigilance cost (K)",
  caption = NULL,
  x_label = "P(arrive)",
  y_label = "P(leave)",
  env_scenarios = NULL,
  caption_suffix = "Code positions: 1st = SSP level (L/M/H), 2nd = predictability (P/U), 3rd = autocorr level (L/M/H).",
  D = NULL,
  add_threshold_lines = FALSE,
  add_ssp_boundaries = TRUE,
  add_region_labels = TRUE,
  threshold_segments = NULL
) {

  # ---- Argument normalization ------------------------------------------------
  fill_type <- match.arg(fill_type)

  # ---- Schema validation -----------------------------------------------------
  # We require LA/LL for coordinates plus:
  #   - facet_rows: row facet field (e.g., model)
  #   - facet_cols: column facet field (e.g., K)
  #   - fill_column: the HV (or overlay) value
  required <- c("LA", "LL", facet_rows, facet_cols, fill_column)
  stopifnot(all(required %in% names(df)))

  # ---- Optional: enrich environment scenarios (for captions) ------------------
  # If the caller passes env_scenarios with LA/LL/env_label, we can compute
  # autocorr classes that are used by env_label_note() for readable captions.
  enriched_envs <- NULL
  if (!is.null(env_scenarios) &&
      is.data.frame(env_scenarios) &&
      all(c("LA", "LL", "env_label") %in% names(env_scenarios))) {
    enriched_envs <- env_scenarios %>%
      mutate(
        autocorr = 1 - (LA + LL),
        autocorr_class = dplyr::case_when(
          autocorr < 0.33 ~ "L",
          autocorr > 0.66 ~ "H",
          TRUE ~ "M"
        )
      )
  }

  # ---- Optional: restrict which K columns appear -----------------------------
  # Useful when building manuscript figures that only show {1,5,9}, etc.
  if (!is.null(K_filter)) {
    df <- df %>% dplyr::filter(.data[[facet_cols]] %in% K_filter)
  }

  # ---- Facet ordering / label cleaning ---------------------------------------
  # Row labels are normalized via map_model_label_values() and then wrapped
  # before parentheses to keep facet strips tidy.
  row_raw <- df[[facet_rows]]
  col_raw <- df[[facet_cols]]

  row_decoded <- map_model_label_values(row_raw)
  row_values_raw <- unique(na.omit(row_decoded))
  if (length(row_values_raw) == 0) row_values_raw <- unique(as.character(row_raw))
  row_values <- wrap_label_before_paren(row_values_raw)

  col_values <- unique(na.omit(as.character(col_raw)))
  if (length(col_values) == 0) col_values <- unique(as.character(col_raw))

  # Build df_plot:
  #   - create a stable fill_value used by ggplot
  #   - store hv_total as an alias (some overlay helpers expect this name)
  #   - convert facet columns to factors with controlled ordering
  df_plot <- df %>%
    mutate(
      fill_value = .data[[fill_column]],
      hv_total = .data[[fill_column]],

      # Wrapped facet row labels for nicer strips.
      facet_row_wrapped = wrap_label_before_paren(map_model_label_values(.data[[facet_rows]])),

      # Overwrite facet_rows column with a factor so facet order is stable.
      !!facet_rows := factor(facet_row_wrapped, levels = row_values),

      # Ensure columns are ordered numerically if possible (or by K_filter order).
      !!facet_cols := factor(
        .data[[facet_cols]],
        levels = if (is.null(K_filter)) sort(unique(col_values)) else K_filter
      )
    )

  # Facet specification is kept explicitly because downstream helpers sometimes
  # convert the ggplot to a gtable and then add row/column headers.
  facet_layer <- ggplot2::facet_grid(
    rows = vars(.data[[facet_rows]]),
    cols = vars(.data[[facet_cols]])
  )

  # ---- Axis limits/breaks ----------------------------------------------------
  # Heatmaps are drawn as square panels (coord_fixed), so we want x/y to share
  # common scaling and consistent break locations.
  x_vals <- df_plot$LA
  y_vals <- df_plot$LL
  x_vals <- x_vals[is.finite(x_vals)]
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(x_vals) == 0) x_vals <- 0
  if (length(y_vals) == 0) y_vals <- 0

  # Compute tile size so edge cells are fully visible (match basic layout).
  x_step <- if (length(unique(x_vals)) > 1) min(diff(sort(unique(x_vals)))) else 0.1
  y_step <- if (length(unique(y_vals)) > 1) min(diff(sort(unique(y_vals)))) else 0.1
  tile_step <- min(x_step, y_step)
  tile_pad <- tile_step / 2

  # Force at least [0, 0.5] range so the canonical environment space is visible,
  # even if a filtered dataset is smaller, then pad to avoid clipping tiles.
  x_min <- min(x_vals, 0)
  x_max <- max(x_vals, 0.5)
  y_min <- min(y_vals, 0)
  y_max <- max(y_vals, 0.5)

  x_limits <- c(x_min - tile_pad, x_max + tile_pad)
  y_limits <- c(y_min - tile_pad, y_max + tile_pad)

  x_breaks <- seq(0, max(0.5, x_max), by = 0.1)
  y_breaks <- seq(0, max(0.5, y_max), by = 0.1)

  # ---- Threshold segments (optional) ----------------------------------------
  # If add_threshold_lines is TRUE and segments were not supplied, compute them
  # using K levels and D. This draws:
  #   - vertical line at λA* = K / D
  #   - horizontal line at λL* = 1 - (K / D)
  # The helper returns segments clipped to x_limits/y_limits.
  K_numeric <- suppressWarnings(as.numeric(col_values))
  K_numeric <- K_numeric[!is.na(K_numeric)]
  if (isTRUE(add_threshold_lines) && is.null(threshold_segments) &&
      length(K_numeric) > 0 && !is.null(D)) {
    threshold_segments <- build_standard_threshold_lines(
      K_levels = K_numeric,
      D = D,
      x_range = x_limits,
      y_range = y_limits
    )
  }

  # ---- Fill scale ------------------------------------------------------------
  # For HV: use the grayscale ramp that matches the reference figure.
  # For overlays: identity scale so fill_value is treated as a literal colour.
  fill_scale <- if (fill_type == "gradient") {
    list(
      ggplot2::scale_fill_gradient(
        limits = c(0, 1),
        low = "white",
        high = "black",
        name = fill_label,
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = scales::label_number(accuracy = 0.01)
      ),
      ggplot2::guides(
        fill = ggplot2::guide_colourbar(
          title = fill_label,
          direction = "horizontal",
          title.position = "top",
          barwidth = grid::unit(4, "in"),
          barheight = grid::unit(0.35, "in"),
          frame.colour = "black",
          frame.linewidth = 0.6,
          ticks = TRUE,
          draw.ulim = TRUE,
          draw.llim = TRUE,
          ticks.colour = "black",
          ticks.linewidth = 0.4,
          title.hjust = 0.5,
          label.position = "bottom"
        )
      )
    )
  } else {
    list(
      ggplot2::scale_fill_identity(guide = "none")
    )
  }

  # ---- Caption construction ---------------------------------------------------
  # If no caption is supplied but env metadata exists, build a standard note.
  caption_final <- caption
  if (is.null(caption_final) && !is.null(enriched_envs)) {
    caption_final <- env_label_note(enriched_envs)
  }
  # Append suffix explaining region-code scheme (if requested).
  if (!is.null(caption_final) && !is.null(caption_suffix)) {
    caption_final <- paste(caption_final, caption_suffix, sep = "\n")
  }

  # ---- Build ggplot -----------------------------------------------------------
  p <- ggplot(df_plot, aes(x = LA, y = LL, fill = fill_value)) +
    # No tile border here because the manuscript grid can look “busy” with borders;
    # panel borders are added instead.
    geom_tile(color = NA, width = tile_step, height = tile_step) +
    facet_layer +
    fill_scale +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      caption = caption_final
    ) +
    theme_thesis_heatmap(base_size = 16) +
    theme(
      # Facet strip text: larger to be readable in multi-panel figures.
      strip.text = ggplot2::element_text(size = 18, face = "plain"),
      strip.text.x = ggplot2::element_text(size = 18, face = "plain"),
      strip.text.y = ggplot2::element_text(size = 16, face = "plain"),

      # Spacing between panels (x and y directions).
      panel.spacing = grid::unit(0.45, "lines"),

      # Legend shown only when we render the HV gradient.
      legend.position = if (fill_type == "gradient") "bottom" else "none",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.background = ggplot2::element_blank(),

      # Legend sizing tuned for manuscript readability.
      legend.key.height = grid::unit(0.45, "in"),
      legend.key.width = grid::unit(0.7, "in"),
      legend.spacing.y = grid::unit(2, "pt"),
      legend.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2),

      # Axis text sizes tuned for dense facet matrices.
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),
      axis.text.y = ggplot2::element_text(angle = 0, size = 12),

      # Axis titles bigger than tick labels to anchor interpretation.
      axis.title.x = ggplot2::element_text(hjust = 0, vjust = 0, margin = ggplot2::margin(t = 8), size = 20),
      axis.title.y = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0, margin = ggplot2::margin(r = 16), size = 20),

      legend.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),

      # Panel border makes each heatmap cell matrix “frame” visible without tile borders.
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.7),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),

      # Remove internal grids; heatmaps read better without them.
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Plot margin tweaks: keep margins tight for compact multi-panel figures.
      plot.margin = ggplot2::margin(t = 4, r = 4, b = 4, l = 4, unit = "mm")
    ) +
    theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, b = 2, unit = "mm"), face = "plain", size = 24),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(t = 0, b = 2, unit = "mm"), face = "plain", size = 16),
      plot.caption = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = 30, unit = "pt"), size = 12)
    ) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = sprintf("%.1f", x_breaks),
      limits = x_limits,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = sprintf("%.1f", y_breaks),
      limits = y_limits,
      expand = c(0, 0)
    ) +
    # If threshold lines are off, we still want any accidental colour scales suppressed.
    (if (isTRUE(add_threshold_lines)) list() else list(ggplot2::scale_colour_identity(guide = "none")))

  # ---- Optional overlays ------------------------------------------------------
  if (isTRUE(add_threshold_lines) && !is.null(threshold_segments)) {
    p <- add_threshold_layer(p, threshold_segments)
  }
  if (isTRUE(add_ssp_boundaries)) {
    boundaries <- build_ssp_predictability_boundaries(x_limits, y_limits)
    p <- add_ssp_boundaries(p, boundaries)
  }
  if (isTRUE(add_region_labels)) {
    region_labels <- build_env_region_labels(df_plot, x_limits, y_limits)
    p <- add_region_label_layer(p, region_labels)
  }

  list(
    plot = p,
    df_plot = df_plot,
    facet_layer = facet_layer
  )
}

# -----------------------------------------------------------------------------
# plot_env_heatmaps_matrix(): “Fig2A layout wrapper” (gtable + patchwork)
# -----------------------------------------------------------------------------
# Role:
#   The Fig2A heatmap matrix in a manuscript often needs:
#     - only the leftmost y-axis and bottom x-axis (to reduce clutter),
#     - a column header (“vigilance cost (K)”),
#     - a row header (“model variant”),
#     - and consistent axis tick placement across all panels.
#
# This helper:
#   1) calls build_env_heatmap_base() to get the core ggplot
#   2) converts it into a gtable layout using helpers:
#        - keep_left_bottom_axes()
#        - add_column_header_to_gtable()
#        - add_row_header_to_gtable()
#   3) wraps the resulting gtable in patchwork so figure scripts can compose it
#      with other panels using `+` / `/` operators.
# -----------------------------------------------------------------------------
#' Wrap the matrix heatmap in headers and axis adjustments so the layout matches Fig2A.
#'
#' @param df Data produced by build_env_heatmaps_for_models().
#' @param fill_label Label for the color scale.
#' @param facet_rows Column used for rows.
#' @param facet_cols Column used for columns.
#' @return Patchwork object that places the clean gtable inside a patchwork wrapper.
#' @details Adds row/column headers and pins the axis ticks for consistent presentation.
plot_env_heatmaps_matrix <- function(
  df,
  fill_label = "hypervigilance",
  facet_rows = "model",
  facet_cols = "K",
  x_label = "P(arrive)",
  y_label = "P(leave)",
  title = NULL,
  subtitle = NULL,
  add_ssp_boundaries = FALSE,
  add_region_labels = FALSE,
  thesis_like = FALSE,
  add_env_key = FALSE,
  column_header = "vigilance cost (K)",
  row_header = "model variant",
  show_row_header = TRUE
) {

  # ---- Defensive checks -------------------------------------------------------
  stopifnot(all(c("LA", "LL", "hv", facet_rows, facet_cols) %in% names(df)))

  # ---- Extract metadata used for subtitles/captions ---------------------------
  meta <- attr(df, "fixed_params")

  # Some pipelines may attach env_scenarios as part of fixed_params; if not present,
  # captions fall back to defaults inside build_env_heatmap_base().
  env_scenarios <- if (!is.null(meta) && is.data.frame(meta$env_scenarios)) meta$env_scenarios else NULL

  # Compose a standard subtitle that includes the sweep parameters.
  subtitle_final <- if (!is.null(subtitle)) {
    subtitle
  } else if (isTRUE(thesis_like)) {
    NULL
  } else {
    hv_subtitle_with_params(meta, NULL)
  }
  title_final <- if (!is.null(title)) {
    title
  } else if (isTRUE(thesis_like)) {
    NULL
  } else {
    expression(paste("Proportion of hypervigilance across ", lambda[A], "-", lambda[L], " environments"))
  }

  # Determine D for threshold overlays (even if not drawn here).
  D_value <- if ("D" %in% names(df)) unique(df$D)[1]
             else if (!is.null(meta) && !is.null(meta$D)) meta$D
             else NULL

  # ---- Build base plot --------------------------------------------------------
  res <- build_env_heatmap_base(
    df = df,
    fill_column = "hv",
    fill_label = fill_label,
    fill_type = "gradient",
    facet_rows = facet_rows,
    facet_cols = facet_cols,
    title = title_final,
    subtitle = subtitle_final,
    x_label = x_label,
    y_label = y_label,
    env_scenarios = if (isTRUE(thesis_like)) NULL else env_scenarios,
    D = D_value,

    # Fig2A (as written here) chooses not to draw analytic threshold lines,
    # but DOES draw SSP boundaries and region labels to guide interpretation.
    add_threshold_lines = FALSE,
    add_ssp_boundaries = add_ssp_boundaries,
    add_region_labels = add_region_labels
  )

  plot_obj <- res$plot

  if (isTRUE(thesis_like)) {
    has_multiple_rows <- length(unique(as.character(df[[facet_rows]]))) > 1

    plot_obj <- plot_obj +
      ggplot2::theme(
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank(),
        plot.caption = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 12, face = "plain"),
        strip.text.x = ggplot2::element_text(size = 12, face = "plain"),
        strip.text.x.top = ggplot2::element_text(size = 12, face = "plain"),
        strip.text.y = if (has_multiple_rows) ggplot2::element_text(size = 11, face = "plain") else ggplot2::element_blank(),
        strip.text.y.left = if (has_multiple_rows) ggplot2::element_text(size = 11, face = "plain") else ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 16, face = "bold", margin = ggplot2::margin(r = 6)),
        panel.spacing = grid::unit(0.10, "lines"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.title = ggplot2::element_text(size = 11, face = "plain", hjust = 0.5),
        legend.text = ggplot2::element_text(size = 10),
        legend.key.height = grid::unit(0.50, "in"),
        legend.key.width = grid::unit(0.18, "in"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin = ggplot2::margin(0, 0, 0, 0),
        plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")
      )
  }

  # ---- Convert plot to gtable and clean axes ---------------------------------
  # keep_left_bottom_axes() removes redundant axes from internal facets.
  # preserve_bottom_axes = FALSE means we keep a clean single bottom axis.
  g <- keep_left_bottom_axes(plot_obj, preserve_bottom_axes = FALSE)

  # Add “global” column/row headers outside facet strips (manuscript style).
  g <- add_column_header_to_gtable(g, header = column_header)
  if (isTRUE(show_row_header) && length(unique(as.character(df[[facet_rows]]))) > 1) {
    g <- add_row_header_to_gtable(g, header = row_header)
  }

  heatmap_panel <- patchwork::wrap_elements(full = g)

  if (isTRUE(add_env_key)) {
    key_k <- if ("K" %in% names(df)) sort(unique(df$K))[1] else NULL
    env_key_panel <- build_env_key_panel(
      df = df,
      k_value = key_k,
      D_fallback = D_value,
      panel_title = "Environment\nKey",
      x_label = NULL,
      y_label = NULL
    )

    return((heatmap_panel | env_key_panel) + patchwork::plot_layout(widths = c(12, 2.6)))
  }

  # Wrap gtable as a patchwork element so figure scripts can compose layouts easily.
  heatmap_panel
}


region_key_caption <- function(env_scenarios = default_health_env_scenarios()) {
  if (is.null(env_scenarios) || nrow(env_scenarios) == 0) return(NULL)
  env_label_note(env_scenarios)
}


#' High-level wrapper for the environment heatmaps grid
plot_env_heatmaps <- function(
  real_data = build_env_heatmaps_for_models(),
  fill_label = "hypervigilance",
  facet_rows = "model",
  facet_cols = "K",
  x_label = "P(arrive)",
  y_label = "P(leave)",
  title = NULL,
  subtitle = NULL,
  add_ssp_boundaries = FALSE,
  add_region_labels = FALSE,
  layout_style = c("default", "thesis_reference")
) {
  layout_style <- match.arg(layout_style)

  thesis_like <- identical(layout_style, "thesis_reference")
  fill_label_final <- if (thesis_like) "hyper-\nvigilance" else fill_label
  x_label_final <- if (thesis_like) "P(appears)" else x_label
  y_label_final <- if (thesis_like) "P(leaves)" else y_label

  plot_env_heatmaps_matrix(
    real_data,
    fill_label = fill_label_final,
    facet_rows = facet_rows,
    facet_cols = facet_cols,
    x_label = x_label_final,
    y_label = y_label_final,
    title = title,
    subtitle = subtitle,
    add_ssp_boundaries = add_ssp_boundaries,
    add_region_labels = add_region_labels,
    thesis_like = thesis_like,
    add_env_key = thesis_like,
    column_header = if (thesis_like) "cost of vigilance ( K )" else "vigilance cost (K)",
    row_header = "model variant",
    show_row_header = !thesis_like
  )
}


#' Environment heatmap variant that includes a region key caption
plot_env_heatmaps_with_region_key <- function(
  real_data = build_env_heatmaps_for_models(),
  fill_label = "hypervigilance",
  facet_rows = "model",
  facet_cols = "K",
  env_scenarios = default_health_env_scenarios(),
  x_label = "P(arrive)",
  y_label = "P(leave)",
  add_ssp_boundaries = FALSE,
  add_region_labels = FALSE
) {
  heatmap <- plot_env_heatmaps(
    real_data = real_data,
    fill_label = fill_label,
    facet_rows = facet_rows,
    facet_cols = facet_cols,
    x_label = x_label,
    y_label = y_label,
    add_ssp_boundaries = add_ssp_boundaries,
    add_region_labels = add_region_labels
  )

  key_caption <- region_key_caption(env_scenarios)
  if (!is.null(key_caption)) {
    heatmap <- heatmap + patchwork::plot_annotation(caption = key_caption)
  }

  heatmap
}
