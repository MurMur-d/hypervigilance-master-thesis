# ============================================================
# File: R/plotting/mechanism_env/03_plot_mechanism_thresholds.R
#
# Purpose: Helpers for the mechanism threshold panels (Fig4C).
# Notes:
#   - Build the conceptual lambda thresholds + K Ã— environment hypervigilance layout.
#   - Reuses the environment heatmap module so panels stay consistent with the rest of the repo.
# ============================================================

# ---- Dependencies ----------------------------------------------------------
# plot_utils.R:
#   - provides the shared house style (theme_vigilance), plus common helpers.
# utils_model_scenarios.R:
#   - provides canonical lists/labels for model variants used across figure scripts.
# plot_env_heatmaps.R + R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R:
#   - provide the standard environment heatmap builders; we reuse those so the
#     threshold figure uses *exactly* the same tile sizing, axes, legend styling,
#     and facet behavior as the other environment figures.
source("R/core/plot_utils.R")
source("R/helpers/utils_model_scenarios.R")
source("R/plotting/env_heatmaps/03_plot_env_heatmaps.R")
source("R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R")

suppressPackageStartupMessages({
  library(dplyr)      # filtering/selecting a single model, subsetting K values, etc.
  library(ggplot2)    # plotting (concept panel uses pure ggplot)
  library(patchwork)  # stacking heatmap + conceptual schematic into one figure
})

# =============================================================================
# Conceptual threshold helpers
# =============================================================================
# The threshold figure has two parts:
#   1) the â€œrealâ€ heatmap(s) produced from simulations/DP, faceted by K
#   2) a schematic â€œthreshold mapâ€ showing *analytic boundaries* in (Î»A, Î»L) space
#
# The schematic is intentionally model-agnostic: it draws the three conceptual
# thresholds derived from simple costâ€“benefit conditions (as used in your Results):
#   - Î»A*  (vertical line): preventative vigilance becomes worthwhile in safe states
#   - Î»L*  (horizontal line): reactive vigilance ceases to be worthwhile in stressed states
#   - Ï€S*  (diagonal line): average stationary stressor probability boundary
#
# In this implementation, all three boundaries are parameterized by the cost ratio r = K/D.
# (We assume 0 < r < 1 to keep thresholds meaningful: K must be smaller than D.)

# -----------------------------------------------------------------------------
# build_threshold_concept_panel()
# -----------------------------------------------------------------------------
# Builds the schematic (no data tiles, just dashed boundaries) for a *set of K values*.
#
# Inputs:
#   K_values : vector of K values shown in the heatmap facet columns
#   D        : the common damage parameter (must be a single positive scalar)
#
# Output:
#   a ggplot with fixed aspect ratio and dashed lines:
#     - vertical:   x = K/D
#     - horizontal: y = 1 - K/D
#     - diagonal:   y = x * (1 - r) / r  (a family, one per r)
#
# Axis handling:
#   we accept x_limits/y_limits so the schematic can match the heatmapâ€™s LA/LL range.
#   we compute breaks explicitly so labels look stable and comparable across figures.
build_threshold_concept_panel <- function(
    K_values,
    D,
    conceptual_title = "Conceptual thresholds",
    x_limits = c(0, 0.5),
    y_limits = c(0, 0.5),
    x_step = 0.1,
    y_step = 0.1
) {
  # ---- Basic validation -----------------------------------------------------
  # The schematic needs at least one K and a single finite positive D.
  if (length(K_values) == 0) stop("K_values is empty")

  D_vals <- unique(D)
  if (length(D_vals) != 1 || !is.finite(D_vals) || D_vals <= 0) {
    stop("D must be a single positive finite value")
  }
  D_val <- D_vals[1]

  # Coerce K values to numeric and drop invalid entries.
  K_vals <- sort(unique(as.numeric(K_values)))
  K_vals <- K_vals[is.finite(K_vals)]
  if (length(K_vals) == 0) stop("K_values must contain finite numbers")

  # ---- Axis limits and safe defaults ---------------------------------------
  # Ensure finite ordering and non-degenerate ranges.
  x_min <- min(x_limits, na.rm = TRUE)
  x_max <- max(x_limits, na.rm = TRUE)
  y_min <- min(y_limits, na.rm = TRUE)
  y_max <- max(y_limits, na.rm = TRUE)

  if (x_min == x_max) x_max <- x_min + 0.1
  if (y_min == y_max) y_max <- y_min + 0.1

  # If steps are invalid, fall back to ~5 ticks.
  x_step <- if (x_step > 0) x_step else (x_max - x_min) / 5
  y_step <- if (y_step > 0) y_step else (y_max - y_min) / 5

  # Build breaks (and force endpoints to appear).
  x_breaks <- unique(c(seq(x_min, x_max, by = x_step), x_min, x_max))
  y_breaks <- unique(c(seq(y_min, y_max, by = y_step), y_min, y_max))

  # ---- Convert costs to threshold ratios -----------------------------------
  # r = K/D is the core â€œthreshold parameterâ€.
  # For most analytic threshold derivations, we only want 0 < r < 1:
  #   - r <= 0 implies free vigilance (degenerate)
  #   - r >= 1 implies vigilance is as costly as the damage (often no vigilance region)
  ratio_vals <- K_vals / D_val
  ratio_vals <- ratio_vals[ratio_vals > 0 & ratio_vals < 1]
  if (length(ratio_vals) == 0) stop("All K values produce invalid thresholds for D")

  # ---- Build line geometries -----------------------------------------------
  # 1) Vertical lines at Î»A* = r
  vertical_lines <- data.frame(
    x = ratio_vals,
    xend = ratio_vals,
    y = y_min,
    yend = y_max,
    stringsAsFactors = FALSE
  )
  # Clip to axis range (avoids drawing outside).
  vertical_lines <- subset(vertical_lines, x >= x_min & x <= x_max)

  # 2) Horizontal lines at Î»L* = 1 - r
  # (You clip by y range, because y is fixed).
  horizontal_lines <- data.frame(
    x = x_min,
    xend = x_max,
    y = 1 - ratio_vals,
    yend = 1 - ratio_vals,
    stringsAsFactors = FALSE
  )
  horizontal_lines <- subset(horizontal_lines, y >= y_min & y <= y_max)

  # 3) Diagonal lines for Ï€S* boundary:
  # For each r, define a curve in (Î»A, Î»L) space:
  #   Î»L = Î»A * (1 - r) / r
  # This produces a family of diagonals (one per r / per K level).
  diag_segments <- lapply(ratio_vals, function(ratio) {
    la_seq <- seq(x_min, x_max, length.out = 200)    # smooth curve
    ll_vals <- la_seq * (1 - ratio) / ratio          # derived diagonal
    df <- data.frame(
      lambda_a = la_seq,
      lambda_l = ll_vals,
      ratio = ratio,
      stringsAsFactors = FALSE
    )
    subset(df, lambda_l >= y_min & lambda_l <= y_max)
  })
  diag_df <- do.call(rbind, diag_segments)
  diag_df <- subset(diag_df, is.finite(lambda_l))

  # ---- Initialize a blank canvas plot --------------------------------------
  # geom_blank() forces the panel to respect the limits even if no geoms are drawn.
  base <- ggplot(
    data.frame(x = c(x_min, x_max), y = c(y_min, y_max)),
    aes(x = x, y = y)
  ) +
    geom_blank() +
    labs(
      title = conceptual_title,
      x = "\u03bbA (appears)",
      y = "\u03bbL (leaves)"
    ) +
    theme_vigilance(base_size = 12) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = x_breaks, labels = sprintf("%.1f", x_breaks), expand = c(0, 0)) +
    scale_y_continuous(breaks = y_breaks, labels = sprintf("%.1f", y_breaks), expand = c(0, 0)) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 9),
      plot.margin = margin(t = 6, r = 6, b = 6, l = 6),
      plot.title = element_text(size = 13, face = "bold")
    )

  # ---- Add the Î»A* vertical threshold(s) -----------------------------------
  # We label with Î»a* once (placed near the top) rather than one label per line.
  if (nrow(vertical_lines) > 0) {
    base <- base +
      geom_segment(
        data = vertical_lines,
        aes(x = x, y = y, xend = xend, yend = yend),
        colour = "#333333",
        linetype = "dashed",
        linewidth = 0.6
      ) +
      annotate(
        "text",
        x = median(vertical_lines$x),
        y = y_max - (y_max - y_min) * 0.05,
        label = expression(lambda[a]^"*"),
        size = 3.4,
        vjust = 1.1
      )
  }

  # ---- Add the Î»L* horizontal threshold(s) ---------------------------------
  # Label is placed near the left side.
  if (nrow(horizontal_lines) > 0) {
    base <- base +
      geom_segment(
        data = horizontal_lines,
        aes(x = x, y = y, xend = xend, yend = yend),
        colour = "#333333",
        linetype = "dashed",
        linewidth = 0.6
      ) +
      annotate(
        "text",
        x = x_min + (x_max - x_min) * 0.05,
        y = median(horizontal_lines$y),
        label = expression(lambda[l]^"*"),
        size = 3.4,
        hjust = 0,
        vjust = -0.6
      )
  }

  # ---- Add the Ï€S* diagonal boundary family --------------------------------
  # Draw each diagonal group (group = ratio) and label Ï€s* once near the middle.
  if (nrow(diag_df) > 0) {
    base <- base +
      geom_path(
        data = diag_df,
        aes(x = lambda_a, y = lambda_l, group = ratio),
        colour = "#333333",
        linetype = "dashed",
        linewidth = 0.6
      )

    # Choose one â€œrepresentativeâ€ diagonal to place the Ï€s* label:
    # we pick the median ratio, then pick a midpoint row along that curve.
    median_row <- diag_df[diag_df$ratio == median(unique(diag_df$ratio)), , drop = FALSE]
    if (nrow(median_row) > 0) {
      idx <- floor(nrow(median_row) / 2)
      target <- median_row[max(1, min(idx, nrow(median_row))), , drop = FALSE]
      base <- base +
        annotate(
          "text",
          x = min(max(target$lambda_a * 1.05, x_min + 0.02), x_max - 0.02),
          y = min(max(target$lambda_l * 0.95, y_min + 0.02), y_max - 0.02),
          label = expression(pi[s]^"*"),
          size = 3.4,
          hjust = -0.2,
          vjust = -0.4
        )
    }
  }

  base
}

# =============================================================================
# plot_model_with_thresholds()
# =============================================================================
# High-level wrapper that produces Fig4C-style â€œheatmap + schematicâ€ for ONE model.
#
# It does three main things:
#   (A) Select a single model variant from env_models_df (by name or index).
#   (B) Reuse plot_env_heatmap_faceted_by_K() to draw the â€œrealâ€ heatmaps.
#   (C) Build a schematic threshold panel using the exact same LA/LL limits.
#
# Why reuse plot_env_heatmap_faceted_by_K() (from R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R)?
#   - ensures tile size, axis breaks, legend formatting, and facet spacing match
#     the rest of your repositoryâ€™s environment panels.
#
# Output:
#   a patchwork object (heatmap stacked over schematic) with a shared title/subtitle.
#' Build the grid plot pairing model-specific heatmaps with the conceptual
#' lambda threshold schematic.
#'
#' @param env_models_df Data frame produced by `run_hypervigilance_pipeline_by_model()` or similar.
#' @param model_name Optional label of the model to highlight.
#' @param model_index Optional 1-based index to select the model variant.
#' @param K_values Optional numeric subset of K values to include.
#' @param D_value Optional override for the D parameter inferred from the data.
#' @param title Plot title.
#' @param subtitle_context Optional subtitle for the combined layout.
#' @param conceptual_title Title for the threshold schematic.
#' @param conceptual_height Height ratio for the schematic relative to the heatmap.
#' @return A `patchwork` layout combining the heatmap and threshold panel.
#' @export
plot_model_with_thresholds <- function(
    env_models_df,
    model_name = NULL,
    model_index = NULL,
    K_values = NULL,
    D_value = NULL,
    title = "Proportion of hypervigilance across environments",
    subtitle_context = NULL,
    conceptual_title = "Conceptual thresholds",
    conceptual_height = 0.4
) {
  # ---- Defensive checks on input -------------------------------------------
  stopifnot(is.data.frame(env_models_df))
  stopifnot("model_label" %in% names(env_models_df))

  available_models <- unique(as.character(env_models_df$model_label))
  if (length(available_models) == 0) stop("env_models_df lacks model labels")

  # ---- Select model variant -------------------------------------------------
  # Priority:
  #   1) model_index (explicit positional selection)
  #   2) model_name  (exact match)
  #   3) fallback to the first available model label
  selected_label <- available_models[1]

  if (!is.null(model_index)) {
    model_index <- as.integer(model_index)
    if (is.na(model_index) || model_index < 1 || model_index > length(available_models)) {
      stop("model_index must be between 1 and ", length(available_models))
    }
    selected_label <- available_models[model_index]
  } else if (!is.null(model_name)) {
    candidate <- as.character(model_name)
    if (!candidate %in% available_models) {
      stop("model_name must be one of: ", paste(available_models, collapse = ", "))
    }
    selected_label <- candidate
  }

  # Subset to the chosen model.
  df_model <- env_models_df %>% filter(as.character(model_label) == selected_label)
  if (nrow(df_model) == 0) stop("No rows for model ", selected_label)

  # ---- Optional K subsetting ------------------------------------------------
  # This is useful when the full grid includes many K values but Fig4C wants a subset.
  if (!is.null(K_values)) {
    requested_K <- sort(unique(as.numeric(K_values)))
    df_model <- df_model %>% filter(K %in% requested_K)
    if (nrow(df_model) == 0) stop("No data for the requested K values")
  }

  # Determine which K values actually appear (after filtering).
  K_plot_values <- sort(unique(df_model$K))
  if (length(K_plot_values) == 0) stop("No K values available after filtering")

  # ---- Determine D ----------------------------------------------------------
  # The schematic needs D to convert K into r = K/D.
  # If D is not unique in the filtered data, we fail fast (unless D_value is supplied),
  # because mixing D values would create multiple incompatible threshold families.
  D_candidates <- unique(df_model$D)
  D_val <- if (!is.null(D_value)) {
    if (length(D_value) != 1 || !is.finite(D_value) || D_value <= 0) {
      stop("D_value must be a single positive number")
    }
    D_value
  } else {
    if (length(D_candidates) != 1 || any(!is.finite(D_candidates))) {
      stop("env_models_df must resolve to a single D value for the filtered rows")
    }
    D_candidates[1]
  }

  # ---- Match schematic limits to heatmap limits -----------------------------
  # We infer the LA/LL range used by the selected heatmap, and reuse it for the schematic
  # so the dashed boundaries are drawn in the *same coordinate system*.
  la_vals <- sort(unique(df_model$LA))
  ll_vals <- sort(unique(df_model$LL))

  # Step sizes are used to generate â€œniceâ€ breaks in the schematic:
  # if LA/LL are on a regular grid, min(diff) recovers the grid spacing.
  x_step <- if (length(la_vals) > 1) min(diff(la_vals)) else 0.1
  y_step <- if (length(ll_vals) > 1) min(diff(ll_vals)) else 0.1

  # Force the standard [0, 0.5] view window to be included (most of your env plots use this).
  x_limits <- c(min(0, min(la_vals, na.rm = TRUE)), max(0.5, max(la_vals, na.rm = TRUE)))
  y_limits <- c(min(0, min(ll_vals, na.rm = TRUE)), max(0.5, max(ll_vals, na.rm = TRUE)))

  # Guard against invalid conceptual_height.
  conceptual_height <- if (is.numeric(conceptual_height) && conceptual_height > 0) {
    conceptual_height
  } else {
    0.4
  }

  # ---- Build the heatmap panel ---------------------------------------------
  # This call is the key â€œconsistencyâ€ step: it ensures this figure uses the same
  # styling, scaling, legend, and facet layout as the other LA/LL environment plots.
  env_plot <- plot_env_heatmap_faceted_by_K(
    df_model,
    subtitle_context = NULL,  # we add a shared subtitle later via patchwork annotation
    row_facet = NULL          # single model -> no row facets
  ) +
    labs(title = NULL, subtitle = NULL)  # titles handled at the combined-figure level

  # ---- Build the conceptual schematic panel --------------------------------
  conceptual_plot <- build_threshold_concept_panel(
    K_values = K_plot_values,
    D = D_val,
    conceptual_title = conceptual_title,
    x_limits = x_limits,
    y_limits = y_limits,
    x_step = x_step,
    y_step = y_step
  )

  # ---- Stack into a single figure ------------------------------------------
  combined <- env_plot / conceptual_plot +
    plot_layout(ncol = 1, heights = c(1, conceptual_height))

  # ---- Shared subtitle logic ------------------------------------------------
  # If the calling script provides a subtitle, use it.
  # Otherwise, generate a compact â€œwhat are we looking atâ€ subtitle.
  subtitle_text <- if (!is.null(subtitle_context)) {
    subtitle_context
  } else {
    sprintf(
      "model = %s | D = %s | K = %s",
      selected_label,
      D_val,
      paste(K_plot_values, collapse = ", ")
    )
  }

  # ---- Add global title + subtitle -----------------------------------------
  combined + plot_annotation(
    title = title,
    subtitle = subtitle_text,
    theme = theme_vigilance(base_size = 11) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 11, margin = margin(b = 6)),
        plot.margin = margin(t = 8, r = 6, b = 6, l = 6)
      )
  )
}

#' Transformation notes:
#' - The environment plot reuses `plot_env_heatmap_faceted_by_K()` so thresholds align
#'   with other mechanism figures.
#' - The schematic draws analytic lambdaA/lambdaL thresholds, respecting the provided
#'   model-derived K/D ratios and the requested axis limits.
