# ============================================================
# File: R/plotting/health/03_plot_health_policy_bars.R
#
# Purpose: Encapsulate the health policy bar panels used by Fig3C/D.
#
# What this module is for (conceptually):
#   These figures are meant to answer: “Given a DP policy table, how often is
#   the optimal action Vigilant vs Relaxed vs Tie, split by:
#     - time (t),
#     - stressor vs no-stressor branch,
#     - environment (LA/LL canonical scenarios),
#     - and either model variant (Fig3C) or vigilance cost K (Fig3D).
#
# Key idea:
#   We are NOT simulating agents here. We are summarising the *DP policy surface*
#   (over labels and health states) into proportions, then plotting those
#   proportions as bar charts.
#
# Inputs:
#   - Environment scenario metadata (LA, LL, labels, descriptions).
#   - Health-policy mix data prepared by `prep_health_policy_mix.R`.
#
# Outputs:
#   - Patchwork objects (plots wrapped in grob/layout helpers) ready to save.
#
# Notes about layout:
#   Because these figures are multi-panel matrices (rows × columns) and ggplot’s
#   default facet strips are sometimes insufficient for paper layouts, this file
#   contains several “gtable surgery” helpers:
#     - keep only one left axis and one bottom axis,
#     - add a shared column header (“environment”),
#     - add row titles and row labels in a gutter,
#     - add right-side labels for “stressor / no stressor”.
# ============================================================

# ---- Sources ----------------------------------------------------------------
# plot_utils.R:
#   - theme_vigilance(), save_graphs(), and shared typography choices.
# utils_health_env_scenarios.R:
#   - default_health_env_scenarios() and canonical environment labels/metadata.
# prep_health_policy_mix.R:
#   - health_policy_action_mix_data() and *_by_model() which compute tidy action
#     proportions from DP policy tables.
source("R/core/plot_utils.R")
source("R/plotting/_shared/utils_health_env_scenarios.R")
source("R/plotting/health/00_data_prep_health_policy_mix.R")

# ---- Packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# =============================================================================
# Layout helpers (gtable surgery)
# =============================================================================
# Why do we need these?
#   Facetted ggplots typically repeat axes and strip labels in every panel.
#   For manuscript figures, that repetition wastes space and makes the layout
#   noisy. These helpers:
#     - remove redundant axes,
#     - add shared headers,
#     - add row-title gutters aligned to facet rows,
#     - and add “stressor / no stressor” labels on the right side.
#
# Technical note:
#   ggplot objects are converted to grobs via ggplotGrob(). We then directly
#   manipulate the grob table (gtable) structure.
# =============================================================================

# ---- Layout helper: keep_left_bottom_axes -----------------------------------
# Goal:
#   Strip out all but ONE left y-axis and ONE bottom x-axis from a facetted plot.
#
# Why:
#   When you facet into a large matrix, ggplot draws an axis for every panel.
#   Keeping only the “outer” axes mimics how journal figures are typically laid out.
#
# How:
#   1) Convert plot to gtable (ggplotGrob).
#   2) Find all grobs whose name matches "axis-l" (left axes) and "axis-b"
#      (bottom axes).
#   3) Keep only the axis grob that is located at:
#        - the bottom-most row (largest b), and
#        - the left-most column among those candidates (smallest l).
#   4) Replace the others with nullGrob() so they take no space.
keep_left_bottom_axes <- function(p, preserve_bottom_axes = FALSE) {
  g <- ggplot2::ggplotGrob(p)

  # Identify grob indices for left and bottom axes in the gtable layout.
  axis_l <- grep("axis-l", g$layout$name)
  axis_b <- grep("axis-b", g$layout$name)

  # --- Keep only one left axis -----------------------------------------------
  if (length(axis_l) > 0) {
    axis_left_df <- g$layout[axis_l, ]
    bottom_row <- max(axis_left_df$b)

    # Candidate left axes at the lowest panel row.
    left_candidates <- axis_l[axis_left_df$b == bottom_row]

    # Among candidates, choose the one furthest left (smallest column index).
    keep <- left_candidates[which.min(axis_left_df$l[axis_left_df$b == bottom_row])]

    # Drop every other left axis grob.
    drop <- setdiff(axis_l, keep)
    if (length(drop) > 0) {
      g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
    }
  }

  # --- Keep only one bottom axis ---------------------------------------------
  if (!preserve_bottom_axes && length(axis_b) > 0) {
    axis_bottom_df <- g$layout[axis_b, ]
    bottom_row <- max(axis_bottom_df$b)

    # Candidate bottom axes at the lowest panel row.
    bottom_candidates <- axis_b[axis_bottom_df$b == bottom_row]

    # Among candidates, choose the one furthest left (smallest column index).
    keep <- bottom_candidates[which.min(axis_bottom_df$l[axis_bottom_df$b == bottom_row])]

    # Drop every other bottom axis grob.
    drop <- setdiff(axis_b, keep)
    if (length(drop) > 0) {
      g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
    }
  }

  g
}

# ---- Layout helper: add_column_header_to_gtable -----------------------------
# Goal:
#   Add a single shared header above the facet-strip row, spanning all columns.
#
# Why:
#   Facet strips label each column individually (e.g., env codes). But the matrix
#   may also need a *higher-level* label: "environment".
#
# How:
#   - Find the top strip row (strip-t).
#   - Insert a new row above it.
#   - Place a text grob spanning from the min strip column to the max strip column.
add_column_header_to_gtable <- function(
    g,
    header = "environment",
    fontsize = 16,
    fontface = "bold",
    row_height = 1.2,
    y = 0.5
) {
  strip_rows <- grep("strip-t", g$layout$name)
  if (length(strip_rows) == 0) return(g)  # if no facet strips, do nothing

  min_col <- min(g$layout$l[strip_rows])
  max_col <- max(g$layout$r[strip_rows])
  strip_top <- min(g$layout$t[strip_rows])

  # Insert a new row above the strip row to hold the header.
  g <- gtable::gtable_add_rows(g, heights = grid::unit(row_height, "lines"), pos = strip_top - 1)

  # Add the header text grob centered across the strip columns.
  g <- gtable::gtable_add_grob(
    g,
    grid::textGrob(
      header,
      gp = grid::gpar(fontface = fontface, fontsize = fontsize),
      x = 0.5, y = y,
      just = "centre"
    ),
    t = strip_top,
    l = min_col,
    r = max_col
  )

  g
}

# ---- Layout helper: add_stressor_labels_right -------------------------------
# Goal:
#   Add a two-line label on the right side of the plot:
#     "stressor"
#     "no stressor"
#
# Why:
#   In the mirrored version of the plot, stressor vs no-stressor is encoded by
#   positive vs negative y-values (above vs below a baseline). These labels make
#   that mapping explicit to the reader.
#
# How:
#   - Determine the gtable rows that correspond to the panel region.
#   - Add a new column at the far right as a gutter.
#   - Place a text grob in that gutter aligned to the panel rows.
#
# Note:
#   stressor_y is currently not used directly (the labels are stacked in one grob),
#   but kept as a parameter in case you later switch to separate grobs.
add_stressor_labels_right <- function(
  g,
  stressor_labels = c("stressor", "no stressor"),
  gutter_right = grid::unit(1.6, "lines"),
  stressor_y = c(0.8, 0.2),
  panel_df = NULL
) {
  # If panel_df not provided, infer panel row spans from the gtable layout.
  if (is.null(panel_df)) {
    panel_rows <- grepl("^panel", g$layout$name)
    panel_df <- g$layout[panel_rows, c("t", "b"), drop = FALSE]
    panel_df <- unique(panel_df[is.finite(panel_df$t) & is.finite(panel_df$b), , drop = FALSE])
    panel_df <- panel_df[order(panel_df$t), , drop = FALSE]
  }
  if (nrow(panel_df) == 0) return(g)

  # Add labels aligned to the *bottom-most* panel row span.
  bottom_idx <- nrow(panel_df)
  trow <- panel_df$t[bottom_idx]
  brow <- panel_df$b[bottom_idx]

  # Create a new right-side gutter column.
  g <- gtable::gtable_add_cols(g, widths = gutter_right, pos = ncol(g))
  col_right <- ncol(g)

  # Combine labels into a single stacked text grob.
  label_text <- paste(stressor_labels, collapse = "\n")
  text_grob <- grid::textGrob(
    label_text,
    x = -4.8,  # push text slightly left from the gutter edge (visual tuning)
    y = 0.5,
    just = c("left", "centre")
  )

  # Insert the grob and allow it to draw outside the panel region (clip = "off").
  g <- gtable::gtable_add_grob(
    g,
    text_grob,
    t = trow, b = brow,
    l = col_right, r = col_right,
    clip = "off"
  )

  g
}

# ---- Layout wrapper: wrap_policy_bars_with_row_title_by_model ----------------
# Goal:
#   Convert a facetted ggplot into a manuscript-style matrix layout with:
#     - shared top header ("environment"),
#     - a left gutter with a vertical row title (e.g., "model variant"),
#     - row labels aligned with each facet row (model names),
#     - and right-side "stressor / no stressor" labels.
#
# Typical use:
#   Fig3C/D style panels where rows = models, cols = environments.
wrap_policy_bars_with_row_title_by_model <- function(
  p,
  row_title = "model variant",
  gutter_row_title = grid::unit(3.5, "lines"),
  gutter_row_labels = grid::unit(1, "lines"),
  row_levels = NULL,
  stressor_labels = c("stressor", "no stressor"),
  gutter_right = grid::unit(1.6, "lines"),
  stressor_y = c(0.8, 0.2)
) {
  # Convert plot to gtable and remove redundant axes.
  g <- keep_left_bottom_axes(p)

  # Add shared column header above the facet strips.
  g <- add_column_header_to_gtable(g, header = "environment")

  # Infer panel rows in the gtable (used to align row labels).
  panel_rows <- grepl("^panel", g$layout$name)
  panel_df <- g$layout[panel_rows, c("t", "b"), drop = FALSE]
  panel_df <- unique(panel_df[is.finite(panel_df$t) & is.finite(panel_df$b), , drop = FALSE])
  panel_df <- panel_df[order(panel_df$t), , drop = FALSE]
  n_panel_rows <- nrow(panel_df)

  if (n_panel_rows == 0) return(patchwork::wrap_elements(full = g))

  # If row_levels not supplied, infer from plot data when possible.
  if (is.null(row_levels) || length(row_levels) != n_panel_rows) {
    inferred_levels <- NULL
    if (!is.null(p$data)) {
      if ("model_fac" %in% names(p$data)) inferred_levels <- levels(p$data$model_fac)
      if (is.null(inferred_levels) && "model_label" %in% names(p$data)) inferred_levels <- unique(p$data$model_label)
    }
    if (!is.null(inferred_levels) && length(inferred_levels) == n_panel_rows) {
      row_levels <- inferred_levels
    } else {
      row_levels <- rep_len(if (is.null(row_levels)) "" else row_levels, n_panel_rows)
    }
  }
  row_levels <- as.character(row_levels)

  # Add a left gutter for the vertical row title.
  g <- gtable::gtable_add_cols(g, widths = gutter_row_title, pos = 0)
  row_title_col <- 1

  # Insert another gutter for per-row labels (just left of the y-axis label).
  ylab_col <- min(g$layout$l[g$layout$name == "ylab-l"])
  g <- gtable::gtable_add_cols(g, widths = gutter_row_labels, pos = ylab_col - 1L)
  label_col <- ylab_col - 1L

  # Add the vertical title text spanning the full height.
  g <- gtable::gtable_add_grob(
    g,
    grid::textGrob(
      row_title,
      rot = 90,
      gp = grid::gpar(fontface = "bold"),
      x = 0.5, y = 0.5,
      just = "centre"
    ),
    t = 1, b = nrow(g),
    l = row_title_col, r = row_title_col,
    clip = "off"
  )

  # Add a row label aligned to each facet row’s panel span.
  for (i in seq_len(n_panel_rows)) {
    g <- gtable::gtable_add_grob(
      g,
      grid::textGrob(
        row_levels[i],
        x = 0.9, y = 0.5,
        just = c("right", "centre")
      ),
      t = panel_df$t[i], b = panel_df$b[i],
      l = label_col, r = label_col,
      clip = "off"
    )
  }

  # Add right-side labels for stressor mapping.
  g <- add_stressor_labels_right(
    g,
    stressor_labels = stressor_labels,
    gutter_right = gutter_right,
    stressor_y = stressor_y,
    panel_df = panel_df
  )

  patchwork::wrap_elements(full = g)
}

# =============================================================================
# Main figure helper: Fig3C style (rows = models, cols = environments)
# =============================================================================
# plot_health_policy_action_bars_by_model()
#
# What it does:
#   1) Calls health_policy_action_mix_data_by_model() to build a tidy table
#      with columns like:
#        - model_label / model_type
#        - env_label, LA, LL
#        - time
#        - stressor (stressor vs no stressor)
#        - action (vigilant / relaxed / tie)
#        - prop (proportion of (health) states with that optimal action)
#   2) Optionally mirrors stressor/no-stressor on the y-axis:
#        - stressor proportions plotted above 0
#        - no-stressor proportions plotted below 0
#      so both branches can be compared in the same panel.
#   3) Builds a facetted bar chart:
#        rows = model variants
#        cols = environments
#   4) Wraps the plot in manuscript gutters and headers.
#
# Important interpretation:
#   These bars summarise the *DP policy table*, not simulated behavior.
#   “Proportion” = fraction of DP states in which an action is optimal.
# =============================================================================

#' Build the mirrored policy bars grid across models + environments.
#'
#' @param K_value Numeric vigilance cost.
#' @param model_scenarios List describing model variants (model_type, policy_args, etc.).
#' @param env_scenarios Canonical environment grid.
#' @param C,D,d Numeric cost parameters.
#' @param T_steps Horizon length.
#' @param states State labels vector.
#' @param base_policy_args List of policy arguments.
#' @param sim_args List of simulation arguments.
#' @param health_level Optional filter for health states.
#' @param mirror_stressor Logical; if TRUE positive/negative for stressor.
#' @param subtitle_context Optional subtitle override.
#' @return Patchwork object with the policy bars + wrapped titles.
#' @export
plot_health_policy_action_bars_by_model <- function(
  K_value = 5,
  model_scenarios = default_model_scenarios,
  env_scenarios = default_health_env_scenarios(),
  C = 0, D = 10, d = 0,
  T_steps = 10,
  states = c("K", "Kd", "C", "CD"),
  base_policy_args = list(),
  sim_args = list(),
  health_level = NULL,
  mirror_stressor = TRUE,
  subtitle_context = NULL,
  add_row_title = TRUE,
  row_title = NULL,
  row_labels = NULL
) {
  # Formatters for embedding numeric parameters in captions.
  fmt_lambda <- function(x) sprintf("%.3f", x)
  fmt_ssp <- function(x) sprintf("%.2f", x)

  # ---------------------------------------------------------------------------
  # 1) Build the tidy policy-action-mix dataset across model scenarios.
  #    This is the *data prep step* for the plot.
  # ---------------------------------------------------------------------------
  df <- health_policy_action_mix_data_by_model(
    K_value = K_value,
    model_scenarios = model_scenarios,
    env_scenarios = env_scenarios,
    C = C, D = D, d = d,
    T_steps = T_steps,
    states = states,
    base_policy_args = base_policy_args,
    health_level = health_level
  )
  stopifnot(nrow(df) > 0)

  # ---------------------------------------------------------------------------
  # 2) Determine row labels (facet row ordering).
  #    If row_labels not supplied, we extract the scenario labels.
  # ---------------------------------------------------------------------------
  format_row_label <- function(lbl, model_type = NULL) {
    lbl_chr <- if (is.null(lbl)) "" else as.character(lbl)
    if (!nzchar(lbl_chr)) return("model variant")

    # Basic model label
    if (!is.null(model_type) && identical(model_type, "basic")) return("basic")

    # Health variants (formatted to match manuscript style)
    if (grepl("no terminal", lbl_chr, ignore.case = TRUE)) {
      return("health:\nno terminal")
    }
    if (grepl("linear", lbl_chr, ignore.case = TRUE)) {
      return("health:\nlinear \u03b3=1")
    }
    if (grepl("power", lbl_chr, ignore.case = TRUE)) {
      return("health:\npower \u03b1=3")
    }
    if (grepl("threshold", lbl_chr, ignore.case = TRUE)) {
      return("health:\nthreshold \u03c4=0.6\u00b7H0")
    }

    # Fallback: return original label
    lbl_chr
  }

  row_levels <- if (is.null(row_labels)) {
    vapply(model_scenarios, function(ms) {
      format_row_label(ms$label, ms$model)
    }, character(1))
  } else {
    row_labels
  }
  row_levels <- unique(as.character(row_levels))

  # Map original scenario labels -> formatted display labels (to keep facet rows aligned)
  raw_labels <- vapply(seq_along(model_scenarios), function(i) {
    ms <- model_scenarios[[i]]
    lbl <- ms$label
    if (is.null(lbl) || !nzchar(lbl)) paste0("model ", i) else as.character(lbl)
  }, character(1))
  row_label_map <- stats::setNames(row_levels, raw_labels)

  # ---------------------------------------------------------------------------
  # 3) Build a caption note that decodes environment codes.
  #    This is printed in the plot caption so the figure is self-contained.
  # ---------------------------------------------------------------------------
  env_ssp <- if ("SSP" %in% names(env_scenarios)) {
    env_scenarios$SSP
  } else {
    env_scenarios$LA / (env_scenarios$LA + env_scenarios$LL)
  }
  env_desc <- if (!is.null(env_scenarios$env_full)) env_scenarios$env_full else env_scenarios$env_label
  autocorr <- 1 - (env_scenarios$LA + env_scenarios$LL)

  env_lines <- sprintf(
    "%s: %s | \u03bbA = %s, \u03bbL = %s, SSP = %s, autocorr ~ %s",
    env_scenarios$env_label,
    env_desc,
    fmt_lambda(env_scenarios$LA),
    fmt_lambda(env_scenarios$LL),
    fmt_ssp(env_ssp),
    fmt_ssp(autocorr)
  )
  env_note <- paste0(
    "Environment codes (SSP = stationary P(stressor) = \u03bbA / (\u03bbA + \u03bbL); autocorr ~ 1 - \u03bbA - \u03bbL):\n",
    paste(env_lines, collapse = "\n")
  )

  # ---------------------------------------------------------------------------
  # 4) Prepare plotting columns and factor levels.
  #    - model_fac controls facet row order.
  #    - action sets bar ordering in the legend and dodge.
  #    - stressor ordering matters for mirroring.
  #    - prop_display optionally mirrors no-stressor below zero.
  # ---------------------------------------------------------------------------
  df <- df %>%
    mutate(
      model_label_display = dplyr::recode(model_label, !!!row_label_map),
      model_fac = factor(model_label_display, levels = row_levels),
      action = factor(action, levels = c("vigilant", "relaxed", "tie")),
      stressor = factor(stressor, levels = c("stressor", "no stressor")),
      prop_display = if (mirror_stressor) ifelse(stressor == "no stressor", -prop, prop) else prop,
      time_f = factor(as.character(time), levels = as.character(sort(unique(time)))),
      env_label_display = factor(env_label, levels = env_scenarios$env_label)
    )

  # Utility: pick the first non-NULL value among arguments.
  pick_first <- function(...) {
    vals <- list(...)
    for (v in vals) if (!is.null(v)) return(v)
    NULL
  }

  # ---------------------------------------------------------------------------
  # 5) Build subtitle text that documents the fixed parameters.
  #    This helps reviewers reproduce the panel without hunting elsewhere.
  # ---------------------------------------------------------------------------
  model_types_used <- unique(as.character(df$model_type))
  h0_val <- pick_first(base_policy_args$h0, base_policy_args$H0, sim_args$h0, sim_args$H0)

  base_subtitle <- sprintf(
    "K = %s | C = %s | D = %s | d = %s | T = %s",
    K_value, C, D, d, T_steps
  )
  if (!is.null(h0_val)) base_subtitle <- paste0(base_subtitle, " | h0 = ", as.character(h0_val))

  # health_reward_subtitle() is assumed to come from plot_utils or subtitle utilities.
  subtitle <- base_subtitle %>% paste0(health_reward_subtitle(base_policy_args, sim_args))
  subtitle <- if (is.null(subtitle_context)) subtitle else paste0(as.character(subtitle_context), " | ", subtitle)

  # y-axis label clarifies that bars are fractions of DP state-space (health × label).
  y_lab <- "proportion of (health) states (%)"

  # Default row title depends on whether we are only comparing health reward variants.
  if (is.null(row_title)) {
    row_title <- if (identical(model_types_used, "health")) "terminal reward model" else "model variant"
  }

  # ---------------------------------------------------------------------------
  # 6) Facetting logic.
  #    If mirror_stressor == TRUE, stressor is encoded by sign (+/-),
  #    so we facet only by model × environment.
  #    Otherwise, we facet by model × stressor × environment.
  # ---------------------------------------------------------------------------
  facet_layer <- if (mirror_stressor) {
    ggplot2::facet_grid(
      rows = ggplot2::vars(model_fac),
      cols = ggplot2::vars(env_label_display),
      switch = "y",
      labeller = ggplot2::labeller(env_label_display = ggplot2::label_value)
    )
  } else {
    ggplot2::facet_grid(
      rows = ggplot2::vars(model_fac, stressor),
      cols = ggplot2::vars(env_label_display),
      switch = "y",
      labeller = ggplot2::labeller(env_label_display = ggplot2::label_value)
    )
  }

  # ---------------------------------------------------------------------------
  # 7) Build the plot.
  #    - time on x (discrete)
  #    - prop_display on y (mirrored or raw)
  #    - fill = action (vigilant/relaxed/tie)
  # ---------------------------------------------------------------------------
  bar_position <- ggplot2::position_dodge(width = 0.8)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = time_f, y = prop_display, fill = action)) +
    ggplot2::geom_col(position = bar_position, width = 0.8, colour = NA) +
    facet_layer +
    ggplot2::scale_fill_manual(
      values = c(vigilant = "#e02b35", relaxed = "#59a89c", tie = "purple"),
      name = "optimal policy"
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.02, 0.1))) +

    # -------------------------------------------------------------------------
    # Mirror-specific y-axis formatting:
    #   If mirror_stressor = TRUE, y ranges from -1..1.
    #   Axis labels display "100 0 100" so the reader reads magnitude as %.
    #   The sign is interpreted via the right-side stressor/no-stressor labels.
    # -------------------------------------------------------------------------
    ggplot2::scale_y_continuous(
      breaks = c(-1, 0, 1),
      labels = c("100", "0", "100"),
      limits = c(-1, 1),
      oob = scales::squish
    ) +
    ggplot2::labs(
      # title = "Optimal policies across environments and models",
      # subtitle = subtitle,
      x = "time",
      y = y_lab
      # caption = env_note
    ) +
    theme_vigilance(base_size = 12, strip_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 8)),
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.height = grid::unit(0.55, "lines"),
      legend.key.width = grid::unit(1.00, "lines"),

      # We suppress the default row facet strips because the wrapper adds
      # cleaner row labels in the left gutter.
      strip.text = ggplot2::element_text(face = "plain", size = 10.5),
      strip.placement = "outside",
      strip.text.y = ggplot2::element_blank(),
      strip.background.y = ggplot2::element_blank(),

      axis.title.x = ggplot2::element_text(vjust = -0.2, hjust = 0, size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold", margin = grid::unit(c(0, 0.2, 0, 0.04), "lines"), hjust = 0),
      axis.text.x = ggplot2::element_text(size = 8.5),
      axis.text.y = ggplot2::element_text(size = 8.5),

      panel.spacing.y = grid::unit(0.5, "lines"),
      panel.spacing.x = grid::unit(0.45, "lines"),
      aspect.ratio = 0.38,

      # More compact margins for thesis export.
      plot.margin = grid::unit(c(8, 8, 10, 48), "pt"),
      plot.caption = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = 12))
    )

  # In mirrored mode, add a baseline separating stressor vs no-stressor halves.
  if (mirror_stressor) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.5)
  }

  # Allow gutter text (row labels and right labels) to render outside plot bounds.
  p <- p + ggplot2::coord_cartesian(clip = "off")

  # Store row levels for wrappers/consumers.
  attr(p, "row_levels") <- levels(df$model_fac)

  # ---------------------------------------------------------------------------
  # 8) Wrap in gtable-based layout (optional).
  # ---------------------------------------------------------------------------
  if (!add_row_title) {
    g <- ggplot2::ggplotGrob(p)
    g <- add_stressor_labels_right(g)
    return(patchwork::wrap_elements(full = g))
  }

  wrap_policy_bars_with_row_title_by_model(
    p,
    row_title = row_title,
    row_levels = row_levels,
    stressor_labels = c("stressor", "no stressor")
  )
}

# =============================================================================
# Wrapper for cost rows (Fig3D style): wrap_policy_bars_with_row_title_by_cost()
# =============================================================================
# This wrapper is parallel to the model-row wrapper, but conceptually:
#   - rows correspond to K levels (cost conditions),
#   - rather than model variants.
#
# It:
#   - keeps only outer axes,
#   - adds shared environment header,
#   - adds a vertical left title “vigilance cost (K)”,
#   - adds per-row labels (e.g., 1, 5, 9),
#   - and adds stressor/no-stressor labels on the right.
wrap_policy_bars_with_row_title_by_cost <- function(
  p,
  row_title = "vigilance cost (K)",
  row_labels = NULL,
  stressor_labels = c("stressor", "no stressor")
) {
  g <- keep_left_bottom_axes(p)
  g <- add_column_header_to_gtable(g, header = "environment")

  # Identify panel row spans for aligning labels.
  panel_rows <- grepl("^panel", g$layout$name)
  panel_df <- g$layout[panel_rows, c("t", "b"), drop = FALSE]
  panel_df <- unique(panel_df[is.finite(panel_df$t) & is.finite(panel_df$b), , drop = FALSE])
  panel_df <- panel_df[order(panel_df$t), , drop = FALSE]
  n_panel_rows <- nrow(panel_df)
  if (n_panel_rows == 0) return(patchwork::wrap_elements(full = g))

  # Infer row labels if not provided.
  # Preferred: factor levels of K_fac; fallback: sorted unique numeric K.
  if (is.null(row_labels) || length(row_labels) != n_panel_rows) {
    inferred_levels <- NULL
    if (!is.null(p$data)) {
      if ("K_fac" %in% names(p$data)) inferred_levels <- levels(p$data$K_fac)
      if (is.null(inferred_levels) && "K" %in% names(p$data)) inferred_levels <- sort(unique(p$data$K))
    }
    if (!is.null(inferred_levels) && length(inferred_levels) == n_panel_rows) {
      row_labels <- inferred_levels
    } else {
      row_labels <- rep_len(if (is.null(row_labels)) "" else row_labels, n_panel_rows)
    }
  }
  row_labels <- as.character(row_labels)

  # Add gutter for the vertical row title.
  g <- gtable::gtable_add_cols(g, widths = grid::unit(1, "lines"), pos = 0)
  row_title_col <- 1

  # Add gutter for per-row labels just left of the y-axis label column.
  ylab_col <- min(g$layout$l[g$layout$name == "ylab-l"])
  g <- gtable::gtable_add_cols(g, widths = grid::unit(0.5, "lines"), pos = ylab_col - 1L)
  label_col <- ylab_col - 1L

  # Add the vertical title spanning the panel region.
  g <- gtable::gtable_add_grob(
    g,
    grid::textGrob(
      row_title,
      rot = 90,
      gp = grid::gpar(fontface = "bold"),
      x = 0.5, y = 0.5,
      just = "centre"
    ),
    t = panel_df$t[1], b = panel_df$b[n_panel_rows],
    l = row_title_col, r = row_title_col,
    clip = "off"
  )

  # Add per-row labels aligned to each panel row.
  for (i in seq_len(n_panel_rows)) {
    g <- gtable::gtable_add_grob(
      g,
      grid::textGrob(
        row_labels[i],
        x = 0.9, y = 0.5,
        just = c("right", "centre")
      ),
      t = panel_df$t[i], b = panel_df$b[i],
      l = label_col, r = label_col,
      clip = "off"
    )
  }

  # Add right-side stressor/no-stressor labels.
  g <- add_stressor_labels_right(
    g,
    stressor_labels = stressor_labels,
    gutter_right = grid::unit(1.6, "lines"),
    stressor_y = c(0.8, 0.2),
    panel_df = panel_df
  )

  patchwork::wrap_elements(full = g)
}

# =============================================================================
# Fig3D style helper: plot_health_policy_action_bars_grid()
# =============================================================================
# This function is the *cost sweep* version:
#   - rows = K values,
#   - cols = environments,
#   - within each panel: time on x, action proportions as bars,
#   - optionally mirrored by stressor/no-stressor.
#
# Data source:
#   health_policy_action_mix_data() (single-policy variant, multiple K values).
plot_health_policy_action_bars_grid <- function(
  K_values = c(1, 5, 9),
  env_scenarios = default_health_env_scenarios(),
  C = 0, D = 10, d = 0,
  T_steps = 10,
  states = c("K", "Kd", "C", "CD"),
  policy_args = list(),
  sim_args = list(),
  health_level = NULL,
  mirror_stressor = TRUE,
  subtitle_context = NULL,
  row_title = "vigilance cost (K)",
  row_labels = NULL
) {

  # 1) Build tidy policy-action proportions across K × env × time × stressor × action.
  df <- health_policy_action_mix_data(
    K_values = K_values,
    env_scenarios = env_scenarios,
    C = C, D = D, d = d,
    T_steps = T_steps,
    states = states,
    policy_args = policy_args,
    health_level = health_level
  )
  stopifnot(nrow(df) > 0)

  # 2) Caption describing environments and their LA/LL/SSP/autocorr.
  fmt_lambda <- scales::label_number(accuracy = 0.001, trim = TRUE)
  fmt_ssp <- scales::label_number(accuracy = 0.01, trim = TRUE)

  env_ssp <- if ("SSP" %in% names(env_scenarios)) {
    env_scenarios$SSP
  } else {
    env_scenarios$LA / (env_scenarios$LA + env_scenarios$LL)
  }
  env_desc <- if (!is.null(env_scenarios$env_full)) env_scenarios$env_full else env_scenarios$env_label
  autocorr <- 1 - (env_scenarios$LA + env_scenarios$LL)

  env_lines <- sprintf(
    "%s: %s | \u03bbA = %s, \u03bbL = %s, SSP = %s, autocorr ~ %s",
    env_scenarios$env_label,
    env_desc,
    fmt_lambda(env_scenarios$LA),
    fmt_lambda(env_scenarios$LL),
    fmt_ssp(env_ssp),
    fmt_ssp(autocorr)
  )
  env_note <- paste0(
    "Environment codes (SSP = \u03bbA / (\u03bbA + \u03bbL); autocorr approx 1 - \u03bbA - \u03bbL):\n",
    paste(env_lines, collapse = "\n")
  )

  # 3) Factor setup + mirroring logic.
  df <- df %>%
    mutate(
      K_fac = factor(K, levels = sort(unique(K))),
      action = factor(action, levels = c("vigilant", "relaxed", "tie")),
      stressor = factor(stressor, levels = c("stressor", "no stressor")),
      prop_display = if (mirror_stressor) ifelse(stressor == "no stressor", -prop, prop) else prop,
      time_f = factor(as.character(time), levels = as.character(sort(unique(time)))),
      env_label_display = factor(env_label, levels = env_scenarios$env_label)
    )

  # Utility to pull h0 from either policy_args or sim_args (if present).
  pick_first <- function(...) {
    vals <- list(...)
    for (v in vals) if (!is.null(v)) return(v)
    NULL
  }
  h0_val <- pick_first(policy_args$h0, policy_args$H0, sim_args$h0, sim_args$H0)

  # 4) Subtitle documenting configuration.
  base_subtitle <- sprintf("C = %s | D = %s | d = %s | T = %s", C, D, d, T_steps)
  if (!is.null(subtitle_context)) base_subtitle <- paste0(as.character(subtitle_context), " | ", base_subtitle)
  subtitle_parts <- c(base_subtitle, sprintf("mode = health"))
  if (!is.null(h0_val)) subtitle_parts <- c(subtitle_parts, sprintf("h0 = %s", as.character(h0_val)))
  subtitle_parts <- c(subtitle_parts, health_terminal_reward_parts(policy_args, sim_args))
  subtitle <- paste(subtitle_parts, collapse = " | ")

  # 5) Facetting.
  facet_layer <- if (mirror_stressor) {
    ggplot2::facet_grid(
      rows = ggplot2::vars(K_fac),
      cols = ggplot2::vars(env_label_display),
      switch = "y",
      labeller = ggplot2::labeller(env_label_display = ggplot2::label_value)
    )
  } else {
    ggplot2::facet_grid(
      rows = ggplot2::vars(K_fac, stressor),
      cols = ggplot2::vars(env_label_display),
      switch = "y",
      labeller = ggplot2::labeller(env_label_display = ggplot2::label_value)
    )
  }

  # 6) Build plot.
  bar_position <- ggplot2::position_dodge(width = 0.8)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time_f, y = prop_display, fill = action)) +
    ggplot2::geom_col(position = bar_position, width = 0.8, colour = NA) +
    facet_layer +
    ggplot2::scale_fill_manual(
      values = c(vigilant = "#e02b35", relaxed = "#59a89c", tie = "purple"),
      name = "optimal policy"
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.02, 0.1))) +
    ggplot2::scale_y_continuous(
      breaks = c(-1, 0, 1),
      labels = c("100", "0", "100"),
      limits = c(-1, 1),
      oob = scales::squish
    ) +
    ggplot2::labs(
      # title = "Optimal health policies across environments and costs",
      # subtitle = subtitle,
      x = "time",
      y = "proportion of (health) states (%)"
      # caption = env_note
    ) +
    theme_vigilance(base_size = 12, strip_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 8)),
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.height = grid::unit(0.55, "lines"),
      legend.key.width = grid::unit(1.00, "lines"),
      strip.text = ggplot2::element_text(face = "plain", size = 10.5),
      strip.placement = "outside",
      strip.text.y = ggplot2::element_blank(),
      strip.background.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(vjust = -0.2, hjust = 0, size = 12, face = "plain"),
      axis.title.y = ggplot2::element_text(size = 12, face = "plain", margin = grid::unit(c(0, 0.2, 0, 0.04), "lines"), hjust = 0),
      axis.text.x = ggplot2::element_text(size = 8.5),
      axis.text.y = ggplot2::element_text(size = 8.5),
      panel.spacing.y = grid::unit(0.5, "lines"),
      panel.spacing.x = grid::unit(0.45, "lines"),
      aspect.ratio = 0.38,
      plot.margin = grid::unit(c(8, 8, 10, 48), "pt"),
      plot.caption = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = 12))
    )

  if (mirror_stressor) p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.5)
  p <- p + ggplot2::coord_cartesian(clip = "off")

  # Store K levels for wrappers/consumers.
  attr(p, "k_levels") <- levels(df$K_fac)

  # 7) Apply the cost-row wrapper to add row title + row labels + right labels.
  wrap_policy_bars_with_row_title_by_cost(
    p,
    row_title = row_title,
    row_labels = row_labels,
    stressor_labels = c("stressor", "no stressor")
  )
}

# =============================================================================
# Scenario registry: health terminal-reward variants used for batch export
# =============================================================================
# Each entry defines:
#   - label  : human-readable caption label
#   - suffix : filename-safe tag used for output stems
#   - policy_args: overrides passed into DP policy builders via preprocessing
health_policy_variants <- list(
  list(
    label = "no terminal reward",
    suffix = "health_no_tr",
    policy_args = list(terminal_reward_weight = 0, terminal_reward_mode = "linear")
  ),
  list(
    label = "linear γ=1",
    suffix = "health_linear",
    policy_args = list(terminal_reward_weight = 1, terminal_reward_mode = "linear")
  ),
  list(
    label = "power α=3",
    suffix = "health_power_a3",
    policy_args = list(
      terminal_reward_weight = 1,
      terminal_reward_mode = "power",
      terminal_power_alpha = 3
    )
  ),
  list(
    label = "threshold τ=0.6·H0",
    suffix = "health_threshold_tau60pct",
    policy_args = list(
      terminal_reward_weight = 1,
      terminal_reward_mode = "threshold"
    )
  )
)

# =============================================================================
# Batch runner: run_all_health_policy_models()
# =============================================================================
# What it does:
#   Iterates over health_policy_variants and for each one:
#     1) builds the K-sweep policy-bars grid (Fig3D style),
#     2) saves plots using save_graphs() into a consistent directory,
#     3) prints progress messages.
#
# Why in this module:
#   - It is a convenience utility for regenerating multiple panels quickly.
#   - Scripts can call this, but the logic stays centralised and consistent.
run_all_health_policy_models <- function(
  K_values = c(1, 5, 9),
  policy_variants = health_policy_variants,
  base_dir = here::here("outputs", "figures", "health_policy_variants")
) {
  if (!requireNamespace("purrr", quietly = TRUE)) stop("purrr required")

  purrr::walk(policy_variants, function(variant) {
    # Build the grid for this variant’s terminal reward settings.
    p <- plot_health_policy_action_bars_grid(
      K_values = K_values,
      policy_args = variant$policy_args
    )

    # File stem encodes which variant produced the output.
    stem <- file.path(base_dir, paste0("health_policy_bars_", variant$suffix))

    # save_graphs() is assumed to standardise formats (png/pdf) + naming.
    save_graphs(
      p,
      stem = stem,
      model = "health",
      policy_args = variant$policy_args
    )

    message("Saved variant: ", variant$label, " -> ", stem)
  })
}

# =============================================================================
# Transformation notes (for reviewers)
# =============================================================================
# - The heavy lifting (enumerating DP states and computing action proportions)
#   is performed in `prep_health_policy_mix.R`.
# - This module:
#     (a) formats that tidy output into manuscript-friendly matrices,
#     (b) provides mirrored y-axis encoding for stressor vs no-stressor branches,
#     (c) adds layout annotations (row/column headers, right-side labels),
#     (d) offers a batch runner to regenerate all terminal reward variants.
# =============================================================================


