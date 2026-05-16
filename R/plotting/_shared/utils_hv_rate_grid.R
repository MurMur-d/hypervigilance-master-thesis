# ------------------------------------------------------------------------------
# FILE: R/plotting/_shared/utils_hv_rate_grid.R
# 
# ROLE:
#   Shared helpers for the HV matrix/heatmap visuals (e.g.,
#   `R/plotting/env_hv_matrix/03_plot_env_hv_matrix.R`).
#
#   This file collects â€œplot plumbingâ€ that would otherwise be duplicated across
#   figure scripts:
#     1) Subtitle formatting (C, D, d, T, N, and optional h0 notes)
#     2) A compact â€œnapkin noteâ€ describing canonical environments (SSP, autocorr)
#     3) gtable/grob manipulation to tidy multi-facet ggplots
#
# DESIGN GOAL:
#   Figure scripts should focus on *layout choices* and saving files, while this
#   helper file provides reusable â€œformatting + gtable surgeryâ€ utilities.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  # dplyr is used only for convenience in other scripts; here itâ€™s not essential,
  # but having it attached avoids repeated imports in plotting pipelines.
  library(dplyr)

  # ggplot2 is needed to convert ggplot objects to grobs (ggplotGrob)
  # and to ensure plot objects are recognized in this helper layer.
  library(ggplot2)

  # gtable + grid underpin the internal layout system ggplot2 uses.
  # We use them to selectively remove axes and add outer headers.
  library(gtable)
  library(grid)
})


# ==============================================================================
# 1) Formatting helpers for subtitles / captions
# ==============================================================================

#' Format a single metadata value (or vector) for subtitles/captions.
#'
#' @param x A scalar or vector, often coming from `attr(df, "fixed_params")`
#'   or `attr(df, "meta")`.
#' @return A human-readable string:
#'   - "?" if NULL,
#'   - comma-separated list if length(x) > 1,
#'   - otherwise a trimmed, non-scientific string for the scalar.
#'
#' WHY:
#'   Many of your plotting functions attach metadata as attributes:
#'     meta$C, meta$D, meta$d, meta$T_steps, meta$N_agents, ...
#'   This helper prevents subtitles like "1e+02" or "NULL" from appearing.
format_subtitle_value <- function(x) {
  # Treat missing metadata as unknown.
  if (is.null(x)) return("?")

  # Sometimes meta contains multiple values (e.g., combined runs).
  # In that case we print a comma-separated list.
  if (length(x) > 1) return(paste(x, collapse = ","))

  # `format(..., scientific = FALSE)` avoids 1e-03.
  # `trim = TRUE` removes leading/trailing spaces.
  format(x, trim = TRUE, scientific = FALSE)
}


#' Build a default subtitle string from a metadata list, with optional prefix.
#'
#' @param meta A list-like object (often a plot/data attribute) containing some
#'   subset of: C, D, d, T_steps, N_agents, and possibly h0/H0.
#' @param subtitle_context Optional caller-provided prefix. If present, itâ€™s
#'   placed *before* the base subtitle.
#' @return A single string. Example:
#'   "my custom note | C = 0 | D = 10 | d = 0 | T = 10 | N = 1000 | h0 = 35"
#'
#' DEPENDENCY NOTE:
#'   This function calls `append_h0_to_subtitle()`, which is defined elsewhere
#'   in your plotting utils (often in `utils_subtitles.R`). Keeping the h0 logic
#'   centralized avoids each plot helper re-implementing it.
hv_subtitle_with_params <- function(meta, subtitle_context = NULL) {
  # Defensive: if the caller passes NULL, treat it as an empty list.
  if (is.null(meta)) meta <- list()

  # Base subtitle always tries to show the core â€œsimulation knobâ€ parameters.
  # These are stable across most figures and help reviewers reproduce settings.
  base_subtitle <- sprintf(
    "C = %s | D = %s | d = %s | T = %s | N = %s",
    format_subtitle_value(meta$C),
    format_subtitle_value(meta$D),
    format_subtitle_value(meta$d),
    format_subtitle_value(meta$T_steps),
    format_subtitle_value(meta$N_agents)
  )

  # If a script supplies a custom context string (e.g., "mode = health"),
  # prepend it so the standard C/D/d/T/N block stays consistent.
  subtitle_final <- if (is.null(subtitle_context) || identical(subtitle_context, "")) {
    base_subtitle
  } else {
    paste(subtitle_context, base_subtitle, sep = " | ")
  }

  # Append h0/H0 notes when available (implementation lives in another helper).
  append_h0_to_subtitle(subtitle_final, meta)
}


# ==============================================================================
# 2) Environment code â€œnapkin notesâ€ used in captions
# ==============================================================================

#' Build a caption block describing canonical environments and their parameters.
#'
#' @param env_scenarios A data.frame typically produced by
#'   `default_health_env_scenarios()`. Expected columns:
#'     env_label, env_full, LA, LL
#'   Optionally:
#'     SSP (if missing, itâ€™s computed), etc.
#'
#' @return A multi-line string suitable for ggplot caption text.
#'
#' BEHAVIOR:
#'   - If env_scenarios is missing or lacks required columns, return a generic
#'     explanation of SSP + label meanings.
#'   - Otherwise, enumerate each environment with (LA, LL, SSP, autocorr).
#'
#' WHY:
#'   Many panels use short codes (L-P, M-U, â€¦). The caption note lets the plot
#'   be self-contained even when printed in isolation.
env_label_note <- function(env_scenarios) {
  # If the caller didn't pass a proper env table, fall back to a generic
  # explanation that still decodes the shorthand labels.
  if (is.null(env_scenarios) || !is.data.frame(env_scenarios) ||
      !"env_label" %in% names(env_scenarios) || !"env_full" %in% names(env_scenarios) ||
      !"LA" %in% names(env_scenarios) || !"LL" %in% names(env_scenarios)) {

    return(paste(
      "SSP = LA / (LA + LL) (stationary stressor probability)",
      "L-P = low SSP, predictable; L-U = low SSP, unpredictable;",
      "M-P = medium SSP, predictable; M-U = medium SSP, unpredictable;",
      "H-P = high SSP, predictable; H-U = high SSP, unpredictable.",
      sep = "\n"
    ))
  }

  # Compute SSP if it is not already included.
  # SSP is a stationary probability for a 2-state Markov stressor process:
  #   SSP = P(stressor) = LA / (LA + LL).
  ssp_vals <- if ("SSP" %in% names(env_scenarios)) {
    env_scenarios$SSP
  } else {
    env_scenarios$LA / (env_scenarios$LA + env_scenarios$LL)
  }

  # Your plots often use the shorthand approximation:
  #   autocorr ~ 1 - (LA + LL)
  # which follows from transition â€œstickinessâ€ (lower total switching means
  # higher autocorrelation).
  autocorr_vals <- 1 - (env_scenarios$LA + env_scenarios$LL)

  # Build one formatted line per environment.
  parts <- sprintf(
    "%s = %s (LA = %.3f, LL = %.3f, SSP = %.3f, autocorr = %.3f)",
    env_scenarios$env_label,
    env_scenarios$env_full,
    env_scenarios$LA,
    env_scenarios$LL,
    ssp_vals,
    autocorr_vals
  )

  paste(
    "SSP = LA / (LA + LL) (stationary stressor probability)",
    paste(parts, collapse = "\n"),
    sep = "\n"
  )
}


# ==============================================================================
# 3) gtable / grob surgery helpers (facet heatmap cleanup)
# ==============================================================================

#' Remove duplicate axes in faceted ggplots, keeping only the canonical ones.
#'
#' @param p A ggplot object, typically a faceted heatmap.
#' @param preserve_bottom_axes Logical:
#'   - FALSE (default): remove all but one bottom x-axis (clean â€œmatrixâ€ look).
#'   - TRUE: keep all bottom axes (useful when each facet needs its own ticks).
#'
#' @return A gtable object (a ggplot grob tree) with selected axes removed.
#'
#' WHY THIS EXISTS:
#'   In facet grids, ggplot draws an x-axis for each column and a y-axis for
#'   each row. For dense matrices, this becomes unreadable and wastes space.
#'   This helper keeps only:
#'     - the bottom-most + left-most y-axis (one y-axis for the whole grid),
#'     - the bottom-most + left-most x-axis (one x-axis for the whole grid),
#'   unless bottom axes are explicitly preserved.
#'
#' HOW IT WORKS:
#'   - Convert ggplot -> gtable via ggplotGrob()
#'   - Identify axis grobs by layout names ("axis-l", "axis-b")
#'   - Replace unwanted grobs with nullGrob() (they occupy no space and render nothing)
keep_left_bottom_axes <- function(p, preserve_bottom_axes = FALSE) {
  # Convert the ggplot to its underlying grob layout structure.
  g <- ggplot2::ggplotGrob(p)

  # Axis grobs are named in the gtable layout. In facet grids there can be many.
  axis_l <- grep("axis-l", g$layout$name)
  axis_b <- grep("axis-b", g$layout$name)

  # ---- LEFT AXES: keep only the bottom-most, left-most one -------------------
  if (length(axis_l) > 0) {
    axis_left_df <- g$layout[axis_l, ]

    # ggplotâ€™s layout uses (t,b,l,r) to describe row/col spans.
    # "bottom-most" means maximum `b` among left axes.
    bottom_row <- max(axis_left_df$b)
    left_candidates <- axis_l[axis_left_df$b == bottom_row]

    # Among bottom-row candidates, keep the one with the smallest column index `l`
    # (i.e., the axis furthest left).
    keep_left <- left_candidates[
      which.min(axis_left_df$l[axis_left_df$b == bottom_row])
    ]
    drop_left <- setdiff(axis_l, keep_left)

    # Replace dropped axes with null grobs so they no longer draw.
    if (length(drop_left) > 0) {
      g$grobs[drop_left] <- replicate(
        length(drop_left),
        grid::nullGrob(),
        simplify = FALSE
      )
    }
  }

  # ---- BOTTOM AXES: keep only one unless preserve_bottom_axes = TRUE ---------
  if (!preserve_bottom_axes && length(axis_b) > 0) {
    axis_bottom_df <- g$layout[axis_b, ]

    # Bottom-most x-axis is the one at the largest `b`.
    bottom_row <- max(axis_bottom_df$b)
    bottom_candidates <- axis_b[axis_bottom_df$b == bottom_row]

    # Keep the bottom-most + left-most one (smallest `l`).
    keep_bottom <- bottom_candidates[
      which.min(axis_bottom_df$l[axis_bottom_df$b == bottom_row])
    ]
    drop_bottom <- setdiff(axis_b, keep_bottom)

    if (length(drop_bottom) > 0) {
      g$grobs[drop_bottom] <- replicate(
        length(drop_bottom),
        grid::nullGrob(),
        simplify = FALSE
      )
    }
  }

  g
}


#' Reposition the y-axis title (Î»_L) so it appears after left facet strips.
#'
#' @param g A gtable object (usually produced by keep_left_bottom_axes()).
#' @return The modified gtable.
#'
#' WHY:
#'   When you place row facet strips on the left (strip-l), the default location
#'   for the y-axis title can end up *outside* those strips or visually awkward.
#'   This helper moves the y-axis title column so that the label appears just to
#'   the right of the strip column block.
#'
#' HOW:
#'   - Find the y-axis label grob ("ylab-l")
#'   - Find all left strip grobs ("strip-l")
#'   - Set the ylabâ€™s column to be one column after the rightmost strip column
move_lambdaL_after_strips <- function(g) {
  lay <- g$layout

  # Identify the y-axis label grob (usually present for standard plots).
  ylab_idx <- which(lay$name == "ylab-l")
  if (length(ylab_idx) == 0) return(g)

  # Identify the left facet strips (row strip labels).
  strip_idx <- grep("strip-l", lay$name)
  if (length(strip_idx) == 0) return(g)

  # Determine where the strips currently live (columns).
  strip_cols <- lay$l[strip_idx]

  # Place ylab one column after the rightmost strip column.
  target_col <- max(strip_cols) + 1L

  # Update both left and right column indices so it occupies exactly that column.
  g$layout$l[ylab_idx] <- target_col
  g$layout$r[ylab_idx] <- target_col

  g
}


#' Add an outer column header above the facet-strip row.
#'
#' @param g A gtable object for a faceted plot.
#' @param header Text to display centered above the facet strip row.
#' @return Modified gtable with an extra row and a text grob.
#'
#' WHY:
#'   Facet grids often have per-column strip labels (e.g., K values),
#'   but no â€œsuper headerâ€ that tells the reader what those columns represent.
#'   This adds a single label spanning all columns.
#'
#' HOW:
#'   - Find the "strip-t" grobs in the layout (top facet strips).
#'   - Add a new row above them.
#'   - Insert a textGrob spanning the min..max columns covered by strips.
add_column_header_to_gtable <- function(
    g,
    header = "environment",
    fontsize = 22,
    fontface = "plain",
    row_height = 1.8,
    y = 0.5
) {
  strip_rows <- grep("strip-t", g$layout$name)
  if (length(strip_rows) == 0) return(g)

  # The strips may span multiple columns; compute the full horizontal span.
  min_col <- min(g$layout$l[strip_rows])
  max_col <- max(g$layout$r[strip_rows])

  # The top-most strip row index in the gtable.
  strip_top <- min(g$layout$t[strip_rows])

  # Insert a row above the strip row with some vertical breathing room.
  g <- gtable_add_rows(
    g,
    heights = unit(row_height, "lines"),
    pos = strip_top - 1
  )

  # Add the header text grob spanning all strip columns.
  g <- gtable_add_grob(
    g,
      textGrob(
        header,
        gp = gpar(fontface = fontface, fontsize = fontsize),
        x = 0.5, y = y,
        just = "centre"
      ),
    t = strip_top,   # row position for the new grob
    l = min_col,     # leftmost column it spans
    r = max_col      # rightmost column it spans
  )

  g
}


#' Add an outer row header to the left of the facet-strip column.
#'
#' @param g A gtable object for a faceted plot.
#' @param header Text to display vertically (rotated) next to the row strips.
#' @return Modified gtable with an extra column and a rotated text grob.
#'
#' WHY:
#'   Similar to add_column_header_to_gtable(), but for rows:
#   if each row is a model variant, itâ€™s useful to have a single â€œmodel variantâ€
#   label on the left gutter rather than repeating y-strip context elsewhere.
#'
#' HOW:
#'   - Find the left strip grobs ("strip-l").
#'   - Add a thin column to their left.
#'   - Add a rotated textGrob spanning the vertical strip block.
add_row_header_to_gtable <- function(g, header = "model variant") {
  strip_cols <- grep("strip-l", g$layout$name)
  if (length(strip_cols) == 0) return(g)

  # Compute the vertical span of all left strips.
  min_row <- min(g$layout$t[strip_cols])
  max_row <- max(g$layout$b[strip_cols])

  # Leftmost strip column: we insert a new column just before it.
  min_col <- min(g$layout$l[strip_cols])

  g <- gtable_add_cols(
    g,
    widths = unit(1.6, "lines"),
    pos = min_col - 1
  )

  g <- gtable_add_grob(
    g,
    textGrob(
      header,
      gp = gpar(fontface = "plain", fontsize = 16),
      rot = 90,
      just = "centre"
    ),
    t = min_row,
    b = max_row,
    l = min_col,
    name = "row-header"
  )

  g
}


# ------------------------------------------------------------------------------
# Summary (what this helper module provides)
# ------------------------------------------------------------------------------
# - hv_subtitle_with_params():
#     Standardizes the subtitle string from metadata attributes (C, D, d, T, N),
#     optionally prefixed with a caller-provided context string, then appends h0.
#
# - env_label_note():
#     Produces a multi-line caption note explaining SSP and the canonical env codes,
#     either by enumerating env rows or by returning a generic fallback text.
#
# - keep_left_bottom_axes():
#     Removes redundant facet axes to make dense heatmap matrices readable.
#
# - move_lambdaL_after_strips():
#     Repositions the y-axis title so it sits nicely after left facet strips.
#
# - add_column_header_to_gtable() / add_row_header_to_gtable():
#     Adds â€œsuper headersâ€ to facet grids by inserting new gtable rows/cols with text.
# ------------------------------------------------------------------------------
