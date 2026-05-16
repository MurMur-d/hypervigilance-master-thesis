# ------------------------------------------------------------------------------
# FILE: R/plotting/_shared/utils_plot_layout.R
#
# ROLE:
#   Layout helpers shared by the environment and autocorrelation heatmaps.
#
#   The key problem these helpers solve:
#     Faceted ggplot2 heatmaps produce *repeated axes* (one per facet panel).
#     In multi-panel figures this becomes visually noisy and wastes space.
#
#   This module therefore provides two families of utilities:
#
#   (A) Axis de-duplication
#       - keep_bottom_left_axes_env()
#       - keep_bottom_left_axes_autocorr()
#     These convert a ggplot into a gtable and remove redundant axis grobs,
#     keeping only the bottom-most left y-axis and the left-most bottom x-axis.
#
#   (B) Row-title gutters
#       - wrap_env_heatmap_with_row_title()
#       - wrap_autocorr_heatmap_with_row_title()
#     These add a left-side gutter with:
#       1) a vertical "row title" (e.g., "model variant")
#       2) a row label for each facet row aligned with the panels
#
#   Implementation detail:
#     ggplot2 objects are converted to gtables via ggplotGrob(), then modified
#     using gtable::gtable_add_* functions. Finally patchwork::wrap_elements()
#     is used to re-wrap the modified gtable into a patchwork-compatible object.
# ------------------------------------------------------------------------------


# ==============================================================================
# A) ENVIRONMENT HEATMAP: KEEP ONLY BOTTOM-LEFT AXES
# ==============================================================================

# Keep only the bottom-left axes when faceting environment heatmaps.
keep_bottom_left_axes_env <- function(p) {

  # Convert ggplot object -> gtable (grid table)
  # This exposes the internal grob layout (axes, strips, panels, labels, etc.)
  g <- ggplot2::ggplotGrob(p)

  # Find all left-side y-axis grobs (ggplot names them "axis-l-*")
  axis_l <- grep("axis-l", g$layout$name)

  # Find all bottom x-axis grobs (ggplot names them "axis-b-*")
  axis_b <- grep("axis-b", g$layout$name)

  # ---- LEFT AXES (y): keep ONLY one -----------------------------------------
  # With facet_grid(), ggplot draws a left axis for many panels.
  # We choose to keep the one that is:
  #   - in the bottom-most row of panels (largest b index)
  #   - and among those, the left-most one (smallest l index)
  #
  # This matches the "bottom-left" convention:
  #   - y-axis aligns with the bottom row (visually anchored)
  #   - reduces clutter while leaving a clear reference axis
  if (length(axis_l) > 0) {

    # Pull layout rows corresponding to those axis grobs
    axis_left_df <- g$layout[axis_l, ]

    # Identify bottom-most row position within the layout grid
    bottom_row <- max(axis_left_df$b)

    # Candidate axes that sit in that bottom-most row
    left_candidates <- axis_l[axis_left_df$b == bottom_row]

    # From the candidates, keep the one furthest left
    keep <- left_candidates[which.min(axis_left_df$l[axis_left_df$b == bottom_row])]

    # Drop all others by replacing their grobs with nullGrob()
    drop <- setdiff(axis_l, keep)
    if (length(drop) > 0) {
      g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
    }
  }

  # ---- BOTTOM AXES (x): keep ONLY one ---------------------------------------
  # Same logic as above, but for the bottom x-axis.
  # In faceted layouts, there can be multiple bottom axes (one per column).
  # We keep:
  #   - the axis in the bottom-most row
  #   - and among those, the left-most one
  if (length(axis_b) > 0) {
    axis_bottom_df <- g$layout[axis_b, ]
    bottom_row <- max(axis_bottom_df$b)
    bottom_candidates <- axis_b[axis_bottom_df$b == bottom_row]
    keep <- bottom_candidates[which.min(axis_bottom_df$l[axis_bottom_df$b == bottom_row])]
    drop <- setdiff(axis_b, keep)
    if (length(drop) > 0) {
      g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
    }
  }

  # Return the modified gtable (caller typically wraps with patchwork)
  g
}


# ==============================================================================
# B) ENVIRONMENT HEATMAP: ADD ROW-TITLE + ROW-LABEL GUTTERS
# ==============================================================================

# Wrap a faceted environment heatmap with a custom row-title gutter.
wrap_env_heatmap_with_row_title <- function(
  p,
  row_title = "model scenario",               # Big vertical label for the whole row facet
  gutter_row_title = grid::unit(1.6, "lines"),# Width of the "row title" gutter column
  gutter_row_labels = grid::unit(1.4, "lines"),# Width of the "row labels" gutter column
  row_levels = NULL,                          # Explicit row labels (one per facet row)
  row_label_width = 18                        # Character width to wrap long labels
) {

  # This wrapper relies on patchwork to re-embed a gtable as a plot element.
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("patchwork required")

  # First de-duplicate axes so the final figure looks clean.
  g <- keep_bottom_left_axes_env(p)

  # Row levels are ideally attached to the ggplot object by upstream plotting
  # code (e.g., attr(p, "row_levels")). If not provided, we try to infer.
  if (is.null(row_levels)) row_levels <- attr(p, "row_levels")

  # Identify panel grobs ("panel-*") to locate each facet row.
  # g$layout stores the grid positions: top row index t and bottom row index b.
  panel_rows <- grepl("^panel", g$layout$name)
  panel_df <- g$layout[panel_rows, c("t", "b"), drop = FALSE]

  # Keep only finite entries, remove duplicates, sort top-to-bottom.
  panel_df <- unique(panel_df[is.finite(panel_df$t) & is.finite(panel_df$b), , drop = FALSE])
  panel_df <- panel_df[order(panel_df$t), , drop = FALSE]

  # Number of facet rows visible in the gtable
  n_panel_rows <- nrow(panel_df)

  # If we somehow have no panels, just return the base plot wrapped as-is.
  if (n_panel_rows == 0) return(patchwork::wrap_elements(full = g))

  # ---- Ensure we have one label per panel row -------------------------------
  # If row_levels is missing or wrong length, infer from plot data:
  #   - prefer factor levels of p$data$facet_row if available
  #   - otherwise fall back to model_label values
  # If inference fails, recycle/blank-fill to match required length.
  if (is.null(row_levels) || length(row_levels) != n_panel_rows) {
    inferred_levels <- NULL
    if (!is.null(p$data)) {
      if ("facet_row" %in% names(p$data)) inferred_levels <- levels(p$data$facet_row)
      if (is.null(inferred_levels) && "model_label" %in% names(p$data)) {
        inferred_levels <- unique(as.character(p$data$model_label))
      }
    }
    if (!is.null(inferred_levels) && length(inferred_levels) == n_panel_rows) {
      row_levels <- inferred_levels
    } else {
      row_levels <- rep_len(if (is.null(row_levels)) "" else row_levels, n_panel_rows)
    }
  }

  # Wrap each label to avoid overly wide gutters in the final figure.
  row_levels <- as.character(row_levels)
  row_levels_wrapped <- vapply(
    row_levels,
    function(x) paste(strwrap(x, width = row_label_width), collapse = "\n"),
    character(1)
  )

  # ---- Add two new columns to the left --------------------------------------
  # Column 1 (pos = 0): the big vertical "row_title"
  # Column 2 (inserted before ylab): the per-row labels aligned to panels
  g <- gtable::gtable_add_cols(g, widths = gutter_row_title, pos = 0)

  # Locate the y-axis title column (usually named "ylab-l") so we can insert the
  # row-label column just before it, keeping ylab close to the panel grid.
  ylab_col <- min(g$layout$l[g$layout$name == "ylab-l"])
  g <- gtable::gtable_add_cols(g, widths = gutter_row_labels, pos = ylab_col - 1L)

  # After inserting columns, define which columns we will draw into:
  row_title_col <- 1          # the new far-left gutter column
  label_col <- ylab_col       # after insertion, this points at the label gutter column

  # ---- Strip hygiene: keep ONLY the top strip-t row --------------------------
  # Facet strips (strip-t) can appear multiple times in the gtable layout, and
  # patchwork compositions sometimes duplicate them.
  # Here we keep the top-most strip-t row and null out the rest.
  strip_t_idx <- grep("^strip-t", g$layout$name)
  if (length(strip_t_idx) > 0) {
    strip_df <- g$layout[strip_t_idx, ]
    top_t <- min(strip_df$t)
    keep <- strip_t_idx[strip_df$t == top_t]
    drop <- setdiff(strip_t_idx, keep)
    if (length(drop) > 0) {
      g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
    }
  }

  # ---- Add the vertical row-title text --------------------------------------
  # Draw one grob spanning the full height of the gtable.
  g <- gtable::gtable_add_grob(
    g,
    grid::textGrob(
      row_title,
      rot = 90,                                 # vertical text
      gp = grid::gpar(fontface = "plain"),       # styling
      x = 0.5, y = 0.5,
      just = "centre"
    ),
    t = 1, b = nrow(g),                         # span full table
    l = row_title_col, r = row_title_col,
    clip = "off"                                # allow drawing in margins
  )

  # ---- Add per-row labels aligned with each facet row ------------------------
  # Use the panel_df t/b indices so each label is vertically aligned with the
  # corresponding row of panels.
  for (i in seq_len(n_panel_rows)) {
    g <- gtable::gtable_add_grob(
      g,
      grid::textGrob(
        row_levels_wrapped[i],
        x = 0.9, y = 0.5,
        just = c("right", "centre"),
        gp = grid::gpar(fontface = "plain")
      ),
      t = panel_df$t[i],
      b = panel_df$b[i],
      l = label_col, r = label_col,
      clip = "off"
    )
  }

  # Wrap the modified gtable back into a patchwork element.
  patchwork::wrap_elements(full = g)
}


# ==============================================================================
# A') AUTOCORRELATION HEATMAP: KEEP ONLY BOTTOM-LEFT AXES
# ==============================================================================

# Keep only the bottom-left axes when faceting autocorrelation heatmaps.
keep_bottom_left_axes_autocorr <- function(p) {

  # This is intentionally identical in logic to keep_bottom_left_axes_env().
  # It is duplicated for clarity and future-proofing: if autocorr plots need
  # special handling later (different axis names, different facet layouts),
  # they can diverge without breaking environment heatmaps.
  g <- ggplot2::ggplotGrob(p)
  axis_l <- grep("axis-l", g$layout$name)
  axis_b <- grep("axis-b", g$layout$name)

  if (length(axis_l) > 0) {
    axis_left_df <- g$layout[axis_l, ]
    bottom_row <- max(axis_left_df$b)
    left_candidates <- axis_l[axis_left_df$b == bottom_row]
    keep <- left_candidates[which.min(axis_left_df$l[axis_left_df$b == bottom_row])]
    drop <- setdiff(axis_l, keep)
    if (length(drop) > 0) g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
  }

  if (length(axis_b) > 0) {
    axis_bottom_df <- g$layout[axis_b, ]
    bottom_row <- max(axis_bottom_df$b)
    bottom_candidates <- axis_b[axis_bottom_df$b == bottom_row]
    keep <- bottom_candidates[which.min(axis_bottom_df$l[axis_bottom_df$b == bottom_row])]
    drop <- setdiff(axis_b, keep)
    if (length(drop) > 0) g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
  }

  g
}


# ==============================================================================
# B') AUTOCORRELATION HEATMAP: ADD ROW-TITLE + ROW-LABEL GUTTERS
# ==============================================================================

# Wrap a faceted autocorrelation heatmap with a row-title gutter.
wrap_autocorr_heatmap_with_row_title <- function(
  p,
  row_title = "model scenario",
  gutter_row_title = grid::unit(1.6, "lines"),
  gutter_row_labels = grid::unit(1.4, "lines"),
  row_levels = NULL,
  row_label_width = 18
) {

  # Same structure as wrap_env_heatmap_with_row_title(), but calls the autocorr
  # axis de-duplication helper. This separation helps keep call sites explicit.
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("patchwork required")
  g <- keep_bottom_left_axes_autocorr(p)
  if (is.null(row_levels)) row_levels <- attr(p, "row_levels")

  # Locate panel rows to align labels with each facet row.
  panel_rows <- grepl("^panel", g$layout$name)
  panel_df <- g$layout[panel_rows, c("t", "b"), drop = FALSE]
  panel_df <- unique(panel_df[is.finite(panel_df$t) & is.finite(panel_df$b), , drop = FALSE])
  panel_df <- panel_df[order(panel_df$t), , drop = FALSE]
  n_panel_rows <- nrow(panel_df)
  if (n_panel_rows == 0) return(patchwork::wrap_elements(full = g))

  # Ensure we have one row label per panel row (infer if needed).
  if (is.null(row_levels) || length(row_levels) != n_panel_rows) {
    inferred_levels <- NULL
    if (!is.null(p$data)) {
      if ("facet_row" %in% names(p$data)) inferred_levels <- levels(p$data$facet_row)
      if (is.null(inferred_levels) && "model_label" %in% names(p$data)) inferred_levels <- unique(as.character(p$data$model_label))
    }
    if (!is.null(inferred_levels) && length(inferred_levels) == n_panel_rows) {
      row_levels <- inferred_levels
    } else {
      row_levels <- rep_len(if (is.null(row_levels)) "" else row_levels, n_panel_rows)
    }
  }

  # Wrap long labels to avoid overly wide gutters.
  row_levels <- as.character(row_levels)
  row_levels_wrapped <- vapply(
    row_levels,
    function(x) paste(strwrap(x, width = row_label_width), collapse = "\n"),
    character(1)
  )

  # Add gutter columns: row title + row labels.
  g <- gtable::gtable_add_cols(g, widths = gutter_row_title, pos = 0)
  ylab_col <- min(g$layout$l[g$layout$name == "ylab-l"])
  g <- gtable::gtable_add_cols(g, widths = gutter_row_labels, pos = ylab_col - 1L)
  row_title_col <- 1
  label_col <- ylab_col

  # Keep only the top strip-t row to avoid duplicated strip headers.
  strip_t_idx <- grep("^strip-t", g$layout$name)
  if (length(strip_t_idx) > 0) {
    strip_df <- g$layout[strip_t_idx, ]
    top_t <- min(strip_df$t)
    keep <- strip_t_idx[strip_df$t == top_t]
    drop <- setdiff(strip_t_idx, keep)
    if (length(drop) > 0) g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
  }

  # Add the vertical row-title grob spanning the full height.
  g <- gtable::gtable_add_grob(
    g,
    grid::textGrob(
      row_title,
      rot = 90,
      gp = grid::gpar(fontface = "bold"),
      x = 0.5, y = 0.5,
      just = "centre"
    ),
    t = 1, b = nrow(g), l = row_title_col, r = row_title_col, clip = "off"
  )

  # Add per-row labels aligned with panel rows.
  for (i in seq_len(n_panel_rows)) {
    g <- gtable::gtable_add_grob(
      g,
      grid::textGrob(
        row_levels_wrapped[i],
        x = 0.9, y = 0.5,
        just = c("right", "centre"),
        gp = grid::gpar(fontface = "bold")
      ),
      t = panel_df$t[i],
      b = panel_df$b[i],
      l = label_col, r = label_col,
      clip = "off"
    )
  }

  patchwork::wrap_elements(full = g)
}

