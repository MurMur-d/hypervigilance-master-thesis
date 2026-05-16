# ==================================================================================================
# File: R/plotting/basic_policy/03_call_basic_dp_policy.R
#
# Manuscript: Fig1A | Basic DP policy visual (full and compact)
# Purpose:
#   Provide plotting helpers for:
#     (1) a full time × state policy tilemap
#     (2) a compact "stationary" view (prior + one representative time)
#     (3) a grid of compact stationary policies across named environments × K
#     (4) an environment-policy matrix (stressor vs no-stressor) faceted by K
#
# Inputs:
#   - Policy matrices produced by `R/plotting/health/01_data_prep_policy_matrix.R`.
#
# Outputs:
#   - ggplot objects (NOT files) that are consumed by higher-level pipelines to save Fig1A.
#
# Design philosophy:
#   - "Scripts" should be thin: mostly wiring + plot calls.
#   - Reusable plotting logic lives here as functions.
#   - Any fragile gtable/patchwork manipulation is wrapped in helper functions to avoid
#     copy/paste in figure scripts.
# ==================================================================================================

FIGURE_SCRIPT <- "R/plotting/basic_policy/03_call_basic_dp_policy.R"

# ---- Project-wide config and logging --------------------------------------------------------------
# `shared_config.R` usually defines:
#   - theme_vigilance() / scale_action_fill()
#   - global constants / output directories
#   - logging helpers (log_figure_start) that stamp console output + reproducibility info
source("R/core/shared_config.R")

# Mark start of this figure script in logs (useful in long pipelines)
log_figure_start(FIGURE_SCRIPT)

# ==============================================================================
# 15) VISUALIZATIONS — QUICK POLICY INSPECTION
# ------------------------------------------------------------------------------
# The policy matrix is prepared elsewhere (data prep module) so this file can be
# strictly about plotting. This also prevents the common bug where different
# scripts compute slightly different policy tables because of diverging defaults.
# ==============================================================================
source("R/plotting/health/01_data_prep_policy_matrix.R")

# ------------------------------------------------------------------------------
# Plots for inspection of the computed optimal policy.
# ------------------------------------------------------------------------------


# ==============================================================================
# 16) PLOT 1 — Full tile map:
#     x = time
#     y = last-observed label (state proxy in "basic" model)
#     fill = optimal action (High/Low/Tie)
#
# Why this plot exists:
#   - This is the "full" DP policy visualization that matches the DP table:
#     you can see how decisions vary over time and state.
#   - Even if the model is time-stationary, plotting the full horizon is a sanity
#     check for backward induction output (e.g., boundary effects at the final step).
# ==============================================================================
plot_optimal_policy <- function(policy_df, subtitle_context = NULL) {

  # ---- (A) State / label ordering ------------------------------------------------
  # `policy_df$label` contains the last-observed label/state category used for decisions.
  #
  # We want a consistent vertical ordering in the plot. In particular, putting
  # "PRIOR" at the bottom helps interpretation: it is conceptually the "starting
  # belief / starting point" and reads like a baseline row.
  labels_present  <- unique(as.character(policy_df$label))

  # Preferred order used throughout the project:
  #   PRIOR at bottom, then safe-state labels, then stressor-state labels.
  preferred_order <- c("PRIOR", "K", "C", "Kd", "CD")

  # `ordered_levels` contains:
  #   - all preferred labels that actually occur in this dataset (intersect)
  #   - followed by any other labels (setdiff) to avoid dropping unexpected states
  ordered_levels <- c(
    intersect(preferred_order, labels_present),
    setdiff(labels_present, preferred_order)
  )

  # Convert to factor with fixed ordering so ggplot respects it.
  policy_df$label <- factor(policy_df$label, levels = ordered_levels)

  # ---- (B) Legend label mapping -------------------------------------------------
  # The underlying policy uses "High"/"Low"/"Tie" (which must match the palette scale),
  # but the manuscript narrative uses "vigilant"/"relaxed".
  #
  # We keep the raw values for fill mapping and only rename them in the legend.
  action_labels <- c(High = "vigilant", Low = "relaxed", Tie = "Tie")

  # ---- (C) Gridline computation ------------------------------------------------
  # We draw black gridlines manually on top of the tiles. This yields a crisp grid
  # regardless of theme or tile border settings.
  #
  # Tile boundaries occur at ±0.5 around integer x breaks (time steps).
  x_vals <- sort(unique(policy_df$time))
  xmin <- min(x_vals); xmax <- max(x_vals)

  # Vertical gridlines at each boundary between time columns.
  vlines <- seq(xmin - 0.5, xmax + 0.5, by = 1)

  # Horizontal gridlines depend on number of factor levels.
  n_y <- length(levels(policy_df$label))
  hlines <- seq(0.5, n_y + 0.5, by = 1)

  # ---- (D) Build plot ----------------------------------------------------------
  ggplot(policy_df, aes(x = time, y = label, fill = optimal_action)) +

    # Core tile layer:
    # - color = NA removes white seams between tiles (common in some rendering backends).
    # - we keep the panel border in the theme instead.
    geom_tile(color = NA) +

    # Gridlines drawn on TOP of tiles so they remain visible.
    geom_vline(xintercept = vlines, colour = "black", linewidth = 0.35) +
    geom_hline(yintercept = hlines, colour = "black", linewidth = 0.35) +

    # Project-standard action palette:
    # - `scale_action_fill()` is expected to be provided by shared_config.R
    # - drop = FALSE keeps "Tie" visible even if absent (important for consistent legends)
    scale_action_fill(drop = FALSE, labels = action_labels) +

    # Keep axis breaks at actual times; remove padding so tiles align to panel edges
    scale_x_continuous(breaks = x_vals, expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +

    # Titles and axis labels written for manuscript-ready readability
    labs(
      title = "Optimal policy based on backward induction",
      subtitle = subtitle_context,
      x = "Time",
      y = "Last Observed Label",
      fill = "Optimal action"
    ) +

    # Use the project theme for consistent typography and borders
    theme_vigilance(base_size = 13) +
    theme(
      legend.position = "right",
      # Small tweak: ensure y-title does not collide with tile grid
      axis.title.y = element_text(vjust = 1)
    )
}


# ==============================================================================
# 17) PLOT 2 — SIMPLIFIED / COMPACT view:
#     prior (time = 0) + one typical time row (time = 1)
#
# Motivation:
#   In a time-stationary model, most rows repeat across time. A compact view is
#   useful in the paper or appendix because it communicates the same logic with
#   far less visual clutter.
#
# NOTE: In the snippet you pasted, this function uses `c("label", "optimal action")`
#       (with a space). In the rest of your repo it looks like the column is
#       typically `optimal_action`. I’m annotating what it *means*, but you may
#       want to standardize that column name to avoid silent bugs.
# ==============================================================================
plot_stationary_policy <- function(policy_df, subtitle_context = NULL) {

  # ---- (A) Extract a minimal “representative” policy table --------------------
  # The intended data frame should include:
  #   - time = 0 (PRIOR row)
  #   - time = 1 (a “typical” decision row)
  #
  # This assumes the DP policy does not vary materially with time, except possibly
  # at the terminal step.
  #
  # IMPORTANT: Column name should likely be "optimal_action" (no space).
  stationary_policy <- rbind(
    policy_df[policy_df$time == 0, c("label", "optimal action")],
    policy_df[policy_df$time == 1, c("label", "optimal action")]
  )

  # ---- (B) Reorder labels so PRIOR is at bottom -------------------------------
  labels_present <- unique(as.character(stationary_policy$label))
  preferred_order <- c("PRIOR", "K", "C", "Kd", "CD")
  ordered_levels <- c(intersect(preferred_order, labels_present),
                      setdiff(labels_present, preferred_order))
  stationary_policy$label <- factor(stationary_policy$label, levels = ordered_levels)

  # ---- (C) Legend relabeling --------------------------------------------------
  action_labels <- c(High = "vigilant", Low = "relaxed", Tie = "Tie")

  # ---- (D) Gridlines for a single column panel --------------------------------
  # x only spans one dummy column (we plot at x=1), so boundaries are 0.5 and 1.5
  n_y <- length(levels(stationary_policy$label))
  vlines <- c(0.5, 1.5)
  hlines <- seq(0.5, n_y + 0.5, by = 1)

  # ---- (E) Plot ----------------------------------------------------------------
  ggplot(stationary_policy, aes(x = 1, y = label, fill = optimal_action)) +
    geom_tile(color = NA) +
    geom_vline(xintercept = vlines, colour = "black", linewidth = 0.35) +
    geom_hline(yintercept = hlines, colour = "black", linewidth = 0.35) +
    scale_action_fill(drop = FALSE, labels = action_labels) +
    scale_x_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title = "Optimal policy based on backward induction",
      subtitle = subtitle_context,
      y = "state",
      x = "",
      fill = "optimal action"
    ) +
    theme_vigilance(base_size = 13) +
    theme(
      legend.position = "right",
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    )
}


# ==============================================================================
# 18) PLOT 3 — GRID OF COMPACT “STATIONARY” POLICIES
#     (Used when you want many small tiles—e.g., varying K and environments.)
#
# NOTE: In your pasted script, this redefines `plot_stationary_policy()` again,
#       which will overwrite the previous definition in the current R session.
#       That may be intentional (script evolution), but it’s usually safer to
#       give it a distinct name (e.g., plot_stationary_policy_grouped()).
#
# What this version does differently:
#   - Collapses multiple original labels into conceptual groups:
#       {Kd, CD} -> "stressor"
#       {K,  C}  -> "no stressor"
#       PRIOR    -> "prior"
#   - Aggregates actions within each group, marking "Tie" if inconsistent.
#   - Optionally strips the panel to be as minimal as possible for grid layouts.
# ==============================================================================
plot_stationary_policy <- function(
  policy_df,
  subtitle_context = NULL,
  minimal_panel = FALSE,
  base_size = 13
) {
  # ---- (A) Pull prior row + typical time row ----------------------------------
  stationary_policy <- rbind(
    policy_df[policy_df$time == 0, c("label", "optimal_action")],
    policy_df[policy_df$time == 1, c("label", "optimal_action")]
  )

  # ---- (B) Collapse detailed labels into high-level groups --------------------
  # This is a narrative simplification:
  #   - "stressor" means last observation indicates stressor context (Kd/CD)
  #   - "no stressor" means last observation indicates safe context (K/C)
  #   - "prior" is kept separate because it often behaves like a baseline policy
  stationary_policy$label_group <- ifelse(
    stationary_policy$label %in% c("Kd", "CD"), "stressor",
    ifelse(stationary_policy$label %in% c("K", "C"), "no stressor",
           tolower(stationary_policy$label))
  )

  # ---- (C) Aggregate: if group members disagree, call it Tie ------------------
  agg_action <- function(actions) {
    u <- unique(stats::na.omit(as.character(actions)))
    if (length(u) == 0) NA_character_
    else if (length(u) == 1) u
    else "Tie"
  }

  # `aggregate()` collapses to one row per label_group.
  stationary_policy <- aggregate(
    optimal_action ~ label_group,
    data = stationary_policy,
    FUN = agg_action
  )

  # Rename label_group -> label to match downstream plotting code.
  names(stationary_policy)[names(stationary_policy) == "label_group"] <- "label"

  # ---- (D) Order groups top-to-bottom ----------------------------------------
  labels_present <- unique(as.character(stationary_policy$label))
  preferred_order <- c("prior", "no stressor", "stressor")
  ordered_levels <- c(
    intersect(preferred_order, labels_present),
    setdiff(labels_present, preferred_order)
  )
  stationary_policy$label <- factor(stationary_policy$label, levels = ordered_levels)

  # ---- (E) Legend labels + gridlines -----------------------------------------
  action_labels <- c(High = "vigilant", Low = "relaxed", Tie = "tie")
  n_y <- length(levels(stationary_policy$label))
  vlines <- c(0.5, 1.5)
  hlines <- seq(0.5, n_y + 0.5, by = 1)

  # ---- (F) Build the compact plot --------------------------------------------
  p <- ggplot(stationary_policy, aes(x = 1, y = label, fill = optimal_action)) +
    geom_tile(color = NA) +
    geom_vline(xintercept = vlines, colour = "black", linewidth = 0.25) +
    geom_hline(yintercept = hlines, colour = "black", linewidth = 0.25) +
    scale_action_fill(drop = FALSE, labels = action_labels) +
    scale_x_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title = "Optimal policy based on backward induction",
      subtitle = subtitle_context,
      y = "state", x = "", fill = "optimal action"
    ) +
    theme_vigilance(base_size = base_size) +
    theme(
      legend.position = "right",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),

      # NOTE: multiplying by 3 makes labels very large—useful for tiny panels,
      # but be careful: in normal-size plots this can look oversized.
      axis.text.y = element_text(size = base_size * 3),
      legend.title = element_text(size = base_size * 3, face = "bold"),
      legend.text  = element_text(size = base_size * 3)
    )

  # ---- (G) Optionally strip plot to “panel-only” minimal style ----------------
  if (minimal_panel) {
    p <- p +
      theme(
        axis.text.y   = element_text(size = base_size * 3),
        axis.title.y  = element_blank(),
        axis.ticks.y  = element_line(),
        axis.text.x   = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        plot.margin   = margin(1, 1, 1, 1)
      )
  }

  p
}


# ------------------------------------------------------------------------------
# POLICY MATRIX OVER NAMED ENVIRONMENTS (faceted by K)
# ------------------------------------------------------------------------------
# The data builder has been moved to `R/plotting/health/01_data_prep_policy_matrix.R`.
# The plotting helper below assumes the dataset was already prepared there.
#
# Why this section exists:
#   - Fig1A often needs a “policy-by-environment” summary rather than a full DP grid.
#   - We want consistent environment naming (L-P, L-U, etc.) and consistent captions
#     describing SSP and predictability.
# ------------------------------------------------------------------------------


# -------------------------------------------------------------------
# Helper: env_label_note()
# -------------------------------------------------------------------
# Some scripts/modules define env_label_note() already (e.g., hv matrix helpers).
# This `if (!exists())` pattern prevents redefinition warnings and keeps this
# script runnable in isolation.
# -------------------------------------------------------------------
if (!exists("env_label_note")) {
  env_label_note <- function(env_scenarios) {

    # If env_scenarios is missing or malformed, return a hard-coded fallback note.
    # This ensures plots still have a meaningful caption even when meta is absent.
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

    # SSP can be precomputed or derived from LA/LL.
    ssp_vals      <- if ("SSP" %in% names(env_scenarios)) env_scenarios$SSP
                     else env_scenarios$LA / (env_scenarios$LA + env_scenarios$LL)

    # In this repo, "autocorr" is approximated by 1 - (LA + LL).
    # (This matches a 2-state Markov chain intuition under certain parameterizations.)
    autocorr_vals <- 1 - (env_scenarios$LA + env_scenarios$LL)

    # Build a line per environment for the caption.
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
}


# -------------------------------------------------------------------
# Helper: keep_left_bottom_axes()
# -------------------------------------------------------------------
# Faceted ggplots repeat axes for every panel. In dense grids this is noisy.
# This helper keeps only the bottom-left axes so the grid reads like a matrix.
# -------------------------------------------------------------------
if (!exists("keep_left_bottom_axes")) {
  keep_left_bottom_axes <- function(p, preserve_bottom_axes = FALSE) {
    g <- ggplot2::ggplotGrob(p)

    # In ggplot's gtable layout:
    #   "axis-l-*" grobs are left axes (y)
    #   "axis-b-*" grobs are bottom axes (x)
    axis_l <- grep("axis-l", g$layout$name)
    axis_b <- grep("axis-b", g$layout$name)

    # ---- LEFT AXES: keep only the bottom-most, left-most one ------------------
    if (length(axis_l) > 0) {
      axis_left_df <- g$layout[axis_l, ]

      # Bottom-most = largest 'b' coordinate
      bottom_row   <- max(axis_left_df$b)

      # Among axes on that bottom row, choose the left-most by smallest 'l'
      left_candidates <- axis_l[axis_left_df$b == bottom_row]
      keep <- left_candidates[which.min(axis_left_df$l[axis_left_df$b == bottom_row])]

      # Replace all other left axes with null grobs
      drop <- setdiff(axis_l, keep)
      if (length(drop) > 0) {
        g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
      }
    }

    # ---- BOTTOM AXES: keep only the bottom-most, left-most one ----------------
    if (!preserve_bottom_axes && length(axis_b) > 0) {
      axis_bottom_df   <- g$layout[axis_b, ]
      bottom_row       <- max(axis_bottom_df$b)
      bottom_candidates <- axis_b[axis_bottom_df$b == bottom_row]
      keep <- bottom_candidates[which.min(axis_bottom_df$l[axis_bottom_df$b == bottom_row])]
      drop <- setdiff(axis_b, keep)
      if (length(drop) > 0) {
        g$grobs[drop] <- replicate(length(drop), grid::nullGrob(), simplify = FALSE)
      }
    }

    g
  }
}


# -------------------------------------------------------------------
# Helper: add_basic_column_header_to_gtable()
# -------------------------------------------------------------------
# Adds a top header centered above facet strips, e.g. "environment".
# This is purely cosmetic but helps when strips are short codes (L-P, etc.).
# -------------------------------------------------------------------
add_basic_column_header_to_gtable <- function(
    g,
    header = "environment",
    fontsize = 24,
    fontface = "bold",
    row_height = 1.45,
    y = 0.58
) {
  strip_rows <- grep("strip-t", g$layout$name)
  if (length(strip_rows) == 0) return(g)

  # Identify the horizontal span of the top strip row.
  min_col   <- min(g$layout$l[strip_rows])
  max_col   <- max(g$layout$r[strip_rows])

  # Top strip row position (in gtable row coordinates).
  strip_top <- min(g$layout$t[strip_rows])

  # Insert a compact row above strips for the header text.
  g <- gtable::gtable_add_rows(
    g,
    heights = grid::unit(row_height, "lines"),
    pos = strip_top - 1
  )

  # Place the header text grob centered across the strip columns.
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


# ==============================================================================
# plot_policy_matrix_over_env()
# ------------------------------------------------------------------------------
# Builds a 2×E tile map:
#   x = named environments (L-P, L-U, ...)
#   y = stressor presence group ("no stressor" vs "stressor")
#   fill = optimal action (High/Low/Tie)
# Faceted by K across columns.
#
# This is the “policy matrix” summary that matches a narrative like:
#   "When there is no stressor, the agent relaxes in low-risk predictable envs..."
# ==============================================================================

plot_policy_matrix_over_env <- function(
    df,
    subtitle_context = NULL,
    title = NULL,
    show_title = TRUE,
    show_subtitle = FALSE
) {

  meta <- attr(df, "fixed_params")
  if (is.null(meta)) meta <- list()

  format_sub <- function(x) {
    if (is.null(x)) return("?")
    if (length(x) > 1) return(paste(x, collapse = ","))
    format(x, trim = TRUE, scientific = FALSE)
  }

  base_subtitle <- sprintf(
    "C = %s | D = %s | d = %s | T = %s | model = %s",
    format_sub(meta$C),
    format_sub(meta$D),
    format_sub(meta$d),
    format_sub(meta$T_steps),
    format_sub(meta$model)
  )

  subtitle_final <- if (is.null(subtitle_context) || identical(subtitle_context, "")) {
    base_subtitle
  } else {
    paste(subtitle_context, base_subtitle, sep = " | ")
  }

  env_scenarios <- meta$env_scenarios
  if (is.null(env_scenarios) || !is.data.frame(env_scenarios)) {
    env_scenarios <- data.frame(
      env_label = unique(df$env_label),
      env_full = unique(df$env_label),
      LA = NA_real_,
      LL = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  env_levels <- env_scenarios$env_label
  action_labels <- c(High = "vigilant", Low = "relaxed", Tie = "tie")

  df_plot <- df %>%
    dplyr::mutate(
      label_group = factor(label_group, levels = c("no stressor", "stressor")),
      env_label_display = factor(env_label, levels = env_levels),
      K = factor(K, levels = sort(unique(K)))
    )

  default_title <- if (grepl("basic", format_sub(meta$model), ignore.case = TRUE)) {
    "Basic Model: Optimal Policy Across Ecological Structure"
  } else {
    "Optimal Policy Across Ecological Structure"
  }
  main_title <- if (!isTRUE(show_title)) NULL else if (is.null(title)) default_title else title

  p <- ggplot(
    df_plot,
    aes(x = label_group, y = env_label_display, fill = optimal_action)
  ) +
    geom_tile(color = "white", linewidth = 0.25, width = 0.95, height = 0.95) +
    facet_wrap(~ K, nrow = 1, drop = FALSE) +
    scale_x_discrete(
      labels = c(
        "no stressor" = "ns",
        "stressor"    = "s",
        "ns"          = "ns",
        "s"           = "s"
      ),
      expand = c(0, 0)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_manual(
      name = "optimal action",
      values = palette_actions,
      drop = FALSE,
      limits = names(palette_actions),
      breaks = names(palette_actions),
      labels = action_labels
    ) +
    guides(fill = guide_legend(override.aes = list(color = "white", linewidth = 0.4))) +
    labs(
      title = main_title,
      subtitle = if (isTRUE(show_subtitle)) paste0(subtitle_final) else NULL,
      x = "env. state",
      y = "environment"
    ) +
    theme_classic(base_size = 26) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.spacing.x = grid::unit(0.45, "lines"),
      panel.spacing.y = grid::unit(0.08, "lines"),
      strip.text = element_text(face = "plain", size = 24),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 22, face = "plain"),
      axis.text.y = element_text(size = 22, face = "plain"),
      axis.title.x = element_text(face = "bold", size = 24, margin = margin(t = 4)),
      axis.title.y = element_text(face = "bold", size = 24, margin = margin(r = 5)),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.35),
      legend.position = "bottom",
      legend.title = element_text(face = "plain", size = 22),
      legend.text = element_text(size = 20, face = "plain"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_rect(fill = "white", colour = "black", linewidth = 0.35),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.spacing.x = grid::unit(3, "pt"),
      legend.spacing.y = grid::unit(1, "pt"),
      plot.margin = margin(t = 3, r = 3, b = 3, l = 3),
      plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
      plot.subtitle = if (isTRUE(show_subtitle)) element_text(size = 16, hjust = 0.5) else element_blank(),
      plot.caption = element_text(face = "plain", size = 12, hjust = 0.5, margin = margin(b = 4)),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    ) +
    coord_equal(expand = FALSE)

  g <- ggplot2::ggplotGrob(p)
  g <- add_basic_column_header_to_gtable(
    g,
    header = "cost of vigilance (K)",
    fontsize = 24,
    fontface = "bold",
    row_height = 1.45,
    y = 0.58
  )

  list(plot = p, grob = g)
}
