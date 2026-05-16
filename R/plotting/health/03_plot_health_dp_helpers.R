# =============================================================================
# File: R/plotting/health/03_plot_health_dp_helpers.R
# Purpose: Support helpers for health DP diagnostics used by the master pipeline.
#
# High-level role in the project:
#   This file contains *diagnostic visualizations* for the HEALTH dynamic
#   programming (DP) model. These plots are meant to help:
#     - sanity-check the DP solution,
#     - understand how optimal actions evolve over time,
#     - communicate qualitative structure (e.g., vigilance vs relaxation)
#       without running forward simulations.
#
# Design principles:
#   - Pure plotting helpers: no simulation, no DP solving, no side effects.
#   - Input is always a DP policy table produced by compute_optimal_policy_health().
#   - Output is a ggplot object ready to be embedded in figures or reports.
#
# Why these helpers exist:
#   - Earlier pipelines referenced plot_op_* helpers that were never implemented.
#   - These functions replace those missing helpers in a transparent,
#     reproducible, and health-modelâ€“specific way.
# =============================================================================

# -----------------------------------------------------------------------------
# Package dependencies
# -----------------------------------------------------------------------------
# ggplot2:
#   Core plotting system.
# dplyr:
#   Used for filtering, grouping, and summarising policy tables.
# tidyr:
#   Used to complete missing (time, action) combinations so plots remain stable
#   even when some actions never occur at certain time steps.
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# -----------------------------------------------------------------------------
# plot_op_health_time()
# -----------------------------------------------------------------------------
# What this plot shows:
#   For each decision time step, this plot shows the *proportion of DP states*
#   (across labels and health levels) in which each action is optimal:
#     - High  â†’ Vigilant
#     - Low   â†’ Relaxed
#     - Tie   â†’ Indifference between actions
#
# Interpretation:
#   - This is NOT a time series of agent behavior.
#   - It is a *cross-sectional summary of the DP policy* at each time step.
#   - A rising High proportion means that, as the horizon approaches,
#     vigilance becomes optimal in more states.
#
# Typical use:
#   - Diagnose horizon effects (e.g., end-of-horizon relaxation).
#   - Check whether terminal rewards shift action balance earlier in time.
#   - Identify large regions of indifference ("Tie") in policy space.
#
# Input expectations:
#   policy_df must contain at least:
#     - time
#     - optimal_action
#
# Output:
#   A ggplot line + point plot, one line per action type.
# -----------------------------------------------------------------------------
#' Plot the proportion of actions (High/Low/Tie) as a function of time.
#'
#' @param policy_df Data frame returned by the health DP solver.
#' @param time_points Vector of time steps to highlight.
plot_op_health_time <- function(policy_df, time_points = NULL) {

  # ---------------------------------------------------------------------------
  # Default behavior: include *all* time steps present in the policy table.
  # This ensures the plot adapts automatically to different horizons.
  # ---------------------------------------------------------------------------
  if (is.null(time_points)) {
    time_points <- sort(unique(policy_df$time))
  }

  # ---------------------------------------------------------------------------
  # Build a summary table:
  #   - Restrict to requested time points
  #   - Convert optimal_action into a factor with a fixed ordering
  #   - Compute, for each (time, action), the fraction of rows where that
  #     action is optimal
  #
  # Important detail:
  #   mean(action == optimal_action) works because within each group,
  #   `action` is fixed. This is equivalent to counting rows and dividing
  #   by the total number of states at that time.
  # ---------------------------------------------------------------------------
  summary_df <- policy_df %>%
    filter(time %in% time_points) %>%
    mutate(
      action = optimal_action %>%
        factor(levels = c("High", "Low", "Tie"))
    ) %>%
    group_by(time, action) %>%
    summarise(
      proportion = mean(action == optimal_action, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # -------------------------------------------------------------------------
    # Ensure that missing (time, action) combinations appear as zero.
    # This prevents broken lines when an action never occurs at a given time.
    # -------------------------------------------------------------------------
    complete(
      time = time_points,
      action = levels(action),
      fill = list(proportion = 0)
    )

  # ---------------------------------------------------------------------------
  # Plot:
  #   - Lines track how the action mix evolves over time.
  #   - Points make discrete decision times visually explicit.
  #   - Colors are chosen to match vigilance semantics used elsewhere:
  #       red   = vigilant
  #       green = relaxed
  #       grey  = tie
  # ---------------------------------------------------------------------------
  ggplot(summary_df, aes(time, proportion, color = action)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c(
        "High" = "#e02b35",   # vigilant
        "Low"  = "#59a89c",   # relaxed
        "Tie"  = "grey40"     # indifference
      )
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = "Health DP: action mix over time",
      subtitle = "Proportion of High / Low / Tie decisions",
      x = "Time",
      y = "Proportion (per decision time step)",
      color = "Optimal action"
    ) +
    theme_minimal(base_size = 12)
}

# -----------------------------------------------------------------------------
# plot_op_stressor_nodes()
# -----------------------------------------------------------------------------
# What this plot shows:
#   A time Ã— stressor-state grid indicating which action is optimal in:
#     - stressor-present states (labels: Kd, CD)
#     - stressor-absent states  (labels: K, C, PRIOR)
#
# Interpretation:
#   - This plot abstracts away health levels and focuses on *environmental
#     contingencies*.
#   - It makes clear whether vigilance is driven primarily by:
#       - anticipation (no stressor â†’ High),
#       - reaction     (stressor â†’ High),
#       - or indifference.
#
# Typical use:
#   - Show how vigilance policies differ between stressed vs safe states.
#   - Diagnose whether hypervigilance is proactive or reactive.
#   - Supplement environment heatmaps with a compact policy-level view.
#
# Input expectations:
#   policy_df must contain:
#     - time
#     - label
#     - optimal_action
#
# Output:
#   A tile plot (time on x, stressor state on y) colored by action.
# -----------------------------------------------------------------------------
#' Display the action choice across stressor vs no-stressor labels over time.
#'
#' @param policy_df Data frame returned by the health DP solver.
#' @param subtitle_context Optional subtitle text.
#' @param node_radius Radius for the circular nodes.
plot_op_stressor_nodes <- function(
  policy_df,
  subtitle_context = NULL,
  node_radius = 0.25
) {

  # ---------------------------------------------------------------------------
  # Recode DP labels into a binary stressor state:
  #   - Kd, CD â†’ stressor present
  #   - everything else â†’ no stressor
  #
  # Also:
  #   - enforce a fixed ordering of actions
  #   - map internal action codes to reader-facing labels
  # ---------------------------------------------------------------------------
  policy_df <- policy_df %>%
    mutate(
      stressor_state = ifelse(
        label %in% c("Kd", "CD"),
        "stressor",
        "no stressor"
      ),
      optimal_action = factor(
        optimal_action,
        levels = c("High", "Low", "Tie")
      ),
      action_label = recode(
        optimal_action,
        High = "Vigilant",
        Low  = "Relaxed",
        Tie  = "Tie"
      )
    )

  # ---------------------------------------------------------------------------
  # Plot:
  #   - Each tile corresponds to a (time, stressor_state) combination.
  #   - Fill color indicates the optimal action.
  #
  # Note:
  #   geom_tile() is used instead of points to emphasize *categorical regimes*
  #   rather than trajectories.
  # ---------------------------------------------------------------------------
  ggplot(policy_df, aes(time, stressor_state)) +
    geom_tile(
      aes(fill = action_label),
      color = "grey80",
      width = 0.9,
      height = 0.8
    ) +
    scale_fill_manual(
      values = c(
        "Vigilant" = "#e02b35",
        "Relaxed"  = "#59a89c",
        "Tie"      = "purple"
      )
    ) +
    labs(
      title = "Health DP actions across stressor vs no-stressor states",
      subtitle = subtitle_context,
      x = "Time",
      y = "Stressor presence",
      fill = "Action"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank()
    )
}

