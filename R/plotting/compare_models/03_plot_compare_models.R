# =============================================================================
# File: R/plotting/compare_models/03_plot_compare_models.R
# Purpose: Visual helpers for the comparative model diagnostics pipeline.
#
# What this file is (and is not):
#   - IS: a collection of â€œpure plottingâ€ helpers (no simulation, no DP, no I/O).
#   - IS NOT: responsible for building the input datasets (that happens upstream,
#     e.g., in `build_real_datasets()` or a comparable data-prep pipeline).
#
# Design goals:
#   - Keep plots small, composable, and consistent (so downstream scripts can
#     stitch them into multi-panel figures).
#   - Accept tidy data frames with standard column names and return ggplot objects.
#   - Fail early with clear errors if required columns are missing.
# =============================================================================

# ---------------------------------------------------------------------------------
# Dependencies / shared styling
# ---------------------------------------------------------------------------------
# project theme helpers / manuscript styling defaults
source("R/core/plot_utils.R")

# External plotting + data manipulation packages:
# - ggplot2: base plotting system
# - ggalluvial: alluvial/Sankey-style plots for categorical flow summaries
# - scales: axis/legend label formatting helpers (percent, number accuracy, etc.)
# - tibble/dplyr: light data manipulation and safety when modifying factors
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggalluvial)
  library(scales)
  library(tibble)
  library(dplyr)
})

# =============================================================================
# plot_hv_heatmap(): Hypervigilance cost/autocorr heatmap per model
# =============================================================================
# Intended use:
#   Visualise how hypervigilance level varies over a 2D grid defined by:
#     - cost (K) on the x-axis
#     - autocorrelation (AC) on the y-axis
#   and then compare across models using faceting.
#
# Expected input schema (minimum):
#   df must contain:
#     - cost      : numeric or factor cost levels (K)
#     - ac        : numeric autocorrelation proxy (0..1 typically)
#     - hv_level  : numeric hypervigilance rate/level in [0,1]
#     - facet_var : default "model" (e.g., "basic", "health", etc.)
#
# Output:
#   A ggplot heatmap (geom_tile) faceted by the chosen facet variable.
plot_hv_heatmap <- function(df, facet_var = "model", fill_label = "HV") {

  # ---- Defensive checks -------------------------------------------------------
  # If facet_var is missing, faceting would silently error later in ggplot.
  stopifnot(facet_var %in% names(df))

  # ---- Plot ------------------------------------------------------------------
  ggplot(df, aes(cost, ac, fill = hv_level)) +

    # Each row corresponds to one cell in the (cost Ã— ac) grid.
    # Light outline improves readability when cells are small.
    geom_tile(color = "grey90") +

    # Use a perceptually-uniform continuous scale (viridis).
    # limits = c(0,1) ensures comparability across models/panels (same legend range).
    # breaks chosen to give more resolution at low HV (where many effects may live).
    scale_fill_viridis_c(
      option = "B",
      name = fill_label,
      limits = c(0, 1),
      breaks = c(0, 0.02, 0.05, 0.1, 0.25, 0.5, 1),
      labels = label_number(accuracy = 0.02)
    ) +

    # Facet by model (or any other variable specified by facet_var).
    # The paste/as.formula pattern keeps this flexible while still using facet_wrap.
    facet_wrap(as.formula(paste("~", facet_var))) +

    # Clear titles/axes: â€œCostâ€ is vigilance cost K; â€œACâ€ is autocorrelation proxy.
    labs(
      title = "Hypervigilance by Cost and Autocorrelation",
      x = "Cost (K)",
      y = "Autocorrelation (AC)"
    ) +

    # Minimal theme keeps grid plots clean; downstream scripts can override theme if needed.
    theme_minimal(base_size = 12)
}

# =============================================================================
# plot_episode_trajectory(): Episode-stage vigilance trajectory across env/models
# =============================================================================
# Intended use:
#   For each model, plot how some â€œtrajectoryâ€ variable (default: vigilance)
#   changes over an ordered episode stage, optionally faceted by environment condition.
#
# Expected input schema (minimum):
#   df must contain:
#     - episode_stage : ordered stage index/label along the x-axis
#     - y_var         : numeric variable to plot (default "vigilance")
#     - color_var     : grouping/color variable (default "model")
#     - facet_var     : environment grouping for panels (default "env_condition")
#
# Notes:
#   - Uses `.data[[...]]` to allow dynamic column names safely.
#   - `group = model` ensures lines connect within each model across stages.
plot_episode_trajectory <- function(
    df,
    color_var = "model",
    facet_var = "env_condition",
    y_var = "vigilance"
) {

  # ---- Defensive checks -------------------------------------------------------
  stopifnot(color_var %in% names(df), facet_var %in% names(df), y_var %in% names(df))

  ggplot(
    df,
    aes(
      episode_stage,
      .data[[y_var]],
      color = .data[[color_var]],
      group = .data[[color_var]]
    )
  ) +
    # Line shows trajectory per model; points highlight discrete stages.
    geom_line(size = 1) +
    geom_point(size = 2) +

    # One panel per environment condition (or whatever facet_var is).
    facet_wrap(as.formula(paste("~", facet_var))) +

    # Brewer qualitative palette for discrete model comparisons.
    scale_color_brewer(palette = "Dark2", name = color_var) +

    labs(
      title = "Vigilance Across Episode",
      x = "Episode stage",
      y = y_var
    ) +
    theme_minimal(base_size = 12)
}

# =============================================================================
# plot_peaks_collapse(): Peaks vs collapse cost markers per model
# =============================================================================
# Intended use:
#   Summarise for each model:
#     - peak_cost     : the K at which HV peaks (circle marker)
#     - collapse_cost : the K at which HV collapses (red triangle marker)
#
# Expected input schema:
#   df must contain:
#     - model
#     - peak_cost
#     - collapse_cost
#
# Notes:
#   - coord_flip() makes model names readable when there are many models.
#   - Two geom_point layers so the collapse marker can have distinct styling.
plot_peaks_collapse <- function(df) {
  ggplot(df, aes(model, peak_cost)) +
    geom_point(size = 3) +
    geom_point(aes(y = collapse_cost), colour = "red", size = 3, shape = 17) +
    coord_flip() +
    labs(
      title = "Peak vs Collapse Cost",
      y = "Cost (K)",
      x = "Model",
      subtitle = "Circle = peak HV, red triangle = collapse"
    ) +
    theme_minimal(base_size = 12)
}

# =============================================================================
# plot_indifference(): Indifference (tie) frequency by environment
# =============================================================================
# Intended use:
#   Compare how often the DP policy returns â€œTieâ€ (or an equivalent indifference flag)
#   across environments and models.
#
# Expected input schema:
#   df must contain:
#     - env   : environment label/category
#     - model : model label/category
#     - freq  : numeric frequency (count or proportion)
#
# Notes:
#   - Uses position="dodge" so model bars sit side-by-side within each environment.
plot_indifference <- function(df) {
  ggplot(df, aes(env, freq, fill = model)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Decision Indifference Frequency",
      x = "Environment",
      y = "Frequency of ties"
    ) +
    theme_minimal(base_size = 12)
}

# =============================================================================
# plot_profile_alluvial(): Behavioral profile alluvial diagram
# =============================================================================
# Intended use:
#   Show how â€œbehavioral profile massâ€ (weight) is distributed across phases,
#   separated by model. This is a compact way to visualise categorical composition.
#
# Expected input schema:
#   df must contain:
#     - model  : stratum for axis1
#     - phase  : stratum for axis2
#     - weight : numeric mass (e.g., proportion or count)
#
# Notes:
#   - ggalluvial expects the data to represent flows between axes.
#   - geom_stratum draws the â€œblocksâ€ for each stratum on each axis.
#   - after_stat(stratum) prints the stratum name onto each block.
plot_profile_alluvial <- function(df) {
  ggplot(df, aes(axis1 = model, axis2 = phase, y = weight)) +
    geom_alluvium(aes(fill = model), alpha = 0.75) +
    geom_stratum(width = 0.2) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_fill_brewer(palette = "Set3") +
    labs(
      title = "Behavioral Profiles by Model",
      x = "",
      y = "Weight"
    ) +
    theme_minimal(base_size = 12)
}

# =============================================================================
# plot_health_action_mix(): Health action mix columns per environment
# =============================================================================
# Intended use:
#   For health-based models, plot the proportion of time spent in each action type:
#     - vigilant
#     - relaxed
#     - tied
#   split by environment condition, with model rows.
#
# Expected input schema:
#   df must contain:
#     - model
#     - env_condition
#     - action      : categories like "vigilant", "relaxed", "tied"
#     - proportion  : numeric in [0,1]
#
# Notes:
#   - We factor env_condition using its existing order in df (so upstream can control it).
#   - We set action factor levels explicitly so bars appear in a stable semantic order.
#   - legend is hidden (legend.position="none") because action is already shown on x-axis.
plot_health_action_mix <- function(df) {

  # ---- Standardise factor ordering (keeps panels stable across runs) ----------
  df <- df %>%
    mutate(
      # Preserve the order env_condition appears in the data (useful when upstream sorts).
      env_condition = factor(env_condition, levels = unique(env_condition)),

      # Stable ordering of action categories (prevents alphabetical re-ordering).
      action = factor(action, levels = c("vigilant", "relaxed", "tied"))
    )

  ggplot(df, aes(action, proportion, fill = action)) +
    # Dodged bars allow comparison across (if you later add a grouping var), while still readable.
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +

    # Manuscript-friendly grid: models down rows, environments across columns.
    facet_grid(rows = vars(model), cols = vars(env_condition)) +

    # Qualitative palette appropriate for categorical actions.
    scale_fill_brewer(palette = "Set2", name = "Action") +

    # Percent axis is the natural scale for a â€œmixâ€ plot.
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +

    labs(
      title = "Action mix (health models) by environment",
      x = "Action",
      y = "Proportion of agent-time steps"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # Hide legend to reduce redundancy (action already labelled on x-axis).
      legend.position = "none",

      # Make facet strip labels more prominent (helps scan models/environments quickly).
      strip.text = element_text(face = "bold"),

      # Add a little extra spacing between environment columns for readability.
      panel.spacing.x = unit(6, "pt")
    )
}

