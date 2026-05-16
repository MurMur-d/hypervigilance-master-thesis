#!/usr/bin/env Rscript
# =============================================================================
# Script: R/plotting/health_policy_bars/03_call_policy_bars_by_model.R
# =============================================================================
# ROLE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating **policy bar 
# comparisons** for **Fig3C**. The purpose of this visualization is to compare 
# the optimal policy decisions across different models, represented by bar charts.
#
# Key Notes:
#   - The main task of this script is to load the necessary plotting logic 
#     and helpers, which are centralized in `R/plotting/plot_health_policy_bars.R`.
#   - This ensures that the visualization logic is consistent, reusable, and 
#     easy to update. The script itself serves to delegate the responsibility 
#     for generating the plot to the helper functions, which contain the detailed 
#     plotting and formatting logic.
#
# This approach adheres to a modular design, where each part of the figure creation 
# process is handled by specialized functions. The overall goal is to facilitate 
# reproducibility, maintainability, and ease of updates to the visualizations.

# =============================================================================
# Metadata and shared configuration
# =============================================================================

# Set the `FIGURE_SCRIPT` variable to track which script is being run. This 
# will be useful for logging purposes in larger pipelines and ensuring reproducibility.
FIGURE_SCRIPT <- "R/plotting/health_policy_bars/03_call_policy_bars_by_model.R"

# Source the shared configuration script which sets up:
#   - Global theme settings (e.g., `theme_vigilance()`).
#   - Custom color palettes and scales.
#   - Logging and reproducibility functions.
source("R/core/shared_config.R")

# Log the start of the figure creation process. This helps to track progress 
# in generating figures and makes it easier to debug or rerun specific steps.
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load the plot-specific module for health policy bars
# =============================================================================

# The module `R/plotting/plot_health_policy_bars.R` contains the specialized 
# plotting functions that generate the bar chart comparisons of the health policies.
# These functions are responsible for:
#   - Plotting the policy decisions (as bars) for each model.
#   - Formatting the bar chart to ensure clarity and consistency.
#   - Adding any necessary labels, legends, or titles to the chart.
# 
# This modular approach allows for the core logic of the plot to be kept centralized
# and reused across different figures or analyses without duplicating code.
source("R/plotting/health/03_plot_health_policy_bars.R")

# =============================================================================
# End of script
# =============================================================================
# This script functions as a simple entry point for generating the health policy bar 
# comparisons for Fig3C. The actual plotting logic is delegated to the helper module 
# `plot_health_policy_bars.R`. By keeping the plotting code in a separate module, 
# we maintain flexibility and consistency across the project.
#
# Key benefits:
#   - **Centralization**: Changes to the plotting logic are made in one place, 
#     affecting all uses of the policy bar plots.
#   - **Consistency**: Ensures that all figures using this plot style share the same look and feel.
#   - **Maintainability**: Future updates or refinements to the plot style can be easily applied.
# =============================================================================
