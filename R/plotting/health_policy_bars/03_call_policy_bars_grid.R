#!/usr/bin/env Rscript
# =============================================================================
# Script: R/plotting/health_policy_bars/03_call_policy_bars_grid.R
# =============================================================================
# ROLE / INTENT
# -----------------------------------------------------------------------------
# This script is the **entry point** for generating a **health policy bar grid** 
# visualization for **Fig3D**. It takes a collection of policy data and plots 
# the relevant bar charts for comparison. Unlike the previous script, which was 
# focused on comparing bars for a single model, this script is designed for 
# visualizing multiple models in a grid layout.

# This is an important visualization that displays a series of health policy 
# bar plots side by side, making it easier to compare the policy decisions 
# across models in a comprehensive manner.

# The script sources a plotting helper module without defining additional functions 
# in this entry point. All of the necessary plotting logic resides in the 
# `R/plotting/plot_health_policy_bars.R` file, which is centralized for 
# consistency and reuse across different figures.

# =============================================================================
# Metadata and shared configuration
# =============================================================================

# The `FIGURE_SCRIPT` variable is used to log the name of the current script, 
# which aids in tracking and reproducibility of figure generation.
FIGURE_SCRIPT <- "R/plotting/health_policy_bars/03_call_policy_bars_grid.R"

# Source the shared configuration script which:
#   - Sets global theme settings.
#   - Defines color palettes.
#   - Contains functions for logging the figure generation process.
source("R/core/shared_config.R")

# Log the start of the figure creation process for reproducibility and debugging.
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load the plot-specific module for health policy bars
# =============================================================================

# The `R/plotting/plot_health_policy_bars.R` module contains the detailed plotting 
# logic that is used to generate the policy bar plots. This module is responsible 
# for:
#   - Creating bar charts to visualize the health policy decisions across different models.
#   - Formatting the bar chart (e.g., adding labels, legends, and titles).
#   - Ensuring that the plot style is consistent across different figures and uses.
#
# By sourcing this module here, we ensure that the figure generation code remains 
# centralized and maintainable. Any changes made in this module will automatically 
# propagate to any script that uses it.

source("R/plotting/health/03_plot_health_policy_bars.R")

# =============================================================================
# End of script
# =============================================================================
# This script acts as an entry point for generating the health policy bar grid for Fig3D.
# It relies on the shared plotting helper module `plot_health_policy_bars.R` for the 
# core plotting logic. By sourcing the helper module, the script maintains clarity 
# and consistency across multiple figures while keeping the code modular and reusable.

# Key benefits:
#   - **Modular design**: The plotting logic is centralized in `plot_health_policy_bars.R`, 
#     allowing for easy updates and modifications.
#   - **Consistency**: The visual style of policy bar plots remains consistent across different figures.
#   - **Maintainability**: This approach makes it easy to manage the figure creation process 
#     by updating just one module, rather than duplicating code across different scripts.
#
# This script ensures that the policy bar comparisons for multiple models are presented 
# clearly and consistently within the broader figure for Fig3D.
# =============================================================================
