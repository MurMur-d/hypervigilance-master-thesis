#!/usr/bin/env Rscript
# =============================================================================
# File: R/plotting/env_hv_matrix/03_call_env_hv_matrix.R
# =============================================================================
# ROLE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating the **environmental 
# hypervigilance (HV) summaries** for **Fig2B**. The purpose is to visualize 
# the hypervigilance values across various environmental scenarios.
#
# Key Notes:
#   - The primary task of this script is to delegate the responsibility for 
#     building the hypervigilance matrix and performing any necessary plotting 
#     to the specialized module `R/plotting/plot_env_hv_matrix.R`.
#   - The script functions in a modular way to ensure that the plotting logic is 
#     encapsulated within reusable helpers. This allows any changes to the plotting 
#     aesthetics or data preparation to be handled in one place, promoting 
#     consistency and maintainability across the project.
#
# The logic for preparing the data, applying model simulations, and finalizing 
# the visualizations will typically be handled in a higher-level pipeline script, 
# which will call this script as part of a larger workflow for generating and 
# saving the figures.

# =============================================================================
# Metadata and shared configuration
# =============================================================================

# Set the `FIGURE_SCRIPT` variable for logging purposes, which helps track the 
# execution of the current script in large pipelines.
FIGURE_SCRIPT <- "R/plotting/env_hv_matrix/03_call_env_hv_matrix.R"

# Source the shared configuration script which sets up:
#   - Global theme settings (e.g., `theme_vigilance()`).
#   - Custom color palettes and scales.
#   - Logging and reproducibility functions.
source("R/core/shared_config.R")

# Log the start of the figure creation process. This is a key part of the 
# reproducibility and tracking pipeline.
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load the plot-specific module for environment hypervigilance matrix
# =============================================================================

# This module contains the specialized plotting functions for generating the 
# environment hypervigilance matrix. It is responsible for:
#   - Plotting the hypervigilance data across various environmental parameters.
#   - Formatting and visualizing the matrix in a clear and consistent manner.
#   - Ensuring any relevant features, such as axis labels, region keys, or 
#     thresholds, are included in the final plot.
#
# The separation of concerns between the plotting logic and the higher-level 
# script allows for easier maintenance and reuse. The plotting logic is kept 
# in `R/plotting/plot_env_hv_matrix.R`, while this script simply delegates 
# the task to the relevant helper function.
source("R/plotting/env_heatmaps/03_plot_env_hv_matrix.R")

# =============================================================================
# End of script
# =============================================================================
# This file serves as an entry point and calls the helper modules responsible 
# for the heavy lifting (data preparation, plotting, etc.). The overall goal 
# is to maintain a clean, modular, and reusable codebase. 
#
# This approach provides:
#   - **Consistency**: Any changes made to the visualization logic or data 
#     transformation are centralized in the appropriate helper module.
#   - **Maintainability**: If future modifications to the plot aesthetics 
#     or data preparation are needed, they can be made without altering this 
#     script.
# =============================================================================
