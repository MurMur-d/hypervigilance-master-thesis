#!/usr/bin/env Rscript
# =============================================================================
# File: R/plotting/env_heatmaps/03_call_env_heatmaps_with_region_key.R
# =============================================================================
# ROLE / INTENT
# -----------------------------------------------------------------------------
# This script serves as an entry point for generating the **environment heatmaps**
# in **Fig2C** that include a **region key**. It relies on helper functions 
# from external R modules to create consistent and reusable code for environment 
# heatmap generation, specifically visualizing environmental scenarios with the 
# addition of a region key.
#
# Key Notes:
#   - The script sources external helper modules, meaning the logic for the 
#     plot design and data preparation is centralized in the `R/plotting` and 
#     `R/plotting/env_heatmaps/` modules, which helps ensure consistency across multiple 
#     figure scripts and makes the pipeline more maintainable.
#   - The figure's pipeline, which includes data preparation, model simulations, 
#     and plot saving, is typically handled by a higher-level script or pipeline 
#     (e.g., `scripts/pipelines/09_run_all_plots.R`).
#
# =============================================================================

# =============================================================================
# Metadata and shared configuration
# =============================================================================

# Define the figure script file name for logging purposes. 
# This will be used by the logging system to identify the current figure script 
# and help track which part of the pipeline has executed.
FIGURE_SCRIPT <- "R/plotting/env_heatmaps/03_call_env_heatmaps_with_region_key.R"

# Load global configuration and utilities shared across the entire project.
# Typically, this includes:
#   - Default theme settings for consistent plotting (e.g., `theme_vigilance()`)
#   - Custom color palettes, scale functions, and helper functions.
#   - Reproducibility helpers, such as logging, figure saving utilities, and path management.
source("R/core/shared_config.R")

# Log the start of this figure script execution to ensure tracking of progress.
# This is useful in large pipelines to identify which script was running at any 
# given time.
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load data preparation and plotting modules for the environment heatmaps
# =============================================================================

# This module is responsible for preparing the data for the environment heatmaps.
# It will:
#   - Construct the grid of LA (lambda A) and LL (lambda L) values.
#   - Aggregate and manipulate any model data required for the heatmaps.
#   - Prepare the data for downstream plotting (e.g., compute metrics like 
#     hypervigilance).
#
# This separation ensures that the data preparation step is independent from the 
# plotting logic.
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")

# This module contains the actual plotting code for the environment heatmaps.
# It handles:
#   - Plotting the environment data using `ggplot2` or any other visualization tools.
#   - Adding any relevant visual elements such as region keys, thresholds, 
#     predictability boundaries, and region labels.
#   - Ensuring that the heatmap layout is consistent across multiple plots.
#
# It is sourced here to ensure that the functions can be reused and maintained 
# in a centralized location.
source("R/plotting/env_heatmaps/03_plot_env_heatmaps.R")

# =============================================================================
# End of script
# =============================================================================
# This file serves only as an entry point to load the necessary functions and 
# begin the process of generating the heatmaps for Fig2C. The heavy lifting 
# (data preparation, model simulations, and figure saving) will be handled 
# elsewhere, typically in a higher-level pipeline script.
#
# This modular approach ensures:
#   - Reusability: Each component of the figure logic (data prep, plotting) is 
#     kept modular and can be reused for different figures or analyses.
#   - Maintainability: Future changes to the plot aesthetics or data preparation 
#     logic can be made in the appropriate helper module without modifying this 
#     script.
# =============================================================================
