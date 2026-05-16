#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/symmetry_autocorr/03_call_autocorr_heatmap.R
# ==================================================================================================
# PURPOSE
# -----------------------------------------------------------------------------
# This script serves as an entry point for registering and sourcing the **autocorrelation heatmap** 
# figure helpers. It ensures that the necessary plotting functions for visualizing symmetry risk 
# and autocorrelation are available to the entire figure pipeline.
#
# Specifically, the functions required to generate the heatmap visualizations of hypervigilance 
# rates across different values of **K** (vigilance cost) and **autocorrelation** are defined 
# in the shared module `R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R`. This script allows those functions to be 
# loaded and used by other scripts or pipelines that generate the autocorrelation sweeps.
#
# The purpose is to keep the plotting logic centralized, so that changes to the plotting functions 
# can be made in one place, rather than repeating or duplicating code across multiple scripts.
#
# This file essentially "registers" the plot functions and prepares the environment to run the 
# autocorrelation sweeps with the necessary settings.

# ==================================================================================================
# INPUTS
# -----------------------------------------------------------------------------
# - **Data from `R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R`**: This script imports all necessary plotting functions 
#   from the shared helper module `R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R`.
# - **Autocorrelation sweeps**: Pipelines rely on this script to run the autocorrelation sweeps, 
#   which rely on the functions sourced from the shared plotting module.

# ==================================================================================================
# OUTPUTS
# -----------------------------------------------------------------------------
# - This script does **not** directly generate plots or outputs.
# - Instead, it ensures that the plotting helpers defined in the shared module `R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R`
#   are sourced and available for use by the pipeline scripts.
# - Any outputs, such as visualizations or figure files, are handled by the subsequent scripts 
#   in the pipeline, which call these shared helpers.

# ==================================================================================================
# SOURCES
# -----------------------------------------------------------------------------
# - **Shared Configuration**: Load common configurations or settings used across the entire project.
# - **Shared Plotting Functions**: Source the shared `R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R` script, which contains
#   the functions to generate the autocorrelation heatmaps based on symmetry risk data.
FIGURE_SCRIPT <- "R/plotting/symmetry_autocorr/03_call_autocorr_heatmap.R"
source("R/core/shared_config.R")  # Load shared configuration (e.g., logging, global settings)
log_figure_start(FIGURE_SCRIPT)   # Log the start of the figure generation process

# ==================================================================================================
# Load Shared Plotting Helpers
# -----------------------------------------------------------------------------
# This will make the functions defined in `R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R` available for the pipeline.
source("R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R")  # Import the shared plotting functions

# ==================================================================================================
# END OF SCRIPT
# -----------------------------------------------------------------------------
# This script serves the critical role of sourcing the necessary plotting functions 
# for autocorrelation sweeps, making them available to the pipelines that will 
# generate and save the actual figure outputs.
