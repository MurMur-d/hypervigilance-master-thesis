#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/symmetry_risk/03_call_risk_heatmap.R
# ==================================================================================================
# PURPOSE
# -----------------------------------------------------------------------------
# This script is responsible for loading the necessary **symmetry risk helpers** that are 
# required for the figure pipeline. Specifically, it sources the plotting functions from the 
# `R/plotting/symmetry_risk/03_plot_symmetry_risk.R` module, which contain the logic for generating the symmetry risk visualizations.
# These functions are then made available for the pipeline to generate risk-related plots based on the 
# provided data. This allows for centralized management of the plotting functions, ensuring consistency 
# across different figure generation steps.
#
# This script serves as a registration point for the helpers needed to render the **symmetry risk** plots 
# for subsequent steps in the pipeline.

# ==================================================================================================
# INPUTS
# -----------------------------------------------------------------------------
# - **`R/plotting/symmetry_risk/03_plot_symmetry_risk.R`**: This is the script where the plotting functions for 
#   symmetry risk visualizations are defined. It contains all the necessary code for visualizing 
#   the risk metrics (e.g., hypervigilance vs risk, K-value vs SSR, etc.).
# 
# The pipeline will load this script to source the functions that can be used for plotting.

# ==================================================================================================
# OUTPUTS
# -----------------------------------------------------------------------------
# - This script **does not generate any direct outputs**.
# - It only ensures that the plotting helpers from `R/plotting/symmetry_risk/03_plot_symmetry_risk.R` are available 
#   for use by the pipeline scripts that will generate and save the figures.

# ==================================================================================================
# LOGGING AND CONFIGURATION
# -----------------------------------------------------------------------------
# Set the figure identifier for the registry logging helpers.
FIGURE_SCRIPT <- "R/plotting/symmetry_risk/03_call_risk_heatmap.R"

# This script begins by loading the shared configuration and logging metadata.
source("R/core/shared_config.R")  # Load the shared configuration (e.g., logging, settings)
log_figure_start(FIGURE_SCRIPT)   # Log the start of this figure script

# ==================================================================================================
# Load Plotting Helpers for Symmetry Risk
# -----------------------------------------------------------------------------
# This line sources the shared plotting functions defined in `R/plotting/symmetry_risk/03_plot_symmetry_risk.R` that are 
# responsible for generating the plots for symmetry risk. By sourcing this script, all the 
# functions required for symmetry risk visualizations are loaded into the current environment.
source("R/plotting/symmetry_risk/03_plot_symmetry_risk.R")  # Import the symmetry risk plotting functions

# ==================================================================================================
# END OF SCRIPT
# -----------------------------------------------------------------------------
# This script does not directly generate figures, but ensures that the necessary plotting functions 
# from `R/plotting/symmetry_risk/03_plot_symmetry_risk.R` are loaded and available for the pipeline scripts to use. 
# These functions will be used later in the pipeline to generate symmetry risk-related visualizations.
