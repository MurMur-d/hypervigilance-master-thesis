# ==================================================================================================
# File: R/plotting/fig05_ssp_ranges/03_call_ssp_ranges_heatmap.R
# Manuscript: Fig5A | SSP range environment heatmap
# ==================================================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the entry point for generating the **SSP-range environment heatmap** for Figure 5A.
# The heatmap visualizes the relationship between the **stationary stressor probability (SSP)** and various
# environmental variables (e.g., the vigilance rate). The heatmap provides a clear view of how different environments
# with varying SSP values interact with the modeled mechanism's outcomes (e.g., hypervigilance).

# The heavy logic for data transformation and model configuration has been centralized in external helpers,
# making this script more modular and easier to maintain. The script's focus is on orchestrating the configuration
# and invoking shared helpers for both data preparation and visualization.

# INPUTS / DEPENDENCIES
# -----------------------------------------------------------------------------
# - **Grid Data**: The grid data for SSP range heatmaps is loaded from the `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R`
#   script. This script contains the logic for preparing the necessary data grid for visualizing the SSP values.
# - **Model Configurations**: It sources model-specific configurations from `basic_model_dp.R` and `health_model_dp.R`
#   to run the appropriate models, depending on the context (basic or health models).
# - **Visualization**: This script sources visualization helpers (`plot_utils.R`) for building consistent ggplot outputs.

# OUTPUTS
# -----------------------------------------------------------------------------
# - The main output of this script is a **ggplot object** representing the heatmap, which is consumed by other
#   scripts or pipelines to generate Figure 5A.
# - The visualization shows how the **stationary stressor probability (SSP)** varies across environmental
#   scenarios, with corresponding hypervigilance levels or other metrics visualized in a faceted grid layout.

# The script organizes and configures the shared helpers, which handle the bulk of the data processing and 
# visualization logic, allowing for clear separation of concerns and making the code easier to maintain.

FIGURE_SCRIPT <- "R/plotting/fig05_ssp_ranges/03_call_ssp_ranges_heatmap.R"
source("R/core/shared_config.R")  # Load shared configuration (e.g., logging, global settings)
log_figure_start(FIGURE_SCRIPT)   # Log the start of the figure generation process

# -----------------------------------------------------------------------------
# NOTE:
# -----------------------------------------------------------------------------
# This file now **reuses** the shared grid and plotting helpers defined in
# `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R`. The original heavy logic 
# was moved there to centralize and modularize the data preparation process.
# The focus of this script is now on configuring and orchestrating the workflow
# for SSP-specific visualizations.
# -----------------------------------------------------------------------------

# Suppressing package startup messages to avoid clutter in the console output.
suppressPackageStartupMessages({
  library(dplyr)       # Data manipulation functions
  library(ggplot2)     # Plotting package for creating heatmaps
  library(future.apply) # Parallel computing package, if needed for processing
})

# Load shared helper functions that define various plot configurations and visualization
source("R/core/plot_utils.R")           # Shared plot utilities
source("R/models/basic/basic_model_dp.R") # Basic model DP-related helpers
source("R/models/basic/basic_model_SIM.R") # Basic model simulation-related helpers
source("R/models/health/health_model_dp.R") # Health model DP-related helpers
source("R/models/health/health_model_SIM.R") # Health model simulation-related helpers

# Source the helper for environment heatmap data preparation.
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")

# -----------------------------------------------------------------------------
# Summary of Operations
# -----------------------------------------------------------------------------
# At this point, the required libraries and helper functions have been sourced.
# You can now use the shared helpers, like `hypervigilance_grid()` (from the loaded helper modules), 
# to build SSP-focused grids or render the faceted heatmaps. This modular approach allows this script to 
# remain focused on configuration and orchestration while leveraging the external helpers for the data 
# preparation and visualization tasks.

# This structure ensures:
#   - **Reusability**: Data transformation and plotting logic are decoupled from this script and can be reused 
#     across different figures and scripts.
#   - **Maintainability**: Changes to the data preparation or plot configuration logic only need to be made in one place.
#   - **Separation of Concerns**: This script focuses only on orchestrating the flow, not the data processing or detailed plotting.

# END OF SCRIPT
