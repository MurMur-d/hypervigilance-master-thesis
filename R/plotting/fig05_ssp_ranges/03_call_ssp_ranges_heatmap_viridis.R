# ==================================================================================================
# File: R/plotting/fig05_ssp_ranges/03_call_ssp_ranges_heatmap_viridis.R
# Manuscript: Fig5B | SSP Verdis environment heatmap
# ==================================================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the entry point for rendering the **Verdis-flavored SSP range environment heatmap**,
# which is an extension of the core SSP heatmap logic used in Fig5A. This visualization presents the 
# stationary stressor probability (SSP) over different environmental conditions, with a specific "Verdis" 
# variant applied to the analysis.
# The heavy data processing and plotting logic are delegated to the shared pipeline to maintain consistency 
# and avoid code duplication between similar scripts for different figures.

# INPUTS / DEPENDENCIES
# -----------------------------------------------------------------------------
# - **Hypervigilance Grid**: The grid data for hypervigilance and related variables comes from `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R`.
# - **Model Configurations**: This script loads model-specific configurations from basic (`basic_model_dp.R` and `basic_model_SIM.R`)
#   and health models (`health_model_dp.R` and `health_model_SIM.R`).
# - **Visualization**: Shared plot helpers from `R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R` keep the Verdis variant aligned with the core LA/LL grid rendering logic.

# OUTPUTS
# -----------------------------------------------------------------------------
# - The output of this script is a **ggplot object** representing the heatmap for the Verdis-flavored SSP-range environment,
#   which will be consumed by the pipeline to generate the final visualization for Figure 5B.
# - The shared pipeline ensures consistent rendering and that the plot stays aligned with the logic used in other heatmaps 
#   across the manuscript.

# This script focuses on configuring and orchestrating the pipeline for the Verdis-flavored SSP-specific visualizations,
# leveraging shared helpers for the heavy lifting related to data preparation and visualization.

FIGURE_SCRIPT <- "R/plotting/fig05_ssp_ranges/03_call_ssp_ranges_heatmap_viridis.R"
source("R/core/shared_config.R")  # Load shared configuration (e.g., logging, global settings)
log_figure_start(FIGURE_SCRIPT)   # Log the start of the figure generation process

# -----------------------------------------------------------------------------
# NOTE:
# -----------------------------------------------------------------------------
# This variant is kept for **backwards compatibility**, but it now delegates the 
# heavy lifting to the centralized data preparation and plotting logic found in
# `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R`.
# The shared helper provides the same API, ensuring that both the **SSP**-specific scripts (for Fig5A and Fig5B)
# stay in sync without duplicating any logic. This makes the workflow more modular and maintainable.
# -----------------------------------------------------------------------------

# Suppressing startup messages from the libraries to clean up console output.
suppressPackageStartupMessages({
  library(dplyr)       # Data manipulation functions for reshaping and transforming the data
  library(ggplot2)     # Main plotting library for creating visualizations like heatmaps
  library(future.apply) # Enables parallel computation for tasks that may benefit from it
})

# Source various shared helper functions for plotting and data manipulation
source("R/core/plot_utils.R")           # Shared plot utilities like themes, colors, etc.
source("R/models/basic/basic_model_dp.R") # Basic model decision process helpers
source("R/models/basic/basic_model_SIM.R") # Basic model simulation helpers
source("R/models/health/health_model_dp.R") # Health model decision process helpers
source("R/models/health/health_model_SIM.R") # Health model simulation helpers

# Source the data preparation helper that centralizes the logic for preparing SSP-related data.
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")  # Prepares the grid data for plotting

# Source the plot-specific helper which consolidates the environment heatmap rendering logic.
source("R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R")

# -----------------------------------------------------------------------------
# Summary of Operations
# -----------------------------------------------------------------------------
# At this point, all required libraries and helper functions have been sourced.
# The shared helpers (e.g., `run_hypervigilance_pipeline_by_model()`) are now available and can be called
# directly below for building the SSP-specific grids and generating the heatmap visualizations.

# This script essentially configures the Verdis-flavored variant of the SSP heatmap while leveraging the 
# **shared pipeline** for data processing and plotting. The modular nature of this setup ensures:
#   - **Reusability**: The data transformation and plotting logic are centralized and reusable across different figures.
#   - **Consistency**: Both Fig5A and Fig5B are consistent in their data preparation and visualization, ensuring uniformity.
#   - **Maintainability**: Any updates or improvements to the pipeline only need to be made in one place, reducing the maintenance overhead.

# END OF SCRIPT
