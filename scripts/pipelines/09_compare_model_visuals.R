#!/usr/bin/env Rscript
# =============================================================================
# Script: scripts/pipelines/09_compare_model_visuals.R
# Purpose: Generate comparative diagnostics that contrast the core models.
# Notes:
#   - Exposes `run_compare_model_visuals()` for orchestration scripts to reuse.
#   - Relies on tidy datasets from `build_real_datasets()` and dedicated plot helpers.
# =============================================================================

# Source necessary configuration and helper functions
if (!exists("HV_SETUP_DONE", envir = .GlobalEnv)) {
  source("R/core/setup_project.R")   # Project setup and configuration
  assign("HV_SETUP_DONE", TRUE, envir = .GlobalEnv)
}
source("R/plotting/compare_models/00_data_prep_compare_model_visuals.R")  # Data preparation for model comparison
source("R/plotting/compare_models/03_plot_compare_models.R")  # Plotting functions specific to model comparisons

#' Run the dedicated compare-model diagnostic suite.
#'
#' This function generates and saves a series of comparative visualizations
#' that help contrast the behavior and performance of different models.
#' The generated figures include hypervigilance heatmaps, vigilance trajectories,
#' and several diagnostic plots, each serving to visualize key model comparisons.
#'
#' @param out_dir Directory where figures should be saved (default is `DIR_FIGURES`).
#' @param params Optional parameter overrides passed to the compare-model prep helpers.
#' @return NULL, saves figures to disk
run_compare_model_visuals <- function(out_dir = DIR_FIGURES, params = list()) {
  # Create the output directory if it doesn't exist
  fs::dir_create(out_dir)

  # Notify the user that the datasets are being built
  message(">>> Building datasets for compare-model visuals")
  
  # Build the datasets required for the visualizations
  real_data <- build_real_datasets(params = params)

  # Define the specifications for the plots to be generated
  plot_specs <- list(
    hv_heatmap = list(
      plot = plot_hv_heatmap(real_data$hv_ac),  # Plot hypervigilance heatmap
      description = "Hypervigilance heatmap (cost vs autocorr)"  # Description for the heatmap plot
    ),
    episode_trajectory = list(
      plot = plot_episode_trajectory(real_data$episode_stage),  # Plot vigilance trajectory across episode stages
      description = "Vigilance trajectory across episode stages"  # Description for the trajectory plot
    ),
    peaks_collapse = list(
      plot = plot_peaks_collapse(real_data$peaks),  # Plot peak vs collapse cost markers
      description = "Peak vs collapse cost markers"  # Description for the peaks plot
    ),
    indifference = list(
      plot = plot_indifference(real_data$indiff),  # Plot decision indifference frequency by environment
      description = "Decision indifference frequency by environment"  # Description for the indifference plot
    ),
    behavior_profile = list(
      plot = plot_profile_alluvial(real_data$profile),  # Plot alluvial diagram of behavioral profiles by model
      description = "Behavioral profile alluvial by model"  # Description for the profile alluvial plot
    ),
    health_action_mix = list(
      plot = plot_health_action_mix(real_data$health_action_mix),  # Plot health action mix for each policy/environment
      description = "Health action mix for each policy/environment"  # Description for the health action mix plot
    )
  )

  # Iterate through each plot specification and generate/save the plots
  purrr::iwalk(plot_specs, function(spec, name) {
    message("-> Compare-model plot: ", spec$description)  # Print the plot description for progress

    print(spec$plot)  # Display the plot

    # Save the plot to the specified output directory with a naming convention
    save_graphs(
      spec$plot,  # Save the current plot
      file.path(out_dir, paste0("compare_model_", name)),  # Output file path
      model = "compare"  # Model type for saving the plot
    )
  })
}

# The function `run_compare_model_visuals` does the following:
# 1. Creates an output directory if it does not exist.
# 2. Uses `build_real_datasets` to prepare the data needed for plotting.
# 3. Iterates over a predefined list of plot specifications (`plot_specs`), where each specification 
#    includes the plot itself and a description.
# 4. For each plot, it prints a message, displays the plot, and saves the plot to disk.
#
# The output consists of several figures that are saved with names indicating their specific comparisons:
#  - Hypervigilance heatmap
#  - Vigilance trajectory
#  - Peaks vs collapse markers
#  - Decision indifference by environment
#  - Behavioral profile alluvial plot
#  - Health action mix plot
#
# This script is intended for use within pipelines, where it's invoked to automatically generate
# the visual diagnostics for model comparison.
