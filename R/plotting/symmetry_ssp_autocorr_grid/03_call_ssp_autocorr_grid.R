#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/symmetry_ssp_autocorr_grid/03_call_ssp_autocorr_grid.R
# ==================================================================================================
# PURPOSE
# -----------------------------------------------------------------------------
# This script generates the **SSP vs autocorrelation model comparison** figure (Fig6A) for the manuscript.
# It leverages various pre-configured helpers for plotting and data preparation to create a grid comparing
# hypervigilance rates (HV rates) and autocorrelation across different model scenarios. The goal of this
# figure is to visually present the relationship between stationary stressor probability (SSP) and autocorrelation
# for different models under various conditions (hypervigilance and autocorrelation).
#
# It follows a modular approach where the heavy lifting of data processing and plotting is done by 
# dedicated helper modules, while the script itself handles configuration and orchestration.
#
# The output includes:
# - A grid-based heatmap visualizing the comparison of HV rates and autocorrelation.
# - A corresponding table with the data used to generate the heatmap.

# ==================================================================================================
# INPUTS
# -----------------------------------------------------------------------------
# - **Hypervigilance Rates**: These values will be used to compute the model comparison.
# - **Autocorrelation**: This is the second variable for the comparison.
# - **SSP**: Stationary Stressor Probability, used in conjunction with autocorrelation.
#
# The data for these variables is generated via helper functions and then passed to plotting functions for visualization.
# - The **model scenarios** are specified in `default_symmetry_model_scenarios()`, and the environment scenarios
#   are defined in `default_env_scenarios()`.
# - The script uses **hypervigilance rate** and **autocorrelation** for comparison across different parameter grids.
# 
# The plot itself is generated using the `plot_ssp_vs_autocorr_by_model_K_hvstyle()` function from the plotting helper.

# ==================================================================================================
# OUTPUTS
# -----------------------------------------------------------------------------
# - The **main output** of this script is the **SSP vs autocorrelation model comparison heatmap**.
# - Additionally, the script will output a **table** with the underlying data used to create the plot, 
#   which can be useful for later analysis or reproducibility.
# - The saved files will be stored under the `outputs/figures/ssp_autocorr` directory, with both the figure 
#   and table stored with appropriate filenames.

FIGURE_SCRIPT <- "R/plotting/symmetry_ssp_autocorr_grid/03_call_ssp_autocorr_grid.R"
source("R/core/shared_config.R")  # Shared configuration for logging and settings
log_figure_start(FIGURE_SCRIPT)   # Logging function to track figure generation progress

message("-> preparing symmetry SSP/autocorr comparison")  # Message to indicate that the script is preparing data

# ==================================================================================================
# SOURCES
# -----------------------------------------------------------------------------
# Load core functions and helpers needed for plotting and data preparation.
source("R/core/plot_utils.R")                 # General plot utilities like themes and colors
source("R/models/basic/basic_model_dp.R")     # Basic model decision process helpers
source("R/models/basic/basic_model_SIM.R")    # Basic model simulation helpers
source("R/models/health/health_model_dp.R")   # Health model decision process helpers
source("R/models/health/health_model_SIM.R")  # Health model simulation helpers

# Load data preparation helpers specific to symmetry risk and autocorrelation.
source("R/plotting/symmetry_risk/00_data_prep_symmetry_risk_autocorr.R")  # Symmetry-specific data prep for SSP and autocorrelation
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")  # Prepares environmental data for the heatmap

# Load the plot-specific helpers for symmetry risk and autocorrelation.
source("R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R")  # Plots related to symmetry and autocorrelation comparisons

# ==================================================================================================
# SIMULATION GRID PARAMETERS
# -----------------------------------------------------------------------------
# Set the parameters for the grid simulation. These parameters define the conditions under which the models
# will be run to compute hypervigilance rates and autocorrelation for different SSP environments.
C <- 0  # Cost for vigilance (this can be adjusted for different models)
D <- 10  # Damage when stressor occurs
d <- 0   # Damage when stressor does not occur
K_vals <- c(1, 5, 9)  # Varying vigilance cost K
T_steps <- 10  # Number of time steps for simulation
N_agents <- 1000  # Number of agents to simulate
states <- c("K", "Kd", "C", "CD")  # Different states to consider in the models
envs <- default_env_scenarios()  # Default environment scenarios to use for comparison

# Policy arguments for the simulation (e.g., health step, reward weight, etc.)
pol_args <- list(
  h0 = 35,                    # Initial health value
  health_step = 1,            # Step size for health updates
  terminal_reward_weight = 0, # Weight for the final reward based on health
  terminal_reward_mode = "linear"  # Mode for how the terminal reward scales with health
)

# Simulation arguments (e.g., agent initialization, shuffling)
sim_args <- list(
  h0 = 35,                               # Initial health for agents
  spread_initial_over_levels = FALSE,    # Don't spread agents evenly across levels
  shuffle = TRUE                         # Shuffle agents at the start of the simulation
)

# Symmetry model scenarios (loaded from a helper function that defines various model configurations)
model_scenarios <- default_symmetry_model_scenarios  # Default model scenarios to apply for the simulation

# ==================================================================================================
# BUILD AND PLOT THE SSP/AUTOCORR COMPARISON GRID
# -----------------------------------------------------------------------------
# Using the model scenarios and grid parameters, we build the data for the SSP vs autocorrelation plot.
df_ssp_auto <- env_autocorr_grid_K_values_by_model(
  model_scenarios = model_scenarios,
  C = C, d = d, D = D,           # Model-specific parameters (e.g., costs, damage)
  K_values = K_vals,             # Range of vigilance costs to simulate
  T_steps = T_steps,             # Number of time steps to simulate
  states = states,               # States considered in the simulation
  N_agents = N_agents,           # Number of agents to simulate
  env_scenarios = envs,          # Environmental scenarios to simulate
  base_policy_args = pol_args,   # Policy-specific arguments (e.g., reward settings)
  base_sim_args = sim_args      # Simulation-specific arguments (e.g., initial health)
)

# Call the plotting function to generate the heatmap plot.
p <- plot_ssp_vs_autocorr_by_model_K_hvstyle(df_ssp_auto)

# Display the plot
print(p)

# Save the plot as a .png image file in the appropriate directory
save_graphs(
  p,
  file.path("outputs", "figures", "ssp_autocorr", "ssp_vs_autocorr_model_comparison")
)

# Save the table containing the data used to generate the heatmap
save_hv_matrix_table(
  df_ssp_auto,
  file.path("outputs", "figures", "ssp_autocorr", "ssp_vs_autocorr_grid_table")
)

# END OF SCRIPT
