#!/usr/bin/env Rscript
# =============================================================================
# Script: R/plotting/health_dp/03_call_health_dp.R
# =============================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating **health DP diagnostics** 
# (Fig3A). It is designed to load the necessary configuration settings and 
# reusable helper functions for plotting the diagnostic visuals for the health 
# decision process (DP).

# The actual rendering logic for the plots is **not** contained in this script. 
# Instead, this script acts as a **wrapper** that sources the configuration and 
# helper files, keeping the code modular and ensuring consistency across the 
# figure creation pipeline.

# **Important Notes**:
#   - This script **does not contain any plot generation logic** itself.
#   - The main figure rendering is done through the pipeline scripts located 
#     in `scripts/pipelines/`, ensuring that any data transformation, preparation, 
#     and output saving logic is managed centrally.

# =============================================================================
# Metadata and Shared Configuration
# =============================================================================

# The `FIGURE_SCRIPT` variable stores the path to this script and is used for logging 
# purposes to track which script is being executed for figure generation.
FIGURE_SCRIPT <- "R/plotting/health_dp/03_call_health_dp.R"

# The `log_figure_start` function logs the start of the figure generation process, 
# which is crucial for maintaining reproducibility and traceability of the figure 
# creation pipeline. This function will store details about the execution for 
# debugging and record-keeping.
source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load Plotting Helpers for Health DP Diagnostics
# =============================================================================

# This line sources the reusable plot helpers for generating health DP diagnostics.
# The `plot_health_dp.R` script contains the detailed code for rendering the health 
# decision process diagnostic plots, including:
#   - Health DP visual diagnostics.
#   - Graphs showcasing decision boundaries, rewards, and value functions.
#   - Support for generating multiple types of plots for analyzing the health model.

# The purpose of sourcing this script is to ensure that the figure generation 
# remains **modular**, meaning that the same plot helpers can be reused across 
# multiple figures or scripts, ensuring consistency in the plot style, layout, 
# and behavior.

source("R/plotting/health/03_plot_health_dp.R")

# =============================================================================
# End of Script
# =============================================================================

# This script acts purely as an **entry point** for rendering the health DP 
# diagnostics plot for Fig3A. By sourcing the necessary configuration and 
# helper files, it keeps the figure creation process **modular** and **centralized**.

# The advantages of this approach include:
#   - **Separation of concerns**: The script does not contain plot logic, making 
#     it easier to manage and update.
#   - **Reusability**: The `plot_health_dp.R` helper can be reused across 
#     different figures or projects.
#   - **Consistency**: Ensures that the plotting style, titles, legends, and 
#     other elements are consistent across the figure generation pipeline.

# This modular design allows for easier maintenance and updates, making it 
# simpler to modify the figure creation logic without affecting the entire pipeline.
# =============================================================================
