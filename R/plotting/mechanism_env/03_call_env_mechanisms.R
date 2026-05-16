#!/usr/bin/env Rscript
# =============================================================================
# Script: R/plotting/mechanism_env/03_call_env_mechanisms.R
# =============================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating **mechanism hypervigilance** 
# visualizations (Fig4A). It is designed to load the **shared configuration** and 
# **plotting helper** for rendering the hypervigilance mechanisms. This modular structure 
# ensures that the script is easy to maintain and extend, as the actual plotting logic 
# resides in the `R/plotting/plot_mechanism.R` helper script.

# **Important Notes**:
#   - This script **does not** define the plotting functions.
#   - All the plotting logic is handled by the `R/plotting/plot_mechanism.R` helper, which
#     is sourced here.
#   - The figure creation pipeline is handled centrally, ensuring consistency and 
#     modularity across all figure generation scripts.

# =============================================================================
# Metadata and Shared Configuration
# =============================================================================

# The `FIGURE_SCRIPT` variable stores the path to this script. It is primarily used 
# by the logging system to track which script is being executed. This log helps 
# monitor the execution and debugging of the figure generation process, which is essential 
# for reproducibility in a complex analysis pipeline.
FIGURE_SCRIPT <- "R/plotting/mechanism_env/03_call_env_mechanisms.R"

# The `log_figure_start()` function is used here to log the start of the figure creation process. 
# This helps in keeping track of the script execution in the logs for debugging and reproducibility purposes.
source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load Plotting Helper for Mechanism Hypervigilance Visualizations
# =============================================================================

# Here, we source the **plotting helper** script `plot_mechanism.R`. This helper contains 
# the actual plotting functions required for visualizing the hypervigilance mechanisms in the environment.
# The main purpose of this script is to keep the figure creation process modular and reusable, 
# ensuring that all relevant plots for Fig4A are generated from this helper.

# By sourcing this script, we bring in all necessary plotting functions while keeping the current 
# script focused on orchestrating the configuration and logging, rather than the detailed plotting logic.
source("R/plotting/mechanism_env/03_plot_mechanism_env.R")

# =============================================================================
# End of Script
# =============================================================================

# The `R/plotting/mechanism_env/03_call_env_mechanisms.R` script serves as a **simple entry point** 
# for generating the **mechanism hypervigilance visualizations** (Fig4A). It sources the necessary 
# configuration and plotting helpers to ensure that the figure creation process is clean and maintainable. 

# This modular structure has multiple benefits:
#   - **Centralized Plotting Logic**: The actual plotting logic is located in `plot_mechanism.R`, 
#     making it easier to update or modify the plot generation for all figures at once.
#   - **Reusability**: The plotting functions can be reused across different figures or parts of 
#     the project without requiring code duplication.
#   - **Maintainability**: Changes to the plot layout or styling need to be done only in the helper, 
#     ensuring consistency across the figures.

# In summary, this approach improves **maintainability**, **consistency**, and **scalability** 
# of the figure generation process, ensuring that the visualizations remain coherent throughout 
# the analysis pipeline.
