#!/usr/bin/env Rscript
# =============================================================================
# Script: R/plotting/health_sim/03_call_health_sim.R
# =============================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating the **health simulation 
# diagnostics figure** (Fig3B). It is designed to load shared configurations and 
# reusable helper functions for plotting the results from health simulations.

# The actual plotting logic is **delegated** to the `plot_health_simulation.R` 
# helper script, which contains the core functions for visualizing the simulation 
# results. This modular approach keeps the codebase clean, maintainable, and reusable.

# **Important Notes**:
#   - This script **does not contain any plotting logic**.
#   - The figure rendering and diagnostic analysis are handled by the `plot_health_simulation.R` 
#     helper, which is sourced here.
#   - The plot generation pipeline and other data transformations are managed by 
#     the central pipeline scripts found in the `scripts/pipelines/` directory.

# =============================================================================
# Metadata and Shared Configuration
# =============================================================================

# The `FIGURE_SCRIPT` variable stores the path to this script. It is used by the 
# logging system to track which script is being executed, providing a way to monitor 
# the progress of figure creation and track each figure generation in the log files.
FIGURE_SCRIPT <- "R/plotting/health_sim/03_call_health_sim.R"

# The `log_figure_start()` function is called to log the beginning of the figure 
# generation process. This step is essential for tracking execution and ensuring that 
# the process is properly logged for reproducibility and debugging.
source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load Plotting Helpers for Health Simulation Diagnostics
# =============================================================================

# Here, we source the reusable plotting helper for health simulation diagnostics.
# The `plot_health_simulation.R` script contains the functions for rendering various 
# types of plots related to health simulations. This includes visualizations like:
#   - Agent health over time.
#   - Distribution of health states across the population.
#   - Comparisons of health metrics under different scenarios.
#   - Any other simulation diagnostics required for the analysis of the health model.

# Sourcing this file ensures that all of the plotting functions are available and 
# keeps the figure creation process modular. By sourcing this helper, we avoid code 
# duplication and maintain consistency in the plot styling and behavior.

source("R/plotting/health/03_plot_health_simulation.R")

# =============================================================================
# End of Script
# =============================================================================

# This script is purely an **entry point** for generating the health simulation 
# diagnostics figure (Fig3B). It sources the necessary configuration and plotting 
# helpers and logs the execution start. The figure creation is modularized and 
# orchestrated in a centralized manner.

# The benefits of this modular approach include:
#   - **Separation of concerns**: This script focuses on loading configurations and helpers, 
#     leaving the plotting logic in a dedicated helper file.
#   - **Reusability**: The `plot_health_simulation.R` helper can be reused for other figures 
#     or experiments, ensuring that the simulation plots are consistent across different visualizations.
#   - **Maintainability**: This approach allows updates to the plot functions without 
#     changing the entire figure creation pipeline, making the code easier to maintain and scale.

# This modular structure improves **maintainability** and ensures **consistency** across 
# the figure generation process, making it easier to generate and analyze multiple figures 
# with minimal changes.
# =============================================================================
