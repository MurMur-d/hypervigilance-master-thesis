#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/mechanism_flow/03_call_vigilance_flow.R
# ==================================================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating **vigilance flow visualizations** 
# (Fig4B). These visualizations depict how agents transition between different vigilance states 
# over time, providing insights into the underlying dynamics of the hypervigilance mechanism.
# The purpose of this script is to source the relevant helper module so that the pipeline and 
# other scripts can reuse its plotting functions without duplicating logic.

# **Key Highlights**:
#   - **Sourcing the helper module**: The script sources `plot_vigilance_flow.R`, which contains
#     the core plotting functions.
#   - **Reuse of logic**: By centralizing the plotting functions in a dedicated module, this 
#     script avoids code duplication and ensures that any changes to the plotting logic are 
#     reflected consistently across the project.
#   - **Modular pipeline**: This approach supports a modular pipeline, allowing for flexibility 
#     and easier maintenance across scripts that generate related visualizations.

# =============================================================================
# Metadata and Shared Configuration
# =============================================================================

# The `FIGURE_SCRIPT` variable holds the file path of the current script, which is used 
# by the logging system to track the execution of this script. This is helpful for debugging 
# and tracking script executions in a more complex pipeline.

FIGURE_SCRIPT <- "R/plotting/mechanism_flow/03_call_vigilance_flow.R"

# The `log_figure_start()` function is called to log the start of the figure generation process.
# This allows tracking of the script's execution and aids in debugging and reproducibility.
source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load Plotting Helper for Vigilance Flow Visualizations
# =============================================================================

# Here, we source the **plotting helper** script `plot_vigilance_flow.R`, which contains 
# the actual plotting functions for generating vigilance flow diagrams. By sourcing this script, 
# we gain access to all the necessary functions for visualizing the flow of agents between 
# vigilance states in a consistent and reusable manner.

# This modular approach ensures that the plotting logic is maintained in a single place 
# (the helper module), so any changes to the visualizations can be made in one location, 
# without affecting the rest of the pipeline. It also ensures consistency in the visualization 
# style across multiple figures and scripts.

source("R/plotting/vigilance_flow/03_plot_vigilance_flow.R")

# =============================================================================
# End of Script
# =============================================================================

# In conclusion, the `R/plotting/mechanism_flow/03_call_vigilance_flow.R` script acts as a simple 
# entry point to generate **vigilance flow visualizations** (Fig4B). The key logic is contained 
# within the sourced `plot_vigilance_flow.R` helper module, which centralizes the plotting functions 
# and allows other scripts and pipelines to reuse them without redundancy. 

# **Benefits of this approach**:
#   - **Centralization of logic**: The plotting functions are stored in a single, reusable helper 
#     module, reducing redundancy and enhancing maintainability.
#   - **Consistency**: By using a shared module, the visualizations across different figures remain 
#     consistent in style and structure.
#   - **Modularity**: The modular design of this pipeline makes it easier to modify, extend, and 
#     debug without affecting other parts of the project.

# This structure improves **reproducibility**, **efficiency**, and **maintainability** in the long 
# term, making it easier to update and modify visualizations as the project evolves.
