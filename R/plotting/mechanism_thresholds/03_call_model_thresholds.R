#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/mechanism_thresholds/03_call_model_thresholds.R
# ==================================================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating **mechanism threshold visualizations** 
# (Fig4C). These visualizations provide a way to examine how different threshold values influence 
# the behavior of the hypervigilance mechanism in a model. The goal is to visually represent 
# thresholds and their impacts on the model's output, aiding in the interpretation and understanding 
# of the system's dynamics.

# **Key Highlights**:
#   - **Sourcing threshold helpers**: The script sources `plot_mechanism_thresholds.R`, which contains 
#     the plotting functions needed to create threshold-based visualizations.
#   - **Reusing shared logic**: By sourcing the shared helper functions, this script avoids duplicating 
#     threshold logic and ensures that all figure scripts that rely on threshold plotting functions use 
#     the same definitions.
#   - **Centralized updates**: If any changes need to be made to how thresholds are visualized, they 
#     only need to be updated in the shared helper file, making the pipeline easier to maintain and modify.

# =============================================================================
# Metadata and Shared Configuration
# =============================================================================

# The `FIGURE_SCRIPT` variable stores the path to the current script. This is useful for logging purposes 
# and tracking which script is responsible for generating the figure. This allows us to monitor the execution 
# of specific figures across the pipeline and maintain traceability.

FIGURE_SCRIPT <- "R/plotting/mechanism_thresholds/03_call_model_thresholds.R"

# The `log_figure_start()` function is invoked to log the start of the figure generation process. 
# This helps in tracking when each figure script starts running and can be valuable for debugging and 
# understanding the sequence of operations in the pipeline.

source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load Shared Plotting Helpers for Mechanism Thresholds
# =============================================================================

# This step sources the `plot_mechanism_thresholds.R` file, which contains the plotting functions 
# for generating the **mechanism threshold visualizations**. By sourcing this helper, we gain access 
# to the defined plotting functions without duplicating the logic here in the script. This helps keep 
# the code clean, modular, and maintainable.

# The plotting logic related to thresholds is central to this visualization and will be used by 
# various figures. Therefore, centralizing this logic in one helper file ensures that all the scripts 
# requiring this functionality can simply reference the same code, promoting consistency and ease of 
# maintenance.

source("R/plotting/mechanism_env/03_plot_mechanism_thresholds.R")

# =============================================================================
# End of Script
# =============================================================================

# In summary, the `R/plotting/mechanism_thresholds/03_call_model_thresholds.R` script is an entry point 
# for generating **mechanism threshold visualizations** (Fig4C). The core plotting functions are defined 
# in the sourced `plot_mechanism_thresholds.R` file, allowing for reusable and centralized plotting logic.
# This approach ensures:
#   - **Modularity**: The plotting logic is maintained in a single helper module, so updates and changes 
#     can be made in one place without affecting other parts of the project.
#   - **Reusability**: Multiple scripts across the pipeline can reuse the same helper functions for 
#     consistent threshold visualizations.
#   - **Maintainability**: By avoiding duplication of threshold logic in every figure script, it becomes 
#     easier to track and modify how thresholds are visualized throughout the project.

# Overall, this structure supports **reproducibility**, **efficiency**, and **consistency** in the figure generation 
# process, streamlining both the creation of the threshold visualizations and the maintenance of the overall pipeline.
