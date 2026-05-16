#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/mechanism_risk_autocorr/03_call_risk_autocorr_mechanisms.R
# ==================================================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating **risk/autocorrelation mechanism panels** 
# (Fig4D). These panels are used to visualize how the risk of hypervigilance (HV) correlates with 
# autocorrelation within the context of the mechanism model.
# The purpose of this visualization is to provide insight into how the dynamic interplay between risk 
# and autocorrelation affects the mechanism's behavior.

# **Key Highlights**:
#   - **Delegation to helper function**: The script delegates the responsibility of the actual plotting 
#     to the reusable helper file `plot_mechanism_risk_autocorr.R`. This modular approach keeps the logic 
#     centralized and makes the script cleaner.
#   - **Centralization of logic**: By sourcing the plotting logic from a shared helper file, this script avoids 
#     duplicating code, making the system more maintainable and ensuring that the same plotting functions are used 
#     consistently throughout the project.
#   - **Reusability**: Any future figure that requires similar risk-autocorrelation visualizations can reuse 
#     the same helper functions, promoting code reusability and consistency.

# =============================================================================
# Metadata and Shared Configuration
# =============================================================================

# The `FIGURE_SCRIPT` variable stores the path to the current script. This is mainly for logging and tracking 
# purposes, as it helps identify which script is responsible for generating specific figures.

FIGURE_SCRIPT <- "R/plotting/mechanism_risk_autocorr/03_call_risk_autocorr_mechanisms.R"

# The `log_figure_start()` function is called to log the start of the figure generation. This helps 
# keep track of which scripts are running and when they started. It is useful for debugging, timing, and 
# organizing the execution sequence.

source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load Shared Plotting Helpers for Risk/Autocorrelation Mechanisms
# =============================================================================

# This line sources the `plot_mechanism_risk_autocorr.R` helper file, which contains the plotting logic 
# for the risk and autocorrelation mechanism panels. By sourcing this file, we are able to use its functions 
# to generate the visualizations for the current figure.

# By centralizing the plotting logic in one helper file, the script can focus solely on orchestrating the 
# figure generation process without dealing with the specific details of the plotting. This reduces duplication 
# and makes it easier to maintain the visualizations across different scripts.

source("R/plotting/mechanism_env/03_plot_mechanism_risk_autocorr.R")

# =============================================================================
# End of Script
# =============================================================================

# In summary, the `R/plotting/mechanism_risk_autocorr/03_call_risk_autocorr_mechanisms.R` script acts as an entry point 
# for generating **risk/autocorrelation mechanism panels** (Fig4D). It delegates the actual plotting logic to 
# the `plot_mechanism_risk_autocorr.R` helper file, ensuring that the code remains modular and reusable.

# The approach of using a shared helper file allows for:
#   - **Centralized logic**: All risk-autocorrelation plotting logic is kept in one place, making updates and 
#     changes easier to manage.
#   - **Consistency**: The same plotting functions are used throughout the project, ensuring uniformity in 
#     the generated visualizations.
#   - **Reusability**: Any other figures that require similar visualizations can easily source the same 
#     helper functions, promoting efficient use of code and reducing redundancy.

# This structure promotes **reusability**, **efficiency**, and **maintainability** in the figure generation 
# process, supporting the overall goal of creating a clean and reproducible visualization pipeline.
