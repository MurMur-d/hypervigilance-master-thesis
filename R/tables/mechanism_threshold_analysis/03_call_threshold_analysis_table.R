#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/tables/mechanism_threshold_analysis/03_call_threshold_analysis_table.R
# ==================================================================================================
# PURPOSE / INTENT
# -----------------------------------------------------------------------------
# This script serves as the **entry point** for generating the **mechanism threshold analysis tables** 
# (Fig4E). The purpose of the analysis is to evaluate the thresholds at which certain behaviors or 
# decisions within the mechanism model shift, and visualize them in table format.
# 
# **Key Highlights**:
#   - **Delegation of Logic**: The script sources a data preparation helper file (`prep_mechanism_threshold_analysis.R`) 
#     to generate the threshold analysis tables. This allows the script to maintain a clean structure by separating 
#     the table generation logic from the script itself.
#   - **Centralization of Data Prep**: By sourcing a separate data preparation helper, any changes to how the 
#     threshold analysis tables are created can be made in one place, without needing to modify this script or the 
#     pipeline logic in other locations.
#   - **Reusability**: The `prep_mechanism_threshold_analysis.R` helper is reusable across different figures or analyses 
#     that require the same threshold analysis logic, making the codebase more modular and maintainable.

# =============================================================================
# Metadata and Shared Configuration
# =============================================================================

# This variable holds the path to the current script. It is used for logging and reproducibility metadata.
FIGURE_SCRIPT <- "R/tables/mechanism_threshold_analysis/03_call_threshold_analysis_table.R"

# The `log_figure_start()` function is called to record the start time of the figure generation. 
# It helps track the execution flow of the figure creation pipeline and can be used for debugging, 
# monitoring, and performance analysis.

source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load Shared Data Preparation Helper for Threshold Analysis
# =============================================================================

# The `prep_mechanism_threshold_analysis.R` script is sourced to handle the preparation of the threshold 
# analysis tables. This helper contains the logic necessary to extract, process, and structure the relevant data 
# for the threshold analysis.

# By sourcing this helper, the script avoids having to redefine data preparation steps and ensures that the 
# analysis logic is centralized in one place. This improves code maintainability and reduces duplication.

source("R/plotting/mechanism_env/01_data_prep_mechanism_threshold_analysis.R")

# =============================================================================
# End of Script
# =============================================================================

# In summary, this script serves as the entry point for generating the **mechanism threshold analysis tables** 
# (Fig4E). It sources the data preparation logic from the `prep_mechanism_threshold_analysis.R` helper file 
# to ensure that the table generation process is consistent and reusable across different parts of the project.

# The benefits of this approach include:
#   - **Centralized logic**: Data preparation and table creation logic are handled in one place, making it easier to 
#     modify and maintain.
#   - **Reusability**: The same data preparation steps can be used for other figures or analyses that require threshold 
#     analysis tables, promoting code efficiency and consistency.
#   - **Clarity**: By separating the data preparation from the visualization, this script remains focused on generating 
#     the figure itself, making it easier to understand and debug.
