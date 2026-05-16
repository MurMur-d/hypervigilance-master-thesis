#!/usr/bin/env Rscript
# ==================================================================================================
# File: R/plotting/symmetry_combo/03_call_combo.R
# Manuscript: Fig6F | Symmetry risk-autocorrelation combo
# ==================================================================================================
# PURPOSE
# -----------------------------------------------------------------------------
# This script combines the **risk heatmap** and **autocorrelation heatmap** into a final symmetry 
# composite figure for **Fig6F**. The composite plot is created by overlaying the risk and autocorrelation 
# heatmaps into a single visual representation that provides a comprehensive view of the relationship between 
# these two metrics across different environmental conditions and model scenarios.

# ==================================================================================================
# INPUTS
# -----------------------------------------------------------------------------
# 1. **`plot_04_symmetry_k_risk.R`**: Contains the plotting functions for generating the **risk heatmap**. 
#    This heatmap visualizes the risk (SSR) across various conditions.
# 2. **`plot_03_symmetry_k_autocorr_heatmap.R`**: Contains the plotting functions for generating the 
#    **autocorrelation heatmap**. This heatmap shows the autocorrelation values (1 - (LA + LL)) for the same set 
#    of environmental conditions and model scenarios.
# 3. **`patchwork`**: The `patchwork` package is used to combine the two ggplot objects into a single composite 
#    plot with shared axes and title/subtitle.

# ==================================================================================================
# OUTPUTS
# -----------------------------------------------------------------------------
# 1. A **composite plot** that combines the risk and autocorrelation heatmaps into a single figure. 
#    This plot is wrapped in a **patchwork layout** and can be saved or displayed as part of the final figure for 
#    **Fig6F**.
# 2. The final composite plot is consumed by the figure pipeline, where it will be saved or used for further 
#    processing and integration into the manuscript.

# ==================================================================================================
# LOGGING AND CONFIGURATION
# -----------------------------------------------------------------------------
# The script starts by logging the start of the figure generation process and ensuring that all dependencies 
# are properly sourced.
# Set the script identifier for the registry.
FIGURE_SCRIPT <- "R/plotting/symmetry_combo/03_call_combo.R"

# Configure logging on entry.
source("R/core/shared_config.R")  # Load shared configuration settings (logging, settings)
log_figure_start(FIGURE_SCRIPT)   # Log the start of the figure generation process

# ==================================================================================================
# RISK & AUTOCORRELATION COMBO
# -----------------------------------------------------------------------------
# The script loads the required plotting functions for symmetry risk and autocorrelation heatmaps.
# These functions are defined in their corresponding modules and are sourced here to combine them.

# Suppress package startup messages for clarity.
suppressPackageStartupMessages({
  library(patchwork)  # Import patchwork package for combining ggplot objects
})

# Load the necessary helper modules for generating the risk and autocorrelation heatmaps.
source("R/plotting/symmetry_risk/03_plot_symmetry_risk.R")            # Load risk heatmap functions
source("R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R")    # Load autocorrelation heatmap functions

# ==================================================================================================
# PLOT COMBINATION FUNCTION
# -----------------------------------------------------------------------------
# This function combines the risk and autocorrelation heatmaps into a single composite plot.
# The final layout has two rows: the first row displays the risk heatmap, and the second row shows the autocorrelation heatmap.
# The title and subtitle can be customized, and the final plot is arranged with the specified layout.

plot_symmetry_risk_autocorr_combo <- function(
    risk_plot,                      # The risk heatmap (ggplot object)
    autocorr_plot,                  # The autocorrelation heatmap (ggplot object)
    heights = c(1, 1),              # The relative heights for each plot in the combo (default: equal heights)
    title = "Risk (SSR) and Autocorrelation heatmaps",  # Title for the combined plot
    subtitle = NULL,                # Optional subtitle for the combined plot
    collect_guides = TRUE           # Whether to collect guides (legends) for both plots
) {
  # Ensure both inputs are ggplot objects
  if (!inherits(risk_plot, "ggplot") || !inherits(autocorr_plot, "ggplot")) {
    stop("risk_plot and autocorr_plot must be ggplot objects produced by the symmetry helpers.")
  }

  # Combine the two plots into a single patchwork layout:
  # - Place the risk plot and autocorrelation plot vertically (stacked).
  # - The relative heights of the plots can be adjusted using the `heights` parameter.
  # - Guides (legends) can either be collected or kept separately using the `collect_guides` parameter.
  combined <- risk_plot / autocorr_plot + 
    patchwork::plot_layout(
      ncol = 1,  # Arrange plots in a single column
      heights = heights,  # Set relative heights for each plot
      guides = if (collect_guides) "collect" else "keep"  # Collect or keep guides
    ) + 
    patchwork::plot_annotation(
      title = title,               # Set the title for the plot
      subtitle = subtitle,         # Set the subtitle for the plot
      theme = theme_vigilance(base_size = 14)  # Apply the shared theme with the specified font size
    )

  # Adjust plot margins to ensure proper spacing between elements
  combined & theme(plot.margin = margin(t = 6, r = 6, b = 6, l = 6))
}

# ==================================================================================================
# END OF SCRIPT
# -----------------------------------------------------------------------------
# The final combined plot is created by calling the `plot_symmetry_risk_autocorr_combo` function, which stacks 
# the risk and autocorrelation heatmaps vertically and adds a title and subtitle. The result is a ggplot object 
# that can be displayed or saved as part of the figure pipeline for Fig6F.
