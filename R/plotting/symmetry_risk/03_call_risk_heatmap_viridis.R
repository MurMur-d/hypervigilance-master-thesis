#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/symmetry_risk/03_call_risk_heatmap_viridis.R
# ==================================================================================================
# PURPOSE
# -----------------------------------------------------------------------------
# This script generates the **Viridis-flavored** version of the symmetry risk heatmap for **Fig6E**. 
# The primary function of this script is to apply the Viridis color scale to the existing symmetry risk heatmap 
# plots to provide a consistent, visually appealing gradient. It sources the shared risk plotting module and 
# only overrides the color scale to use the Viridis palette.

# ==================================================================================================
# INPUTS
# -----------------------------------------------------------------------------
# 1. **`R/plotting/symmetry_risk/03_plot_symmetry_risk.R`**: Contains the plotting functions for generating symmetry risk heatmaps 
#    (SSR heatmaps) for model scenarios and environmental conditions.
# 2. **Viridis Color Palette**: The **Viridis** color scale is applied to the heatmaps to enhance the visual 
#    presentation, providing a perceptually uniform and colorblind-friendly gradient for visualizing hypervigilance.

# ==================================================================================================
# OUTPUTS
# -----------------------------------------------------------------------------
# 1. **Viridis-style risk heatmaps**: The final output consists of **Viridis-style** heatmaps where the 
#    hypervigilance values (risk) are color-coded using the Viridis palette. These plots can be consumed by 
#    downstream scripts or saved as part of the final figure for **Fig6E**.
# 2. **The modified heatmap functions**: The `plot_K_vs_SSR_heatmap_viridis` and 
#    `plot_K_vs_SSR_heatmap_by_model_viridis` functions are used to generate the risk heatmaps with the 
#    Viridis color scale.

# ==================================================================================================
# LOGGING AND CONFIGURATION
# -----------------------------------------------------------------------------
# The script starts by logging the figure generation process and sourcing the necessary configuration 
# and plotting helpers.
FIGURE_SCRIPT <- "R/plotting/symmetry_risk/03_call_risk_heatmap_viridis.R"
source("R/core/shared_config.R")  # Load shared configuration settings (logging, settings)
log_figure_start(FIGURE_SCRIPT)   # Log the start of the figure generation process

# ==================================================================================================
# RISK HEATMAP FUNCTIONS
# -----------------------------------------------------------------------------
# The main change in this script is the application of the **Viridis** color scale. 
# The script defines a custom fill scale that overrides the default color scale for the heatmaps.

source("R/plotting/symmetry_risk/03_plot_symmetry_risk.R")  # Load the shared risk plotting module

# ==================================================================================================
# CUSTOM FILL SCALE: Viridis
# -----------------------------------------------------------------------------
# The following function defines the **Viridis** color scale, which will be used in the heatmaps.
# The scale maps hypervigilance values (risk) to the Viridis palette, ensuring that the color range is 
# perceptually uniform, meaning that the color gradient is consistent across the scale, making it easy to interpret.
# This color scale is particularly useful for accessibility, being colorblind-friendly.

viridis_risk_fill <- function() {
  ggplot2::scale_fill_viridis_c(
    limits = c(0, 1),                 # The limits for the scale (0 to 1, representing minimum to maximum hypervigilance)
    option = "viridis",               # Use the "viridis" color palette
    direction = 1,                    # Direction of the palette (1 means left-to-right, 0 would reverse it)
    name = "hyper-vigilance",         # Name of the color scale for the legend
    labels = scales::label_number(accuracy = 0.02),  # Formatting for the scale labels (rounded to 2 decimal places)
    guide = ggplot2::guide_colorbar(  # Custom colorbar guide with styling
      frame.colour = "black",         # Border color of the colorbar
      frame.linewidth = 0.5,          # Line width of the border
      ticks.colour = "black",         # Color of ticks on the colorbar
      barheight = grid::unit(100, "pt"),   # Height of the colorbar
      barwidth = grid::unit(28, "pt"),    # Width of the colorbar
      ticks.linewidth = 0.5           # Width of the ticks
    )
  )
}

# ==================================================================================================
# WRAPPERS FOR VIRIDIS COLOR SCALE HEATMAPS
# -----------------------------------------------------------------------------
# These functions act as wrappers around the original symmetry risk heatmap functions, but apply the Viridis
# color scale to the heatmaps. They ensure that the heatmaps are visualized with the Viridis color gradient 
# for the SSR (risk) values.

# Wrapper for the single heatmap with Viridis scale
plot_K_vs_SSR_heatmap_viridis <- function(..., fill_scale = NULL) {
  scale <- if (is.null(fill_scale)) viridis_risk_fill() else fill_scale  # Apply Viridis if no custom scale is provided
  plot_K_vs_SSR_heatmap(..., fill_scale = scale)  # Call the original function with the Viridis scale
}

# Wrapper for the faceted heatmap by model with Viridis scale
plot_K_vs_SSR_heatmap_by_model_viridis <- function(..., heatmap_fill_scale = NULL) {
  scale <- if (is.null(heatmap_fill_scale)) viridis_risk_fill() else heatmap_fill_scale  # Apply Viridis if no custom scale is provided
  plot_K_vs_SSR_heatmap_by_model(..., heatmap_fill_scale = scale)  # Call the original function with the Viridis scale
}

# ==================================================================================================
# END OF SCRIPT
# -----------------------------------------------------------------------------
# This script defines the necessary functions to apply the **Viridis** color scale to symmetry risk heatmaps.
# The heatmaps are generated by calling the wrapper functions `plot_K_vs_SSR_heatmap_viridis` and 
# `plot_K_vs_SSR_heatmap_by_model_viridis`. These functions replace the default color scale with the 
# Viridis palette, ensuring that the visualizations follow a consistent and accessible color scheme.
