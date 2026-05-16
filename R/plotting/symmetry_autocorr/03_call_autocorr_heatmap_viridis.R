#!/usr/bin/env Rscript
# ==================================================================================================
# Script: R/plotting/symmetry_autocorr/03_call_autocorr_heatmap_viridis.R
# Purpose: Provide Viridis-flavored variants of the autocorrelation heatmaps (Fig6B).
# Notes:
#   - Imports the shared autocorrelation module and layers a Viridis fill scale for the colourbar.
# ==================================================================================================

FIGURE_SCRIPT <- "R/plotting/symmetry_autocorr/03_call_autocorr_heatmap_viridis.R"
source("R/core/shared_config.R")
log_figure_start(FIGURE_SCRIPT)

source("R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R")

viridis_autocorr_fill <- function() {
  ggplot2::scale_fill_viridis_c(
    limits = c(0, 1),
    option = "viridis",
    direction = 1,
    name = "hyper-\nvigilance",
    breaks = c(0, 0.02, 0.05, 0.1, 0.25, 0.5, 1),
    labels = scales::label_number(accuracy = 0.02),
    guide = ggplot2::guide_colorbar(
      frame.colour = "black",
      frame.linewidth = 0.5,
      ticks.colour = "black",
      barheight = grid::unit(100, "pt"),
      barwidth = grid::unit(28, "pt"),
      ticks.linewidth = 0.5
    )
  )
}

plot_K_vs_autocorr_heatmap_viridis <- function(..., fill_scale = NULL) {
  scale <- if (is.null(fill_scale)) viridis_autocorr_fill() else fill_scale
  plot_K_vs_autocorr_heatmap(..., fill_scale = scale)
}

plot_K_vs_autocorr_heatmap_by_model_viridis <- function(..., heatmap_fill_scale = NULL) {
  scale <- if (is.null(heatmap_fill_scale)) viridis_autocorr_fill() else heatmap_fill_scale
  plot_K_vs_autocorr_heatmap_by_model(..., heatmap_fill_scale = scale)
}
