# ENV Heatmaps Module

## Purpose
- Provide the canonical LA/LL x K hypervigilance heatmaps used across the manuscript (same axes, legend, facet layout).
- Surface a consistent interface for building data grids, tile plots, and the HV matrix table so downstream figures stay aligned.

## Inputs / Outputs
- **Inputs:** the canonical environment grid from `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R`, the scenario metadata from `default_env_model_scenarios()`, and shared theming helpers (`R/core/plot_utils.R` plus `R/plotting/_shared/*`).
- **Outputs:** ready-to-save ggplot objects (`plot_env_heatmaps()`, `plot_env_heatmaps_with_region_key()`, `hv_matrix_over_env()`), plus the `run_hypervigilance_pipeline()` wrappers used by downstream tables and panels.

## Key functions
- `build_env_heatmaps_for_models()` builds the facetable LA/LL grid with model scenario metadata attached.
- `plot_env_heatmaps()` / `plot_env_heatmaps_with_region_key()` apply the standard layout, palette, and subtitles to deliver consistent heatmaps.
- `hv_matrix_over_env()` consolidates HV rates across environments and K for the canonical HV table export.
- `run_hypervigilance_pipeline_by_model()` wraps the grid builders so multiple scenarios can be rendered with identical layouts.

## Dependency chain (00 -> 01 -> 02 -> 03)
1. **00 shared helpers:** `R/plotting/_shared/*` plus `R/core/plot_utils.R` supply themes, subtitles, palettes, and helper layouts.
2. **01 data prep:** `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R` builds the base LA/LL x K grid and attaches hypervigilance outcomes.
3. **02 plot setup:** `R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R` sources the grid builders, standardises the axes, and defines the pipeline wrappers.
4. **03 plot constructors:** `R/plotting/env_heatmaps/03_plot_env_heatmaps.R` and `R/plotting/env_heatmaps/03_plot_env_hv_matrix.R` render the final plots and build the HV matrix table.
