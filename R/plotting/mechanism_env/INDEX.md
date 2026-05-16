# Mechanism Environment Module

## Purpose
- Build the Fig4 panels that overlay mechanism-specific contributions (preventative vs spillover) on top of the LA/LL heatmap.
- Provide helpers for the bivariate HV tiles, split mechanism heatmaps, stacked bars, and the threshold/risk panels that share the env grid layout.

## Inputs / Outputs
- **Inputs:** mechanism metadata from `R/plotting/mechanism_env/00_data_prep_mechanism_data.R`, the canonical env grids from `R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R`, and shared utilities (`R/helpers/utils_model_scenarios.R`, `R/plotting/_shared/*`).
- **Outputs:** ready-to-save plots such as `plot_env_bivariate_hv_heatmap()`, `plot_env_split_mechanism_heatmaps()`, `plot_env_stacked_hv_bars()`, plus the risk/autocorr combos like `plot_K_vs_SSR_mechanism_by_model()` and `plot_K_vs_autocorr_mechanism_by_model()`.

## Key functions
- `plot_env_bivariate_hv_heatmap()` renders grayscale HV tiles with translucent mechanism colours and optional threshold overlays.
- `plot_env_split_mechanism_heatmaps()` draws separate preventative/spillover panels while reusing the env heatmap layout.
- `plot_env_stacked_hv_bars()` compares mechanism shares across the canonical scenarios.
- `plot_K_vs_SSR_mechanism_by_model()` and `plot_K_vs_autocorr_mechanism_by_model()` reuse mechanism summaries for the symmetry panels.

## Dependency chain (00 -> 01 -> 02 -> 03)
1. **00 shared helpers:** `R/plotting/_shared/*` plus `R/core/plot_utils.R` handle themes, subtitles, and palettes.
2. **01 data prep:** `R/plotting/mechanism_env/00_data_prep_mechanism_data.R` normalises mechanism rates and adds colour cues.
3. **02 plot setup:** `R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R` supplies the standard layout scaffold (`plot_env_heatmap_faceted_by_K()`).
4. **03 plot constructors:** `R/plotting/mechanism_env/03_plot_mechanism_env.R`, `03_plot_mechanism_risk_autocorr.R`, and `03_plot_mechanism_thresholds.R` glue the mechanism data to the env grid layout.
