# Pipeline Manifest

Canonical reviewer entry points:

- `scripts/reproduce/00_reproduce_everything.R`: full thesis reproduction, including figures and tables.
- `scripts/run_thesis_figures.R`: figure-only reproduction wrapper.
- `scripts/pipelines/09_run_all_plots.R`: main plotting orchestration used by both entry points.

Supporting scripts retained because they are sourced by the main plotting orchestration:

- `scripts/pipelines/09_compare_model_visuals.R`
- `scripts/pipelines/09_env_heatmap_plots.R`

Exploratory scripts, debug scripts, old app versions, and archive files from the working repository are intentionally excluded from this public repository.
