#!/usr/bin/env Rscript
# =============================================================================
# File: R/plotting/env_heatmaps/03_call_env_heatmaps_by_model.R
# =============================================================================
# ROLE / INTENT
# -----------------------------------------------------------------------------
# This is a *thin entry-point script* whose only job is to load (source) the
# helper modules needed to build environment heatmaps for Fig2A.
#
# Key idea:
#   - The heavy lifting (data prep + plotting functions) lives in reusable
#     modules under `R/plotting/env_heatmaps/` and `R/plotting/`.
#   - The actual “run everything, save figures, manage paths” logic is handled by
#     a pipeline script (e.g., scripts/pipelines/09_run_all_plots.R).
#
# Why keep this file so small?
#   1) Reproducibility: the figure pipelines can reliably source this file to
#      guarantee the same helper code is loaded.
#   2) Separation of concerns: no data generation, no figure saving here—only
#      imports + logging.
#   3) Faster iteration: changes to plot aesthetics can be made in the plotting
#      module without touching pipeline logic.
# =============================================================================

#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Shebang line:
#   - Allows this file to be executed directly from a shell on systems where
#     `Rscript` is available and executable.
#   - Example: `./R/plotting/env_heatmaps/03_call_env_heatmaps_by_model.R`
#
# Note: this line is ignored if the script is sourced from within R.
# -----------------------------------------------------------------------------

# =============================================================================
# Metadata and shared configuration
# =============================================================================

# A string identifier used for:
#   - logging (so you know which figure script started)
#   - sometimes naming output subfolders or embedding provenance in filenames
FIGURE_SCRIPT <- "R/plotting/env_heatmaps/03_call_env_heatmaps_by_model.R"

# Load project-wide plotting + reproducibility defaults.
# Typically `shared_config.R` defines things like:
#   - theme_vigilance() and any other theme/palette helpers
#   - scale functions (e.g., scale_action_fill())
#   - directory helpers / figure saving defaults
#   - logging utilities such as log_figure_start()
#
# Keeping this as the first source call ensures:
#   - consistent theme and palette behavior across every figure script
#   - consistent logging format across the whole pipeline
source("R/core/shared_config.R")

# Emit a standard log marker indicating this script has begun.
# In larger pipelines, this helps trace where errors occur and provides a
# timestamped record of what ran.
log_figure_start(FIGURE_SCRIPT)

# =============================================================================
# Load required modules for environment heatmaps
# =============================================================================

# Data-prep module for environment heatmaps.
# This module is expected to provide functions that:
#   - construct the LA × LL grid (and possibly model variants × K grids)
#   - attach hypervigilance outcomes and any derived metrics
#   - attach metadata attributes used for subtitles/captions
#
# Importantly, it should NOT do plotting (that belongs in R/plotting).
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")

# Plotting module for environment heatmaps.
# This module is expected to provide functions that:
#   - take the tidy environment grid data and produce ggplot objects
#   - enforce consistent axis breaks, themes, facet layout, legends, etc.
#   - optionally add thresholds, SSP/predictability boundaries, region labels
#
# This separation lets you:
#   - reuse the same plotting code across multiple figures/panels
#   - test plotting on cached/precomputed data without rerunning simulations
source("R/plotting/env_heatmaps/03_plot_env_heatmaps.R")

# =============================================================================
# End of script
# -----------------------------------------------------------------------------
# This file intentionally does not:
#   - compute policies
#   - run simulations
#   - generate data tables
#   - save figures to disk
#
# Those steps are performed by the pipeline scripts that source this file.
# =============================================================================
