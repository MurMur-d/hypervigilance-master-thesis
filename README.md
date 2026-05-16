# Hypervigilance Thesis Repository

MSc Biological Sciences: Evolution of Behaviour and Mind.

**Thesis title:** Hypervigilance as an Adaptive Response: A State-Dependent Dynamic Optimisation Model.

This repository contains the reviewer-facing R code for a thesis project on hypervigilance as an adaptive response in dynamic environments. It includes dynamic programming models, forward simulation helpers, figure and table pipelines, selected final outputs, tests, and an interactive Shiny app.

Hypervigilance is treated as vigilance-environment mismatch: the optimal policy chooses vigilance, but no stressor occurs at the next step. This can be costly in the short term while still being adaptive when vigilance protects against larger damage if a stressor appears.

## Quick Start

From the repository root:

```r
# Restore package versions after renv.lock has been generated
renv::restore()

# Run the Shiny app
shiny::runApp("app")

# Regenerate thesis figures and tables
source("scripts/reproduce/00_reproduce_everything.R")

# Regenerate figures only
source("scripts/run_thesis_figures.R")
```

The committed `renv.lock` records the package versions available in the local R 4.4.1 environment used to prepare this public export.

## Repository Structure

```text
hypervigilance-thesis/
├── README.md
├── LICENSE
├── .gitignore
├── R/
│   ├── core/        # Setup, shared defaults, caching, plotting utilities
│   ├── models/      # Basic and health dynamic programming/simulation models
│   ├── plotting/    # Figure-specific data prep and plotting modules
│   ├── tables/      # Table-generation code
│   └── helpers/     # Shared validation, labeling, and scenario helpers
├── app/             # Canonical Shiny app
├── scripts/         # Reproduction and plotting entry points
├── docs/            # Methodology and reproduction notes
├── outputs/         # Selected final figures and tables
└── tests/           # Smoke and configuration tests
```

## Models

- **Basic Model:** finite-horizon dynamic programming model with a binary environment and two actions: high vigilance or low vigilance.
- **Health Model:** extends the Basic Model with an integer health state and death as an absorbing state.
- **Terminal Reward variants:** health-model variants that add value to preserved final health.

The model code is in `R/models/`. Shared assumptions, defaults, figure registry metadata, and output directories are defined in `R/core/shared_config.R` and `R/core/setup_project.R`.

## Main Entry Points

- Interactive app: `app/app.R`
- Full reproduction: `scripts/reproduce/00_reproduce_everything.R`
- Figure-only reproduction: `scripts/run_thesis_figures.R`
- Main plotting orchestration: `scripts/pipelines/09_run_all_plots.R`
- Tests: `tests/testthat.R` and `tests/smoke_prep.R`

## Outputs

Selected final figures are in `outputs/figures/` with short reviewer-friendly filenames. Selected thesis tables are in `outputs/tables/`.

Large caches and intermediate result grids are intentionally excluded. They are regenerated when the reproduction scripts are run.

## Notes

This repository is a research compendium, not a formal R package. The folder layout is optimized for clarity, thesis review, and result reproduction.

Questions or issues can be sent to `mare.hamelink@student.uva.nl`.
