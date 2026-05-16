# Hypervigilance Thesis Repository

MSc Biological Sciences: Evolution of Behaviour and Mind
University of Amsterdam

**Thesis title:** *Hypervigilance as an Adaptive Response: A State-Dependent Dynamic Optimisation Model*

**Repository:** https://github.com/MurMur-d/hypervigilance-master-thesis

This repository contains the R code used for an MSc thesis project on hypervigilance as an adaptive response in dynamic environments. It includes stochastic dynamic programming models, forward simulations, figure and table pipelines, selected final outputs, validation tests, and an interactive Shiny application for exploring model behaviour.

In this project, hypervigilance is operationalised as vigilance–environment mismatch: the optimal policy selects vigilance, but no stressor occurs at the subsequent time step. Although this may appear excessive in hindsight, it can still emerge as an adaptive strategy when vigilance protects against larger damage if a stressor occurs.

The repository is intended as a reproducible research compendium accompanying the thesis.

---

## Theoretical Background

The models draw on:

* stochastic dynamic programming
* state-dependent optimisation
* behavioural ecology
* ecological uncertainty
* temporal autocorrelation
* defensive behaviour and threat monitoring
* optimality theory

---

## Quick Start

From the repository root:

```r
# Restore package versions
renv::restore()

# Run the Shiny app
shiny::runApp("app")

# Reproduce all thesis figures and tables
source("scripts/reproduce/00_reproduce_everything.R")

# Reproduce figures only
source("scripts/run_thesis_figures.R")
```

The committed `renv.lock` file records the package versions used in the local R 4.4.1 environment from which this public repository was generated.

---

## Repository Structure

```text
hypervigilance-thesis/
├── README.md
├── LICENSE
├── .gitignore
├── R/
│   ├── core/        # Setup, shared defaults, caching, plotting utilities
│   ├── models/      # Dynamic programming and simulation models
│   ├── plotting/    # Figure-specific data preparation and plotting modules
│   ├── tables/      # Table-generation code
│   └── helpers/     # Shared validation, labeling, and scenario helpers
├── app/             # Interactive Shiny application
├── scripts/         # Reproduction and plotting entry points
├── docs/            # Methodology and reproduction notes
├── outputs/         # Selected final figures and tables
└── tests/           # Smoke and configuration tests
```

---

## Models

### Basic Model

A finite-horizon dynamic programming model with a binary environment and two actions: vigilant or relaxed. The model isolates ecological effects by examining how environmental transition probabilities and vigilance costs shape optimal vigilance allocation.

### Health Model

An extension of the Basic Model that adds an integer-valued health state and death as an absorbing state. Vigilance and stressor exposure deplete health over time.

### Terminal Reward Variants

Health-model variants that assign value to preserved health at the end of the planning horizon, allowing the model to examine how future survival valuation reshapes vigilance allocation.

The core model implementations are located in `R/models/`. Shared assumptions, defaults, figure metadata, and output directories are defined in `R/core/shared_config.R` and `R/core/setup_project.R`.

---

## Main Entry Points

| Purpose                    | File                                          |
| -------------------------- | --------------------------------------------- |
| Interactive application    | `app/app.R`                                   |
| Full reproduction pipeline | `scripts/reproduce/00_reproduce_everything.R` |
| Figure-only reproduction   | `scripts/run_thesis_figures.R`                |
| Plot orchestration         | `scripts/pipelines/09_run_all_plots.R`        |
| Tests                      | `tests/testthat.R` and `tests/smoke_prep.R`   |

---

## Outputs

Selected final figures are available in `outputs/figures/` using short reviewer-friendly filenames. Selected thesis tables are available in `outputs/tables/`.

Large caches and intermediate result grids are intentionally excluded from the repository and are regenerated automatically when the reproduction scripts are run.

---

## Notes

This repository is a research compendium rather than a formal R package. The folder structure is optimized for transparency, thesis review, and reproducibility.

Simulations and fine environmental grids can be computationally intensive. Cached files are generated automatically during reproduction.

---

## Citation

If you use this repository or build upon this work, please cite:

```text
Hamelink, M. (2026). Hypervigilance as an Adaptive Response:
A State-Dependent Dynamic Optimisation Model.
MSc thesis, Biological Sciences: Evolution of Behaviour and Mind,
University of Amsterdam.
```

---

## Support

Questions, issues, or reproducibility concerns can be sent to:

`mare.hamelink@student.uva.nl`

---

## Acknowledgements

This repository was developed as part of an MSc thesis in Biological Sciences: Evolution of Behaviour and Mind at the University of Amsterdam.

I thank Dr. Willem E. Frankenhuis for his supervision, guidance, enthusiasm, and intellectual support throughout the project. I also thank Dr. Nicole Walasek for her valuable feedback, encouragement, and assistance with the computational modelling, visualisation, and interpretation of the results.

Finally, I thank my peers, friends, family, and the Evolution of Behaviour and Mind track for their support and for providing the intellectual environment in which this project developed.

