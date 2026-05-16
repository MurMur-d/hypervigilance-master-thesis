# Reproducing Results

## Requirements

Use R 4.3.1 or later. The thesis work was developed with R 4.4.0.

Core packages used by the app and analysis include:

```r
install.packages(c(
  "shiny", "shinycssloaders", "shinyjs", "shinyBS", "bslib", "plotly",
  "dplyr", "ggplot2", "tidyr", "purrr", "tibble", "future",
  "future.apply", "memoise", "forcats", "patchwork", "here", "fs",
  "cowplot", "scales", "ggalluvial", "ggforce", "ggnewscale",
  "testthat", "flextable", "officer", "rempsyc"
))
```

## Full Reproduction

Run:

```r
source("scripts/reproduce/00_reproduce_everything.R")
```

This loads project setup, runs the main plotting pipeline, and exports thesis tables.

## Figures Only

Run:

```r
source("scripts/run_thesis_figures.R")
```

## Tests

Run:

```r
testthat::test_dir("tests/testthat")
source("tests/smoke_prep.R")
```

## Caches

The public repository excludes `cache/` and `data/cache/`. These directories are recreated by `R/core/setup_project.R` and populated by model and plotting scripts as needed.

The first full reproduction run can therefore be slower than later runs.

## renv

The public repository includes `renv.lock`. Restore the recorded package versions with:

```r
install.packages("renv")
renv::restore()
```

If dependencies change later, update the lockfile with `renv::snapshot()`. Commit `renv.lock`, but do not commit `renv/library/`.
