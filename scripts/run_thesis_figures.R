#!/usr/bin/env Rscript
# Reviewer-facing entry point for regenerating thesis figures.

if (!exists("HV_SETUP_DONE", envir = .GlobalEnv)) {
  source("R/core/setup_project.R")
}
assign("HV_SETUP_DONE", TRUE, envir = .GlobalEnv)

source("scripts/pipelines/09_run_all_plots.R")
