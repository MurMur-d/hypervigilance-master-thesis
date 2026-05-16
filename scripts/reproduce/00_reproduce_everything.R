#!/usr/bin/env Rscript
# =============================================================================
# Script: scripts/reproduce/00_reproduce_everything.R
# Purpose: Run project setup, the master plot pipeline, and the table exports while logging progress.
# =============================================================================

suppressPackageStartupMessages({
  library(fs)
})

if (!exists("HV_SETUP_DONE", envir = .GlobalEnv)) {
  source("R/core/setup_project.R")
}
assign("HV_SETUP_DONE", TRUE, envir = .GlobalEnv)

message("=== START FULL REPRODUCTION ===")

dirs_to_create <- c(
  DIR_DATA_RAW,
  DIR_DATA_PROCESSED,
  DIR_DATA_CACHE,
  DIR_FIGURES,
  DIR_TABLES
)
fs::dir_create(dirs_to_create)

message("-> running charts pipeline")
source("scripts/pipelines/09_run_all_plots.R")

message("-> exporting tables")
table_scripts <- c(
  "R/tables/threshold_analysis/00_data_prep_tables_apa_max_hv_env_grit.R",
  "R/tables/threshold_analysis/01_data_prep_tables_apa_thresholds.R",
  "R/tables/_shared/00_data_prep_export_tables_apa_word.R"
)
for (script_path in table_scripts) {
  message("   sourcing ", script_path)
  source(script_path)
}

message("=== FULL REPRODUCTION COMPLETE ===")
