# ===================================================================================================
# File: prep_tables_apa_max_hv_env_grit.R
#
# Purpose:
#   Build the APA-style “maximum hypervigilance grid” table (Model × Cost) and export it to Word.
#
# Conceptually, this table answers:
#   “For each model variant and each vigilance cost level, what is the *maximum* hypervigilance
#    observed anywhere in the full λA–λL environment grid, and what mechanism dominates there?”
#
# Inputs:
#   - Multi-model LA/LL grid outputs produced by `run_hypervigilance_pipeline_by_model()`
#     (which itself computes and stacks hypervigilance grids across model scenarios + K columns).
#
# Outputs:
#   - A Word document at `tables/Max_HV_Grid_Table.docx` containing a single APA-styled table.
#
# Notes / design choices:
#   - This script is intentionally “inspectable”: each transformation step is a helper function.
#   - “prep_*” naming: this file both *prepares* the table and performs the final export.
#   - We only keep K in {1,5,9} to match the paper’s Low/Moderate/High cost reporting convention.
#   - Mechanistic interpretation is produced by `summarize_vigilance_mechanisms()` so wording stays
#     consistent with other tables/appendix exports.
# ===================================================================================================

suppressPackageStartupMessages({
  library(dplyr)      # mutate/filter/select/arrange pipelines
  library(rempsyc)    # used by nice_table() in some setups (APA-ish table defaults)
  library(flextable)  # table rendering + styling
  library(officer)    # Word document creation/writing
})

# ---- Project sources ----------------------------------------------------------------------------
# plot_utils.R:
#   - provides default_env_model_scenarios, validate_default(), and the hv model metadata helpers
#   - also gives us caching/memoization helpers used throughout the pipeline helpers
source("R/core/plot_utils.R")

# env heatmap helpers:
#   - `run_hypervigilance_pipeline_by_model()` lives in the shared plotting setup for the environment heatmaps
#   - `summarize_vigilance_mechanisms()` is defined in the data prep pipeline and standardises the max-hv summaries
source("R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R")
source("R/plotting/env_heatmaps/01_data_prep_environment_heatmap_pipeline.R")

# utils_table_helpers.R:
#   - provides: nice_table(), blank_model_repeats(), hv_model_name_map / hv_model_levels, etc.
source("R/helpers/utils_table_helpers.R")


# ================================================================================================
# build_max_hv_table()
# ================================================================================================
# This is the “data prep” core:
#   1) Run the (expensive) multi-model grid pipeline once.
#   2) Collapse that full grid into the *maximum* hv per (model × K).
#   3) Attach a mechanistic interpretation for that cost/model combination.
#   4) Map internal model labels to the paper-ready display labels.
#   5) Restrict to the reporting costs {1,5,9} and format values for APA export.
#
# Output:
#   A tidy data.frame with columns:
#     - Model
#     - Vigilance cost
#     - Max HV
#     - Mechanistic interpretation
# ================================================================================================

#' Build the tidy table summarising max hypervigilance by model and vigilance cost.
#'
#' @return data.frame with columns Model, Vigilance cost, Max HV, Mechanistic interpretation
build_max_hv_table <- function() {

  # ---- Step 1: run the full LA/LL grid pipeline across model variants ---------------------------
  # run_hypervigilance_pipeline_by_model() is the “big” helper that:
  #   - builds an LA/LL grid (typically LA,LL ∈ [0,0.5] with step = grid_step)
  #   - sweeps vigilance cost K across RatioDK columns (ratio_step)
  #   - stacks results across multiple model scenarios (basic + health variants)
  #   - returns both the stacked data (df_env_models) and the plot object (p_faceted)
  #
  # Here, we only use pipeline$df_env_models for the table.
  pipeline <- run_hypervigilance_pipeline_by_model(
    model_scenarios = default_env_model_scenarios,  # canonical model variants used in env figures
    D = validate_default("D"),                      # vigilant damage magnitude
    C = validate_default("c"),                      # baseline relaxed cost
    d = validate_default("d"),                      # relaxed damage magnitude
    T_steps = validate_default("T"),                # horizon length
    states = c("K", "Kd", "C", "CD"),               # canonical DP branches used in policy tables
    N_agents = validate_default("N"),               # agent count per grid cell
    grid_step = 0.1,                                # LA/LL resolution for the env grid
    ratio_step = 0.2,                               # RatioDK resolution, translated into K columns

    # Base args for *all* health scenarios:
    # These are merged with per-scenario policy/sim settings from default_env_model_scenarios.
    base_policy_args = list(
      h0 = validate_default("h0"),                  # health ceiling for health model DP
      health_step = 1                               # discretisation step for health transitions
    ),
    base_sim_args = list(
      h0 = validate_default("h0"),                  # match simulation ceiling to policy ceiling
      spread_initial_over_levels = FALSE,           # keep initial health distribution fixed (typical default)
      shuffle = TRUE                                # randomise agent ordering (but seed-controlled upstream)
    ),

    # Choose which hv column is considered the primary “hypervigilance” measure for the grid.
    # HypervigilanceRate_all usually means mean(hv) across *all* agent-time rows.
    rate_column = "HypervigilanceRate_all"
  )

  # ---- Step 2: collapse full grid → one row per (Model × K) -------------------------------------
  # summarize_vigilance_mechanisms() is expected to:
  #   - group by model and K
  #   - compute the max hv within the LA/LL grid for that group
  #   - compute prevention/spill totals and format them into a textual mechanism label
  #
  # This keeps the “mechanism labeling logic” consistent across all your tables.
  summarize_vigilance_mechanisms(pipeline$df_env_models) %>%
    # ---- Step 3: harmonise model labels to paper display names ----------------------------------
    mutate(
      # hv_model_name_map usually maps internal model labels (e.g., "Health (ω=0)")
      # to the exact naming convention used in the paper/appendix.
      Model = recode(Model, !!!hv_model_name_map),

      # The summariser returns `vigilance cost` (lowercase) to match earlier table conventions.
      # Here we rename it to title-case for the final APA table column.
      `Vigilance cost` = `vigilance cost`
    ) %>%
    # ---- Step 4: restrict to the canonical reporting K levels -----------------------------------
    # The appendix/paper cost tiers are typically {Low, Moderate, High} = {1, 5, 9}.
    # Keeping only those costs ensures the Word table matches the narrative.
    filter(`Vigilance cost` %in% c(1, 5, 9)) %>%
    # ---- Step 5: select/format the final columns ------------------------------------------------
    transmute(
      Model,
      `Vigilance cost`,
      `Max HV` = round(`max hv`, 2),                       # round to match APA-style numeric presentation
      `Mechanistic interpretation` = `mechanistic interpretation`
    ) %>%
    # ---- Step 6: enforce deterministic ordering -------------------------------------------------
    mutate(
      # hv_model_levels defines the intended row order across *all* APA tables.
      Model = factor(Model, levels = hv_model_levels),

      # Cost order is numeric but converted to factor so Word export preserves ordering.
      `Vigilance cost` = factor(`Vigilance cost`, levels = sort(unique(`Vigilance cost`)))
    ) %>%
    arrange(Model, `Vigilance cost`) %>%
    # ---- Step 7: cosmetic tweaks for APA-style grouped rows -------------------------------------
    mutate(Model = as.character(Model)) %>%
    blank_model_repeats()
    # blank_model_repeats() typically replaces repeated Model names with "" so the Word table
    # visually groups costs under each model without redundant repetition.
}


# ================================================================================================
# build_max_hv_flextable()
# ================================================================================================
# Converts the tidy table into a styled flextable suitable for Word.
#
# Styling choices:
#   - nice_table() provides standard APA-ish header/title/note formatting used elsewhere
#   - column alignments: Model left; costs/numbers centered; interpretation left
#   - numeric format for Max HV to 2 decimal places (again for APA consistency)
# ================================================================================================

#' Convert that tidy frame into a styled flextable for Word output.
#'
#' @param table_data tidy table produced by `build_max_hv_table()`
#' @return flextable object with percentages and layout aligned to APA-style expectations
build_max_hv_flextable <- function(table_data) {

  # ---- Step 1: create a baseline APA-styled flextable ------------------------------------------
  # nice_table() is your project’s wrapper around flextable styling and title/note conventions.
  ft <- nice_table(
    table_data,
    title = c(
      "Max HV summary",
      "Maximum hypervigilance across the full λA-λL grid by model and vigilance cost"
    ),
    note = "HV = hypervigilance; Mechanistic interpretation describes the dominant preventative vs spillover mix."
  )

  # ---- Step 2: numeric formatting ---------------------------------------------------------------
  # Ensure Max HV is printed consistently even if upstream rounding changes slightly.
  ft <- flextable::colformat_num(ft, j = "Max HV", digits = 2, na_str = "NA")

  # ---- Step 3: alignment conventions ------------------------------------------------------------
  ft <- flextable::align(ft, j = "Model", align = "left", part = "all")
  ft <- flextable::align(ft, j = "Vigilance cost", align = "center", part = "all")
  ft <- flextable::align(ft, j = "Max HV", align = "center", part = "all")
  ft <- flextable::align(ft, j = "Mechanistic interpretation", align = "left", part = "all")

  # Return the flextable object so the caller can:
  #   - preview it in RStudio
  #   - or write it to Word via save_max_hv_table()
  ft
}


# ================================================================================================
# save_max_hv_table()
# ================================================================================================
# Writes the flextable into a Word document at a fixed path.
#
# Implementation details:
#   - Creates `tables/` if missing (safe to call repeatedly).
#   - Uses officer::read_docx() to create a blank document.
#   - Adds the flextable into the body.
#   - Writes to `tables/Max_HV_Grid_Table.docx`.
# ================================================================================================

#' Write the Word document to `tables/Max_HV_Grid_Table.docx`.
#'
#' @param ft flextable object created by `build_max_hv_flextable()`
save_max_hv_table <- function(ft) {
  # Ensure output directory exists. recursive=TRUE avoids failure if parent folders are missing.
  dir.create("tables", showWarnings = FALSE, recursive = TRUE)

  # Create a new Word document object.
  doc <- read_docx()

  # Insert the table into the Word body.
  doc <- doc %>% body_add_flextable(ft)

  # Write the .docx to disk.
  print(doc, target = file.path("tables", "Max_HV_Grid_Table.docx"))
}


# ================================================================================================
# build_and_save_max_hv_table()
# ================================================================================================
# “One-shot” entry point:
#   - builds the data
#   - formats as flextable
#   - saves to Word
#
# This is what runs when the script is sourced/executed.
# ================================================================================================

#' Full pipeline entry point.
build_and_save_max_hv_table <- function() {
  table_data <- build_max_hv_table()
  ft <- build_max_hv_flextable(table_data)
  save_max_hv_table(ft)
}

# Execute immediately when sourced (common pattern in your export scripts).
build_and_save_max_hv_table()
