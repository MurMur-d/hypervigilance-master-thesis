# ===================================================================================================
# File: export_tables_apa_word.R
# Purpose:
#   Export the six APA-style hypervigilance appendix tables (A1–A6) into ONE Word
#   document. Each table corresponds to a single environment class (L-P, L-U, … H-U).
#
# Inputs:
#   - generate_hypervigilance_tables() output (built upstream in prep_hypervigilance_ranges.R)
#     Expected structure (by convention in this project):
#       hv_tables$by_env[[env_key]] is a data frame with rows by Model × Cost,
#       containing Safe HV rate and Mechanistic interpretation (and possibly other fields).
#
# Outputs:
#   - tables/Hypervigilance_Tables_A1_to_A6.docx
#
# Design notes:
#   - The workflow is split into small helpers so each transformation step is explicit:
#       (1) build_apa_env_table(): clean and format data for one env
#       (2) build_environment_flextables(): apply APA styling and column widths
#       (3) write_apa_word(): append all tables into one .docx
#       (4) export_hypervigilance_tables(): orchestrate the whole pipeline
#   - This makes it easy to unit-test each step (data ↔ formatting ↔ export).
# ===================================================================================================


# ---------------------------------------------------------------------------------------------------
# Load packages quietly (avoid noisy logs when running pipelines)
# ---------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)      # data wrangling (mutate/select/arrange)
  library(rempsyc)    # often used for APA table helpers; may be used inside nice_table()
  library(flextable)  # table objects + formatting that officer can write to Word
  library(officer)    # Word document creation and manipulation (read_docx, body_add_*, print)
})

# ---------------------------------------------------------------------------------------------------
# Load project-specific helpers + the table generator
# ---------------------------------------------------------------------------------------------------
# prep_hypervigilance_ranges.R is assumed to define:
#   - generate_hypervigilance_tables()
#   - hv_model_name_map, hv_model_levels (or they may come from utils_table_helpers)
#
# utils_table_helpers.R is assumed to define helpers like:
#   - nice_table(): returns a flextable with consistent APA-ish formatting + title/note
#   - format_hv_percent(): formats a proportion/rate into a percent string with digits
#   - blank_model_repeats(): replaces repeated model labels with blanks for readability
source("R/tables/_shared/00_data_prep_hypervigilance_ranges.R")
source("R/helpers/utils_table_helpers.R")


# ---------------------------------------------------------------------------------------------------
# Environment metadata: labels, titles, and ordering
# ---------------------------------------------------------------------------------------------------
# We hard-code the table numbers (A1–A6) and the descriptive subtitles.
# This ensures:
#   - stable appendix numbering
#   - tables always appear in the intended order
#
# Keys (e.g., "L-P") are used consistently throughout:
#   - as list names in hv_tables$by_env
#   - as ordering keys for export
#   - as lookup keys for titles
title_main <- c(
  "L-P" = "Table A1",
  "L-U" = "Table A2",
  "M-P" = "Table A3",
  "M-U" = "Table A4",
  "H-P" = "Table A5",
  "H-U" = "Table A6"
)

# Subtitles are human-facing descriptions used in the Word doc table header.
# These should match the paper’s language for risk and predictability.
title_sub <- c(
  "L-P" = "Hypervigilance in Low-risk, Predictable environments (L-P)",
  "L-U" = "Hypervigilance in Low-risk, Unpredictable environments (L-U)",
  "M-P" = "Hypervigilance in Medium-risk, Predictable environments (M-P)",
  "M-U" = "Hypervigilance in Medium-risk, Unpredictable environments (M-U)",
  "H-P" = "Hypervigilance in High-risk, Predictable environments (H-P)",
  "H-U" = "Hypervigilance in High-risk, Unpredictable environments (H-U)"
)

# Export order matters in Word (it determines the sequence of tables A1..A6).
env_order <- c("L-P", "L-U", "M-P", "M-U", "H-P", "H-U")

# Cost order is used to ensure "Low, Moderate, High" appears in a consistent
# and interpretable order rather than alphabetical order.
cost_order <- c("Low", "Moderate", "High")


# ---------------------------------------------------------------------------------------------------
# build_apa_env_table()
# ---------------------------------------------------------------------------------------------------
# Convert ONE environment’s table into a clean, APA-ready tibble with the final
# column names and ordering used in the Word export.
#
# Inputs:
#   env_key: one of "L-P", "L-U", "M-P", "M-U", "H-P", "H-U"
#   hv_tables: output from generate_hypervigilance_tables()
#   digits: number of decimals when formatting HV as percent
#
# Output columns (final contract):
#   - Model
#   - Vigilance cost
#   - HV
#   - Mechanistic interpretation
#
# Key transformations:
#   - Standardize/pretty model names (recode + factor levels)
#   - Standardize cost label order (factor levels)
#   - Convert Safe HV rate numeric to a formatted percent string
#   - Arrange in a stable order and blank repeated model labels for readability
# ---------------------------------------------------------------------------------------------------
build_apa_env_table <- function(env_key, hv_tables, digits = 2) {

  # hv_tables$by_env is expected to be a named list by environment key.
  # Each element should contain rows for Model × Cost.
  hv_tables$by_env[[env_key]] %>%
    mutate(
      # 1) Make model names publication-ready:
      #    hv_model_name_map is expected to map internal model names → display names.
      #    !!! unquotes/splices the mapping vector into recode().
      Model = recode(Model, !!!hv_model_name_map),

      # 2) Fix model ordering for tables:
      #    hv_model_levels defines the intended sequence in the appendix.
      Model = factor(Model, levels = hv_model_levels),

      # 3) Convert Cost into a display column ("Vigilance cost") with stable ordering.
      `Vigilance cost` = factor(Cost, levels = cost_order),

      # 4) Format Safe HV rate into a percent string.
      #    format_hv_percent() likely expects a proportion and returns "12.34%" etc.
      HV = format_hv_percent(`Safe HV rate`, digits = digits)
    ) %>%
    # Keep only the fields needed for the APA table.
    select(Model, `Vigilance cost`, HV, `Mechanistic interpretation`) %>%
    # Enforce consistent row ordering:
    #   - group by model in the intended order
    #   - within each model, list costs Low → Moderate → High
    arrange(Model, `Vigilance cost`) %>%
    mutate(
      # Convert factors back to character so they print cleanly and don’t carry
      # factor attributes into flextable formatting.
      Model = as.character(Model),
      `Vigilance cost` = as.character(`Vigilance cost`)
    ) %>%
    # Replace repeated model labels with blanks on subsequent rows.
    # This is a classic APA readability tactic when each model spans multiple rows.
    blank_model_repeats()
}


# ---------------------------------------------------------------------------------------------------
# build_environment_flextables()
# ---------------------------------------------------------------------------------------------------
# Convert each environment’s tibble into a fully formatted flextable:
#   - Adds titles (Table A1 + descriptive subtitle)
#   - Adds a note clarifying abbreviations
#   - Applies column-specific alignment
#   - Sets column widths suitable for Word page layout
#   - Autofits to content (final adjustment)
#
# Input:
#   env_tables: named list of tibbles (one per env_key), usually produced by
#               lapply(env_order, build_apa_env_table, ...)
#
# Output:
#   named list of flextable objects, keyed by env_key, ready to insert into Word
# ---------------------------------------------------------------------------------------------------
build_environment_flextables <- function(env_tables) {

  # Use base lapply to preserve simple semantics:
  # for each env_key, create one flextable.
  lapply(names(env_tables), function(env_key) {

    df_env <- env_tables[[env_key]]

    # nice_table() is a project helper that likely:
    #   - creates a flextable
    #   - applies APA-ish style defaults (fonts, borders, header formatting)
    #   - supports multi-line titles and an optional note
    ft <- nice_table(
      df_env,

      # title is passed as a character vector so nice_table can render it as
      # a stacked title block (e.g., main title line + subtitle line).
      title = c(title_main[[env_key]], title_sub[[env_key]]),

      # Standard explanatory note; keeps abbreviations consistent across tables.
      note = "HV = hypervigilance; risk refers to baseline stressor probability."
    )

    # Column alignments:
    # - Model: left align (text label)
    # - Vigilance cost: centered (categorical level)
    # - HV: centered (numeric/percent)
    # - Mechanistic interpretation: left align (long text)
    ft <- flextable::align(ft, j = "Model", align = "left", part = "all")
    ft <- flextable::align(ft, j = "Vigilance cost", align = "center", part = "all")
    ft <- flextable::align(ft, j = "HV", align = "center", part = "all")
    ft <- flextable::align(
      ft,
      j = "Mechanistic interpretation",
      align = "left",
      part = "all"
    )

    # Explicit widths (in inches by default for flextable):
    # These are tuned so that:
    #   - short columns don’t waste space
    #   - interpretation column has enough width to wrap nicely
    #   - overall table fits well on a standard Word page
    ft <- flextable::width(
      ft,
      j = c("Model", "Vigilance cost", "HV", "Mechanistic interpretation"),
      width = c(2.2, 1.6, 1.4, 5.0)
    )

    # autofit() makes a final pass based on content, padding, and font metrics.
    # Often helpful for Word exports where rendering can differ slightly across systems.
    flextable::autofit(ft)
  }) %>%
    # Preserve names so later writing can iterate in a stable way.
    setNames(names(env_tables))
}


# ---------------------------------------------------------------------------------------------------
# write_apa_word()
# ---------------------------------------------------------------------------------------------------
# Append each flextable to a single Word document in order.
#
# Inputs:
#   ft_list: named list of flextables (one per environment)
#   target: output path for the .docx
#
# Behavior:
#   - Ensures the output directory exists
#   - Creates a new Word doc (blank)
#   - Adds each table followed by a blank paragraph as spacing
#   - Writes the final document to disk
# ---------------------------------------------------------------------------------------------------
write_apa_word <- function(
  ft_list,
  target = file.path("tables", "Hypervigilance_Tables_A1_to_A6.docx")
) {
  # Ensure parent directory exists (safe to call repeatedly)
  dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)

  # Start a new Word document (default template)
  doc <- read_docx()

  # Iterate in the order of ft_list names (caller controls ordering)
  for (env_key in names(ft_list)) {
    doc <- doc %>%
      # Insert the flextable object
      body_add_flextable(ft_list[[env_key]]) %>%
      # Add a blank paragraph to visually separate tables
      body_add_par("")
  }

  # Persist document to disk
  print(doc, target = target)
}


# ---------------------------------------------------------------------------------------------------
# export_hypervigilance_tables()
# ---------------------------------------------------------------------------------------------------
# Master “pipeline” function:
#   1) Generate raw hypervigilance tables (models × costs × env)
#   2) Convert each env table into APA-ready structure
#   3) Convert each env data frame into a formatted flextable with title + note
#   4) Write all flextables into a single Word document
#
# This function is the single entry point a user/pipeline calls.
# ---------------------------------------------------------------------------------------------------
export_hypervigilance_tables <- function() {

  # Step 1: produce tables in a structured list (by_env, maybe also overall, etc.)
  hv_tables <- generate_hypervigilance_tables()

  # Step 2: build the six env-specific APA tibbles
  # lapply(env_order, ...) ensures correct ordering (A1..A6 sequence).
  env_tables <- lapply(
    env_order,
    build_apa_env_table,
    hv_tables = hv_tables,
    digits = 2
  )
  names(env_tables) <- env_order

  # Step 3: convert each tibble into a formatted flextable
  ft_list <- build_environment_flextables(env_tables)

  # Step 4: write Word document
  write_apa_word(ft_list)
}


# ---------------------------------------------------------------------------------------------------
# Script execution
# ---------------------------------------------------------------------------------------------------
# When this script is sourced (or run as a standalone script), immediately export.
# This is convenient in “pipeline mode” where scripts are run non-interactively.
#
# If you ever want this file to be *importable* without side effects, you would
# remove this call and let the pipeline call export_hypervigilance_tables() explicitly.
export_hypervigilance_tables()

