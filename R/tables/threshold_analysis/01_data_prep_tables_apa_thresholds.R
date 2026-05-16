# -------------------------------------------------------------------
# File: prep_tables_apa_thresholds.R
#
# APA-style threshold tables (X1–X3)
#
# What this script does:
#   1) Takes the *already computed* threshold comparison outputs (vertical / horizontal / diagonal)
#      produced by your threshold analysis figure script.
#   2) Filters to the three APA reporting costs: K ∈ {1, 5, 9}.
#   3) Computes ΔHV = |HV_above − HV_below| and classifies the “switch type”.
#   4) Recodes internal model IDs to paper-ready display names and orders rows consistently.
#   5) Builds three APA-styled flextables (X1–X3) and writes them into ONE Word document.
#
# Reporting conventions enforced:
#   - Only vigilance costs 1, 5, 9 appear (Low / Moderate / High).
#   - Model name is shown only once per model block (blanked on subsequent cost rows).
#   - NA numeric values are displayed explicitly as "NA" in the Word output.
# -------------------------------------------------------------------

# ---- Packages ------------------------------------------------------------------------------------
# dplyr: data wrangling (filter/mutate/transmute/arrange)
# rempsyc: often used by your `nice_table()` wrapper for APA-ish defaults
# flextable: render and format tables (including NA formatting)
# officer: create and write Word documents
library(dplyr)
library(rempsyc)
library(flextable)
library(officer)

# ---- Project sources -----------------------------------------------------------------------------
# This script now relies on the shared threshold analysis helper that exposes
# `run_threshold_analysis()`. It returns all three summaries as a named list.
source("R/plotting/mechanism_env/01_data_prep_mechanism_threshold_analysis.R")

# Utilities for:
#   - threshold_model_name_map: internal model keys -> display names
#   - threshold_model_levels: desired model ordering in tables
#   - blank_model_repeats(): blank repeated model labels for APA grouped layout
#   - nice_table(): your project’s APA-style flextable wrapper (titles/notes/borders/fonts)
source("R/helpers/utils_table_helpers.R")


# ---- Global table settings -----------------------------------------------------------------------
# Keep *exactly* these vigilance costs for APA tables.
# (This matches your Low/Moderate/High reporting convention across the appendix.)
allowed_costs <- c(1, 5, 9)


# ================================================================================================
# Helper: classify_switch()
# ================================================================================================
# Given |ΔHV|, return a qualitative label describing how “sharp” the switch looks.
#
# Interpretation logic (heuristic, for readability):
#   - NA: threshold is outside the discretized grid (we couldn’t sample “below/above” properly)
#   - 0: no change across threshold
#   - (0, 0.20): small-but-nonzero change (soft transition)
#   - ≥ 0.20: large change (sharp switch)
#
# NOTE: The cutoffs are reporting choices; they don't change the underlying analysis.
#       If you later decide to use different bins (e.g., 0.10 / 0.30), you only change them here.
classify_switch <- function(delta_abs) {
  dplyr::case_when(
    is.na(delta_abs)                  ~ "Threshold outside grid",
    delta_abs == 0                    ~ "No switch",
    delta_abs > 0 & delta_abs < 0.20  ~ "Soft transition",
    delta_abs >= 0.20                 ~ "Sharp switch"
  )
}


# ================================================================================================
# Table X1 builder: Vertical threshold (λₐ*)
# ================================================================================================
# Input expectation (vertical_raw):
#   A data frame with at least:
#     - model (or model label column used upstream)
#     - vigilance_cost (K)
#     - lambda_l_slice (the fixed LL slice used for the comparison)
#     - lambda_a_star (analytic λA* = K/D)
#     - hv_below, hv_above (hv values just below/above the threshold)
#
# Output:
#   Tidy table with APA-friendly column names and ordering.
build_table_X1 <- function(vertical_raw) {
  vertical_raw %>%
    # ---- Step 1: keep only the APA reporting costs ----------------------------------------------
    filter(vigilance_cost %in% allowed_costs) %>%

    # ---- Step 2: compute ΔHV and an interpretation label ----------------------------------------
    mutate(
      # ΔHV is only meaningful when both sides are available.
      ΔHV = if_else(
        is.na(hv_below) | is.na(hv_above),
        NA_real_,
        round(abs(hv_above - hv_below), 2)
      ),
      Interpretation = classify_switch(ΔHV),

      # ---- Step 3: recode model names to paper display names -----------------------------------
      # `threshold_model_name_map` is supplied by utils_table_helpers.R.
      # `.default` keeps unknown labels unchanged (safer than turning them into NA).
      Model = dplyr::recode(
        as.character(model),
        !!!threshold_model_name_map,
        .default = as.character(model)
      )
    ) %>%

    # ---- Step 4: rename/select the exact columns for the Word table -----------------------------
    # We round here so the exported doc is stable (no floating-point surprises downstream).
    transmute(
      Model,
      `Vigilance cost` = vigilance_cost,
      `λₗ slice`       = round(lambda_l_slice, 2),
      `λₐ*`            = round(lambda_a_star, 2),
      `HV Below`       = round(hv_below, 2),
      `HV Above`       = round(hv_above, 2),
      `ΔHV`,
      Interpretation
    ) %>%

    # ---- Step 5: enforce row ordering -----------------------------------------------------------
    # threshold_model_levels is the canonical model order used across threshold tables.
    mutate(Model_order = factor(Model, levels = threshold_model_levels)) %>%
    arrange(Model_order, `Vigilance cost`) %>%
    select(-Model_order) %>%

    # ---- Step 6: APA grouped display ------------------------------------------------------------
    # Blank repeated model labels so each model appears as a block with cost rows underneath.
    blank_model_repeats()
}


# ================================================================================================
# Table X2 builder: Horizontal threshold (λₗ*)
# ================================================================================================
# Input expectation (horizontal_raw):
#   columns analogous to X1, but:
#     - lambda_a_slice (fixed LA slice)
#     - lambda_l_star (analytic λL* = 1 - K/D)
build_table_X2 <- function(horizontal_raw) {
  horizontal_raw %>%
    filter(vigilance_cost %in% allowed_costs) %>%
    mutate(
      ΔHV = if_else(
        is.na(hv_below) | is.na(hv_above),
        NA_real_,
        round(abs(hv_above - hv_below), 2)
      ),
      Interpretation = classify_switch(ΔHV),
      Model = dplyr::recode(
        as.character(model),
        !!!threshold_model_name_map,
        .default = as.character(model)
      )
    ) %>%
    transmute(
      Model,
      `Vigilance cost` = vigilance_cost,
      `λₐ slice`       = round(lambda_a_slice, 2),
      `λₗ*`            = round(lambda_l_star, 2),
      `HV Below`       = round(hv_below, 2),
      `HV Above`       = round(hv_above, 2),
      `ΔHV`,
      Interpretation
    ) %>%
    mutate(Model_order = factor(Model, levels = threshold_model_levels)) %>%
    arrange(Model_order, `Vigilance cost`) %>%
    select(-Model_order) %>%
    blank_model_repeats()
}


# ================================================================================================
# Table X3 builder: Diagonal threshold (πₛ*)
# ================================================================================================
# Input expectation (diagonal_raw):
#   - target_pi_S (analytic πS* = K/D)
#   - hv_below/hv_above evaluated at grid points with πS just below/above target
#   - plus model + vigilance_cost
build_table_X3 <- function(diagonal_raw) {
  diagonal_raw %>%
    filter(vigilance_cost %in% allowed_costs) %>%
    mutate(
      ΔHV = if_else(
        is.na(hv_below) | is.na(hv_above),
        NA_real_,
        round(abs(hv_above - hv_below), 2)
      ),
      Interpretation = classify_switch(ΔHV),
      Model = dplyr::recode(
        as.character(model),
        !!!threshold_model_name_map,
        .default = as.character(model)
      )
    ) %>%
    transmute(
      Model,
      `Vigilance cost` = vigilance_cost,
      `πₛ*`            = round(target_pi_S, 2),
      `HV Below`       = round(hv_below, 2),
      `HV Above`       = round(hv_above, 2),
      `ΔHV`,
      Interpretation
    ) %>%
    mutate(Model_order = factor(Model, levels = threshold_model_levels)) %>%
    arrange(Model_order, `Vigilance cost`) %>%
    select(-Model_order) %>%
    blank_model_repeats()
}


# ================================================================================================
# Build the three tidy data frames from the threshold outputs
# ================================================================================================
threshold_outputs <- run_threshold_analysis()
table_X1 <- build_table_X1(threshold_outputs$vertical)
table_X2 <- build_table_X2(threshold_outputs$horizontal)
table_X3 <- build_table_X3(threshold_outputs$diagonal)


# ================================================================================================
# Create APA-style flextables
# ================================================================================================
# nice_table():
#   - typically applies the project’s consistent font, borders, header styling,
#     and embeds the title + subtitle + note as table captions.
#
# We then explicitly set NA formatting for numeric columns with na_str = "NA"
# so Word shows NA rather than blank cells.
ft_X1 <- nice_table(
  table_X1,
  title = c(
    "Table X1",
    "Vertical threshold: hypervigilance change across λₐ*"
  ),
  note = "Formula: λₐ* = K / D. ΔHV = |HV_above − HV_below|."
)

ft_X2 <- nice_table(
  table_X2,
  title = c(
    "Table X2",
    "Horizontal threshold: hypervigilance change across λₗ*"
  ),
  note = "Formula: λₗ* = 1 − (K / D). ΔHV = |HV_above − HV_below|."
)

ft_X3 <- nice_table(
  table_X3,
  title = c(
    "Table X3",
    "Diagonal threshold: hypervigilance change across πₛ*"
  ),
  note = "Formula: πₛ = λₐ / (λₐ + λₗ); threshold at πₛ* = K / D."
)

# ---- Numeric column lists -----------------------------------------------------------------------
# We specify numeric columns explicitly so:
#   - formatting is predictable even if column order changes
#   - NA printing is guaranteed for all numeric cells
num_cols_X1 <- c("Vigilance cost", "λₗ slice", "λₐ*", "HV Below", "HV Above", "ΔHV")
num_cols_X2 <- c("Vigilance cost", "λₐ slice", "λₗ*", "HV Below", "HV Above", "ΔHV")
num_cols_X3 <- c("Vigilance cost", "πₛ*", "HV Below", "HV Above", "ΔHV")

# Apply numeric formatting AND make NA explicit.
# digits=2 matches the rounding choices in the builders above; this keeps output consistent.
ft_X1 <- flextable::colformat_num(ft_X1, j = num_cols_X1, digits = 2, na_str = "NA")
ft_X2 <- flextable::colformat_num(ft_X2, j = num_cols_X2, digits = 2, na_str = "NA")
ft_X3 <- flextable::colformat_num(ft_X3, j = num_cols_X3, digits = 2, na_str = "NA")


# ================================================================================================
# Save into /tables as a single Word file
# ================================================================================================
# We create the output folder if missing, then build a single docx containing:
#   - Table X1
#   - blank paragraph spacer
#   - Table X2
#   - blank paragraph spacer
#   - Table X3
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

doc <- read_docx()

# nice_table() already bakes titles/notes into the flextable object,
# so here we simply append each table to the document body in order.
doc <- doc %>%
  body_add_flextable(ft_X1) %>%
  body_add_par("") %>%
  body_add_flextable(ft_X2) %>%
  body_add_par("") %>%
  body_add_flextable(ft_X3)

# Final write
print(doc, target = file.path("tables", "Threshold_Tables_X1_to_X3.docx"))
