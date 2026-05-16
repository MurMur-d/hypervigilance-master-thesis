# ===================================================================================================
# File: utils_table_helpers.R
# Purpose: Shared formatting helpers for the APA-style tables (max HV, threshold tables, etc.).
#
# Big picture:
#   A lot of your manuscript tables are built from many pipelines, and those pipelines
#   all need to present *the same* concepts (hypervigilance rate, model names, mechanisms)
#   in a consistent, publication-ready way.
#
# This file centralizes the “presentation layer” for tables:
#   - how we print HV as percentages
#   - how we reduce repeated labels (APA-style readability)
#   - how we map internal model identifiers → reader-facing model names
#   - how we turn prevention/spill counts into a one-line mechanistic interpretation
#
# Design goals:
#   1) Stable: changes here propagate across all tables (avoids drift).
#   2) Defensive: handles NAs and edge cases gracefully (so Word exports don’t break).
#   3) Predictable: minimal cleverness; reviewers can follow the logic at a glance.
# ===================================================================================================


#' Format hypervigilance rates as percentage strings for Word tables.
#'
#' @param rate numeric vector in [0,1]
#'   Expected input is a proportion (e.g., 0.1234 means 12.34%).
#'
#' @param digits number of decimal places to show
#'   Default is 2 to match typical APA formatting.
#'
#' @return character vector of percentages (e.g., "12.34%")
#'
#' @details
#'   - We return *strings*, not numbers, because Word tables often want fixed formatting.
#'   - NA values are explicitly printed as "NA" rather than empty cells, which helps reviewers
#'     detect “missing because not computed” vs “missing because blank formatting”.
#'   - We do not clamp to [0,1] here: clamping should happen in the HV computation pipeline.
#'     This keeps the function pure “formatting”, not “data cleaning”.
format_hv_percent <- function(rate, digits = 2) {
  # Build a sprintf format string like "%.2f%%"
  # The doubled %% prints a literal percent sign.
  fmt <- paste0('%.', digits, 'f%%')

  # vapply enforces that output is always character(1) per element
  # (safer than sapply, which can silently simplify incorrectly).
  vapply(rate, function(x) {
    if (is.na(x)) {
      # Explicit NA label for APA tables (important when exporting to Word).
      'NA'
    } else {
      # Convert proportion → percent.
      sprintf(fmt, x * 100)
    }
  }, character(1))
}


#' Blank repeated model names to reduce visual clutter in APA tables.
#'
#' @param df data.frame already sorted by `Model`
#'   IMPORTANT: This expects the table has already been arranged so all rows for
#'   a given Model are contiguous (e.g., Model → Cost ordering).
#'
#' @return data.frame with duplicate `Model` entries blanked after the first occurrence
#'
#' Why this exists:
#'   APA-style tables often show one row block per model (with multiple costs beneath it).
#'   Repeating "Basic" / "Health" on every row makes the table harder to scan.
#'   The convention is:
#'     - print the model name once at the start of its block
#'     - leave the subsequent rows blank in that column
#'
#' Implementation details:
#'   - We convert Model to character to avoid factor-level surprises.
#'   - We group by Model and then replace all but the first row with "".
#'   - We ungroup afterwards to avoid accidental grouped operations in downstream steps.
blank_model_repeats <- function(df) {
  df %>%
    mutate(Model = as.character(Model)) %>%
    group_by(Model) %>%
    mutate(
      # Keep the first row’s Model label, blank the rest.
      Model = if_else(row_number() == 1, Model, '')
    ) %>%
    ungroup()
}


#' Canonical model name mapping used across multiple tables.
#'
#' What this is:
#'   A named character vector mapping *raw/internal* model labels to the
#'   *manuscript/reader-facing* labels.
#'
#' Why we need it:
#'   Different pipelines sometimes emit different strings (e.g., "Health: linear ω=1"
#'   vs "Linear"). Tables should still show a unified label set so readers can compare
#'   across appendices without relearning labels.
#'
#' How it’s used:
#'   Typically via:
#'     dplyr::recode(Model, !!!hv_model_name_map)
#'
#' Note:
#'   You include a few synonyms for the same display label. That’s intentional:
#'   it makes this mapping robust to small changes in upstream scripts.
hv_model_name_map <- c(
  'Basic'                      = 'Basic',
  'Health'                     = 'Health (β = 0)',
  'Linear'                     = 'Linear (β = 1)',
  'Power'                      = 'Power (a = 3)',
  'Threshold'                  = 'Threshold (τ = 0.6·H0)',
  'Health: no terminal'        = 'Health (β = 0)',
  'Health: linear ω=1'         = 'Linear (β = 1)',
  'Health: power a=3'          = 'Power (a = 3)',
  'Health: threshold τ=30'     = 'Threshold (τ = 0.6·H0)',
  'Health: no terminal reward' = 'Health (β = 0)',
  'Health: linear reward'      = 'Linear (β = 1)'
)

# Canonical ordering used across tables/plots.
# Keeping this as a shared vector prevents "Basic, Linear, Health, ..." reorder bugs.
hv_model_levels <- c(
  'Basic',
  'Health (β = 0)',
  'Linear (β = 1)',
  'Power (a = 3)',
  'Threshold (τ = 0.6·H0)'
)


#' Describe the dominant mechanism using counts of prevent vs spillover steps.
#'
#' @param prevent_total vector of preventive counts
#'   Count of safe-HV events classified as "preventative" (i.e., vigilant in safe state
#'   with a safe→safe transition context, depending on your safe-count definition).
#'
#' @param spill_total vector of spillover counts
#'   Count of safe-HV events classified as "spillover" (i.e., vigilant in safe state
#'   immediately following a stressor context).
#'
#' @param safe_hv_total vector of safe hypervigilance counts
#'   Denominator representing the total number of safe-HV events (prevent + spill).
#'   When this is 0/NA, there is no safe-HV to interpret and we return "None".
#'
#' @return character vector describing the mechanism per row
#'
#' Interpretation logic (table-friendly):
#'   - None:
#'       safe_hv_total is NA or 0 → no safe-HV events observed.
#'   - Preventative only:
#'       safe_hv_total > 0 and spill_total == 0 and prevent_total > 0
#'   - Spillover only:
#'       safe_hv_total > 0 and prevent_total == 0 and spill_total > 0
#'   - Mixed:
#'       safe_hv_total > 0 and prevent_total > 0 and spill_total > 0
#       → print a compact label including an approximate spillover percentage.
#'
#' Why the “≈” percent:
#'   Tables benefit from a single scalar summary, and the spillover share is the most
#'   intuitive “mix” indicator. We round to whole percentages to avoid overstating precision.
format_mechanism <- function(prevent_total, spill_total, safe_hv_total) {
  # Allow inputs to be scalars or vectors: recycle shorter vectors to the maximum length.
  len <- max(length(prevent_total), length(spill_total), length(safe_hv_total))
  prevent_total <- rep_len(prevent_total, len)
  spill_total   <- rep_len(spill_total, len)
  safe_hv_total <- rep_len(safe_hv_total, len)

  # If there are no safe-HV events, mechanism is "None".
  is_none <- is.na(safe_hv_total) | safe_hv_total == 0
  res <- rep('None', len)

  # Only evaluate mechanism labels where safe_hv_total is positive and defined.
  safe_nonzero <- !is_none

  # Pure preventative: no spillover counted, but at least one preventative event exists.
  prevent_only <- safe_nonzero & spill_total == 0 & prevent_total > 0
  res[prevent_only] <- 'Preventative only'

  # Pure spillover: no preventative counted, but at least one spill event exists.
  spill_only <- safe_nonzero & prevent_total == 0 & spill_total > 0
  res[spill_only] <- 'Spillover only'

  # Mixed: both components appear.
  mixed <- safe_nonzero & spill_total > 0 & prevent_total > 0
  if (any(mixed)) {
    # Spillover share is spill / total safe-HV.
    # Round to whole percent for a clean APA-style descriptive label.
    spill_pct <- round(100 * spill_total[mixed] / safe_hv_total[mixed])
    res[mixed] <- sprintf('Mixed preventative + spillover (≈%d%% spillover)', spill_pct)
  }

  res
}


#' Alternate label maps used by the threshold tables.
#'
#' Why a separate map from hv_model_name_map:
#'   Threshold pipelines often operate on scenario labels from `default_env_model_scenarios`
#'   (e.g., "no terminal reward (ω = 0)") rather than already-normalized "Health"/"Linear" ids.
#'   This map therefore accepts both:
#'     - human scenario labels (long strings)
#'     - short internal slugs ("basic", "health_threshold", ...)
#'   and returns the unified manuscript labels.
threshold_model_name_map <- c(
  'basic model'                        = 'Basic',
  'no terminal reward (ω = 0)'         = 'Health (β = 0)',
  'terminal reward (ω = 1, linear)'    = 'Linear (β = 1)',
  'power terminal reward (α = 3)'      = 'Power (a = 3)',
  'threshold terminal reward (τ = 0.6·H0)' = 'Threshold (τ = 0.6·H0)',
  'basic'                              = 'Basic',
  'health'                             = 'Health (β = 0)',
  'health_linear'                      = 'Linear (β = 1)',
  'health_power'                       = 'Power (a = 3)',
  'health_threshold'                   = 'Threshold (τ = 0.6·H0)'
)

# Threshold tables use the same canonical ordering as other tables.
threshold_model_levels <- hv_model_levels
