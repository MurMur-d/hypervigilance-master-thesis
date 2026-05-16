# ===================================================================================================
# File: prep_environment_heatmap_pipeline.R
# Purpose:
#   Summarize (collapse) LA/LL hypervigilance grid outputs into:
#     - hypervigilance ranges (min/max across the environment grid)
#     - prevention vs spillover shares (mechanistic decomposition)
#     - per-cost (K) â€œmax HVâ€ + mechanistic interpretation labels used in tables
#
# What this file *does NOT* do:
#   - No DP solving
#   - No agent simulation
#
# Instead:
#   - It takes the already-computed grid tables produced by `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R`
#     (which already did policy + simulation + HV extraction per LA/LL cell)
#   - And aggregates them into small tibbles for downstream figures/tables.
#
# Typical upstream input schema (from hypervigilance_grid_by_Kratio_by_model()):
#   df contains one row per (model scenario Ã— K Ã— LA Ã— LL) grid cell, with columns like:
#     LA, LL, K, RatioDK
#     HypervigilanceRate_all / HypervigilanceRate_filtered (rates)
#     HypervigilanceSafeHV, HypervigilancePreventCount, HypervigilanceSpillCount (mechanism counts)
#     model_label, model_id, model (scenario identifiers)
#
# Outputs:
#   - summarize_hypervigilance_grid_stats(): one row per model scenario
#   - summarize_vigilance_mechanisms(): one row per model scenario Ã— K (for appendix tables)
#
# Notes for reviewers:
#   - This file is intentionally â€œpure summarizationâ€: it converts dense grid data into
#     interpretable summary metrics and labels.
#   - Inline annotations explain each aggregation and the interpretation of each derived quantity.
# ===================================================================================================


# ---------------------------------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------------------------------
# We load dplyr for group/summarise and rlang for tidy evaluation of rate_column.
# ggplot2/future.apply are imported here because some pipeline scripts may source this file
# in contexts where those packages are already assumed; however, these functions below do
# not directly plot or parallelize.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(future.apply)
  library(rlang)
})

# Project sources:
# plot_utils.R:
#   - may provide theme helpers, memoised policy/sim wrappers, etc.
# basic/health model code:
#   - not used directly in the summarizers below, but often sourced in pipelines so the
#     environment is fully initialized (especially if other scripts call the grid builders).
# prep_environment_heatmap_data.R:
#   - defines the grid builders + plot helper; here we only consume the grid tables they produce.
# utils_table_helpers.R:
#   - provides format_mechanism(), used to turn counts into â€œpreventative/spillover/mixedâ€ text.
source("R/core/plot_utils.R")
source("R/models/basic/basic_model_dp.R")
source("R/models/basic/basic_model_SIM.R")
source("R/models/health/health_model_dp.R")
source("R/models/health/health_model_SIM.R")

source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")
source("R/helpers/utils_table_helpers.R")


# ===================================================================================================
# 1) summarize_hypervigilance_grid_stats()
# ===================================================================================================

#' Collapse each model grid into hv min/max + prevention/spill totals.
#'
#' @param df data frame produced by `hypervigilance_grid_by_Kratio_by_model()`
#' @param rate_column column name to summarise (defaults to pooled hv rate)
#'
#' @return tibble with one row per model scenario containing:
#'   - hv_min / hv_max: minimum and maximum HV observed anywhere on the LAÃ—LL grid
#'   - hv_range: hv_max - hv_min (a simple â€œspreadâ€ measure across environments)
#'   - safe_hv_total: total â€œsafe HVâ€ count across grid cells (depends on hv_meta definition)
#'   - prevent_total / spill_total: totals of preventive vs spillover counts
#'   - protective_rate: prevent_total / safe_hv_total (share of safe HV that is preventive)
#'   - spillover_rate: spill_total / safe_hv_total (share of safe HV that is spillover)
#'
#' @details
#'   - Groups by model_label, model_id, model so different model variants remain separated.
#'   - rate_column is evaluated with tidy eval so callers can choose:
#       "HypervigilanceRate_all" or "HypervigilanceRate_filtered"
#   - Protective/spillover rates are computed as *shares of safe HV*:
#       safe_hv_total is the denominator because â€œpreventâ€ and â€œspillâ€ are conceptually
#       subdivisions of hypervigilance in safe/no-stressor contexts (project convention).
summarize_hypervigilance_grid_stats <- function(
    df,
    rate_column = "HypervigilanceRate_all"
) {
  # If upstream grid generation produced no rows, return an empty tibble
  if (nrow(df) == 0) return(dplyr::tibble())

  # Ensure the requested rate_column exists, otherwise fail loudly
  if (!(rate_column %in% names(df))) stop("rate_column not found in df")

  # Convert the string column name to a symbol so we can write:
  #   min(!!rate_sym), max(!!rate_sym)
  rate_sym <- rlang::sym(rate_column)

  df %>%
    # Group by scenario identifiers.
    # model_label: human-readable scenario label used in plots (â€œHealth (Ï‰=1)â€, etc.)
    # model_id: stable numeric ordering
    # model: model family (â€œbasicâ€ vs â€œhealthâ€)
    dplyr::group_by(model_label, model_id, model) %>%

    # Summarise across *all* LAÃ—LLÃ—K grid cells that belong to the scenario.
    # Important: Because K is NOT included in the grouping here, hv_min/hv_max are
    # â€œglobal across costsâ€ unless df has already been filtered to a single K set.
    #
    # If you want per-cost hv_min/hv_max, you would add K to the group_by.
    dplyr::summarise(
      # Range of HV values across the grid
      hv_min = min(!!rate_sym, na.rm = TRUE),
      hv_max = max(!!rate_sym, na.rm = TRUE),

      # Mechanism totals across all grid cells.
      # These are â€œcountsâ€ aggregated over the entire grid, not rates.
      # Their magnitude depends on:
      #   - how hv_meta defines safe_hv/prevent/spill
      #   - grid size (number of LAÃ—LLÃ—K cells)
      safe_hv_total = sum(HypervigilanceSafeHV, na.rm = TRUE),
      prevent_total = sum(HypervigilancePreventCount, na.rm = TRUE),
      spill_total   = sum(HypervigilanceSpillCount, na.rm = TRUE),

      .groups = "drop"
    ) %>%

    # Derive share metrics and a simple hv_range for interpretability.
    dplyr::mutate(
      # protective_rate:
      #   fraction of â€œsafe HVâ€ events classified as â€œpreventiveâ€
      # If safe_hv_total == 0 (no safe HV anywhere), return NA to avoid dividing by 0.
      protective_rate = dplyr::if_else(
        safe_hv_total == 0,
        NA_real_,
        prevent_total / safe_hv_total
      ),

      # spillover_rate:
      #   fraction of â€œsafe HVâ€ events classified as â€œspilloverâ€
      spillover_rate = dplyr::if_else(
        safe_hv_total == 0,
        NA_real_,
        spill_total / safe_hv_total
      ),

      # hv_range:
      #   a compact measure of how much HV varies across environments/costs in this scenario
      hv_range = hv_max - hv_min
    )
}


# ===================================================================================================
# 2) summarize_vigilance_mechanisms()
# ===================================================================================================

#' Derive mechanistic labels (preventative / spillover / mixed) plus max-hv per cost.
#'
#' @param df grid data frame (stacked across models and costs)
#' @param rate_column column to draw hv maxima from
#'
#' @return tibble with one row per (Model Ã— K) containing:
#'   - Model: display label
#'   - vigilance cost: K
#'   - max hv: maximum HV over the LAÃ—LL grid at that cost
#'   - mechanistic interpretation: a text label derived from prevent/spill totals
#'
#' @details
#'   This is the table-facing summarizer:
#   - We group by scenario + K so each cost column gets its own â€œmechanism labelâ€.
#   - We compute max_hv at that K to report â€œhow high HV can getâ€ under that cost.
#   - We compute preventive/spillover shares and then use format_mechanism()
#     to map counts into a stable textual interpretation.
#   - The output columns are named to match your table exports.
summarize_vigilance_mechanisms <- function(
    df,
    rate_column = "HypervigilanceRate_all"
) {
  if (nrow(df) == 0) return(dplyr::tibble())
  if (!(rate_column %in% names(df))) stop("rate_column not found in df")

  rate_sym <- rlang::sym(rate_column)

  df %>%
    # Group by scenario + cost:
    # This ensures each cost level gets its own max HV and mechanism decomposition.
    dplyr::group_by(model_label, model_id, model, K) %>%

    dplyr::summarise(
      # max_hv:
      #   maximum hypervigilance rate across all LAÃ—LL cells at this cost.
      # This is often what you want in tables/captions: â€œin the worst environment,
      # HV can reach X under cost Kâ€.
      max_hv = max(!!rate_sym, na.rm = TRUE),

      # Aggregate mechanism counts across all environments at this cost.
      safe_hv_total = sum(HypervigilanceSafeHV, na.rm = TRUE),
      prevent_total = sum(HypervigilancePreventCount, na.rm = TRUE),
      spill_total   = sum(HypervigilanceSpillCount, na.rm = TRUE),

      .groups = "drop"
    ) %>%

    dplyr::mutate(
      # Convert totals into shares, mainly for interpretability/debugging.
      protective_rate = dplyr::if_else(
        safe_hv_total == 0,
        NA_real_,
        prevent_total / safe_hv_total
      ),
      spillover_rate = dplyr::if_else(
        safe_hv_total == 0,
        NA_real_,
        spill_total / safe_hv_total
      ),

      # Mechanism label:
      # format_mechanism() is assumed to encode your projectâ€™s rule for turning counts into
      # â€œpreventativeâ€, â€œspilloverâ€, or â€œmixedâ€ (and possibly thresholded variants).
      #
      # Passing raw totals (rather than rates) is intentional because:
      #   - format_mechanism can use both absolute and relative information if desired
      #   - totals are stable across environments given a fixed grid definition
      mechanism = format_mechanism(prevent_total, spill_total, safe_hv_total),

      # Rename K into a table-friendly field
      vigilance_cost = K
    ) %>%

    # Select and rename columns to match the table export schema exactly.
    dplyr::select(
      Model = model_label,
      `vigilance cost` = vigilance_cost,
      `max hv` = max_hv,
      `mechanistic interpretation` = mechanism
    )
}


# ---------------------------------------------------------------------------------------------------
# Summary block (what each function is for)
# ---------------------------------------------------------------------------------------------------
# - summarize_hypervigilance_grid_stats():
#     One row per model scenario.
#     Collapses HV to global min/max and computes overall preventive vs spillover shares
#     across the entire grid (and across K unless df is pre-filtered).
#
# - summarize_vigilance_mechanisms():
#     One row per model scenario Ã— cost K.
#     Computes â€œmax HV at cost Kâ€ and attaches a mechanism label used in table exports.
# ---------------------------------------------------------------------------------------------------
