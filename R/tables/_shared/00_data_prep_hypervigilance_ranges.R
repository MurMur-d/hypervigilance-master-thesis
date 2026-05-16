# ===================================================================================================
# File: prep_hypervigilance_ranges.R
# Purpose:
#   Build the “Tables A1–A6” summaries that report:
#     (1) Safe hypervigilance rates (HV when no stressor is present)
#     (2) The dominant mechanism label (preventative vs spillover vs mixed),
#         based on prevention/spill transition counts.
#
# What this file does (big picture):
#   - Defines a canonical set of model scenarios (basic + health variants),
#     canonical environments (L-P … H-U), and canonical cost levels (Low/Moderate/High).
#   - For every Scenario × Cost × Environment cell:
#       a) Compute the optimal policy
#       b) Simulate agents under that policy (memoized via mem_simulate_agents)
#       c) Compute canonical HV metrics (via hv_rate_helpers.R)
#       d) Extract prevention/spill counts (via prep_hypervigilance_data.R → hv_rate_helpers.R)
#   - Summarizes those cell-level quantities into:
#       * A master “long” tibble (all envs stacked)
#       * A named list of six per-environment tibbles (A1–A6 slices)
#
# Inputs:
#   - DP policy + simulation functions (basic & health), wrapped via get_model()/mem_* helpers
#   - Environment definitions from utils_health_env_scenarios.R
#   - Canonical HV definition from hv_rate_helpers.R
#
# Outputs:
#   - list(master = tibble, by_env = named list of tibbles)
#
# Notes / design philosophy:
#   - This is a “prep” module: no file I/O. Export to Word/CSV happens elsewhere.
#   - Heavy computations are *wrapped* in a caching helper (compute_cell_stats) so:
#       * repeated calls don’t re-run simulations
#       * tests can be written against deterministic outputs
# ===================================================================================================

# ---- Dependencies -------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(rlang)
})

# Project helpers:
source("R/core/plot_utils.R")                  # get_model(), mem_* caching, and shared defaults
source("R/helpers/utils_model_scenarios.R")    # canonical lists of model scenarios (labels/args)
source("R/plotting/_shared/utils_health_env_scenarios.R") # canonical environment definitions
source("R/core/hv_rate_helpers.R")             # canonical HV metrics (pooled safe HV definition)
source("R/helpers/utils_table_helpers.R")      # format_mechanism(), hv_model_name_map, etc.


# ================================================================================================
# Constants: canonical states, horizon, and agent count used for the appendix tables
# ================================================================================================
# These are the “table defaults”. They are intentionally fixed so the appendix tables
# are stable and reproducible. If you want tables for different horizons or agent counts,
# you change them here (or refactor to accept parameters).
STATES   <- c("Kd", "K", "CD", "C")  # DP state labels; ordering doesn’t matter for counting
T_STEPS  <- 10L                     # horizon length used for policy + sim
N_AGENTS <- 1000L                   # simulation draws per cell

# Default model scenario list:
# default_model_specs() is defined in your shared plot_utils.R and returns:
#   model_id, model_label, model_type, policy_args, sim_args
DEFAULT_MODEL_SCENARIOS <- default_model_specs()

# Short display names used in the appendix tables.
# These are the names you want readers to see (not internal model IDs).
MODEL_SHORT_LABELS <- setNames(
  c("Basic", "Health", "Linear", "Power", "Threshold"),
  DEFAULT_MODEL_SCENARIOS$model_id
)

# Cost levels used in the tables:
#   - COST_CATALOG: numeric K values actually passed into the policy/simulation
#   - COST_DISPLAY: the human-readable label used in tables/plots
COST_CATALOG <- c(low = 1, moderate = 5, high = 9)
COST_DISPLAY <- c(low = "Low", moderate = "Moderate", high = "High")

# Environment scenarios used in the appendix:
# Each env has an env_label (e.g., "L-P") and numeric LA, LL coordinates.
ENV_SCENARIOS <- default_health_env_scenarios()

# A compact “environment metadata” table to attach descriptive text + parameters later.
ENV_SUMMARY <- ENV_SCENARIOS %>%
  dplyr::transmute(
    env_key     = env_label,  # e.g., "L-P"
    description = env_full,   # longer prose label
    lambda_a    = LA,         # stressor appearance probability/rate
    lambda_l    = LL          # stressor disappearance probability/rate
  )

# An in-memory cache to avoid re-running the same Scenario × Cost × Env cell repeatedly.
# This is *separate* from the file-backed memoization used by mem_compute_policy/mem_simulate_agents.
# Rationale:
#   - mem_* caches at the policy/simulation object level.
#   - This cache stores *already summarized cell statistics* for table construction.
CELL_STATS_CACHE <- new.env(parent = emptyenv())


# ================================================================================================
# Core helper: compute_cell_stats()
# ================================================================================================
# This is the “one cell” workhorse:
#   given (scenario_id, model variant args, K, LA, LL),
#   it returns the statistics that the appendix tables care about.
#
# Output columns are chosen to support:
#   - safe HV rate (filtered HV)
#   - mechanism labeling via prevent/spill counts
# ================================================================================================

#' Build per-cell hypervigilance statistics while caching repeated scenarios.
#'
#' @param scenario_id identifier drawn from `default_model_specs()` (model_id)
#' @param model       model slug ("basic" or "health")
#' @param policy_args list passed to `mem_compute_policy` or get_model() (health variants)
#' @param sim_args    list passed to `mem_simulate_agents` (health sim knobs)
#' @param K           vigilance cost for this cell
#' @param LA          λA coordinate (appearance rate)
#' @param LL          λL coordinate (disappearance rate)
#'
#' @return tibble with:
#'   - hv_rate            : canonical “safe HV” pooled rate (no-stressor rows)
#'   - hv_safe_steps      : number of no-stressor timesteps seen (denominator proxy)
#'   - hv_prevent_count   : safe HV events classified as preventative
#'   - hv_spill_count     : safe HV events classified as spillover
compute_cell_stats <- function(scenario_id, model, policy_args, sim_args, K, LA, LL) {

  # ---- Step 0: Check in-memory cache first ----------------------------------
  # We key on scenario + numeric parameters. If the same cell is requested again,
  # we just return the cached tibble.
  cache_key <- paste(scenario_id, K, LA, LL, sep = "|")
  if (exists(cache_key, envir = CELL_STATS_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = CELL_STATS_CACHE))
  }

  # Normalize NULL args to empty lists so downstream code can safely index/merge.
  policy_args <- if (is.null(policy_args)) list() else policy_args
  sim_args    <- if (is.null(sim_args)) list() else sim_args

  # ---- Step 1: Compute optimal policy for this cell --------------------------
  # get_model() returns wrappers so we can call compute_optimal_policy with a uniform signature.
  #
  # NOTE: This code sets D_value = max(10, K). That’s a *local design choice*:
  #   - it prevents degenerate cases where D < K (which can violate some assumptions)
  #   - it ensures D is at least 10, matching typical defaults used elsewhere
  #
  # If you want tables strictly at DEFAULT D (e.g. D=10 always), replace this with:
  #   D_value <- validate_default("D")
  model_fns <- get_model(model, policy_args = policy_args, sim_args = sim_args)
  D_value <- max(10, K)

  # Compute the DP policy on the canonical horizon and state set.
  # Signature matches the “basic” model, even for health models, because get_model() wraps args.
  policy <- model_fns$compute_optimal_policy(
    K,        # vigilance cost
    0,        # C (baseline relaxed cost) set to 0 for these tables
    D_value,  # D (damage when vigilant under stressor) as chosen above
    0,        # d (damage when relaxed under stressor) set to 0 for these tables
    LA, LL,   # environment coordinates
    T_STEPS,  # horizon
    STATES    # DP states tracked
  )

  # ---- Step 2: Simulate agents following the policy --------------------------
  # We call mem_simulate_agents() (file-backed cache) so repeated calls across
  # scripts/pipelines are cheap.
  simulation <- mem_simulate_agents(
    model = model,
    policy_df = policy,
    LA = LA, LL = LL,
    T_steps = T_STEPS,
    N_agents = N_AGENTS,
    K = K, C = 0, D = D_value, d = 0,
    sim_args = sim_args
  )

  # ---- Step 3: Compute canonical HV rates + metadata -------------------------
  # hv_rate_helpers.R defines compute_hv_rates_from_agent_data(), which returns:
  #   - HypervigilanceRate_filtered: pooled mean HV over “no stressor” rows
  #   - hv_meta: includes safe_steps, safe_hv, prevent, spill (counts)
  hv_rates <- compute_hv_rates_from_agent_data(
    simulation$agent_data,
    T_steps = T_STEPS,
    N_agents = N_AGENTS
  )
  hv_meta <- hv_rates$hv_meta

  # ---- Step 4: Assemble the per-cell output ---------------------------------
  # This is the minimal information needed to build:
  #   - safe HV rates for the table
  #   - mechanism labels (prevent/spill proportions)
  stats <- tibble(
    hv_rate          = hv_rates$HypervigilanceRate_filtered,
    hv_safe_steps    = hv_meta$safe_steps,
    hv_prevent_count = hv_meta$prevent,
    hv_spill_count   = hv_meta$spill
  )

  # Store in the in-memory cache for reuse within the same R session.
  assign(cache_key, stats, envir = CELL_STATS_CACHE)
  stats
}


# ================================================================================================
# Public API: generate_hypervigilance_tables()
# ================================================================================================
# This is the function that downstream exporters call.
# It builds a full factorial grid:
#   Scenario × Cost × Environment
# computes cell stats, then formats a master table and a list of env-specific slices.
# ================================================================================================

#' Generate the set of hypervigilance range tables (master + per environment).
#'
#' @return list(master = tibble, by_env = named list of tibbles)
#'
#' @details
#'   Steps:
#'     1) Expand Scenario × Cost × Env combinations
#'     2) For each row, compute per-cell stats (hv_rate + prevent/spill counts)
#'     3) Collapse to the exact aggregation used by the appendix tables
#'     4) Add human-readable labels (model names, cost labels, mechanism text)
#'     5) Return:
#'         - master: all envs stacked
#'         - by_env: list of 6 tables (A1–A6), each without the env_key column
generate_hypervigilance_tables <- function() {

  # ---- Step 1: Canonical scenario table with arguments -----------------------
  # Keep only the fields we need for building the grid and running compute_cell_stats().
  scenario_df <- DEFAULT_MODEL_SCENARIOS %>%
    dplyr::transmute(
      scenario_id    = model_id,
      scenario_label = model_label,
      model          = model_type,
      policy_args,
      sim_args
    )

  # ---- Step 2: Canonical environment coordinates ----------------------------
  # env_key is the table key (L-P, L-U, ...).
  env_combos <- ENV_SCENARIOS %>%
    dplyr::select(env_key = env_label, LA, LL)

  # ---- Step 3: Build the full factorial grid --------------------------------
  # Each row corresponds to one Scenario × Cost × Environment cell.
  base_grid <- tidyr::expand_grid(
    scenario_df,
    cost_label = names(COST_CATALOG),
    env_combos
  ) %>%
    # Translate cost_label (“low/moderate/high”) into numeric K
    dplyr::mutate(K = COST_CATALOG[cost_label]) %>%
    # Compute per-cell stats via pmap (vectorized mapping over multiple columns)
    dplyr::mutate(
      stats = purrr::pmap(
        list(scenario_id, model, policy_args, sim_args, K, LA, LL),
        compute_cell_stats
      )
    ) %>%
    # stats is a list-column of tibbles; unnest to regular columns
    tidyr::unnest(cols = c(stats))

  # ---- Step 4: Collapse within each Scenario × Env × Cost -------------------
  # In principle, base_grid already has one row per cell, so mean/sum are redundant.
  # But we keep this grouping layer because:
  #   - it makes the intent explicit (“this is the summary grain”)
  #   - it is robust if the upstream grid ever expands to multiple replicates per cell
  combined_summary <- base_grid %>%
    dplyr::group_by(scenario_id, env_key, cost_label) %>%
    dplyr::summarise(
      safe_hv_rate   = mean(hv_rate, na.rm = TRUE),
      prevent_total  = sum(hv_prevent_count, na.rm = TRUE),
      spill_total    = sum(hv_spill_count, na.rm = TRUE),
      hv_safe_steps  = sum(hv_safe_steps, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Step 5: Attach metadata + compute mechanism labels -------------------
  final_raw <- combined_summary %>%
    dplyr::inner_join(ENV_SUMMARY, by = "env_key") %>%
    dplyr::mutate(
      # Human-readable cost label for the table
      vigilance_cost = COST_DISPLAY[cost_label],

      # Mechanism shares are computed from prevent/spill totals
      total_events  = prevent_total + spill_total,
      prevent_share = dplyr::if_else(total_events == 0, NA_real_, prevent_total / total_events),
      spill_share   = dplyr::if_else(total_events == 0, NA_real_, spill_total / total_events),

      # Optional: rounded percentages (often useful for debugging / captions)
      prevent_pct = round(prevent_share * 100),
      spill_pct   = round(spill_share * 100),

      # Short model name shown in appendix tables
      model_short = MODEL_SHORT_LABELS[scenario_id],

      # format_mechanism() is your central text rule:
      # it converts (prevent_total, spill_total, safe_steps) into:
      #   “Preventative”, “Spillover”, or “Mixed” (or similar)
      mechanism = format_mechanism(prevent_total, spill_total, hv_safe_steps),

      # Round HV rate for clean reporting
      safe_hv_rate = round(safe_hv_rate, 3),

      # Factor ordering to guarantee stable table ordering
      model_order = factor(model_short, levels = MODEL_SHORT_LABELS),
      cost_order  = factor(vigilance_cost, levels = c("Low", "Moderate", "High")),
      env_order   = factor(env_key, levels = c("L-P", "L-U", "M-P", "M-U", "H-P", "H-U"))
    ) %>%
    dplyr::arrange(env_order, model_order, cost_order)

  # ---- Step 6: Final “master” table shape used by exporters -----------------
  # Keep exactly the columns used by the APA export script.
  hv_master <- final_raw %>%
    dplyr::select(
      env_key,
      Model = model_short,
      Cost  = vigilance_cost,
      `Safe HV rate` = safe_hv_rate,
      `Mechanistic interpretation` = mechanism
    )

  # ---- Step 7: Per-environment slices (A1–A6) -------------------------------
  # We return a named list so exporters can iterate in env_order and title tables.
  hv_by_env <- hv_master %>%
    dplyr::group_split(env_key) %>%
    rlang::set_names(purrr::map_chr(., ~ unique(.x$env_key))) %>%
    purrr::map(~ dplyr::select(.x, -env_key))

  list(master = hv_master, by_env = hv_by_env)
}
