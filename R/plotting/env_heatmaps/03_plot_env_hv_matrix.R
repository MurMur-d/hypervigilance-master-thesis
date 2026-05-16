# ============================================================
# File: R/plotting/env_heatmaps/03_plot_env_hv_matrix.R
# Purpose: Reusable helpers for the environment hypervigilance matrix (Fig2B).
# Notes:
#   - Pure module: returns tables + metadata without saving files (scripts do the saving).
#   - This file is the *table/data constructor* behind Fig2B: it computes a compact
#     matrix of hypervigilance outcomes for a fixed set of canonical environments,
#     a set of vigilance costs, and a set of model variants.
#   - â€œMatrixâ€ here means: rows = environments, columns = K (often) and/or model,
#     with each cell summarising a simulation outcome (HV rate).
# ============================================================

# -----------------------------------------------------------------------------
# Upstream dependencies (project-level helpers)
# -----------------------------------------------------------------------------
# plot_utils.R:
#   Shared themes, caption helpers, and commonly used constants.
source("R/core/plot_utils.R")

# utils_health_env_scenarios.R:
#   Defines the canonical environment list used throughout the paper
#   (e.g., L-P, L-U, M-P, ...), with LA/LL pairs and descriptive labels.
source("R/plotting/_shared/utils_health_env_scenarios.R")

# utils_subtitles.R:
#   Helper utilities for caption/subtitle metadata (notably: collect_h0_values()).
source("R/plotting/_shared/utils_subtitles.R")

# Model implementations:
#   These are sourced so that mem_compute_policy() and mem_simulate_agents()
#   are available for both basic and health model variants.
source("R/models/basic/basic_model_dp.R")
source("R/models/basic/basic_model_SIM.R")
source("R/models/health/health_model_dp.R")
source("R/models/health/health_model_SIM.R")

# -----------------------------------------------------------------------------
# Package dependencies
# -----------------------------------------------------------------------------
# dplyr:
#   Used for safe summaries (coalesce/mean) and row-binding.
# future.apply:
#   Used to parallelise the grid sweep. This matters because the pipeline
#   runs a DP solve + simulation for *every* model Ã— environment Ã— K cell.
suppressPackageStartupMessages({
  library(dplyr)
  library(future.apply)
})

# -----------------------------------------------------------------------------
# hv_matrix_over_env(): core data builder for Fig2B
# -----------------------------------------------------------------------------
# What this function does (conceptually):
#   For each combination of:
#     - model variant (from model_specs)
#     - canonical environment (from env_scenarios)
#     - vigilance cost K (from K_values)
#   it:
#     1) computes an optimal policy via mem_compute_policy()
#     2) simulates N_agents trajectories under that policy via mem_simulate_agents()
#     3) extracts summary hypervigilance rates:
#          - hv_rate: typically hv_rate from agent_stats (HV when no stressor)
#          - hv_rate_no_stressor: HV restricted to stressor==0 from agent_data
#        (with fallbacks to remain robust if columns differ across simulator versions)
#   Finally it returns a tidy table where each row is one (model, env, K) cell.
#
# Why we compute two HV measures:
#   - hv_rate_stat:
#       pulled from agent_stats$hv_rate, which is usually defined as:
#       â€œproportion of no-stressor steps that were Highâ€.
#   - hv_any:
#       computed from agent_data$hv (a per-step indicator) averaged across time.
#       Depending on how hv is defined upstream, this can represent:
#         (a) HV among no-stressor steps only (ideal), or
#         (b) HV across all steps if hv is pre-filtered (fallback).
#   - hv_rate_no_stressor:
#       explicitly recomputed from agent_data restricted to stressor==0.
#
# Practical reason for the redundancy:
#   Your project has multiple simulation modules and evolving schemas.
#   This function is written defensively so Fig2B still builds even if:
#     - hv_rate is absent from agent_stats
#     - agent_data stores hv as hv vs Hypervigilance
#     - stressor is stored as stressor vs Stressor
# -----------------------------------------------------------------------------
#' Build the HV-rate matrix across canonical environments + policy variants.
#'
#' @param env_scenarios Data frame of canonical health environments.
#' @param K_values Numeric vector of vigilance-cost values.
#' @param C,D,d Numeric cost parameters.
#' @param T_steps Integer horizon length.
#' @param states Character vector of state labels.
#' @param N_agents Integer count of simulated agents per panel.
#' @param model_specs Data frame of scenarios (model_id, model_label, policy_args, sim_args).
#' @return A tibble with hv_rate, hv_rate_no_stressor, and metadata attached to `attr(result, "fixed_params")`.
#' @export
hv_matrix_over_env <- function(
    env_scenarios = default_health_env_scenarios(),
    K_values = c(1, 3, 5, 7, 9),
    C = 0, D = 10, d = 0,
    T_steps = 10,
    states = c("K", "Kd", "C", "CD"),
    N_agents = 1000,
    model_specs = default_model_specs()
) {
  # ---- Input validation ------------------------------------------------------
  # These are the minimum checks to avoid cryptic errors deep inside the DP or simulator.
  stopifnot(is.data.frame(env_scenarios), nrow(env_scenarios) > 0)
  stopifnot(is.data.frame(model_specs), nrow(model_specs) > 0)

  # ---- Build the full factorial sweep grid -----------------------------------
  # We use expand.grid() because it is fast and creates a compact indexing table.
  # Each row corresponds to one DP+simulation run.
  combos <- expand.grid(
    model_row = seq_len(nrow(model_specs)),      # which model variant row in model_specs
    env_id = seq_len(nrow(env_scenarios)),       # which environment row in env_scenarios
    K = K_values,                                # vigilance cost
    KEEP.OUT.ATTRS = FALSE
  )

  # ---- Run the sweep (parallel-friendly) -------------------------------------
  # future_lapply() can run in parallel depending on the future plan set in setup_project.R.
  # We set:
  #   - future.seed = TRUE: reproducible randomness across parallel workers
  #   - future.packages: ensures workers load needed packages
  #
  # Each iteration returns a *single-row* data.frame that we later row-bind.
  rows <- future.apply::future_lapply(
    seq_len(nrow(combos)),
    future.seed = TRUE,
    future.packages = c("dplyr"),
    FUN = function(i) {
      combo <- combos[i, ]
      spec <- model_specs[combo$model_row, ]
      env <- env_scenarios[combo$env_id, ]

      # -----------------------------------------------------------------------
      # 1) Compute the optimal policy for this cell (DP solve)
      # -----------------------------------------------------------------------
      # mem_compute_policy() is a project-level wrapper that dispatches to:
      #   - basic DP solver for model_type == "basic"
      #   - health DP solver for model_type == "health"
      # It returns a tidy policy table, typically indexed by time and label (and health for health model).
      policy_df <- mem_compute_policy(
        model = spec$model_type,
        K = combo$K, C = C, D = D, d = d,
        LA = env$LA, LL = env$LL,
        T_steps = T_steps, states = states,
        policy_args = spec$policy_args[[1]]
      )

      # -----------------------------------------------------------------------
      # 2) Simulate agents under that policy (forward simulation)
      # -----------------------------------------------------------------------
      # mem_simulate_agents() dispatches to the appropriate simulator based on model_type.
      # It returns:
      #   - agent_data: per-agent, per-time trajectories + flags
      #   - agent_stats: per-agent summary metrics (including hv_rate if available)
      sim <- mem_simulate_agents(
        model = spec$model_type,
        policy_df = policy_df,
        LA = env$LA, LL = env$LL,
        T_steps = T_steps, N_agents = N_agents,
        K = combo$K, C = C, D = D, d = d,
        sim_args = spec$sim_args[[1]]
      )

      # -----------------------------------------------------------------------
      # 3) Extract HV rates with robust fallbacks
      # -----------------------------------------------------------------------
      # Primary source (preferred):
      #   agent_stats$hv_rate usually encodes HV *conditional on no-stressor* steps.
      stats <- sim$agent_stats
      hv_rate_stat <- if ("hv_rate" %in% names(stats)) {
        dplyr::coalesce(mean(stats$hv_rate, na.rm = TRUE), NA_real_)
      } else {
        NA_real_
      }

      # Secondary source:
      #   agent_data$hv is a per-step indicator (TRUE when High+no-stressor).
      #   Some older/alternative schemas may store this as "Hypervigilance".
      agent_data <- sim$agent_data
      hv_vec <- if ("hv" %in% names(agent_data)) {
        as.numeric(agent_data$hv)
      } else if ("Hypervigilance" %in% names(agent_data)) {
        as.numeric(as.logical(agent_data$Hypervigilance))
      } else {
        NULL
      }

      # Stressor indicator is needed to compute HV specifically on no-stressor steps.
      # Again: schema can vary (stressor vs Stressor).
      stressor_vec <- if ("stressor" %in% names(agent_data)) {
        as.integer(agent_data$stressor)
      } else if ("Stressor" %in% names(agent_data)) {
        as.integer(agent_data$Stressor)
      } else {
        NULL
      }

      # hv_any:
      #   If hv_vec exists, take its mean across all recorded steps.
      #   If not, fall back to hv_rate_stat.
      #
      # Note: hv_any is used as a â€œbest availableâ€ HV estimate if stats are missing.
      hv_any <- if (!is.null(hv_vec)) {
        dplyr::coalesce(mean(hv_vec, na.rm = TRUE), hv_rate_stat)
      } else {
        hv_rate_stat
      }

      # hv_no_stressor:
      #   The most interpretable metric for Fig2B: mean(hv | stressor == 0).
      #   If stressor_vec is missing, we fall back to hv_any.
      hv_no_stressor <- if (!is.null(hv_vec) && !is.null(stressor_vec)) {
        dplyr::coalesce(mean(hv_vec[stressor_vec == 0L], na.rm = TRUE), hv_any)
      } else {
        hv_any
      }

      # -----------------------------------------------------------------------
      # 4) Emit a single-row summary for this cell
      # -----------------------------------------------------------------------
      # We keep both:
      #   - hv_rate: â€œcanonicalâ€ (prefer agent_stats hv_rate), else fallback
      #   - hv_rate_no_stressor: explicitly conditional estimate
      data.frame(
        model_id = spec$model_id,
        model_label = spec$model_label,
        model_type = spec$model_type,
        env_label = env$env_label,
        LA = env$LA,
        LL = env$LL,
        K = combo$K,

        # hv_rate: prefer per-agent hv_rate from stats, otherwise compute from hv vector.
        hv_rate = dplyr::coalesce(hv_rate_stat, hv_any, 0),

        # hv_rate_no_stressor: best-effort HV conditional on stressor absence.
        hv_rate_no_stressor = dplyr::coalesce(hv_no_stressor, hv_rate_stat, hv_any, 0)
      )
    }
  )

  # ---- Combine all single-row outputs into one tidy frame ---------------------
  result <- dplyr::bind_rows(rows)

  # ---- Collect h0 values (health model) for subtitles/captions ----------------
  # collect_h0_values() is a shared helper that searches lists for an h0 field.
  # This allows your plotting scripts to display (or log) which health grid
  # resolution was used without manually wiring it everywhere.
  h0_values <- unique(c(
    collect_h0_values(model_specs$policy_args),
    collect_h0_values(model_specs$sim_args)
  ))

  # Build subtitle metadata only if we found h0 values.
  subtitle_meta <- if (!is.null(h0_values)) {
    list(policy_args = list(h0 = h0_values))
  } else {
    NULL
  }

  # ---- Attach fixed-parameter metadata ---------------------------------------
  # This mirrors your other pipelines: rather than repeating constants in each row,
  # we attach a compact meta list for captions, logging, and reproducibility.
  fixed_params <- list(
    env_scenarios = env_scenarios,
    K_values = K_values,
    C = C, D = D, d = d,
    T_steps = T_steps,
    states = states,
    N_agents = N_agents
  )
  if (!is.null(subtitle_meta)) fixed_params$subtitle_meta <- subtitle_meta
  attr(result, "fixed_params") <- fixed_params

  result
}

# -----------------------------------------------------------------------------
# env_condition_texts(): human-readable environment row descriptions
# -----------------------------------------------------------------------------
# Purpose:
#   Provide a compact textual description of each canonical environment row so:
#     - figure captions can embed the LA/LL parameterization,
#     - tables can include a â€œlegendâ€ of environment meaning,
#     - debugging output can identify which row corresponds to which scenario.
#
# How it works:
#   - Requires LA, LL, env_label
#   - Uses env_full if present; otherwise repeats env_label
#   - Computes:
#       SSP  = LA / (LA + LL)  (if SSP column absent)
#       autocorr = 1 - (LA + LL)
#   - Returns one formatted string per environment row.
# -----------------------------------------------------------------------------
#' Provide text descriptions for each canonical environment row.
#'
#' @param env_scenarios Data frame with LA, LL, env_label (and optional env_full, SSP).
#' @return Character vector with one row per environment.
#' @export
env_condition_texts <- function(env_scenarios = default_health_env_scenarios()) {
  stopifnot(is.data.frame(env_scenarios))

  required <- c("LA", "LL", "env_label")
  stopifnot(all(required %in% names(env_scenarios)))

  # Prefer verbose descriptions if provided.
  env_full <- if ("env_full" %in% names(env_scenarios)) env_scenarios$env_full else env_scenarios$env_label

  # SSP may be precomputed upstream; if not, compute it from LA/LL.
  # Note: if LA+LL == 0, this would be NaN; in canonical environments this usually doesnâ€™t happen.
  ssp_vals <- if ("SSP" %in% names(env_scenarios)) {
    env_scenarios$SSP
  } else {
    env_scenarios$LA / (env_scenarios$LA + env_scenarios$LL)
  }

  # Autocorrelation proxy used throughout your paper for predictability/persistence.
  autocorr <- 1 - (env_scenarios$LA + env_scenarios$LL)

  sprintf(
    "%s = %s (LA = %.3f, LL = %.3f, SSP = %.3f, autocorr = %.3f)",
    env_scenarios$env_label,
    env_full,
    env_scenarios$LA,
    env_scenarios$LL,
    ssp_vals,
    autocorr
  )
}

# -----------------------------------------------------------------------------
# Transformation notes (pipeline summary)
# -----------------------------------------------------------------------------
# - hv_matrix_over_env():
#     For each (model spec Ã— environment Ã— K):
#       1) solve the DP policy,
#       2) simulate N_agents agents,
#       3) summarise HV rates with schema-robust fallbacks,
#       4) attach fixed-parameter metadata for reproducibility.
#
# - env_condition_texts():
#     Produces readable environment descriptors that include LA/LL/SSP/autocorr.
#
# - collect_h0_values():
#     Imported from utils_subtitles.R so that health-model â€œh0â€ settings are
#     captured in panel subtitles without duplicating logic across modules.

