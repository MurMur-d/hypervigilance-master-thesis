# ==================================================================================================
# File: prep_symmetry_risk_autocorr.R
#
# Purpose:
#   Generate the “symmetry / risk / autocorrelation” tidy tables used by the
#   symmetry figures and pipelines.
#
#   This file provides *data prep* helpers that:
#     1) Sweep grids of environment parameters (LA, LL) defined through
#        either:
#          - SSR (steady-state risk proxy) at a fixed total transition rate, or
#          - a symmetric constraint LA == LL (autocorrelation proxy)
#     2) For each grid cell and each vigilance cost K:
#          - Solve the optimal DP policy (cached)
#          - Simulate agents under that policy (cached)
#          - Compute canonical hypervigilance rates (using hv_rate_helpers.R)
#     3) Return a tidy data frame annotated with enough metadata to plot
#        without re-deriving sweep settings.
#
# Inputs:
#   - DP policy + simulation helpers (basic + health) called via mem_* wrappers
#   - Scenario lists from utils_model_scenarios.R (e.g., default_symmetry_model_scenarios)
#   - Canonical HV metric computation via hv_rate_helpers.R
#
# Outputs:
#   - risk_grid_K_vs_SSR():           tidy table for K × SSR sweeps
#   - risk_grid_K_vs_SSR_by_model():  stacked version across model scenarios (adds model_label/model_id)
#   - symmetry_grid_K_vs_autocorr():  tidy table for K × autocorr sweeps with LA == LL
#   - symmetry_grid_K_vs_autocorr_by_model(): stacked version across model scenarios
#
# Notes / design philosophy:
#   - Pure data prep: no file I/O and no plotting in this module.
#   - Uses memoised DP and simulation calls (mem_compute_policy / mem_simulate_agents)
#     so repeated plotting runs do not re-simulate expensive grids.
#   - Attaches `attr(df, "meta")` (and sometimes `attr(df, "row_levels")`)
#     to keep captions/faceting deterministic and reviewer-friendly.
# ==================================================================================================

# ---- Dependencies -------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)  # bind_rows(), filter(), mutate(), coalesce(), etc.
})

# We source both basic and health model DP/SIM files so the compute/sim backends exist
# when mem_compute_policy() / mem_simulate_agents() dispatch based on `model`.
source("R/models/basic/basic_model_dp.R")
source("R/models/basic/basic_model_SIM.R")
source("R/models/health/health_model_dp.R")
source("R/models/health/health_model_SIM.R")

# Scenario registries and argument helpers (e.g., default_symmetry_model_scenarios)
# NOTE: this file likely defines merge_autocorr_args (used later). If it doesn’t,
# you’ll want to route that call to merge_risk_args for consistency.
source("R/helpers/utils_model_scenarios.R")

# Canonical hypervigilance metrics (pooled safe/no-stressor definitions)
source("R/core/hv_rate_helpers.R")


# ================================================================================================
# Helper: merge_risk_args()
# ================================================================================================
# Each scenario may provide its own policy_args / sim_args overrides (e.g., terminal reward settings).
# We also support “base args” (defaults) provided by the caller.
#
# Rule:
#   - If override_args is NULL: keep base_args unchanged
#   - Otherwise: override base_args keys with override_args keys (modifyList semantics)
# ================================================================================================

#' Merge base + scenario-specific arguments for risk runs.
#'
#' @param base_args list of defaults
#' @param override_args scenario overrides (can be NULL)
#' @return merged list with override values taking precedence
merge_risk_args <- function(base_args, override_args) {
  if (is.null(base_args)) base_args <- list()
  if (is.null(override_args)) return(base_args)
  modifyList(base_args, override_args)
}

# Alias: use the same scenario list as “symmetry” by default for risk figures too.
# (Kept as a separate name so future refactors can decouple these lists.)
default_risk_model_scenarios <- default_symmetry_model_scenarios


# ================================================================================================
# risk_grid_K_vs_SSR()
# ================================================================================================
# What is SSR here?
#   SSR is used as a convenient 0..1 slider that determines the balance between LA and LL
#   given a fixed total transition rate:
#
#     total_rate = LA + LL
#     SSR        = LA / (LA + LL)   (conceptually)
#
# Implementation:
#   We construct LA and LL directly:
#     LA = SSR * total_rate
#     LL = (1 - SSR) * total_rate
#
# Autocorrelation proxy:
#   In this project, autocorr is often used as:
#     autocorr = 1 - (LA + LL)
#   (bounded later for plotting, but here it’s computed directly and then rounded).
#
# Core output:
#   A tidy table with one row per (K × SSR) point, containing:
#     - LA, LL, SSR, autocorr
#     - HypervigilanceRate_all
#     - HypervigilanceRate_filtered  (pooled across no-stressor rows)
# ================================================================================================

## Computes SSR (steady-state risk) grid for a single model/policy configuration.
## Walks over every K × SSR combination, runs the DP policy, simulates agents, and records the
## average hypervigilance rates both overall and filtered to the no-stressor branch.
## The returned table carries `attr(..., "meta")` so downstream helpers know the sweep settings.
risk_grid_K_vs_SSR <- function(
    C, d, deltaD,                 # costs/damages: relaxed baseline (C), relaxed damage (d), vigilant damage (deltaD = D)
    K_values,                     # vector of vigilance costs to sweep
    T_steps, states, N_agents,     # DP horizon, DP state labels, number of simulated agents per cell
    ssr_step = 0.05,              # resolution for SSR in [0,1]
    total_rate = 0.5,             # fixed LA + LL total transition rate
    model = c("basic", "health"), # which model backend to use
    policy_args = list(),         # forwarded to mem_compute_policy() for health variants
    sim_args = list()             # forwarded to mem_simulate_agents() for health variants
) {
  model <- match.arg(model)

  # ---- Step 1: build the SSR grid and derive LA/LL -----------------------------------------------
  # SSR is sampled on [0,1] with the chosen resolution.
  ssr_seq <- seq(0, 1, by = ssr_step)

  # Convert SSR to LA/LL under the fixed total transition rate.
  #   - When SSR = 0:   LA = 0,       LL = total_rate
  #   - When SSR = 1:   LA = total_rate, LL = 0
  base_grid <- data.frame(
    SSR = ssr_seq,
    LA  = ssr_seq * total_rate,
    LL  = (1 - ssr_seq) * total_rate
  )

  # Autocorrelation proxy used across the project.
  # (Higher LA+LL => more frequent switching => lower autocorr.)
  base_grid$autocorr <- 1 - (base_grid$LA + base_grid$LL)

  # ---- Step 2: keep only canonical LA/LL ranges -------------------------------------------------
  # Many environment plots in the project assume LA and LL are both within [0, 0.5].
  # If total_rate is large, LA or LL could exceed 0.5 at extremes; we drop those to
  # keep comparisons consistent with the canonical environment square.
  base_grid <- base_grid %>%
    dplyr::filter(LA >= 0, LL >= 0, LA <= 0.5, LL <= 0.5)

  # ---- Step 3: preallocate output container -----------------------------------------------------
  # We’ll fill a list of small data.frames then bind_rows at the end.
  # This avoids repeated rbind() (slow in base R loops).
  out_rows <- vector("list", length(K_values) * nrow(base_grid))
  row_id <- 0

  # ---- Step 4: sweep costs K and SSR grid -------------------------------------------------------
  for (K in K_values) {

    # Here deltaD is the vigilant damage magnitude D.
    # We keep the variable name `D` locally because downstream plotting expects it.
    D <- deltaD

    # Feasibility gate:
    # If vigilance cost + relaxed damage exceeds relaxed cost + vigilant damage, the problem
    # can become degenerate (depending on the DP reward structure used elsewhere).
    # This is the same gate used in the LA/LL grid builders.
    if ((K + d) > (C + D)) next

    # Iterate each SSR point (equivalently each LA/LL point)
    for (i in seq_len(nrow(base_grid))) {
      LAi  <- base_grid$LA[i]
      LLi  <- base_grid$LL[i]
      SSRi <- base_grid$SSR[i]

      # ---- Step 4A: compute optimal policy (cached) ---------------------------------------------
      # mem_compute_policy() handles:
      #   - selecting the correct backend (basic vs health)
      #   - injecting policy_args when needed
      #   - caching the DP solution on disk keyed by parameters
      pol <- mem_compute_policy(
        model = model, K = K, C = C, D = D, d = d,
        LA = LAi, LL = LLi, T_steps = T_steps, states = states,
        policy_args = policy_args
      )

      # ---- Step 4B: simulate agents following the policy (cached) -------------------------------
      # mem_simulate_agents() similarly:
      #   - chooses the simulator backend
      #   - includes sim_args when needed
      #   - caches simulation outputs keyed by env/cost/policy signature
      sim <- mem_simulate_agents(
        model = model, policy_df = pol,
        LA = LAi, LL = LLi, T_steps = T_steps, N_agents = N_agents,
        K = K, C = C, D = D, d = d, sim_args = sim_args
      )

      # ---- Step 4C: compute canonical HV metrics ------------------------------------------------
      # IMPORTANT: We do not rely on sim$agent_stats$hv_rate here because:
      #   - some pipelines define hv_rate differently
      #   - hv_rate_helpers.R enforces one canonical definition across the whole project
      ad <- sim$agent_data

      hv_rates <- compute_hv_rates_from_agent_data(ad, T_steps = T_steps, N_agents = N_agents)
      h_all <- hv_rates$HypervigilanceRate_all
      h_fil <- hv_rates$HypervigilanceRate_filtered

      # ---- Step 4D: record one tidy output row --------------------------------------------------
      row_id <- row_id + 1
      out_rows[[row_id]] <- data.frame(
        K = as.integer(round(K)),
        D = D,
        LA = LAi,
        LL = LLi,
        SSR = round(SSRi, 3),
        autocorr = round(1 - (LAi + LLi), 3),
        HypervigilanceRate_all      = dplyr::coalesce(h_all, 0),
        HypervigilanceRate_filtered = dplyr::coalesce(h_fil, 0)
      )
    }
  }

  # ---- Step 5: assemble final table and attach metadata -----------------------------------------
  out <- dplyr::bind_rows(out_rows[seq_len(row_id)])

  # The meta attribute is intentionally “caption-ready”:
  # plotting scripts can pull this and print the sweep settings without
  # hardcoding values again.
  attr(out, "meta") <- list(
    C = C, d = d, D = deltaD, T_steps = T_steps, N_agents = N_agents,
    mode = model, ssr_step = ssr_step, total_rate = total_rate,
    K_values = K_values, policy_args = policy_args, sim_args = sim_args
  )

  out
}


# ================================================================================================
# risk_grid_K_vs_SSR_by_model()
# ================================================================================================
# This is the “scenario stacker”:
#   - loops over each model_scenario (basic/health variants)
#   - merges base args with scenario-specific policy/sim args
#   - runs risk_grid_K_vs_SSR()
#   - tags rows with model_label/model_id/model so plotting can facet by scenario
#
# Output:
#   A stacked tidy frame with factor ordering preserved via row_levels.
# ================================================================================================

## Repeats `risk_grid_K_vs_SSR()` for each scenario defined in `helpers/model_scenarios.R`,
## attaching a model label/id so the caller can facet over variants.
risk_grid_K_vs_SSR_by_model <- function(
    model_scenarios = default_risk_model_scenarios,
    C, d, deltaD, K_values,
    T_steps, states, N_agents,
    ssr_step = 0.05,
    total_rate = 0.5,
    base_policy_args = list(),
    base_sim_args = list()
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (length(model_scenarios) == 0) stop("model_scenarios must have at least one entry")

  # ---- Step 1: define deterministic row labels for plotting facets -------------------------------
  # If scenarios have a label field, use it; otherwise invent “model i” names.
  row_levels <- vapply(seq_along(model_scenarios), function(i) {
    lbl <- model_scenarios[[i]]$label
    if (is.null(lbl) || !nzchar(lbl)) paste0("model ", i) else lbl
  }, character(1))

  # ---- Step 2: run each scenario and tag its rows -----------------------------------------------
  rows <- lapply(seq_along(model_scenarios), function(i) {
    scenario <- model_scenarios[[i]]

    # Model selection:
    # Most scenario objects store `model` ("basic" or "health") but we default to "health"
    # when missing to match other project conventions.
    model <- if (is.null(scenario$model)) "health" else scenario$model

    # Merge base args with scenario overrides (scenario wins).
    pol_args <- merge_risk_args(base_policy_args, scenario$policy_args)
    sim_args <- merge_risk_args(base_sim_args, scenario$sim_args)

    # Run the core SSR sweep for this scenario.
    df <- risk_grid_K_vs_SSR(
      C = C, d = d, deltaD = deltaD, K_values = K_values,
      T_steps = T_steps, states = states, N_agents = N_agents,
      ssr_step = ssr_step, total_rate = total_rate,
      model = model, policy_args = pol_args, sim_args = sim_args
    )
    if (nrow(df) == 0) return(NULL)

    # Tag with scenario identifiers used by plotting scripts.
    df$model_label <- row_levels[i]
    df$model_id <- i
    df$model <- model
    df
  })

  # Drop NULL scenarios (e.g., if they produced no feasible rows).
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())

  out <- dplyr::bind_rows(rows)

  # Keep facet order stable and only include labels that actually appear.
  row_levels <- row_levels[row_levels %in% out$model_label]
  if (length(row_levels) == 0) row_levels <- unique(as.character(out$model_label))
  out$model_label <- factor(out$model_label, levels = row_levels)

  # Attach facet ordering and global meta so plots can inherit settings consistently.
  attr(out, "row_levels") <- levels(out$model_label)
  attr(out, "meta") <- list(
    C = C, d = d, D = deltaD, T_steps = T_steps, N_agents = N_agents,
    mode = if (length(unique(out$model)) == 1) unique(out$model) else "mixed",
    ssr_step = ssr_step, total_rate = total_rate, K_values = K_values
  )
  out
}


# ================================================================================================
# symmetry_grid_K_vs_autocorr()
# ================================================================================================
# This is the “LA == LL” special case used to focus on autocorrelation as a 1D axis.
#
# Construction:
#   LA = LL = la_seq
#   autocorr = 1 - (LA + LL) = 1 - 2*LA
#
# For each (K, LA):
#   - compute policy
#   - simulate
#   - compute hv_rate_all + hv_rate_filtered
#
# Output:
#   tidy frame ready for heatmaps with:
#     x = autocorr, y = K (or vice versa)
# ================================================================================================

## Builds the symmetric-autocorrelation grid restricted to LA==LL values.
## This outputs the same hypervigilance rates as above but arranged by the autocorrelation proxy,
## enabling the heatmap at `R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R`.
symmetry_grid_K_vs_autocorr <- function(
    C, d, deltaD,
    K_values,
    T_steps, states, N_agents,
    step = 0.025,
    model = c("basic", "health"),
    policy_args = list(),
    sim_args = list()
) {
  model <- match.arg(model)

  # ---- Step 1: build the LA==LL grid -------------------------------------------------------------
  la_seq <- seq(0, 0.5, by = step)
  base_grid <- data.frame(LA = la_seq, LL = la_seq)
  base_grid$autocorr <- 1 - (base_grid$LA + base_grid$LL)  # = 1 - 2*LA

  # ---- Step 2: preallocate output ---------------------------------------------------------------
  out_rows <- vector("list", length(K_values) * nrow(base_grid))
  row_id <- 0

  # ---- Step 3: sweep K and the symmetric environment grid --------------------------------------
  for (K in K_values) {
    D <- deltaD
    if ((K + d) > (C + D)) next

    for (i in seq_len(nrow(base_grid))) {
      LAi <- base_grid$LA[i]
      LLi <- base_grid$LL[i]

      # DP solve (cached)
      pol <- mem_compute_policy(
        model = model, K = K, C = C, D = D, d = d,
        LA = LAi, LL = LLi, T_steps = T_steps, states = states,
        policy_args = policy_args
      )

      # Simulation (cached)
      sim <- mem_simulate_agents(
        model = model, policy_df = pol,
        LA = LAi, LL = LLi, T_steps = T_steps, N_agents = N_agents,
        K = K, C = C, D = D, d = d, sim_args = sim_args
      )

      # Canonical HV metrics
      ad <- sim$agent_data
      hv_rates <- compute_hv_rates_from_agent_data(ad, T_steps = T_steps, N_agents = N_agents)
      h_all <- hv_rates$HypervigilanceRate_all
      h_fil <- hv_rates$HypervigilanceRate_filtered

      # Record output row
      row_id <- row_id + 1
      out_rows[[row_id]] <- data.frame(
        K = as.integer(round(K)),
        D = D,
        LA = LAi,
        LL = LLi,
        autocorr = round(1 - (LAi + LLi), 3),
        HypervigilanceRate_all      = dplyr::coalesce(h_all, 0),
        HypervigilanceRate_filtered = dplyr::coalesce(h_fil, 0)
      )
    }
  }

  out <- dplyr::bind_rows(out_rows[seq_len(row_id)])

  # Attach sweep settings for reproducibility/captions
  attr(out, "meta") <- list(
    C = C, d = d, D = deltaD, T_steps = T_steps, N_agents = N_agents,
    mode = model, step = step, K_values = K_values,
    policy_args = policy_args, sim_args = sim_args
  )
  out
}


# ================================================================================================
# symmetry_grid_K_vs_autocorr_by_model()
# ================================================================================================
# Stacks the LA==LL autocorrelation sweep across scenarios, analogous to the SSR version.
#
# NOTE (potential cleanup):
#   This function calls merge_autocorr_args(), which is *not* defined in this file.
#   If utils_model_scenarios.R does not define it, you should replace it with merge_risk_args()
#   (or define merge_autocorr_args as an alias of merge_risk_args) to avoid runtime errors.
# ================================================================================================

## Mirrors `symmetry_grid_K_vs_autocorr()` over multiple model scenarios so the heatmap can
## compare the basic and health variants (or any custom scenario list defined/shared in helpers).
symmetry_grid_K_vs_autocorr_by_model <- function(
    model_scenarios = default_symmetry_model_scenarios,
    C, d, deltaD, K_values,
    T_steps, states, N_agents,
    step = 0.025,
    base_policy_args = list(),
    base_sim_args = list()
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (length(model_scenarios) == 0) stop("model_scenarios must have at least one entry")

  # Deterministic facet ordering
  row_levels <- vapply(seq_along(model_scenarios), function(i) {
    lbl <- model_scenarios[[i]]$label
    if (is.null(lbl) || !nzchar(lbl)) paste0("model ", i) else lbl
  }, character(1))

  rows <- lapply(seq_along(model_scenarios), function(i) {
    scenario <- model_scenarios[[i]]
    model <- if (is.null(scenario$model)) "health" else scenario$model

    # If merge_autocorr_args() exists in your project, keep it.
    # Otherwise you can safely replace both merge_autocorr_args calls with merge_risk_args.
    pol_args <- merge_autocorr_args(base_policy_args, scenario$policy_args)
    sim_args <- merge_autocorr_args(base_sim_args, scenario$sim_args)

    df <- symmetry_grid_K_vs_autocorr(
      C = C, d = d, deltaD = deltaD, K_values = K_values,
      T_steps = T_steps, states = states, N_agents = N_agents,
      step = step, model = model, policy_args = pol_args, sim_args = sim_args
    )
    if (nrow(df) == 0) return(NULL)

    df$model_label <- row_levels[i]
    df$model_id <- i
    df$model <- model
    df
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())

  out <- dplyr::bind_rows(rows)

  # Preserve facet order and attach metadata
  row_levels <- row_levels[row_levels %in% out$model_label]
  if (length(row_levels) == 0) row_levels <- unique(as.character(out$model_label))
  out$model_label <- factor(out$model_label, levels = row_levels)

  attr(out, "row_levels") <- levels(out$model_label)
  attr(out, "meta") <- list(
    C = C, d = d, D = deltaD, T_steps = T_steps, N_agents = N_agents,
    mode = if (length(unique(out$model)) == 1) unique(out$model) else "mixed",
    step = step, K_values = K_values
  )
  out
}

# --------------------------------------------------------------------------------------------------
# Summary (for reviewers)
#
# - risk_grid_K_vs_SSR*():
#     Enumerates each K × SSR pair, converts SSR to (LA, LL) under a fixed total transition rate,
#     solves the DP policy, simulates agents, and records hypervigilance rates along with SSR/autocorr.
#
# - risk_grid_K_vs_SSR_by_model():
#     Repeats the sweep over a scenario list, tagging each row with model_label/model_id so plots can
#     facet while retaining deterministic ordering.
#
# - symmetry_grid_K_vs_autocorr*():
#     Mirrors the above but constrains LA == LL to yield a 1D autocorrelation axis, enabling “K vs autocorr”
#     heatmaps and comparisons across model variants.
# --------------------------------------------------------------------------------------------------
