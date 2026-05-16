#+ ==================================================================================================
#| File: prep_health_policy_mix.R
#| Purpose:
#|   Produce tidy “policy-action mix” tables that summarize how often each optimal
#|   action appears in the *policy table* (not simulations), split by:
#|     - time step
#|     - stressor vs no-stressor branch (based on state labels)
#|     - action category (High/Low/Tie → vigilant/relaxed/tie)
#|
#| Why this exists:
#|   Your health-policy figures (R/plotting/health_policy_bars/03_call_*, using `plot_health_policy_bars.R`) typically
#|   show bar grids: “At time t, in stressor states, what share of policy states
#|   recommends vigilance vs relaxation vs tie?”
#|   This file creates exactly that table in a consistent, reproducible format.
#|
#| Inputs:
#|   - mem_compute_policy() output for each (environment, cost, model variant)
#|   - environment scenario metadata (SSP/predictability grid)
#|
#| Outputs:
#|   - Tidy tibbles consumed by plotting scripts (no I/O here):
#|       time × stressor × action proportions, indexed by env and cost (and model variant)
#|
#| Notes:
#|   - This module does NOT simulate agents; it summarizes *policy tables*.
#|   - The “proportions” here are proportions of *policy states* (rows in the DP table),
#|     not proportions of agent-time behavior.
#|   - Inline commentary aims to make the aggregation steps reviewer-traceable.
#+ ==================================================================================================


# --------------------------------------------------------------------------------------------------
# Dependencies
# --------------------------------------------------------------------------------------------------
# dplyr: group_by/summarise/mutate/transmute
# tidyr: expand_grid/complete (ensures full grids for plotting)
# purrr: (imported; not used in these functions right now, but often used in nearby modules)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# Source model DP implementations so compute_optimal_policy_* functions exist.
# (mem_compute_policy() is assumed to be defined elsewhere, typically in plot_utils.R,
#  which should be sourced by the calling pipeline before using these helpers.)
source("R/models/basic/basic_model_dp.R")
source("R/models/health/health_model_dp.R")

# Environment scenario constructors (defines env_label, LA, LL)
source("R/plotting/_shared/utils_health_env_scenarios.R")

# Scenario utilities:
#   - default_model_scenarios, merge/modify args patterns, etc.
source("R/helpers/utils_model_scenarios.R")

# Checks:
#   - check_required_cols() ensures env_scenarios has expected column names
source("R/helpers/utils_checks.R")


# ===================================================================================================
# 1) health_policy_action_mix_data()
# ===================================================================================================

#' Computes proportion of optimal action states for a given set of health environments.
#'
#' Concept:
#'   For each environment and each vigilance cost K, we:
#'     1) compute the optimal policy table (DP)
#'     2) filter it to the time steps and state labels that the plot expects
#'     3) count how many policy states recommend each action (High/Low/Tie)
#'     4) convert those counts to proportions within each (time × stressor) slice
#'
#' Important interpretation note:
#'   “prop” here is NOT agent behavior. It is the fraction of DP *policy entries* that
#'   choose each action. This is what your policy-bar figures visualize.
#'
#' @param K_values numeric vector of vigilance costs to include (columns in plots)
#' @param env_scenarios tibble of SSP/predictability environments (must include env_label, LA, LL)
#' @param C,D,d numeric cost parameters
#' @param T_steps integer horizon (DP time index typically goes 0..T_steps)
#' @param states character vector of state labels to track (expected: c("K","Kd","C","CD"))
#' @param policy_args list forwarded to `mem_compute_policy()` (health model settings, terminal reward, etc.)
#' @param health_level optional health tier to filter (only relevant if policy table has health column)
#' @return tibble with one row per (K × env × time × stressor × action) containing prop
health_policy_action_mix_data <- function(
  K_values = c(1, 5, 9),
  env_scenarios = default_health_env_scenarios(),
  C = 0, D = 10, d = 0,
  T_steps = 10,
  states = c("K", "Kd", "C", "CD"),
  policy_args = list(),
  health_level = NULL
) {
  # --- Defensive checks -------------------------------------------------------
  # These stop early with readable errors instead of failing deep inside loops.
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr required")

  if (T_steps <= 1) stop("T_steps must be at least 2")
  stopifnot(is.data.frame(env_scenarios), nrow(env_scenarios) > 0)

  # Enforce required environment columns for consistent downstream plotting.
  check_required_cols(env_scenarios, c("env_label", "LA", "LL"))

  stopifnot(length(K_values) > 0)

  # ---------------------------------------------------------------------------
  # Step 1: Enumerate all (K, environment) combinations
  # ---------------------------------------------------------------------------
  # We build a “combos” table where each row is one specific DP policy request:
  #   - K (vigilance cost)
  #   - LA/LL (environment parameters)
  #   - env_label (plot facet label)
  #
  # Using expand_grid ensures we include every combination, even if some later
  # produce empty policies (we fill with zeros so plotting remains stable).
  combos <- tidyr::expand_grid(
    K = K_values,
    env_id = seq_len(nrow(env_scenarios))
  )

  # Attach environment metadata by indexing into env_scenarios.
  combos$env_label <- env_scenarios$env_label[combos$env_id]
  combos$LA <- env_scenarios$LA[combos$env_id]
  combos$LL <- env_scenarios$LL[combos$env_id]

  # ---------------------------------------------------------------------------
  # Step 2: Define the “complete grid” levels for plotting
  # ---------------------------------------------------------------------------
  # We want every output to have the same categorical levels so that:
  #   - bar plots can facet consistently
  #   - missing combinations don’t drop entire panels
  action_levels   <- c("High", "Low", "Tie")          # raw action labels expected in policy
  stressor_levels <- c("stressor", "no stressor")     # derived from state labels
  time_seq        <- seq_len(T_steps - 1)             # we typically plot times 1..(T-1)

  # ---------------------------------------------------------------------------
  # Step 3: Loop over each (K, env) combo and summarize the policy table
  # ---------------------------------------------------------------------------
  rows <- lapply(seq_len(nrow(combos)), function(i) {
    combo <- combos[i, ]

    # --- 3A: Compute optimal policy (cached) ---------------------------------
    # mem_compute_policy() should memoize policies so repeated plotting calls are fast.
    # Here we force model = "health" because this helper is explicitly for health-policy bars.
    pol <- mem_compute_policy(
      model = "health",
      K = combo$K, C = C, D = D, d = d,
      LA = combo$LA, LL = combo$LL,
      T_steps = T_steps,
      states = states,
      policy_args = policy_args
    )

    # --- 3B: Standardize relevant policy columns -----------------------------
    # In health DP outputs, we expect lowercase columns:
    #   time, label, optimal_action
    # We copy into a minimal df so later code is stable and easy to audit.
    df <- data.frame(
      time = pol$time,
      label = pol$label,
      optimal_action = pol$optimal_action,
      stringsAsFactors = FALSE
    )

    # --- 3C: Filter policy rows to the subset used in figures ----------------
    # Typical plotting convention:
    #   - ignore t=0 (initial boundary conditions) and t=T (terminal step)
    #   - keep only the four canonical labels that define stressor/no-stressor branches
    #   - drop NA actions (unresolved/unfilled policy entries)
    df <- df[
      df$time > 0 &
        df$time < T_steps &
        df$label %in% c("K", "Kd", "C", "CD") &
        !is.na(df$optimal_action),
      ,
      drop = FALSE
    ]

    # Optional: filter to a particular health tier (only when policy exposes it)
    if (!is.null(health_level) && "health" %in% names(pol)) {
      df <- df[df$health == health_level, , drop = FALSE]
    }

    # --- 3D: If no policy rows remain, emit a zero-filled complete grid -------
    # This is a plotting robustness trick:
    # - even if some combo yields no usable policy states, downstream facets should still exist
    # - so we create all (time × stressor × action) rows with prop=0
    if (nrow(df) == 0) {
      df <- tidyr::expand_grid(
        time = time_seq,
        stressor = stressor_levels,
        optimal_action = action_levels
      )
      df$count <- 0
      df$total_states <- 0
      df$prop <- 0
    } else {

      # --- 3E: Convert state label → stressor/no-stressor branch --------------
      # Project convention:
      #   - labels Kd and CD correspond to “stressor present” branches
      #   - labels K and C correspond to “no stressor”
      #
      # This allows plots that split bars by stressor branch.
      df <- df %>%
        dplyr::mutate(
          stressor = ifelse(label %in% c("Kd", "CD"), "stressor", "no stressor")
        ) %>%

        # --- 3F: Count how many policy states choose each action --------------
        # This counts policy-table entries (not agents).
        dplyr::group_by(time, stressor, optimal_action) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%

        # --- 3G: Ensure the full grid exists (complete) -----------------------
        # complete() will add missing combinations and fill count = 0.
        # This is crucial because if an action never occurs at a given time/stressor,
        # it would otherwise be missing and could break stacked bars.
        tidyr::complete(
          time = time_seq,
          stressor = stressor_levels,
          optimal_action = action_levels,
          fill = list(count = 0)
        ) %>%

        # --- 3H: Convert counts to proportions within each time×stressor slice
        dplyr::group_by(time, stressor) %>%
        dplyr::mutate(
          total_states = sum(count),
          prop = ifelse(total_states > 0, count / total_states, 0)
        ) %>%
        dplyr::ungroup()
    }

    # --- 3I: Attach identifiers and recode actions to plot-friendly labels ----
    # We return tidy columns expected by bar-grid scripts:
    #   action ∈ {vigilant, relaxed, tie}
    df %>%
      dplyr::transmute(
        K = combo$K,
        env_label = combo$env_label,
        LA = combo$LA,
        LL = combo$LL,
        time = time,
        action = dplyr::recode(optimal_action,
          High = "vigilant",
          Low  = "relaxed",
          Tie  = "tie"
        ),
        stressor = stressor,
        prop = prop
      )
  })

  # Stack all combos into one tidy data frame
  dplyr::bind_rows(rows)
}


# ===================================================================================================
# 2) health_policy_action_mix_data_by_model()
# ===================================================================================================

#' Similar to `health_policy_action_mix_data()` but loops over a list of model variants.
#'
#' When you want comparator panels (facet rows = model variants), you need to compute
#' policy-action mixes for each scenario. Each scenario may:
#'   - use a different model family (basic or health)
#'   - specify different policy_args (e.g., terminal reward settings)
#'
#' This function:
#'   - iterates over (model_scenario × environment)
#'   - computes the optimal policy at one fixed K_value
#'   - aggregates policy states into action proportions (time × stressor × action)
#'
#' @param K_value vigilance cost used for all scenarios (single cost comparator)
#' @param model_scenarios list of model scenarios (each with label, model/model_type, policy_args)
#' @param env_scenarios tibble of environments (env_label, LA, LL)
#' @param C,D,d,T_steps,states cost and DP settings
#' @param base_policy_args defaults merged with scenario-specific policy args
#' @param health_level optional health tier filter
#' @return tibble with model × env × time × stressor × action proportions
health_policy_action_mix_data_by_model <- function(
  K_value = 5,
  model_scenarios = default_model_scenarios,
  env_scenarios = default_health_env_scenarios(),
  C = 0, D = 10, d = 0,
  T_steps = 10,
  states = c("K", "Kd", "C", "CD"),
  base_policy_args = list(),
  health_level = NULL
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr required")

  if (T_steps <= 1) stop("T_steps must be at least 2")
  stopifnot(is.data.frame(env_scenarios), nrow(env_scenarios) > 0, length(model_scenarios) > 0)

  # ---------------------------------------------------------------------------
  # Step 1: Extract stable per-scenario labels and model types
  # ---------------------------------------------------------------------------
  # model_labels: human-facing labels for plotting
  model_labels <- vapply(seq_along(model_scenarios), function(i) {
    ms <- model_scenarios[[i]]
    lbl <- ms$label
    if (is.null(lbl) || !nzchar(lbl)) paste0("model ", i) else lbl
  }, character(1))

  # model_types: which model family to call inside mem_compute_policy()
  # Supports either scenario$model or scenario$model_type, defaulting to "health".
  model_types <- vapply(seq_along(model_scenarios), function(i) {
    ms <- model_scenarios[[i]]
    mt <- NULL
    if (!is.null(ms$model)) mt <- ms$model
    else if (!is.null(ms$model_type)) mt <- ms$model_type
    if (is.null(mt) || !nzchar(mt)) "health" else as.character(mt)
  }, character(1))

  # ---------------------------------------------------------------------------
  # Step 2: Enumerate all (scenario, environment) combos
  # ---------------------------------------------------------------------------
  combos <- tidyr::expand_grid(
    model_id = seq_along(model_scenarios),
    env_id   = seq_len(nrow(env_scenarios))
  )

  # Attach scenario + environment metadata
  combos$model_label <- model_labels[combos$model_id]
  combos$model_type  <- model_types[combos$model_id]
  combos$env_label   <- env_scenarios$env_label[combos$env_id]
  combos$LA <- env_scenarios$LA[combos$env_id]
  combos$LL <- env_scenarios$LL[combos$env_id]

  # Define complete-grid levels (same as earlier helper)
  action_levels   <- c("High", "Low", "Tie")
  stressor_levels <- c("stressor", "no stressor")
  time_seq        <- seq_len(T_steps - 1)

  # ---------------------------------------------------------------------------
  # Step 3: Loop combos → compute policy → summarize action mix
  # ---------------------------------------------------------------------------
  rows <- lapply(seq_len(nrow(combos)), function(i) {
    combo <- combos[i, ]

    # Pull the scenario object and build policy args
    scenario <- model_scenarios[[combo$model_id]]

    # Decide which model engine to use for this scenario
    scenario_model <- if (is.null(combo$model_type) || !nzchar(combo$model_type)) {
      "health"
    } else {
      as.character(combo$model_type)
    }

    # Merge global base args with scenario overrides
    scenario_pa <- if (is.null(scenario$policy_args)) {
      base_policy_args
    } else {
      modifyList(base_policy_args, scenario$policy_args)
    }

    # --- 3A: Compute policy (cached) -----------------------------------------
    pol <- mem_compute_policy(
      model = scenario_model,
      K = K_value, C = C, D = D, d = d,
      LA = combo$LA, LL = combo$LL,
      T_steps = T_steps,
      states = states,
      policy_args = scenario_pa
    )

    # --- 3B: Normalize column naming across models ---------------------------
    # Some policies might come out with uppercase column names (older code paths).
    # We detect the correct column names and then build a standardized df.
    time_col  <- if ("time" %in% names(pol)) "time" else if ("Time" %in% names(pol)) "Time" else NULL
    label_col <- if ("label" %in% names(pol)) "label" else if ("Label" %in% names(pol)) "Label" else NULL
    act_col   <- if ("optimal_action" %in% names(pol)) "optimal_action" else if ("Optimal_Action" %in% names(pol)) "Optimal_Action" else NULL

    if (is.null(time_col) || is.null(label_col) || is.null(act_col)) {
      stop("Policy table missing expected columns")
    }

    df <- data.frame(
      time = pol[[time_col]],
      label = pol[[label_col]],
      optimal_action = pol[[act_col]],
      stringsAsFactors = FALSE
    )

    # Filter to mid-horizon + canonical labels + non-missing actions
    df <- df[
      df$time > 0 &
        df$time < T_steps &
        df$label %in% c("K", "Kd", "C", "CD") &
        !is.na(df$optimal_action),
      ,
      drop = FALSE
    ]

    # Optional: restrict to a health tier if the policy actually exposes it
    if (!is.null(health_level) && !is.null(pol$health)) {
      df <- df[df$health == health_level, , drop = FALSE]
    }

    # If empty after filtering, emit complete zero grid so plotting never drops panels
    if (nrow(df) == 0) {
      df <- tidyr::expand_grid(time = time_seq, stressor = stressor_levels, optimal_action = action_levels)
      df$count <- 0
      df$total_states <- 0
      df$prop <- 0
    } else {
      # Derive stressor branch and compute proportions (same logic as earlier helper)
      df <- df %>%
        dplyr::mutate(stressor = ifelse(label %in% c("Kd", "CD"), "stressor", "no stressor")) %>%
        dplyr::group_by(time, stressor, optimal_action) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
        tidyr::complete(
          time = time_seq,
          stressor = stressor_levels,
          optimal_action = action_levels,
          fill = list(count = 0)
        ) %>%
        dplyr::group_by(time, stressor) %>%
        dplyr::mutate(
          total_states = sum(count),
          prop = ifelse(total_states > 0, count / total_states, 0)
        ) %>%
        dplyr::ungroup()
    }

    # Attach scenario identifiers + return tidy output
    df %>%
      dplyr::transmute(
        model_label = combo$model_label,
        model_type  = combo$model_type,
        env_label   = combo$env_label,
        LA = combo$LA,
        LL = combo$LL,
        time = time,
        action = dplyr::recode(optimal_action, High = "vigilant", Low = "relaxed", Tie = "tie"),
        stressor = stressor,
        prop = prop
      )
  })

  dplyr::bind_rows(rows)
}

# --------------------------------------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------------------------------------
# - health_policy_action_mix_data():
#     *Health model only*, sweeps K_values across environments.
#     Returns time × stressor × action proportions for each (K, env).
#
# - health_policy_action_mix_data_by_model():
#     Sweeps model_scenarios across environments at one fixed K_value.
#     Useful for comparator panels faceted by model variant.
#
# Both helpers:
#   - standardize output to a full grid (complete()) so plotting is robust
#   - interpret “proportions” as fractions of policy states, not simulated behavior
# --------------------------------------------------------------------------------------------------
