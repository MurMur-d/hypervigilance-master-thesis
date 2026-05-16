# ===================================================================================================
# File: prep_policy_matrix_data.R
#
# Purpose:
#   Build the condensed “policy matrix” used in the BASIC DP policy panel.
#
#   Concretely, this module answers:
#     “For each canonical environment (LA, LL) and each vigilance cost K,
#      what action does the *optimal policy* prescribe in the stationary time step,
#      separately for the stressor vs no-stressor branches?”
#
# Inputs:
#   - Optimal policy tables returned by mem_compute_policy() (basic model by default)
#   - Canonical environments provided by the caller (env_scenarios: env_label, LA, LL)
#   - A sweep over vigilance costs K_values
#
# Outputs:
#   - policy_matrix_over_env() returns a tidy data frame with one row per:
#       (environment × K × branch)
#     containing:
#       env_label, LA, LL, K, label_group, optimal_action
#
#   - The returned object also stores sweep metadata in attr(result, "fixed_params")
#     so plotting scripts can report the configuration in captions/logs without
#     duplicating constants.
#
# Notes / design principles:
#   - Pure transformation module: no file I/O, no plotting.
#   - Uses mem_compute_policy() so DP solutions are cached on disk (fast reruns).
#   - Intentionally collapses multi-state actions into a single descriptor per branch:
#       High / Low / Tie
#     to keep the “matrix” interpretable and figure-friendly.
# ===================================================================================================

# ---- Dependencies -------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)   # bind_rows(), general data manipulation
})

# Project helpers:
#   - plot_utils.R defines mem_compute_policy() and get_model() wrappers + caching logic
#   - basic_model_dp.R defines the underlying DP solver used for the "basic" model
#   - basic_model_SIM.R is sourced for completeness (often paired with DP), though not called directly here
source("R/core/plot_utils.R")
source("R/models/basic/basic_model_dp.R")
source("R/models/basic/basic_model_SIM.R")


# ================================================================================================
# Helper: collapse a set of actions to a single “branch descriptor”
# ================================================================================================
# In the DP policy table, each time step includes multiple labels/states (K, Kd, C, CD).
# For the policy matrix figure, we want one action for:
#   - the stressor branch (Kd, CD)
#   - the no-stressor branch (K, C)
#
# But even within a branch, actions could vary across labels.
# This helper produces an interpretable summary:
#   - If the branch always picks a single action -> return that action
#   - If the branch contains multiple different actions -> return "Tie"
#   - If the branch has no valid actions -> NA
#
# IMPORTANT: This “Tie” is a *collapsed descriptor* for heterogeneity across labels,
# not necessarily the DP’s own optimal_action == "Tie" state.
# (If the DP uses "Tie" explicitly, that will appear as a unique value and will
#  be returned as-is if it is the only value.)
# ================================================================================================

#' Reduce a set of optimal actions to a single descriptor (High/Low/Tie).
#'
#' @param actions character vector of optimal_action values for a branch
#' @return "High", "Low", "Tie", or NA when no actions exist
agg_action_simple <- function(actions) {
  # Drop NA and coerce to character for safety (policy tables sometimes store factors)
  vals <- unique(stats::na.omit(as.character(actions)))

  if (length(vals) == 0) {
    # No action was available (e.g., missing policy rows)
    NA_character_
  } else if (length(vals) == 1) {
    # The branch is internally consistent: one action across its states
    vals
  } else {
    # The branch contains multiple different actions; collapse to “Tie”
    "Tie"
  }
}


# ================================================================================================
# Main API: policy_matrix_over_env()
# ================================================================================================
# This function builds the matrix used by the basic policy panel by sweeping:
#   env_scenarios × K_values
#
# For each cell:
#   1) Compute (cached) DP policy for that environment and cost
#   2) Take a *single time slice* (time == 1) to represent the stationary prescription
#   3) Group states into {stressor, no stressor}
#   4) Collapse the set of actions in each branch via agg_action_simple()
#
# The output is tidy and ready for ggplot faceting or tile plots:
#   x = env, y = K, fill = action, facet = branch, etc.
# ================================================================================================

#' Build the policy matrix by sweeping each canonical environment × vigilance cost.
#'
#' @param env_scenarios tibble with columns env_label, LA, LL
#' @param K_values numeric vector of vigilance costs to evaluate
#' @param C,D,d cost parameters forwarded to `mem_compute_policy()`
#' @param T_steps horizon length passed to DP
#' @param states DP state labels
#' @param model string ("basic" by default)
#' @param policy_args list forwarded to the DP helper
#'
#' @return data.frame with env_label, LA, LL, K, label_group, optimal_action;
#'         attr `fixed_params` holds the sweep configuration
policy_matrix_over_env <- function(
    env_scenarios,
    K_values    = c(1, 3, 5, 7, 9),
    C           = 0,
    D           = 10,
    d           = 0,
    T_steps     = 10,
    states      = c("K", "Kd", "C", "CD"),
    model       = "basic",
    policy_args = list()
) {

  # ---- Step 0: basic input validation ---------------------------------------
  # We require at least one environment row; the rest is assumed to be numeric.
  stopifnot(is.data.frame(env_scenarios), nrow(env_scenarios) > 0)

  # ---- Step 1: create the sweep index (env × K) -----------------------------
  # expand.grid() gives all combinations of environment rows and K values.
  # We store env_id (row index) rather than duplicating LA/LL immediately.
  combos <- expand.grid(
    env_id = seq_len(nrow(env_scenarios)),
    K = K_values,
    KEEP.OUT.ATTRS = FALSE
  )

  # ---- Step 2: loop over each combination and compute the collapsed matrix ---
  # This is written as lapply() for clarity and because each iteration is independent
  # (and mem_compute_policy() provides caching to keep it fast on reruns).
  rows <- lapply(seq_len(nrow(combos)), function(i) {

    combo <- combos[i, ]
    env <- env_scenarios[combo$env_id, ]

    # ---- Step 2A: compute the DP policy (cached) ----------------------------
    # mem_compute_policy() is the “cache wrapper”:
    #   - key is model + parameters + policy_args
    #   - returns a policy table (time × label × optimal_action, plus extras)
    pol <- mem_compute_policy(
      model = model,
      K = combo$K,
      C = C,
      D = D,
      d = d,
      LA = env$LA,
      LL = env$LL,
      T_steps = T_steps,
      states = states,
      policy_args = policy_args
    )

    # ---- Step 2B: choose the “stationary” time slice ------------------------
    # Many DP plots use a particular time (often t=1) as a representative slice
    # because:
    #   - early steps can be horizon-affected
    #   - later steps can contain terminal artifacts
    # Here we explicitly select time == 1.
    #
    # NOTE: This assumes the policy table has `time`, `label`, and `optimal_action`.
    # If column names vary across models, you’d need a detection helper like in other files.
    stationary <- pol[pol$time == 1, c("label", "optimal_action")]

    # ---- Step 2C: map labels → branch ---------------------------------------
    # The state space uses:
    #   - K, C   : “no stressor” branch
    #   - Kd, CD : “stressor” branch
    # We collapse these into a single categorical variable used by the policy matrix facet.
    stationary$label_group <- ifelse(
      stationary$label %in% c("Kd", "CD"),
      "stressor",
      "no stressor"
    )

    # ---- Step 2D: collapse multiple labels into one action per branch --------
    # aggregate() groups by label_group and applies agg_action_simple() to the
    # set of actions observed across the branch’s labels.
    collapsed <- aggregate(
      optimal_action ~ label_group,
      data = stationary,
      FUN = agg_action_simple
    )

    # ---- Step 2E: return a tidy row block for this env × K -------------------
    # We return one row per branch, so there will be 2 rows per env × K combination.
    data.frame(
      env_label      = env$env_label,
      LA             = env$LA,
      LL             = env$LL,
      K              = combo$K,
      label_group    = collapsed$label_group,
      optimal_action = collapsed$optimal_action,
      stringsAsFactors = FALSE
    )
  })

  # ---- Step 3: stack everything into one tidy data frame ---------------------
  result <- dplyr::bind_rows(rows)

  # ---- Step 4: attach metadata so plots/captions can report settings ---------
  # Instead of hardcoding K_values, D, T_steps, etc. again in plotting scripts,
  # we attach them once here and let downstream callers retrieve them.
  attr(result, "fixed_params") <- list(
    env_scenarios = env_scenarios,
    K_values      = K_values,
    C             = C,
    D             = D,
    d             = d,
    T_steps       = T_steps,
    states        = states,
    model         = model
  )

  result
}


#' Summary:
#' - We exhaustively sweep every canonical LA/LL × K combination, compute the cached policy,
#'   and snapshot the time==1 actions as a stationary reference point.
#' - The stressor vs no-stressor branches are collapsed to a single High/Low/Tie descriptor
#'   for figure-friendly matrix plotting.
#' - The returned data frame stores its sweep configuration in `attr(..., "fixed_params")`
#'   to reduce duplication and improve reproducibility in downstream plots/captions.