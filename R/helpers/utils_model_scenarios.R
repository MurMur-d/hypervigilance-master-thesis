# ===================================================================================================
# File: utils_model_scenarios.R
# Purpose: Centralise the scenario definitions and argument merges used by environment and symmetry sweeps.
#
# What problem this solves:
#   Many of the project’s figures are *comparative*: they rerun the same grid sweep
#   (over environments, costs, SSR, autocorrelation, etc.) under multiple model variants.
#   Without a single shared place to define those variants, it’s easy for:
#     - one pipeline to quietly compare a different set of models than another
#     - subtle parameter differences to creep in (e.g., a changed τ or α in one script)
#     - “basic vs health” comparisons to become inconsistent across figures/tables
#
# This file therefore:
#   1) Defines the canonical list(s) of model scenarios used across the project.
#   2) Provides a consistent, explicit merging rule for base args + scenario overrides.
#   3) Exposes aliases so downstream code can ask for "env scenarios" vs "symmetry scenarios"
#      without duplicating lists or logic.
#
# Design philosophy:
#   - Scenario lists are simple R lists-of-lists so they are easy to inspect and edit.
#   - The "label" is the human-facing string used in plot facets / table rows.
#   - The "model" string selects which DP/SIM backend is used ("basic" vs "health").
#   - "policy_args" stores only what differs from the base defaults; everything else comes
#     from the calling pipeline (e.g., h0, health_step, shuffle flags).
# ===================================================================================================


# ---- Scenario merging helpers ------------------------------------------------------------------
#' Merge base policy/simulation arguments with scenario-specific overrides.
#'
#' @param base_args list of base defaults (can be NULL)
#'   Think of this as the "shared defaults" applied to *all* scenarios in a sweep.
#'   Examples:
#'     - list(h0 = 35, health_step = 1)
#'     - list(shuffle = TRUE)
#'
#' @param override_args scenario-specific overrides (can be NULL)
#'   These are per-scenario differences that should override the base defaults.
#'   Examples:
#'     - list(terminal_reward_weight = 0)
#'     - list(terminal_reward_mode = "power", terminal_power_alpha = 3)
#'
#' @return list
#'   Combined list where `override_args` take precedence over `base_args`.
#'
#' @details
#'   - This uses `modifyList()` so only the explicitly provided override fields replace base values.
#'   - If either argument is NULL, it is treated as an empty list to keep calling code clean.
#'   - This helper is used both for policy arguments (DP) and simulation arguments (SIM)
#'     whenever a sweep wants a shared baseline plus per-scenario tweaks.
merge_model_args <- function(base_args, override_args) {
  # Standardize NULL → empty list so downstream calls are predictable.
  if (is.null(base_args)) base_args <- list()

  # If there are no overrides, return base args unchanged (fast path).
  if (is.null(override_args)) return(base_args)

  # modifyList merges recursively, with values from override_args replacing base_args
  # when they share the same key.
  modifyList(base_args, override_args)
}


#' Alias for `merge_model_args()` when autocorrelation scenarios need custom defaults.
#'
#' @inheritParams merge_model_args
#' @return merged list
#'
#' @rationale
#'   Some pipelines distinguish "environment sweeps" from "autocorrelation sweeps" for clarity,
#'   even when the underlying merge rule is identical. This alias lets those pipelines read as:
#'
#'     pol_args <- merge_autocorr_args(base_policy_args, scenario$policy_args)
#'
#'   which makes intent clearer to reviewers skimming the code.
merge_autocorr_args <- function(base_args, override_args) {
  merge_model_args(base_args, override_args)
}


# ---- Scenario definitions -----------------------------------------------------------------------
# The canonical scenario set used throughout the project.
#
# Structure of each scenario entry:
#   list(
#     label      = <human-readable name used in plots/tables>,
#     model      = <backend selector: "basic" or "health">,
#     policy_args = <DP-only overrides for that variant>
#     # (optionally sim_args could be included too, but here we keep this list DP-focused)
#   )
#
# Notes on naming:
#   - ω = terminal_reward_weight (how strongly end-of-horizon health is valued)
#   - α = terminal_power_alpha (curvature for the "power" terminal reward)
#   - τ = terminal_threshold_tau (switch point for the "threshold" terminal reward)
#
# Notes on what is *not* specified here:
#   - h0, health_step, shuffle, spread_initial_over_levels, etc.
#     Those are typically supplied as *base args* by the pipeline and merged in using
#     `merge_model_args()`. This keeps scenario definitions minimal and avoids repetition.
default_env_model_scenarios <- list(

  # 1) Basic model baseline ----------------------------------------------------
  list(
    # Label is what the reader sees in facets/tables.
    label = "basic model",

    # Selects the basic DP/SIM functions.
    model = "basic",

    # No overrides: basic model has no terminal reward mechanics to configure.
    policy_args = list()
  ),

  # 2) Health model with *no* terminal reward ---------------------------------
  list(
    label = "no terminal reward (ω = 0)",
    model = "health",

    # Explicitly set ω = 0 and keep mode linear (mode doesn’t matter when ω=0,
    # but specifying it avoids ambiguity in logs and filenames).
    policy_args = list(
      terminal_reward_weight = 0,
      terminal_reward_mode   = "linear"
    )
  ),

  # 3) Health model with linear terminal reward -------------------------------
  list(
    label = "terminal reward (ω = 1, linear)",
    model = "health",

    # Standard linear terminal reward with unit weight.
    policy_args = list(
      terminal_reward_weight = 1,
      terminal_reward_mode   = "linear"
    )
  ),

  # 4) Health model with power terminal reward --------------------------------
  list(
    label = "power terminal reward (α = 3)",
    model = "health",

    # Power-form terminal reward; α controls curvature (fixed at 3 in this project).
    policy_args = list(
      terminal_reward_weight = 1,
      terminal_reward_mode   = "power",
      terminal_power_alpha   = 3
    )
  ),

  # 5) Health model with threshold terminal reward ----------------------------
  list(
    label = "threshold terminal reward (τ = 0.6·H0)",
    model = "health",

    # Threshold-form terminal reward; τ defaults to 0.6·H0 if not explicitly provided.
    policy_args = list(
      terminal_reward_weight  = 1,
      terminal_reward_mode    = "threshold"
    )
  )
)


# The symmetry and environment pipelines compare the same set of model variants.
# We keep separate names for readability, but point them to the same object so
# updates to the canonical scenarios automatically apply everywhere.
default_symmetry_model_scenarios <- default_env_model_scenarios

# Some pipelines use the shorter generic name.
default_model_scenarios <- default_env_model_scenarios
