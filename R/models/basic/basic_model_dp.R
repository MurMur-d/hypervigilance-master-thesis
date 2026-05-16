# ==============================================================================
# File: R/models/basic/basic_model_dp.R
#
# BACKWARDS REASONING — Basic model
# This function computes the optimal vigilance policy using backward induction
# (dynamic programming) in a simple environment with a binary stressor.
# ------------------------------------------------------------------------------
# 1) MODEL EXPLAINED
# ------------------------------------------------------------------------------
# - Using backward induction, we compute the optimal policy (min-cost action)
#   and the expected future cost for every time and state.
#
# - At each time step t, the environment may have a stressor.
# - The agent chooses between High or Low vigilance, aiming to minimize
#   expected total cost over the time horizon.
# - Costs are immediate effort (action) +
#   possible damage if a stressor is present.
# - The environment has short memory:
#   the probability that a stressor appears or disappears depends only on
#   whether one was present in the last observed label:
#      - last label in {Kd, CD} -> stressor present
#                                -> P(stressor next) = 1 - P(leaving)
#      - last label in {K, C, PRIOR} -> stressor absent
#                                    -> P(stressor next) = P(appearing)
#
# Outputs
# - A tidy data.frame over (time, label) with:
#     optimal_action, expected cost (future_cost_oa),
#     and diagnostics (fc_high, fc_low).
#
# Notes
# - Time t=0 is a PRIOR decision before the first observed stressor.
# - The first stressor is drawn from the stationary prior LA/(LA+LL)
#   if LA+LL > 0, otherwise from 0.5.
# - Ties (equal expected costs) are recorded as "Tie"; a simulator can
#   break ties at random during forward simulation.
# - High refers to vigilant state, low to relaxed state (used intertchangably).
# ==============================================================================

# ==============================================================================
# 2) PROJECT SETUP — SOURCING SUPPORT
# ------------------------------------------------------------------------------
# We load the project setup: packages, caches, parallel plan, RNG seed, etc.
# ==============================================================================
source("R/core/setup_project.R")  # load project setup (parallel plan, caches, RNG seed)
source("R/models/basic/basic_model_SIM.R")   # forward simulator


# ==============================================================================
# 3) PARAMETERS
# ------------------------------------------------------------------------------
# - k_cost, c_cost : effort costs when no stressor is present (Hvigilant vs relaxed)
# - d_high, d_low  : damages when a stressor is present under vigilance vs relaxed
# - la_prob (LA)   : P(stressor appears | previously absent)
# - ll_prob (LL)   : P(stressor leaves  | previously present)
# - horizon        : number of decision steps/ time steps
# - states         : state labels (Kd = vigilant (high) with stressor present,
#                                  K = vigilant (high) without stressor,
#                                  CD = relaxed (low) with stressor present,
#                                  C = relaxed (low) without stressor)
# ==============================================================================


# ==============================================================================
# 4) FUNCTION SUMMARY — WHAT compute_optimal_policy() DOES
# ------------------------------------------------------------------------------
# Approach
# - We solve by backward induction over time and last-observed label.
# - At each (t, label), compare expected costs of choosing High vs Low.
# - Pick the action with the lower expected cost; record “Tie” if equal.
#
# Output
# - A data.frame with columns:
#     time, label, optimal_action, future_cost_oa, fc_high, fc_low, current_state
#   One row per (time, last-observed-label). The row time=0 summarizes the PRIOR.
# ==============================================================================

# compute_optimal_policy(...) ---------------------------------------------------
compute_optimal_policy <- function(
  k_cost, c_cost, d_high, d_low,                  # costs (effort and damages)
  la_prob, ll_prob,                               # environment probabilities
  horizon,                                        # planning horizon
  states = c("Kd", "K", "CD", "C")                # state labels
) {

  # ============================================================================
  # 5) STEP 0 — BASIC VALIDATION
  # ----------------------------------------------------------------------------
  # We check for sensible inputs up front to avoid silent mistakes later.
  # ============================================================================
  stopifnot(k_cost >= 0, c_cost >= 0, d_high >= 0, d_low >= 0)              # non-negative costs
  stopifnot(la_prob >= 0, la_prob <= 0.5, ll_prob >= 0, ll_prob <= 0.5)     # keep probs in stated range
  stopifnot(horizon >= 1)                                                   # horizon must be at least 1
  # sanity check: High-with-stressor should not be worse than Low-with-stressor
  if ((k_cost + d_low) > (c_cost + d_high))
    stop("Unrealistic tuple: require k_cost + d_low <= c_cost + d_high.")

  # ============================================================================
  # 6) STEP 1 — STATE CATEGORIES AND TIMELINE
  # ----------------------------------------------------------------------------
  # Labels are the last observed labels (K, Kd, C, CD). Time runs 0..horizon;
  # time 0 is PRIOR (decision before first observed stressor).
  # ============================================================================
  labels <- states
  times  <- 0:horizon

  # ============================================================================
  # 7) STEP 2 — ALLOCATE DP TABLES
  # ----------------------------------------------------------------------------
  # v_mat : expected future cost (value) for each (time, label)
  # a_mat : optimal action (High/Low/Tie) at each (time, label)
  # fh/fl : expected cost if choose High / Low now
  # ============================================================================
  v_mat <- matrix(
    0,
    nrow = length(times),
    ncol = length(labels),
    dimnames = list(Time = as.character(times), Label = labels)
  )
  a_mat <- matrix(
    NA_character_,
    nrow = nrow(v_mat), ncol = ncol(v_mat),
    dimnames = dimnames(v_mat)
  )
  fh_mat <- matrix(NA_real_, nrow = nrow(v_mat), ncol = ncol(v_mat), dimnames = dimnames(v_mat))
  fl_mat <- matrix(NA_real_, nrow = nrow(v_mat), ncol = ncol(v_mat), dimnames = dimnames(v_mat))

  # ============================================================================
  # 8) STEP 3 — STRESSOR PROBABILITY HELPER
  # ----------------------------------------------------------------------------
  # Short memory:
  #   If last_label ∈ {Kd, CD} → stressor was present → P(stressor next) = 1 - LL
  #   Else                    → stressor was absent  → P(stressor next) = LA
  # ============================================================================
  stressor_prob <- function(last_label) {
    if (last_label %in% c("Kd", "CD")) 1 - ll_prob else la_prob
  }

  # ============================================================================
  # 9) STEP 4 — IMMEDIATE OUTCOMES PER ACTION
  # ----------------------------------------------------------------------------
  # For each action (High/Low) and whether a stressor occurs (present / absent),
  # we specify:
  #   - immediate cost (effort + possible damage)
  #   - the next label (deterministic)
  # ============================================================================
  step_actions <- list(
    High = list(
      cost_str = k_cost + d_low,   # stressor present & choose High
      cost_nst = k_cost,           # no stressor & choose High
      next_str = "Kd",             # next label if stressor present
      next_nst = "K"               # next label if no stressor
    ),
    Low = list(
      cost_str = c_cost + d_high,  # stressor present & choose Low
      cost_nst = c_cost,           # no stressor & choose Low
      next_str = "CD",             # next label if stressor present
      next_nst = "C"               # next label if no stressor
    )
  )

  # ============================================================================
  # 10) STEP 5 — TERMINAL BOUNDARY AT HORIZON
  # ----------------------------------------------------------------------------
  # At the final time (t = horizon) there are no future steps, so future cost = 0.
  # We mark actions/cost diagonistics as NA at the terminal time.
  # ============================================================================
  v_mat[as.character(horizon), ]  <- 0             # future cost = 0 at horizon
  a_mat[as.character(horizon), ]  <- NA_character_ # optimal action undefined at horizon
  fh_mat[as.character(horizon), ] <- NA_real_
  fl_mat[as.character(horizon), ] <- NA_real_

  # ============================================================================
  # 11) STEP 6 — BACKWARD INDUCTION (CORE DP LOOP)
  # ----------------------------------------------------------------------------
  # For each time t (from horizon-1 down to 1) and each label,
  # Choose the action with the smaller expected cost (or "Tie" if equal).
  # ============================================================================
  for (time_idx in seq(horizon - 1, 1, by = -1)) {  # move backwards through time
    for (label in labels) {                         # evaluate each possible last label
      p_prob <- stressor_prob(label)                # P(stressor appears at next step)         

      # Expected cost if we choose High now:
      #   = P      * (immediate cost with stressor + future cost from next label "Kd")
      #   + (1 - P)* (immediate cost without stressor + future cost from next label "K")
      fh <- p_prob * (step_actions$High$cost_str + v_mat[as.character(time_idx + 1), step_actions$High$next_str]) +
        (1 - p_prob) * (step_actions$High$cost_nst + v_mat[as.character(time_idx + 1), step_actions$High$next_nst])

      # Expected cost if we choose Low now (same structure, different costs/labels)
      fl <- p_prob * (step_actions$Low$cost_str  + v_mat[as.character(time_idx + 1), step_actions$Low$next_str]) +
        (1 - p_prob) * (step_actions$Low$cost_nst  + v_mat[as.character(time_idx + 1), step_actions$Low$next_nst])

      # Record per-action expected costs for checks
      fh_mat[as.character(time_idx), label] <- fh
      fl_mat[as.character(time_idx), label] <- fl

      # Pick the cheaper action; treat very small numerical differences as a tie
      if (abs(fh - fl) < 1e-12) {
        a_mat[as.character(time_idx), label] <- "Tie"
        v_mat[as.character(time_idx), label] <- fh     # either value works (they're equal)
      } else if (fl < fh) {                            # Low is better
        a_mat[as.character(time_idx), label] <- "Low"
        v_mat[as.character(time_idx), label] <- fl
      } else {                                         # High is better                
        a_mat[as.character(time_idx), label] <- "High"
        v_mat[as.character(time_idx), label] <- fh
      }
    }
  }

  # ============================================================================
  # 12) STEP 6b — SPECIAL HANDLING FOR TIME 0 (PRIOR)
  # ----------------------------------------------------------------------------
  # At time 0 there is no last observed label. We use the stationary prior
  # probability that the environment has a stressor at the first realization.
  # ============================================================================
  p0  <- if ((la_prob + ll_prob) == 0) 0.5 else la_prob / (la_prob + ll_prob)
  fh0 <- p0 * (step_actions$High$cost_str + v_mat["1", step_actions$High$next_str]) +
    (1 - p0) * (step_actions$High$cost_nst + v_mat["1", step_actions$High$next_nst])
  fl0 <- p0 * (step_actions$Low$cost_str  + v_mat["1", step_actions$Low$next_str]) +
    (1 - p0) * (step_actions$Low$cost_nst  + v_mat["1", step_actions$Low$next_nst])
  a0  <- if (abs(fh0 - fl0) < 1e-9) "Tie" else if (fl0 < fh0) "Low" else "High"
  v0  <- min(fh0, fl0)

  # ============================================================================
  # 13) STEP 7 — BUILD TIDY OUTPUT TABLE
  # ----------------------------------------------------------------------------
  # We return one row per (time, label) with the optimal action and expected cost.
  # The PRIOR row (time = 0) summarizes the initial decision.
  # ============================================================================
  out_core <- expand.grid(
    time  = 1:horizon,          # times 1..horizon (PRIOR added separately)
    label = labels,
    stringsAsFactors = FALSE
  )

  # Fill from DP tables (index by (time, label))
  out_core$optimal_action <- mapply(function(tt, lab) a_mat[as.character(tt), lab], out_core$time, out_core$label)
  out_core$future_cost_oa <- mapply(function(tt, lab) v_mat[as.character(tt), lab], out_core$time, out_core$label)
  out_core$fc_high        <- mapply(function(tt, lab) fh_mat[as.character(tt), lab], out_core$time, out_core$label)
  out_core$fc_low         <- mapply(function(tt, lab) fl_mat[as.character(tt), lab], out_core$time, out_core$label)

  # PRIOR row (time = 0)
  prior_row <- data.frame(
    time = 0, label = "PRIOR",
    optimal_action = a0, future_cost_oa = v0, fc_high = fh0, fc_low = fl0, stringsAsFactors = FALSE
  )

  out <- rbind(prior_row, out_core)

  # ============================================================================
  # 14) STEP 8 — SORT AND RETURN
  # ----------------------------------------------------------------------------
  # Final tidy data.frame, ordered by time then label.
  # ============================================================================
  out[order(out$time, out$label), ]
}

