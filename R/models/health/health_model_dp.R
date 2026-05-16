# ==============================================================================
# File: R/models/health/health_model_dp.R
#
# BACKWARD REASONING - HEALTH MODEL WITH INTEGER HEALTH LEVELS
# We compute an optimal vigilance policy via backward induction (dynamic
# programming) where the state now includes an INTEGER HEALTH level.
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
# DIFFERENT FROM THE BASIC MODEL:
# - The state now includes an integer health level (1..H0).
# - Costs translate into health losses (rounded to integers).
# - If health ≤ 0, the agent transitions to DEAD (absorbing state with zero cost).
# - Optionally, we can add a terminal reward at the end of the horizon to
#   encourage finishing with higher health.
#
# Output
# - A tidy table with the optimal action and expected cost for each
#   (time, label, health), plus diagnostics (fc_high, fc_low).
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
# Loads packages, caches, parallel plan, RNG seed, etc.
# ==============================================================================
source("R/core/setup_project.R")  # parallel plan + caches + seed

# ==============================================================================
# 3) FUNCTION SUMMARY — WHAT compute_optimal_policy_health() DOES
# ------------------------------------------------------------------------------
# Approach
# - Backward induction over time, last-observed label, and health level.
# - For each (t, label, health): compare expected costs of High vs Low.
# - Record the action with lower expected cost; mark "Tie" if equal.
#
# Health & terminal reward
# - Health is an integer grid (1..h0, step = health_step).
# - Costs translate into health losses (simple rounding-to-integer mapping).
# - Optional terminal reward encourages finishing with higher health.
#
# Output columns
# - time, label, health, optimal_action, future_cost_oa, fc_high, fc_low,
#   health_rep_used
# ==============================================================================

# compute_optimal_policy_health(...) -------------------------------------------
compute_optimal_policy_health <- function(
  k_cost, c_cost, d_high, d_low,                              # costs (effort and damages)
  la_prob, ll_prob, horizon,                                  # environment probabilities + horizon
  h0 = 35,                                                   # max integer health
  health_step = 1,                                            # health grid resolution (integer)
  terminal_reward_weight = 1,                                 # weight for terminal health reward
  terminal_reward_mode   = c("linear", "power", "threshold"), # mode for terminal reward
  terminal_power_alpha   = 1.0,                               # exponent if mode = "power"
  terminal_threshold_tau = NULL                               # threshold if mode = "threshold"
) {

  # ============================================================================
  # 4) STEP 0 — BASIC VALIDATION
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
  # 5) STEP 1 — AXES (TIME, LABELS, HEALTH GRID)
  # ----------------------------------------------------------------------------
  # Health is an integer ladder from 1 to h0, with step = health_step.
  # Labels include an absorbing "DEAD" and the usual PRIOR/K/Kd/C/CD.
  # ============================================================================
  h0 <- as.integer(h0)                                 # ensure h0 is integer

  # health_step must be a positive integer
  if (is.null(health_step) || !is.finite(health_step))
    stop("health_step must be a finite number.")
  health_step_int <- as.integer(health_step)
  if (health_step_int <= 0)
    stop("health_step must be a positive integer (>= 1).")

  # Construct the integer sequence of possible health levels
  # from 1 up to h0, stepping by health_step_int
  health_levels <- seq.int(1L, h0, by = health_step_int)

  labels <- c("DEAD", "PRIOR", "K", "Kd", "C", "CD")   # state labels
  times  <- 0:horizon                                  # time steps

  # ============================================================================
  # 6) STEP 2 — ALLOCATE DP TABLES (TIME × LABEL × HEALTH)
  # ----------------------------------------------------------------------------
  # v_arr : expected future cost
  # a_arr : optimal action ("High", "Low", "Tie")
  # fh/fl : expected cost if choose High / Low now
  # ============================================================================
  v_arr  <- array(0, dim = c(length(times), length(labels), length(health_levels)),
                  dimnames = list(Time = as.character(times), Label = labels, Health = as.character(health_levels)))
  a_arr  <- array(NA_character_, dim = dim(v_arr), dimnames = dimnames(v_arr))
  fh_arr <- array(NA_real_, dim = dim(v_arr), dimnames = dimnames(v_arr))
  fl_arr <- array(NA_real_, dim = dim(v_arr), dimnames = dimnames(v_arr))

  # ============================================================================
  # 7) STEP 3 — STRESSOR PROBABILITY HELPER
  # ----------------------------------------------------------------------------
  # Short memory:
  #   If last_label ∈ {Kd, CD} → stressor was present → P(stressor next) = 1 - LL
  #   Else                    → stressor was absent  → P(stressor next) = LA
  # ============================================================================
  stressor_prob <- function(last_label) {
    if (last_label %in% c("Kd", "CD")) 1 - ll_prob else la_prob
  }

  # ============================================================================
  # 8) STEP 4 — TERMINAL REWARD (OPTIONAL)
  # ----------------------------------------------------------------------------
  # Encourages finishing with higher health. Mode can be:
  #   - "linear"   : u(h) = h
  #   - "power"    : u(h) = h^alpha
  #   - "threshold": u(h) = 1(h >= tau)
  # Terminal reward is subtracted from costs at t = horizon (i.e., negative cost).
  # ============================================================================
 terminal_reward_mode <- match.arg(terminal_reward_mode)              # validate mode name
  term_u <- function(h) {                                              # terminal utility function
    if (!is.finite(h) || h <= 0) return(0)                             # invalid health → 0 reward
    if (terminal_reward_mode == "linear") {
      h                                                               # u(h) = h
    } else if (terminal_reward_mode == "power") {
      a <- if (is.null(terminal_power_alpha)) 1 else as.numeric(terminal_power_alpha)
      if (!is.finite(a) || a <= 0) return(0)
      h^a                                                             # u(h) = h^α
    } else {
      tau <- if (is.null(terminal_threshold_tau)) 0.6 * h0 else as.numeric(terminal_threshold_tau)
      as.numeric(h >= tau)                                            # u(h) = 1(h ≥ τ)
    }
  }

  if (is.null(terminal_reward_weight) || !is.finite(terminal_reward_weight)) terminal_reward_weight <- 1  # default weight
  if (is.null(terminal_power_alpha)  || !is.finite(terminal_power_alpha))  terminal_power_alpha <- 1       # default α

  reward_active <- (terminal_reward_weight > 0) &&                     # reward applied only if active
    !(identical(terminal_reward_mode, "power") && terminal_power_alpha == 0)

  rep_h <- as.numeric(health_levels)                                   # health grid as numeric

  v_arr[as.character(horizon), , ] <- 0                                # init terminal V values
  a_arr[as.character(horizon), , ] <- NA_character_                    # no actions at horizon

# If reward is active, write it into the terminal value function as a **negative cost**
  if (reward_active) {
    term_values <- - terminal_reward_weight * vapply(rep_h, term_u, numeric(1))  # compute final rewards
    for (lab in setdiff(labels, "DEAD")) {
      v_arr[as.character(horizon), lab, ] <- term_values               # apply to all live states
    }
  }
  # ============================================================================
  # 9) STEP 5 — IMMEDIATE COSTS & HEALTH-LOSS MAPPING
  # ----------------------------------------------------------------------------
  # Define immediate costs/next labels for High/Low, then map those costs to
  # integer health losses (simple rounded, min loss = 1).
  # ============================================================================
  step <- list(                                                 # define action specs
    High = list(cost_str = k_cost + d_low, cost_nst = k_cost,   # vigilant: stressed / no-stress costs
                next_str = "Kd", next_nst = "K"),               # next states under stress / no stress
    Low  = list(cost_str = c_cost + d_high, cost_nst = c_cost,  # relaxed: stressed / no-stress costs
                next_str = "CD", next_nst = "C")                # next states under stress / no stress
  )

  cost_to_health_loss <- function(cost) {                       # map cost → health loss (int)
    cnum <- as.numeric(cost)                                    # numeric conversion
    if (!is.finite(cnum) || cnum <= 0) return(0L)               # nonpositive → 0 loss
    loss <- as.integer(round(cnum))                             # round to nearest int
    if (loss < 1L) loss <- 1L                                   # ensure ≥ 1
    loss
  }

  loss_high_str <- cost_to_health_loss(step$High$cost_str)      # Vigilant w/ stress
  loss_high_nst <- cost_to_health_loss(step$High$cost_nst)      # Vigilant action no stress
  loss_low_str  <- cost_to_health_loss(step$Low$cost_str)       # Relaxed action w/ stress
  loss_low_nst  <- cost_to_health_loss(step$Low$cost_nst)       # Relaxed action no stress

  # ============================================================================
  # EXTRA) HEALTH LABEL MAPPING FUNCTION
  # ----------------------------------------------------------------------------
  # Maps next health value to the corresponding label
  # ============================================================================
  # map to next health label (DEAD if ≤ 0; else clamp to health grid)
  map_health_label <- function(nh) {
      if (nh <= 0) return("DEAD")
      nh_clamped <- min(max(nh, min(health_levels)), max(health_levels))  # clamp to grid
      as.character(as.integer(nh_clamped))
      }

  # ============================================================================
  # 10) STEP 6 — BACKWARD INDUCTION (CORE DP LOOP)
  # ----------------------------------------------------------------------------
  # For each (t, label, health):
  #   - compute expected cost for High (fh) and Low (fl)
  #   - pick the action with lower expected cost, record "Tie" if equal
  # DEAD is absorbing with zero future cost.
  # Health transitions subtract integer losses; if health ≤ 0 -> DEAD.
  # ============================================================================
  for (t in seq(horizon - 1L, 0L, by = -1L)) {                                # move backwards through time
    for (lab in labels) {                                                     # evaluate each possible last label
      for (h_val in health_levels) {                                          # evaluate each possible health level

        # DEAD absorbing (or invalid health)
        if (lab == "DEAD" || !is.finite(h_val) || h_val <= 0) { 
          v_arr[as.character(t), lab, as.character(h_val)] <- 0               # future cost = 0
          a_arr[as.character(t), lab, as.character(h_val)] <- NA_character_   # no action
          next
        }

        p_prob <- stressor_prob(lab)                                       # stressor probability at next step

        # next health after each action/outcome
        nh_h_str <- h_val - loss_high_str                                     # vigilant + stressor
        nh_h_nst <- h_val - loss_high_nst                                     # vigilant + no stressor
        nh_l_str <- h_val - loss_low_str                                      # relaxed + stressor
        nh_l_nst <- h_val - loss_low_nst                                      # relaxed + no stressor
        nh_h_str_label <- map_health_label(nh_h_str) # next health after High + stressor
        nh_h_nst_label <- map_health_label(nh_h_nst) # next health after High + no stressor
        nh_l_str_label <- map_health_label(nh_l_str) # next health after Low + stressor
        nh_l_nst_label <- map_health_label(nh_l_nst) # next health after Low + no stressor

        # future costs at t+1 (DEAD -> 0)
        future_h_str <- if (nh_h_str_label == "DEAD") 0 else v_arr[as.character(t + 1L), "Kd", nh_h_str_label] # next health after High + stressor
        future_h_nst <- if (nh_h_nst_label == "DEAD") 0 else v_arr[as.character(t + 1L), "K",  nh_h_nst_label] # next health after High + no stressor
        future_l_str <- if (nh_l_str_label == "DEAD") 0 else v_arr[as.character(t + 1L), "CD", nh_l_str_label] # next health after Low + stressor
        future_l_nst <- if (nh_l_nst_label == "DEAD") 0 else v_arr[as.character(t + 1L), "C",  nh_l_nst_label] # next health after Low + no stressor

         # Expected cost if we choose High now:
        #   = P(stressor)      * (immediate High cost under stress + future cost from next label "Kd")
        #   + (1 - P(stressor))* (immediate High cost without stress + future cost from next label "K")
        fh <- p_prob * (step$High$cost_str + future_h_str) +
              (1 - p_prob) * (step$High$cost_nst + future_h_nst)

        # Expected cost if we choose Low now (same structure, different costs/labels)
        fl <- p_prob * (step$Low$cost_str  + future_l_str) +
              (1 - p_prob) * (step$Low$cost_nst  + future_l_nst)

        # Record per-action expected costs for checks
        fh_arr[as.character(t), lab, as.character(h_val)] <- fh
        fl_arr[as.character(t), lab, as.character(h_val)] <- fl

        # Pick the cheaper action; treat very small numerical differences as a tie
        if (abs(fh - fl) < 1e-12) {
          a_arr[as.character(t), lab, as.character(h_val)] <- "Tie"
          v_arr[as.character(t), lab, as.character(h_val)] <- fh    # either value works (they're equal)
        } else if (fl < fh) {
          a_arr[as.character(t), lab, as.character(h_val)] <- "Low" # Low is better
          v_arr[as.character(t), lab, as.character(h_val)] <- fl
        } else {
          a_arr[as.character(t), lab, as.character(h_val)] <- "High" # High is better
          v_arr[as.character(t), lab, as.character(h_val)] <- fh
        }
      }
    }
  }

  # ============================================================================
  # 11) STEP 6b — SPECIAL HANDLING FOR TIME 0 (PRIOR)
  # ----------------------------------------------------------------------------
  # At time 0 there is no last observed label. We use the stationary prior
  # probability that the environment has a stressor at the first realization.
  # ============================================================================
  p0 <- if ((la_prob + ll_prob) == 0) 0.5 else la_prob / (la_prob + ll_prob)     # stationary prior

  for (h in health_levels) {
    if (h <= 0) {                                                                # DEAD/invalid
      v_arr["0", "PRIOR", as.character(h)] <- 0
      a_arr["0", "PRIOR", as.character(h)] <- NA_character_
      next
    }

    # next health values
    nh_h_str <- h - loss_high_str; nh_h_nst <- h - loss_high_nst
    nh_l_str <- h - loss_low_str;  nh_l_nst <- h - loss_low_nst

    # map to labels (use the same map_health_label you defined earlier)
    LhS <- map_health_label(nh_h_str); LhN <- map_health_label(nh_h_nst)
    LlS <- map_health_label(nh_l_str);  LlN <- map_health_label(nh_l_nst)

    # V at t = 1 (DEAD → 0)
    VhS <- if (LhS == "DEAD") 0 else v_arr["1", "Kd", LhS]
    VhN <- if (LhN == "DEAD") 0 else v_arr["1", "K",  LhN]
    VlS <- if (LlS == "DEAD") 0 else v_arr["1", "CD", LlS]
    VlN <- if (LlN == "DEAD") 0 else v_arr["1", "C",  LlN]

    # EC at t = 0
    fh0 <- p0 * (step$High$cost_str + VhS) + (1 - p0) * (step$High$cost_nst + VhN)
    fl0 <- p0 * (step$Low$cost_str  + VlS) + (1 - p0) * (step$Low$cost_nst  + VlN)

    # store decision/value
    key <- c("0", "PRIOR", as.character(h))
    if (abs(fh0 - fl0) < 1e-12) {
      a_arr[key[1], key[2], key[3]] <- "Tie";  v_arr[key[1], key[2], key[3]] <- fh0
    } else if (fl0 < fh0) {
      a_arr[key[1], key[2], key[3]] <- "Low";  v_arr[key[1], key[2], key[3]] <- fl0
    } else {
      a_arr[key[1], key[2], key[3]] <- "High"; v_arr[key[1], key[2], key[3]] <- fh0
    }
  }

  # ============================================================================
  # 11) STEP 7 — BUILD TIDY OUTPUT TABLE
  # ----------------------------------------------------------------------------
  # Return one row per (time, label, health) with optimal action and costs.
  # Rows are ordered by time, label, and descending health (easier to scan).
  # ============================================================================
  out <- expand.grid(time = times, label = labels, health = health_levels, stringsAsFactors = FALSE)
  out$optimal_action <- mapply(function(tt, lab, h) a_arr[as.character(tt), lab, as.character(h)],
                               out$time, out$label, out$health)
  out$future_cost_oa <- mapply(function(tt, lab, h) v_arr[as.character(tt), lab, as.character(h)],
                               out$time, out$label, out$health)
  out$fc_high        <- mapply(function(tt, lab, h) fh_arr[as.character(tt), lab, as.character(h)],
                               out$time, out$label, out$health)
  out$fc_low         <- mapply(function(tt, lab, h) fl_arr[as.character(tt), lab, as.character(h)],
                               out$time, out$label, out$health)

  out$health_rep_used <- out$health

  out[order(out$time, out$label, -out$health), ]
}
