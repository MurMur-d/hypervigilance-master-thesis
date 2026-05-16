# =============================================================================
# file: R/models/health/health_model_SIM.R
#
# FORWARD SIMULATION WITH HEALTH LEVELS (health model)
# This script simulates agents step-by-step using a precomputed policy table
# by 03_health_dp.R. Each agent has:
# ------------------------------------------------------------------------------
# - First we generate a stressor timeline (0/1) for each agent via a simple
#   two-state Markov chain with appear/leave probabilities (LA, LL).
# - At each step, we look up the optimal action in the policy (High/Low)
#   based on (time-1, last_label, health). Exact ties are broken randomly.
# - We apply integer health loss based on the action and stressor presence,
#   update the health level, and update the label (K, Kd, C, CD; DEAD if health
#   <= 0).
# - We record the realized label (K, Kd, C, CD) and helpful flags.
#
# Outputs
# - $agent_data  : per-agent, per-time records (action, state, health, flags, …)
# - $agent_stats : per-agent summary metrics (rates, totals, survival info)
#
# Starting health
# - If spread_initial_over_levels = TRUE, agents are evenly distributed across
#   the integer health grid (1..H0), optionally shuffled.
# - Otherwise all agents start at health = H0.
# ==============================================================================


# ==============================================================================
# 2) LOAD SUPPORT FILES AND DP POLICY
# ------------------------------------------------------------------------------
# Load shared setup and the health-based DP policy.
# ==============================================================================
source("R/core/setup_project.R")
source("R/models/health/health_model_dp.R")


# ==============================================================================
# 3) FUNCTION SUMMARY — WHAT simulate_agents_forward_health() DOES
# ------------------------------------------------------------------------------
# Approach
# - Build a 3D policy lookup: (time × label × health) → optimal action.
# - Assign initial integer health to agents.
# - Generate a length-horizon stressor path via a 2-state
#   Markov chain with appear (LA) and leave (LL) probabilities.
# - For each time step:
#     1) lookup action using (t-1, last_label, health),
#     2) apply integer health loss (effort + possible stressor damage),
#     3) update the state label (K, Kd, C, CD; DEAD if health <= 0),
#     4) record per-step flags and accumulate totals.
#
# Notes
# - Ties/missing policy entries are resolved by random choice {High, Low}.
# - Health loss mapping matches the DP: round costs to integers (min loss = 1).
# ==============================================================================
simulate_agents_forward_health <- function(
    policy_df,                             # policy from compute_optimal_policy_health (time/label/health/... or legacy names)
    la_prob, ll_prob,                      # environment probabilities (appear / leave)
    horizon,                               # simulation horizon (number of time steps)
    n_agents = 1000,                       # number of agents
    k_cost = NULL, c_cost = NULL, d_high = NULL, d_low = NULL, # costs/damages
    h0 = 35,                               # maximum integer health (alive states 1..h0)
    spread_initial_over_levels = FALSE,     # if TRUE, evenly spread agents across integer health levels
    shuffle = FALSE,                        # shuffle initial assignment
    seed = NULL                            # optional RNG seed
) {
  # ============================================================================
  # 4) STEP 0 — REPRODUCIBILITY
  # ----------------------------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)

  # ============================================================================
  # 5) STEP 1 — BUILD HEALTH GRID AND POLICY LOOKUP
  # ----------------------------------------------------------------------------
  # Health grid is 1..H0 (integer). We build a 3D array for O(1) lookups:
  # action = a_lookup[t-1, last_label, health].
  # ============================================================================
  h0 <- as.integer(h0)
  health_levels <- seq_len(h0)                           # health grid: 1..h0
  times_policy  <- sort(unique(policy_df$time))
  labels_policy <- sort(unique(policy_df$label))

  # 3D action lookup: time × label × health
  a_lookup <- array(
    NA_character_,                                                                # default missing
    dim = c(length(times_policy), length(labels_policy), length(health_levels)),  # define dimensions
    dimnames = list(time = as.character(times_policy),
                    label = labels_policy,
                    health = as.character(health_levels))
  )

  # Fill lookup from policy rows (policy must include rows for time/label/health)
  for (i in seq_len(nrow(policy_df))) {                                         # for each policy row
    r  <- policy_df[i, ]                                                        # extract one row
    th <- as.character(r$time); lab <- r$label; h <- as.character(r$health)     # get dimension keys
    if (is.na(th) || is.na(lab) || is.na(h)) next                               # skip if missing any key
    if (!(th %in% dimnames(a_lookup)$time &&                                    # skip if outside grid
          lab %in% dimnames(a_lookup)$label && 
          h  %in% dimnames(a_lookup)$health)) next
    a_lookup[th, lab, h] <- r$optimal_action                                    # assign optimal action
  }

  # ============================================================================
  # 6) STEP 2 — INITIAL HEALTH ASSIGNMENT
  # ----------------------------------------------------------------------------
  # Evenly distribute agents across 1..H0 (optional shuffle), else set all to H0.
  # ============================================================================
  make_initial_healths <- function(n, health_levels, shuffle = TRUE) {
    q <- n %/% length(health_levels); r <- n %% length(health_levels)                   # divide agents evenly: q per level, r leftover
    counts <- rep(q, length(health_levels))                                             # base count of agents per health level
    if (r > 0) counts[seq_len(r)] <- counts[seq_len(r)] + 1                             # assign remainder agents to first r levels
    init <- rep(health_levels, times = counts)                                          # repeat each level according to count
    if (length(init) > n) init <- init[seq_len(n)]                                      # trim if overshoot
    if (length(init) < n) init <- c(init, rep(tail(health_levels, 1), n - length(init)))# pad if short (rare safety case)
    if (isTRUE(shuffle)) init <- sample(init, length(init))                             # randomize assignment across agents
    init
  }

  # Assign initial healths based on chosen setup
  if (isTRUE(spread_initial_over_levels)) {
    initial_healths <- make_initial_healths(n_agents, health_levels, shuffle = shuffle) # evenly spread agents (optionally shuffled)
  } else {
    initial_healths <- rep(h0, n_agents)                                                # all agents start at full health
  }

  # ============================================================================
  # 7) STEP 3 — COST-> INTEGER HEALTH LOSS (MATCH DP LOGIC)
  # ----------------------------------------------------------------------------
  # Map effort/damage costs to integer losses (rounded; minimum loss = 1).
  # ============================================================================
  cost_to_health_loss <- function(cost) {
    # handle NULL, empty, NA, non-finite, and non-scalar inputs safely
    if (is.null(cost) || length(cost) == 0) return(0L)
    cnum <- as.numeric(cost)
    if (length(cnum) == 0 || is.na(cnum) || !is.finite(cnum) || cnum <= 0) return(0L) # skip invalid or nonpositive
    loss <- as.integer(round(cnum))                                                   # round to nearest integer
    if (is.na(loss) || loss < 1L) loss <- 1L                                          # enforce minimum 1 for positive costs
    loss
  }
  # treat missing cost components as zero.
  k0  <- if (is.null(k_cost)) 0 else k_cost # vigilant(high) cost
  c0  <- if (is.null(c_cost)) 0 else c_cost # relaxed(low) cost
  dh0 <- if (is.null(d_high)) 0 else d_high # stressor damage when vigilant
  dl0 <- if (is.null(d_low))  0 else d_low  # stressor damage when relaxed

  loss_high_str <- cost_to_health_loss(k0 + dl0)    # High + stressor
  loss_high_nst <- cost_to_health_loss(k0)          # High + no stressor
  loss_low_str  <- cost_to_health_loss(c0 + dh0)    # Low  + stressor
  loss_low_nst  <- cost_to_health_loss(c0)          # Low  + no stressor

  # ============================================================================
  # 8) STEP 4 — HELPER: SIMULATE STRESSOR TIMELINE (length = horizon)
  # ----------------------------------------------------------------------------
  #    - Uses a 2-state Markov chain with appear (LA) and leave (LL) probs.
  #    - Initial state drawn from the stationary distribution.
  # ============================================================================
  sim_env <- function(la_prob, ll_prob, horizon) {
    s <- integer(horizon)                                                           # preallocate 0/1 stressor vector
    prior <- if ((la_prob + ll_prob) == 0) 0.5 else la_prob / (la_prob + ll_prob)   # stationary prior for the first step
    s[1] <- rbinom(1, 1, prior)                                                     # initial stressor
    for (t in 2:horizon) {                                                          # propogate forward
      prev <- s[t - 1]                                                              # previous stressor state (0/1)
      p    <- if (prev == 1) 1 - ll_prob else la_prob                               # if present → stays with 1-LL; else appears with LA
      s[t] <- rbinom(1, 1, p)                                                       # draw new stressor state
    }
    s                                                                               # return 0/1 vector
  }

  # ============================================================================
  # 9) STEP 5 — SIMULATE ALL AGENTS
  # ----------------------------------------------------------------------------
  # For each agent:
  #    - draw stressor path
  #    - walk through time applying the policy
  #    - record realized state (K, Kd, C, CD), health, and flags
  #    - summarize per-agent metrics
  # ============================================================================
  results <- future.apply::future_lapply(
    seq_len(n_agents),
    future.seed = TRUE,
    function(a) {
      # Stressor path for this agent
      # Simulate stressor timeline (0/1) using the helper above
      env <- sim_env(la_prob, ll_prob, horizon)

      # Preallocate storage vectors
      actions <- states <- character(horizon)
      healths <- numeric(horizon)
      alive   <- logical(horizon)
      hv <- prepared <- relaxed <- exposed <- logical(horizon)
      step_loss <- numeric(horizon)   # integer health loss per step (from costs)
      last_label <- "PRIOR"
      H <- initial_healths[a]
      died <- FALSE
      death_time <- NA_integer_
      death_cause <- NA_character_
      total_loss <- total_vig_loss <- total_str_loss <- 0

      # Step through time
      for (t in 1:horizon) {          # if dead, keep recording DEAD rows
        if (died) {
          actions[t] <- NA_character_
          states[t] <- "DEAD"
          healths[t] <- 0
          alive[t] <- FALSE
          next
        }

        # Map current health to string for lookup (clamp into 1..H0)
        h_str <- as.character(min(max(as.integer(H), 1L), h0))

        # Lookup action for (t-1, last_label, health)
        act <- a_lookup[as.character(t - 1), last_label, h_str]
        if (is.na(act) || act == "Tie") act <- sample(c("High", "Low"), 1)

        s <- env[t] # stressor at time t

        # Map (action, stressor) -> realized state and flags
        if (act == "High" && s == 1) {        # Vigilant + stressor
          states[t]    <- "Kd"                # prepared
          step_loss[t] <- loss_high_str
          hv[t] <- FALSE
          prepared[t] <- TRUE
          relaxed[t] <- FALSE
          exposed[t] <- FALSE
        } else if (act == "High" && s == 0) { # Vigilant + no stressor
          states[t]    <- "K"                 # hypervigilant
          step_loss[t] <- loss_high_nst
          hv[t] <- TRUE
          prepared[t] <- FALSE
          relaxed[t] <- FALSE
          exposed[t] <- FALSE
        } else if (act == "Low" && s == 1) { # Relaxed + stressor
          states[t]    <- "CD"               # exposed    
          step_loss[t] <- loss_low_str
          hv[t] <- FALSE
          prepared[t] <- FALSE
          relaxed[t] <- FALSE
          exposed[t] <- TRUE
        } else {                              # Relaxed & no stressor
          states[t]    <- "C"                 # relaxed      
          step_loss[t] <- loss_low_nst
          hv[t] <- FALSE
          prepared[t] <- FALSE
          relaxed[t] <- TRUE
          exposed[t] <- FALSE
        }

        actions[t] <- act # store action

        # Update health and alive flag
        H <- max(0, H - step_loss[t])
        healths[t] <- H
        alive[t]   <- (H > 0)

        # Totals (note: vig vs stressor components approximate after rounding)
        total_loss     <- total_loss + step_loss[t]
        total_vig_loss <- total_vig_loss + (if (act == "High") loss_high_nst else loss_low_nst)
        total_str_loss <- total_str_loss + (if (s == 1) {
          if (act == "High") (loss_high_str - loss_high_nst) else (loss_low_str - loss_low_nst)
        } else 0)

        if (H == 0 && !died) { # record death event once
          died <- TRUE
          death_time <- t
          death_cause <- paste0(act, "+", ifelse(s == 1, "Stressor", "NoStressor"))
        }

        last_label <- if (!died) states[t] else "DEAD" # update label for next decision
      }

      # Tidy per-time panel
      agent_data <- data.frame(
        agent    = a,
        time     = 1:horizon,
        stressor = env,
        action   = actions,
        state    = states,
        health   = healths,
        alive    = alive,
        hv       = hv,
        prepared = prepared,
        relaxed  = relaxed,
        exposed  = exposed,
        step_loss = step_loss,
        stringsAsFactors = FALSE
      )

      # Per-agent summary
      n_no_str  <- sum(env == 0)    # steps without stressor
      hv_count  <- sum(hv)          # hypervigilance count
      agent_stats <- data.frame(    # per-agent summary
        agent               = a,
        stressor_rate       = mean(env),
        vigilance_rate      = mean(actions == "High", na.rm = TRUE),
        hv_rate             = if (n_no_str > 0) hv_count / n_no_str else 0,
        total_loss          = total_loss,
        total_vig_loss      = total_vig_loss,
        total_stressor_loss = total_str_loss,
        final_health        = if (any(!is.na(healths))) tail(healths[!is.na(healths)], 1) else NA_real_,
        died                = died,
        lifespan            = if (is.na(death_time)) horizon else death_time,
        death_time          = death_time,
        death_cause         = death_cause,
        stringsAsFactors    = FALSE
      )

      list(agent_data = agent_data, agent_stats = agent_stats)
    }
  )

  # ============================================================================
  # 10) STEP 6 — COMBINE RESULTS AND RETURN
  # ----------------------------------------------------------------------------
  agent_data_all  <- do.call(rbind, lapply(results, `[[`, "agent_data"))
  agent_stats_all <- do.call(rbind, lapply(results, `[[`, "agent_stats"))
  list(agent_data = agent_data_all, agent_stats = agent_stats_all)
}

