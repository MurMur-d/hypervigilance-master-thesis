# ------------------------------------------------------------------------------
# File: R/models/basic/basic_model_SIM.R
#
# FORWARD SIMULATION (Basic model policy)
# This script simulates agents step-by-step using a precomputed policy table
# by 01_basic_dp.R.
# ------------------------------------------------------------------------------
# 1) FORWARD SIMULATION EXPLAINED
# ------------------------------------------------------------------------------
# - First we generate a stressor timeline (0/1) for each agent via a simple
#   two-state Markov chain with appear/leave probabilities (LA, LL).
# - At each step, we look up the optimal action in the policy (High/Low)
#   based on (time-1, last_label). Exact ties are broken randomly.
# - We record the realized label (K, Kd, C, CD) and helpful flags.
# - We return a clean list:
#       $agent_data  : one row per agent per time
#       $agent_stats : one row per agent with summary metrics
#
# Output
# - $agent_data  : per-agent, per-time records (action, state, , flags, …)
# - $agent_stats : per-agent summary metrics (rates, totals, …)
#
# KEY DEFINITIONS:
#   - "Hv" = chose High when stressor was absent (High & no stressor)
#   - "highrate_all"   = proportion of all time steps with action High
#   - "hv_rate" = proportion of no-stressor steps that were High
#
# Time indexing convention (very important):
# - Decision at time t is made *before* stressor at time t+1.
# - The row labeled Time = t (t >= 1) shows the realized label from:
#       Action at (t-1)  +  Stressor at t.
# - Thus, Time = 1 reflects the PRIOR (t=0) decision combined with the first realized stressor.
# ------------------------------------------------------------------------------

# ==============================================================================
# 2) LOAD SUPPORT FILES (SETUP)
#    - 00_setup.R: seeds, packages, helpers, etc.
#    - NOTE: compute_optimal_policy() lives in 01_basic_dp.R and should be
#      sourced upstream to avoid circular sourcing between these files.
# ==============================================================================
source("R/core/setup_project.R")

# ==============================================================================
# 3) FUNCTION SUMMARY — WHAT simulate_agents_forward() DOES
# ------------------------------------------------------------------------------
# Approach
# - Build a fast lookup table (time, last_label) -> optimal_action from policy_df.
# - Generate a length-horizon stressor path via a 2-state
#   Markov chain with appear (LA) and leave (LL) probabilities.
# - For each agent (1..n_agents):
#   Draw a stressor path env[1:horizon], then for t = 1..horizon:
#   Choose action = policy(time = t-1, last_label); break ties or NAs
#   by sampling uniformly between "High" and "Low".
# - Map realized state and flags:
#       K  : Vigilant (high) without stressor  -> "Hypervigilant"
#       Kd : Vigilant (high) with stressor present -> "Prepared"
#       C  : Relaxed (low) without stressor)  -> "Relaxed (Exposed=FALSE)"
#       CD : Relaxed (low) with stressor present -> "Damaged (Exposed=TRUE)"

# ==============================================================================
simulate_agents_forward <- function(
  policy_df,                               # from compute_optimal_policy(): (time, label) -> optimal_action
  la_prob, ll_prob,                        # Markov chain: appear (LA) and leave (LL) probabilities
  horizon,                                 # number of time steps in the simulation
  n_agents = 1000,                         # number of agents
  k_cost = NULL, c_cost = NULL,            # optional cost params for parity/logging (not used in sim)
  d_high = NULL, d_low = NULL,             # optional cost params for parity/logging (not used in sim)
  seed = NULL                              # optional RNG seed for reproducibility
) {
  # ============================================================================
  # 4) STEP 1 — REPRODUCIBILITY
  # ----------------------------------------------------------------------------
  # Set a seed if provided
  # ============================================================================
  if (!is.null(seed)) set.seed(seed)


  # ============================================================================
  # 5) STEP 2 — FAST POLICY LOOKUP
  # ----------------------------------------------------------------------------
  # (time, last_label) -> optimal action
  # ============================================================================
  times  <- sort(unique(policy_df$time))                              # collect all time indices (0..T)
  labels <- sort(unique(policy_df$label))                             # collect all possible labels       
  a_lookup <- matrix(                                                 # initialize lookup matrix
    NA_character_,                                                    # start with NAs
    nrow = length(times),
    ncol = length(labels),                                            # shape: |times| x |labels|
    dimnames = list(time = as.character(times), label = labels)       # row names are time as strings, col names are labels
  )
  for (i in seq_len(nrow(policy_df))) {                               # fill in optimal actions
    r <- policy_df[i, ]                                               # extract row
    a_lookup[as.character(r$time), r$label] <- r$optimal_action       # assign action
  }

  # ============================================================================
  # 6) STEP 3 — HELPER: SIMULATE STRESSOR TIMELINE (length = horizon)
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
  # 7) STEP 4 — SIMULATE ALL AGENTS
  # ----------------------------------------------------------------------------
  # For each agent:
  #    - draw stressor path
  #    - walk through time applying the policy
  #    - record realized state (K, Kd, C, CD) and flags
  #    - summarize per-agent metrics
  # ============================================================================
  results <- lapply(seq_len(n_agents), function(agent_id) {
    # Stressor path for this agent
    env <- sim_env(la_prob, ll_prob, horizon)

    # Preallocate storage vectors
    actions  <- character(horizon)
    states   <- character(horizon)
    hv       <- logical(horizon)     # TRUE when High action with no stressor
    prepared <- logical(horizon)     # TRUE when High action with stressor
    relaxed  <- logical(horizon)     # TRUE when Low action with no stressor
    exposed  <- logical(horizon)     # TRUE when Low action with stressor

    # Start from the PRIOR label (decision at t=0)
    last_label <- "PRIOR"

    # Step through time
    for (t in 1:horizon) {
      # Action depends on (time-1, last_label)
      act <- a_lookup[as.character(t - 1), last_label]                    # lookup action
      if (is.na(act) || act == "Tie") act <- sample(c("High", "Low"), 1)  # break ties or NAs randomly
      actions[t] <- act                                                   # record action

      # Realized stressor at time t
      str <- env[t]

      # Map (action, stressor) -> realized state and flags
      if (act == "High" && str == 1) {
        states[t]   <- "Kd"     # prepared
        hv[t]       <- FALSE
        prepared[t] <- TRUE
        relaxed[t]  <- FALSE
        exposed[t]  <- FALSE
      } else if (act == "High" && str == 0) {
        states[t]   <- "K"      # hypervigilant
        hv[t]       <- TRUE
        prepared[t] <- FALSE
        relaxed[t]  <- FALSE
        exposed[t]  <- FALSE
      } else if (act == "Low" && str == 1) {
        states[t]   <- "CD"     # damaged (exposed)
        hv[t]       <- FALSE
        prepared[t] <- FALSE
        relaxed[t]  <- FALSE
        exposed[t]  <- TRUE
      } else {
        states[t]   <- "C"      # relaxed (no stressor)
        hv[t]       <- FALSE
        prepared[t] <- FALSE
        relaxed[t]  <- TRUE
        exposed[t]  <- FALSE
      }

      # Update the label for the next decision
      last_label <- states[t]
    }

    # Tidy per-time panel
    agent_data <- data.frame(
      agent    = agent_id,  # agent ID
      time     = 1:horizon, # time step
      stressor = env,       # stressor presence (0/1)
      action   = actions,   # action taken, "High"/"Low" (ties already resolved)
      state    = states,    # realized state (K, Kd, C, CD)
      hv       = hv,        # chose High when no stressor
      prepared = prepared,  # vigilant & stressor present
      relaxed  = relaxed,   # relaxed & no stressor
      exposed  = exposed,   # relaxed & stressor present
      stringsAsFactors = FALSE
    )

    # Per-agent summary
    n_no_stressor <- sum(env == 0)              # count no-stressor steps
    n_high        <- sum(actions == "High")     # count High actions
    n_hv          <- sum(hv)                    # count Hv actions

    agent_stats <- data.frame(
      agent            = agent_id,                                           # agent ID
      stressor_rate    = mean(env),                                          #  proporition of time steps with stressor
      high_rate        = n_high / horizon,                                   # proportion of all time steps with High action
      hv_rate          = if (n_no_stressor > 0) n_hv / n_no_stressor else 0, # proportion of no-stressor steps that were High
      n_high_stress    = sum(prepared),                                      # count High & stressor present
      n_high_no_stress = sum(hv),                                            # count High & no stressor
      n_low_stress     = sum(exposed),                                       # count Low & stressor present
      n_low_no_stress  = sum(relaxed),                                       # count Low & no stressor
      stringsAsFactors = FALSE
    )

    list(agent_data = agent_data, agent_stats = agent_stats)
  })

  # 4.5) Stack all agents' results and return
  agent_data_all  <- do.call(rbind, lapply(results, `[[`, "agent_data"))
  agent_stats_all <- do.call(rbind, lapply(results, `[[`, "agent_stats"))

  list(
    agent_data  = agent_data_all,
    agent_stats = agent_stats_all
  )
}
