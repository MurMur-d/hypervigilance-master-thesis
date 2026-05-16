# ------------------------------------------------------------------------------
# FILE: R/plotting/_shared/utils_health_visual.R
#
# ROLE:
#   Small helper functions used across the project.
#
#   These functions do NOT implement the model itself. Instead, they build
#   *argument lists* in a consistent shape, so that calls to the health model
#   stay readable and reproducible across scripts.
#
#   In practice, the health model tends to require a handful of knobs that
#   appear again and again (binning choices, terminal reward settings, initial
#   health settings). Passing these as a single list reduces boilerplate and
#   makes it harder to accidentally mix parameter conventions between figures.
#
#   Primary helpers:
#     â€¢ health_policy_args(): parameters for health DP / policy computation
#     â€¢ health_sim_args():    parameters for forward simulation initialisation
#
# NOTE:
#   - These are optional conveniences. You can still call mem_compute_policy()
#     and mem_simulate_agents() without them (by supplying lists manually).
#   - The point is *standardization*: you get the same parameter names and
#     validation checks everywhere.
# ------------------------------------------------------------------------------


# ==============================================================================
# STEP 1: Helper to build policy arguments for the HEALTH model
# ==============================================================================

#' Construct a policy-argument list for the health model DP.
#'
#' This function returns a list that is intended to be passed as `policy_args`
#' into your policy computation wrapper (e.g., mem_compute_policy(model="health", ...)).
#'
#' It bundles together:
#'   - Health discretization (bins)
#'   - Terminal reward settings (how final health is valued)
#'
#' Using this helper ensures that:
#'   (a) categorical arguments are validated via match.arg()
#'   (b) downstream code always sees the same names (H0, bin_width, ...)
#'
#' @param terminal_reward_weight Numeric. Weight `w` applied to the terminal
#'   (final timestep) health reward.
#'   - 0 means â€œno terminal reward termâ€ (purely immediate costs/benefits).
#'   - Larger values make the policy more forward-looking about ending health.
#'
#' @param terminal_reward_mode Character. How terminal reward scales with final health.
#'   Must be one of:
#'     - "linear":    reward proportional to health (baseline case)
#'     - "power":     reward proportional to health^alpha
#'     - "threshold": reward includes a threshold/cutoff (tau)
#'
#' @param H0 Numeric. Upper bound / reference maximum for health.
#'   In your project, H0 often plays two roles:
#'     1) Default initial health for simulations (see health_sim_args)
#'     2) Upper bound when creating discretized health bins
#'
#' @param bin_width Numeric. If bin_edges is NULL, bins are constructed as
#'   contiguous intervals of this width (e.g., 0â€“20, 20â€“40, ... up to H0).
#'
#' @param bin_edges Numeric vector or NULL. Explicit bin boundaries. If provided,
#'   it overrides bin_width. This is useful if you want non-uniform bin sizes
#'   (e.g., finer resolution near low health).
#'
#' @param bin_eval Character. Representative point used for DP computations
#'   within each bin. Must be one of:
#'     - "mid":   use the midpoint of the bin
#'     - "lower": use the lower edge of the bin
#'     - "upper": use the upper edge of the bin
#'
#'   WHY THIS MATTERS:
#'     The DP typically needs a single numeric â€œhealth valueâ€ to evaluate
#'     rewards/transitions per discrete bin. This choice affects approximation
#'     bias (midpoint is often a neutral default).
#'
#' @param terminal_power_alpha Numeric. Exponent alpha used when
#'   terminal_reward_mode == "power". (Ignored otherwise.)
#'
#' @param terminal_threshold_tau Numeric or NULL. Threshold used when
#'   terminal_reward_mode == "threshold". (Ignored otherwise.)
#'
#' @return A named list, suitable for passing directly into the health policy solver.
#'
#' DOWNSTREAM EXPECTATION:
#'   The returned list is typically forwarded into something like
#'   compute_optimal_policy_health(..., policy_args = <this list>).
health_policy_args <- function(
    terminal_reward_weight = 0,                       # w: weight on final-step health reward
    terminal_reward_mode   = c("linear","power","threshold"), # reward scaling rule
    H0         = 35,                                  # top of health range / bin upper bound
    bin_width  = 20,                                  # default bin width if edges not provided
    bin_edges  = NULL,                                # optional explicit edges; overrides bin_width
    bin_eval   = c("mid","lower","upper"),            # representative health within each bin
    terminal_power_alpha   = 1.0,                     # alpha for "power" terminal reward
    terminal_threshold_tau = NULL                     # tau for "threshold" terminal reward
) {
  # --- STEP 1A: Validate categorical argument choices -------------------------
  # match.arg() enforces that the argument equals one of the allowed choices.
  # This prevents silent bugs like "Linear" vs "linear", or accidental typos.
  terminal_reward_mode <- match.arg(terminal_reward_mode)
  bin_eval <- match.arg(bin_eval)

  # (Optional sanity checks you *could* add here, but currently do not):
  # - stopifnot(is.numeric(H0), H0 > 0)
  # - stopifnot(is.numeric(bin_width), bin_width > 0)
  # - if (!is.null(bin_edges)) stopifnot(is.numeric(bin_edges), is.sorted(bin_edges))
  # The project may already validate these downstream; this helper focuses on
  # lightweight standardization + categorical validation.

  # --- STEP 1B: Return a tidy, named list ------------------------------------
  # Key idea: the health DP solver expects a list of knobs. By naming them here,
  # you ensure all figure scripts pass consistent keys (no naming drift).
  #
  # NOTE ON bin_edges vs bin_width:
  # - If bin_edges is non-NULL, the DP should use those explicit boundaries.
  # - Otherwise, it should generate edges using bin_width up to H0.
  list(
    H0 = H0,
    bin_width = bin_width,
    bin_edges = bin_edges,
    bin_eval  = bin_eval,

    # Terminal reward controls:
    terminal_reward_weight = terminal_reward_weight,
    terminal_reward_mode   = terminal_reward_mode,
    terminal_power_alpha   = terminal_power_alpha,
    terminal_threshold_tau = terminal_threshold_tau
  )
}


# ==============================================================================
# STEP 2: Helper to build simulation arguments for the HEALTH model
# ==============================================================================

#' Construct a simulation-argument list for health model forward simulation.
#'
#' This function returns a list intended to be passed as `sim_args` into your
#' simulation wrapper (e.g., mem_simulate_agents(model="health", ...)).
#'
#' It controls how agents are initialised in terms of health:
#'   - Start everyone at H0 (default)
#'   - Or spread initial health across bins (for robustness / diagnostics)
#'
#' @param H0 Numeric. Default starting health if not spreading agents.
#'
#' @param spread_initial_over_bins Logical.
#'   - FALSE (default): all agents start at H0 (or a single starting point).
#'   - TRUE: agents are distributed across health bins at t=0 (useful to see
#'           whether policies behave similarly across the health range).
#'
#' @param rep_mode Character. If spreading across bins, which representative
#'   health point in each bin should be used when assigning agents?
#'   Must be one of: "mid", "lower", "upper".
#'
#'   IMPORTANT DISTINCTION:
#'     - bin_eval (in policy_args) controls how the DP evaluates bins.
#'     - rep_mode (in sim_args) controls how you *instantiate* agents when
#'       spreading them across bins.
#'   You may choose them to match (often sensible), but they are logically distinct.
#'
#' @param shuffle Logical.
#'   - TRUE: randomize which agent gets which bin (avoids patterned ordering).
#'   - FALSE: deterministic ordering (useful for exact reproducibility in debugging).
#'
#' @return A named list, suitable for passing to the health simulation function.
health_sim_args <- function(
    H0 = 35,                               # default starting health if not spreading agents
    spread_initial_over_bins = FALSE,      # TRUE -> distribute starting health across bins
    rep_mode = c("mid","lower","upper"),   # representative point per bin when spreading
    shuffle  = TRUE                        # whether to randomize agent-bin assignment
) {
  # --- STEP 2A: Validate categorical argument choice --------------------------
  # Again: match.arg() prevents typos and forces consistent casing across scripts.
  rep_mode <- match.arg(rep_mode)

  # --- STEP 2B: Return a tidy, named list ------------------------------------
  # This list will be passed to your forward simulation function, typically:
  #   simulate_agents_forward_health(..., sim_args = <this list>)
  #
  # In other words: health_sim_args() standardizes *initial conditions*.
  list(
    H0 = H0,
    spread_initial_over_bins = spread_initial_over_bins,
    rep_mode = rep_mode,
    shuffle  = shuffle
  )
}

