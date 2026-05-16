# =============================================================================
# File: R/plotting/compare_models/00_data_prep_compare_model_visuals.R
# Purpose:
#   Construct all *tidy*, analysis-ready datasets used by the comparative model
#   visualization pipeline.
#
# Design philosophy:
#   - This file does NO plotting.
#   - It prepares summarized, long-form datasets suitable for ggplot faceting
#     across models, environments, and costs.
#   - It modularizes logic that used to live inline in
#     scripts/pipelines/09_compare_model_visuals.R.
#
# Outputs (returned as a named list by build_real_datasets()):
#   - hv_model_vs_k: hypervigilance vs cost summaries
#   - hv_ac: autocorrelation sweeps
#   - episode_stage: early/mid/late episode summaries
#   - profile: model-specific stage weighting profiles
#   - indiff: tie (indifference) frequency in optimal policies
#   - health_action_mix: action mixture summaries for health models
#   - peaks: peak/collapse diagnostics derived from hv_model_vs_k
#
# Exposed functions:
#   - build_real_datasets()
#   - build_health_action_mix()
# =============================================================================


# -----------------------------------------------------------------------------
# Package imports (quietly)
# -----------------------------------------------------------------------------
# suppressPackageStartupMessages() keeps pipeline logs clean.
# These are core tidyverse-style helpers used throughout:
#   - dplyr: mutate/select/group_by/summarise/transmute
#   - purrr: map_dfr/pmap_dfr for iteration over models/environments/costs
#   - tibble: explicit tibble() construction
#   - tidyr: reshaping (pivot_longer)
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
})


# -----------------------------------------------------------------------------
# Source shared helpers
# -----------------------------------------------------------------------------
# plot_utils.R:
#   - provides model dispatch (get_model), caching wrappers (mem_compute_policy,
#     mem_simulate_agents), and possibly default_model_specs().
#
# utils_health_env_scenarios.R:
#   - provides default_health_env_scenarios() which defines the canonical set of
#     environment conditions and their (LA, LL) values.
#
# hv_rate_helpers.R:
#   - defines compute_hv_rates_from_agent_data(), the *canonical* HV definition
#     used consistently across the project (especially â€œsafe/no-stressorâ€ HV).
source("R/core/plot_utils.R")
source("R/plotting/_shared/utils_health_env_scenarios.R")
source("R/core/hv_rate_helpers.R")


# -----------------------------------------------------------------------------
# Default model specifications for the comparative pipeline
# -----------------------------------------------------------------------------
# This wrapper exists so compare-model code can call a â€œcompare-defaultâ€ function,
# even if the project-wide default_model_specs() changes later.
#
# It also makes it easy to override the health ceiling h0 from shared defaults.
# -----------------------------------------------------------------------------

#' Default model specifications (matches the pipeline presets).
#'
#' @param h0 Health ceiling (default from shared config)
default_compare_model_specs <- function(h0 = validate_default("h0")) {
  # default_model_specs() is expected to be defined upstream (plot_utils/shared_config).
  # It should return a data.frame with columns like:
  #   model_id, model_label, model_type, policy_args, sim_args
  default_model_specs(h0 = h0)
}


# -----------------------------------------------------------------------------
# Canonical environment grid
# -----------------------------------------------------------------------------
# Comparative visuals operate over a predefined set of environments.
# Each environment is characterized by:
#   - env_condition: label used in plots/tables (e.g. "L-P", "H-U", etc.)
#   - LA: stressor appearance probability/rate
#   - LL: stressor disappearance probability/rate
#
# This function standardizes how that environment grid is represented so every
# pipeline step uses the same columns and naming.
# -----------------------------------------------------------------------------
default_compare_env_grid <- function() {
  df <- default_health_env_scenarios()

  df %>%
    tibble::as_tibble() %>%
    dplyr::select(
      # Standardize naming: downstream expects env_condition, LA, LL
      env_condition = env_label,
      LA,
      LL
    )
}


# -----------------------------------------------------------------------------
# Compare-model pipeline defaults (â€œparameter bagâ€)
# -----------------------------------------------------------------------------
# Instead of passing a long list of scalar arguments into every function,
# this pipeline uses a single named list (â€œparamsâ€) containing:
#   - cost sweep values (K_values)
#   - baseline cost parameters (K, C, D, d)
#   - simulation controls (T_steps, states, N_agents)
#   - mechanism parameters (autocorr_step)
#   - episode staging breaks (stage_breaks)
#   - tie_K: K used when we specifically want tie prevalence / episode panels
#
# Benefits:
#   - easier to override subsets in one place
#   - fewer helper signatures to maintain
#   - avoids silent mismatch where one helper uses T=10 and another uses T=12
# -----------------------------------------------------------------------------

#' Compare-model pipeline defaults (shared across every helper).
default_compare_model_params <- function() {
  list(
    # Costs to sweep over in HV-vs-cost plots
    K_values = c(1, 3, 5, 7, 9),

    # â€œBaseâ€ K when not sweeping (used in some summaries)
    K = 5,

    # Model costs/damages (project conventions: C baseline relaxed cost, D/d damage)
    C = 0,
    D = 10,
    d = 0,

    # Decision horizon / episode length
    T_steps = 10,

    # DP states used by the basic model (and possibly mirrored by health model)
    # (often corresponds to "K", "Kd", "C", "CD" in your project)
    states = c("K", "Kd", "C", "CD"),

    # Simulation size for Monte Carlo summaries
    N_agents = 1000,

    # Step size used in autocorrelation sweeps
    autocorr_step = 0.05,

    # Episode staging boundaries:
    #   time in [0,3] â†’ Early, (3,7] â†’ Mid, (7,Inf] â†’ Late (labels defined later)
    stage_breaks = c(0, 3, 7, Inf),

    # When we want to emphasize â€œties/indifferenceâ€ behavior, use a central K
    tie_K = 5
  )
}


# -----------------------------------------------------------------------------
# Parameter resolver (merge user overrides with defaults)
# -----------------------------------------------------------------------------
# Many plotting pipelines call build_real_datasets(params = list(...)).
# resolve_compare_model_params() ensures:
#   - missing parameters fall back to canonical defaults
#   - user-supplied values override those defaults
#
# modifyList() is a base R way to do "defaults %<- user overrides".
resolve_compare_model_params <- function(params = list()) {
  if (is.null(params)) params <- list()
  modifyList(default_compare_model_params(), params)
}


# -----------------------------------------------------------------------------
# Build action-mix summaries for health models
# -----------------------------------------------------------------------------
# For each health model variant Ã— environment:
#   1) Compute optimal policy (DP)
#   2) Simulate N agents forward using that policy
#   3) Compute proportions of actions:
#        - High (vigilant)
#        - Low  (relaxed)
#        - Tie  (indifferent)
#
# Output:
#   A long-form tibble with columns:
#     model, env_condition, action, proportion
#
# This is ideal for stacked bar charts or facet grids.
# -----------------------------------------------------------------------------
build_health_action_mix <- function(
    model_specs = default_compare_model_specs(),
    env_grid = default_compare_env_grid(),
    params = list()
) {
  # Merge params with defaults so downstream code can assume fields exist
  params <- resolve_compare_model_params(params)

  # Pull out frequently used params into locals for readability
  # (this also prevents repeatedly indexing params$... in inner loops)
  K <- params$K
  C <- params$C
  D <- params$D
  d <- params$d
  T_steps <- params$T_steps
  states <- params$states
  N_agents <- params$N_agents

  # Restrict to health models only, since the â€œactionâ€ columns here assume
  # the health simulation schema (and because the basic model may not have ties
  # in the same way depending on implementation).
  specs_health <- dplyr::filter(model_specs, model_type == "health")

  # Outer loop over models (rows in specs_health)
  purrr::map_dfr(seq_len(nrow(specs_health)), function(i) {
    spec <- specs_health[i, ]

    # Inner loop over environments (rows in env_grid)
    purrr::pmap_dfr(env_grid, function(env_condition, LA, LL) {

      # --- Step 1: compute policy --------------------------------------------
      # mem_compute_policy() is a cached wrapper. It will return quickly if the
      # policy has already been computed and stored in cache.
      pol <- mem_compute_policy(
        model = spec$model_type,  # "health"
        K = K, C = C, D = D, d = d,
        LA = LA, LL = LL,
        T_steps = T_steps,
        states = states,
        policy_args = spec$policy_args[[1]]
      )

      # --- Step 2: simulate forward ------------------------------------------
      # mem_simulate_agents() is also cached and keyed by (policy + env + costs + args).
      sim <- mem_simulate_agents(
        model = spec$model_type,
        policy_df = pol,
        LA = LA, LL = LL,
        T_steps = T_steps,
        N_agents = N_agents,
        K = K, C = C, D = D, d = d,
        sim_args = spec$sim_args[[1]]
      )

      # --- Step 3: action proportions ----------------------------------------
      # Here we compute the fraction of agent-time rows with each action label.
      # This is pooled across all agents and timesteps.
      tibble::tibble(
        model = spec$model_label,
        env_condition = env_condition,
        vigilant = mean(sim$agent_data$action == "High", na.rm = TRUE),
        relaxed  = mean(sim$agent_data$action == "Low",  na.rm = TRUE),
        tied     = mean(sim$agent_data$action == "Tie",  na.rm = TRUE)
      )
    })
  }) %>%
    # Convert wide columns (vigilant/relaxed/tied) to long form:
    #   action âˆˆ {vigilant, relaxed, tied}
    #   proportion âˆˆ [0,1]
    tidyr::pivot_longer(
      c(vigilant, relaxed, tied),
      names_to = "action",
      values_to = "proportion"
    )
}


# -----------------------------------------------------------------------------
# Sweep hypervigilance rates across vigilance cost K
# -----------------------------------------------------------------------------
# For a FIXED environment (LA, LL), and for each model variant:
#   For each K in params$K_values:
#     1) compute optimal policy
#     2) simulate forward
#     3) compute canonical HV metrics from agent_data
#
# Key definition detail:
#   hv_rate is taken from HypervigilanceRate_filtered, which is:
#     mean(hv) pooled across all agent-time rows where the stressor is absent.
#
# This matches the "safe HV rate" interpretation and aligns with environment heatmaps.
# -----------------------------------------------------------------------------
run_model_vs_k_hv <- function(
    LA,
    LL,
    model_specs = default_compare_model_specs(),
    params = list()
) {
  params <- resolve_compare_model_params(params)

  # Sanity checks: we need at least one K to sweep, and model_specs must be a df.
  stopifnot(length(params$K_values) > 0L, is.data.frame(model_specs))

  # Outer loop: iterate model variants
  purrr::map_dfr(seq_len(nrow(model_specs)), function(i) {
    spec <- model_specs[i, ]

    # Pull health/basic specific argument lists from list-columns
    policy_args <- spec$policy_args[[1]]
    sim_args <- spec$sim_args[[1]]

    # Inner loop: iterate cost values
    purrr::map_dfr(params$K_values, function(K) {

      # --- Policy computation (cached) ---------------------------------------
      policy <- mem_compute_policy(
        model = spec$model_type,
        K = K, C = params$C, D = params$D, d = params$d,
        LA = LA, LL = LL,
        T_steps = params$T_steps,
        states = params$states,
        policy_args = policy_args
      )

      # --- Simulation (cached) ------------------------------------------------
      sim <- mem_simulate_agents(
        model = spec$model_type,
        policy_df = policy,
        LA = LA, LL = LL,
        T_steps = params$T_steps,
        N_agents = params$N_agents,
        K = K, C = params$C, D = params$D, d = params$d,
        sim_args = sim_args
      )

      # --- Canonical HV metric extraction ------------------------------------
      # compute_hv_rates_from_agent_data() returns multiple HV-related summaries.
      # We use the filtered (no-stressor) pooled mean as "hv_rate".
      hv_rates <- compute_hv_rates_from_agent_data(
        sim$agent_data,
        T_steps = params$T_steps,
        N_agents = params$N_agents
      )

      hv_filtered <- hv_rates$HypervigilanceRate_filtered

      # hv_draws: here represents how many no-stressor rows contributed.
      # This is *not* number of agents; itâ€™s agentÃ—time rows filtered to str==0.
      tibble::tibble(
        model_id = spec$model_id,
        model_label = spec$model_label,
        model_type = spec$model_type,
        K = K,

        # Canonical â€œsafe HVâ€ pooled mean:
        hv_rate = hv_filtered,

        # Use spread among no-stressor rows as an SD-like variability diagnostic.
        # (Not the same as SD across agents; it is SD across pooled rows.)
        hv_rate_sd = hv_rates$Spread_no_stressor,

        # Count of contributing rows (pooled)
        hv_draws = hv_rates$n_no_stressor_rows,

        # Include extra columns for debugging/traceability:
        HypervigilanceRate_filtered = hv_filtered,
        HypervigilanceRate_all = hv_rates$HypervigilanceRate_all,
        Spread_no_stressor = hv_rates$Spread_no_stressor
      )
    })
  }) %>%
    # Wrap into a list to keep API compatible with earlier pipeline versions
    # that returned list(df = ...).
    list(df = .)
}


# -----------------------------------------------------------------------------
# Master dataset builder for comparative visuals
# -----------------------------------------------------------------------------
# build_real_datasets() is the main pipeline entry point used by plotting scripts.
#
# It produces everything needed for the compare-model figures:
#   - hv_model_vs_k: HV rate vs cost (at a reference environment)
#   - hv_ac: HV vs autocorrelation sweeps (symmetry grid tool)
#   - episode_stage: Early/Mid/Late summaries over env_grid
#   - profile: per-model weights over episode stages
#   - indiff: tie frequency in optimal policy across env_grid
#   - health_action_mix: (health only) action distribution across env_grid
#   - peaks: derived summary of peak and â€œcollapseâ€ costs
# -----------------------------------------------------------------------------
build_real_datasets <- function(
    model_specs = default_compare_model_specs(),
    env_grid = default_compare_env_grid(),
    params = list()
) {
  params <- resolve_compare_model_params(params)

  # -----------------------------------------------------------
  # 1) Hypervigilance vs cost (single reference environment)
  # -----------------------------------------------------------
  # This is a *single* LA/LL environment used for the â€œHV vs Kâ€ comparison plot.
  # You hardcode LA=0.2, LL=0.3 here as a representative environment.
  # (If you later want this configurable, pass it via params instead.)
  hv_model_vs_k <- run_model_vs_k_hv(
    LA = 0.2,
    LL = 0.3,
    model_specs = model_specs,
    params = params
  )$df

  # -----------------------------------------------------------
  # 2) Autocorrelation sweeps across cost
  # -----------------------------------------------------------
  # symmetry_grid_K_vs_autocorr() appears to be a specialized generator that:
  #   - sweeps over K and some autocorrelation axis
  #   - returns HypervigilanceRate_all (and possibly other metrics)
  #
  # Here we run it once per model variant and then standardize column names.
  hv_ac <- purrr::map_dfr(seq_len(nrow(model_specs)), function(i) {
    spec <- model_specs[i, ]

    df <- symmetry_grid_K_vs_autocorr(
      C = params$C,
      d = params$d,
      deltaD = params$D,
      K_values = params$K_values,
      T_steps = params$T_steps,
      states = params$states,
      N_agents = params$N_agents,
      step = params$autocorr_step,
      model = spec$model_type,
      policy_args = spec$policy_args[[1]],
      sim_args = spec$sim_args[[1]]
    )

    # Standardize output columns so plotting scripts donâ€™t depend on the generator schema.
    df %>%
      transmute(
        model_id = spec$model_id,
        model = spec$model_label,
        model_type = spec$model_type,
        cost = K,
        ac = autocorr,
        hv_level = HypervigilanceRate_all
      )
  })

  # -----------------------------------------------------------
  # 3) Full episode-level panel data (time Ã— action)
  # -----------------------------------------------------------
  # This produces a â€œlongâ€ dataset of raw trajectories for plotting:
  #   - model Ã— environment Ã— time Ã— (action/hv)
  #
  # tie_K is used here so policies are computed under a consistent K that tends
  # to produce some indifference region (so we can visualize behavior around ties).
  episode_panel <- purrr::map_dfr(seq_len(nrow(model_specs)), function(i) {
    spec <- model_specs[i, ]

    purrr::pmap_dfr(env_grid, function(env_condition, LA, LL) {

      # Policy for this model/env under tie_K
      pol <- mem_compute_policy(
        model = spec$model_type,
        K = params$tie_K,
        C = params$C,
        D = params$D,
        d = params$d,
        LA = LA,
        LL = LL,
        T_steps = params$T_steps,
        states = params$states,
        policy_args = spec$policy_args[[1]]
      )

      # Simulation under same tie_K
      sim <- mem_simulate_agents(
        model = spec$model_type,
        policy_df = pol,
        LA = LA,
        LL = LL,
        T_steps = params$T_steps,
        N_agents = params$N_agents,
        K = params$tie_K,
        C = params$C,
        D = params$D,
        d = params$d,
        sim_args = spec$sim_args[[1]]
      )

      # Keep only what we need for time-course plots:
      #   action: High/Low/Tie
      #   hv: numeric or logical hypervigilance indicator
      sim$agent_data %>%
        transmute(
          model = spec$model_label,
          env_condition = env_condition,
          time = time,
          action = action,
          hv = hv
        )
    })
  })

  # -----------------------------------------------------------
  # 4) Aggregate episode stages (Early / Mid / Late)
  # -----------------------------------------------------------
  # We discretize the timeline into stages using params$stage_breaks
  # (default: Early 0â€“3, Mid 3â€“7, Late 7+).
  #
  # Then we compute:
  #   - vigilance: fraction of rows with action == "High"
  #   - hv_rate: mean(hv) (note: this hv is whatever hv column encodes;
  #            it may or may not match canonical "no-stressor" hv rate)
  episode_stage <- episode_panel %>%
    mutate(
      episode_stage = cut(
        time,
        breaks = params$stage_breaks,
        labels = c("Early", "Mid", "Late"),
        include.lowest = TRUE
      )
    ) %>%
    group_by(model, env_condition, episode_stage) %>%
    summarise(
      vigilance = mean(action == "High"),
      hv_rate = mean(hv),
      .groups = "drop"
    ) %>%
    arrange(env_condition, episode_stage)

  # -----------------------------------------------------------
  # 5) Model-specific episode-stage weighting profiles
  # -----------------------------------------------------------
  # For some figures, you may want a single per-model profile describing where
  # vigilance happens over the episode (Early/Mid/Late).
  #
  # weight is normalized within model:
  #   weight(stage) = vigilance(stage) / sum_stage vigilance(stage)
  profile <- episode_stage %>%
    group_by(model) %>%
    mutate(weight = vigilance / sum(vigilance)) %>%
    ungroup() %>%
    transmute(model, phase = episode_stage, weight)

  # -----------------------------------------------------------
  # 6) Indifference (Tie) frequency in optimal policies
  # -----------------------------------------------------------
  # This inspects the *policy table itself* (not simulations) to measure how often
  # "Tie" is the optimal action across states/time entries in the policy grid.
  #
  # We need to handle inconsistent naming:
  #   - some policies might use "optimal_action"
  #   - others might use "Optimal_Action"
  indiff <- purrr::map_dfr(seq_len(nrow(model_specs)), function(i) {
    spec <- model_specs[i, ]

    purrr::pmap_dfr(env_grid, function(env_condition, LA, LL) {

      pol <- mem_compute_policy(
        model = spec$model_type,
        K = params$tie_K,
        C = params$C,
        D = params$D,
        d = params$d,
        LA = LA,
        LL = LL,
        T_steps = params$T_steps,
        states = params$states,
        policy_args = spec$policy_args[[1]]
      )

      # Determine which action column exists
      act_col <- if ("optimal_action" %in% names(pol)) {
        "optimal_action"
      } else if ("Optimal_Action" %in% names(pol)) {
        "Optimal_Action"
      } else {
        NULL
      }

      # Extract actions if possible; otherwise treat as empty
      acts <- if (!is.null(act_col)) pol[[act_col]] else character(0)

      # freq = fraction of policy entries that are ties
      tibble::tibble(
        model = spec$model_label,
        env = env_condition,
        freq = if (length(acts) == 0) NA_real_
               else mean(acts == "Tie", na.rm = TRUE)
      )
    })
  })

  # -----------------------------------------------------------
  # 7) Health-model action mix
  # -----------------------------------------------------------
  # This is the action mixture summary specifically for health models.
  # It reuses build_health_action_mix() which already filters model_specs.
  health_action_mix <- build_health_action_mix(
    model_specs = model_specs,
    env_grid = env_grid,
    params = params
  )

  # -----------------------------------------------------------
  # 8) Peak and collapse diagnostics
  # -----------------------------------------------------------
  # Derived summary from hv_model_vs_k:
  #   - peak_cost: K where hv_rate is maximal
  #   - collapse_cost: first K where hv_rate drops to <= 0.05 (if it ever does)
  #
  # This is useful for labeling plots or reporting â€œcollapse thresholdsâ€.
  peaks <- hv_model_vs_k %>%
    group_by(model_label) %>%
    summarise(
      peak_cost = K[which.max(hv_rate)],
      collapse_cost = if (any(hv_rate <= 0.05, na.rm = TRUE))
                        min(K[hv_rate <= 0.05])
                      else NA,
      .groups = "drop"
    ) %>%
    rename(model = model_label)

  # -----------------------------------------------------------
  # Return all datasets as a named list
  # -----------------------------------------------------------
  # This is the standard pattern used in plotting pipelines:
  #   ds <- build_real_datasets(...)
  #   ds$hv_model_vs_k â†’ plot HV vs K
  #   ds$episode_stage â†’ plot early/mid/late bars
  #   ds$indiff â†’ plot tie frequency heatmaps
  list(
    hv_ac = hv_ac,
    episode_stage = episode_stage,
    peaks = peaks,
    indiff = indiff,
    profile = profile,
    hv_model_vs_k = hv_model_vs_k,
    health_action_mix = health_action_mix
  )
}
