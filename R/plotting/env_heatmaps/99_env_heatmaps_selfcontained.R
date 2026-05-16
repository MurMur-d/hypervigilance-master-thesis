#!/usr/bin/env Rscript
# =============================================================================
# File: R/plotting/env_heatmaps/99_env_heatmaps_selfcontained_restructured.R
#
# Purpose
#   Build environment heatmaps showing hypervigilance frequency over a grid of
#   environments, for multiple model variants and multiple vigilance costs K.
#
# What you get
#   - A faceted heatmap: rows = model variant, columns = K
#   - Optional overlays: region boundaries + region labels
#   - A side "key" panel showing the region coding scheme in LA/LL space
#   - Saved outputs in PNG and/or PDF
#
# External (project) dependencies (MUST already be loaded in R session):
#   - compute_optimal_policy(...)               [basic DP]
#   - compute_optimal_policy_health(...)        [health DP]
#   - simulate_agents_forward(...)              [basic simulation]
#   - simulate_agents_forward_health(...)       [health simulation]
#
# Minimal run:
#   source("R/plotting/env_heatmaps/99_env_heatmaps_selfcontained_restructured.R")
#   build_and_save_env_heatmaps_all_models()
#
# Notes on interpretation
#   - x-axis: LA = P(arrive)  (0 -> 1 transition prob for stressor)
#   - y-axis: LL = P(leave)   (1 -> 0 transition prob for stressor)
#   - SSP = LA / (LA + LL)    stationary stressor probability (if LA+LL>0)
#   - autocorr ~ 1-(LA+LL)    persistence proxy (higher = more predictable)
#
# Hypervigilance metric used for fill:
#   HypervigilanceRate_filtered = mean(hv | stressor == 0)
#   i.e., vigilance on "safe" steps only.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(gtable)
  library(patchwork)
  library(cowplot)
  library(scales)
})

# -----------------------------------------------------------------------------
# 0) Optional parallelization (safe fallback to lapply)
# -----------------------------------------------------------------------------
.maybe_parallel_lapply <- function(x, fun) {
  # If future.apply is available, use it; else base lapply.
  if (requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply(x, fun, future.seed = TRUE)
  } else {
    lapply(x, fun)
  }
}

if (requireNamespace("future", quietly = TRUE)) {
  # Only set a multisession plan if none is already set
  if (!inherits(future::plan(), "multisession")) {
    future::plan(future::multisession, workers = max(1, parallel::detectCores() - 1))
  }
}

# -----------------------------------------------------------------------------
# 1) Subtitle + metadata helpers (used to stamp parameters on the plots)
# -----------------------------------------------------------------------------
format_subtitle_value <- function(x) {
  if (is.null(x)) return("?")
  if (length(x) > 1) return(paste(x, collapse = ","))
  format(x, trim = TRUE, scientific = FALSE)
}

# Recursively search for h0 in nested lists (policy args, sim args, etc.)
find_h0_from_entry <- function(entry) {
  if (is.null(entry)) return(NULL)

  is_list_of_lists <- is.list(entry) && length(entry) > 0 &&
    all(vapply(entry, is.list, logical(1), USE.NAMES = FALSE))

  if (is_list_of_lists) {
    values <- unlist(lapply(entry, find_h0_from_entry), use.names = FALSE)
    if (length(values) == 0) return(NULL)
    return(unique(values))
  }

  for (key in c("h0", "H0")) {
    if (!is.null(entry[[key]])) return(entry[[key]])
  }
  NULL
}

find_h0_from_meta <- function(meta) {
  if (is.null(meta)) return(NULL)
  candidate_args <- list(
    meta$policy_args,
    meta$sim_args,
    meta$subtitle_meta$policy_args,
    meta$subtitle_meta$sim_args
  )
  values <- unlist(lapply(candidate_args, find_h0_from_entry), use.names = FALSE)
  if (length(values) == 0) return(NULL)
  unique(values)
}

append_h0_to_subtitle <- function(subtitle, meta) {
  h0_vals <- find_h0_from_meta(meta)
  if (is.null(h0_vals) || length(h0_vals) == 0) return(subtitle)
  h0_part <- paste0("H0 = ", paste(unique(h0_vals), collapse = ", "))
  if (is.null(subtitle) || subtitle == "") return(h0_part)
  paste(subtitle, h0_part, sep = " | ")
}

hv_subtitle_with_params <- function(meta, subtitle_context = NULL) {
  if (is.null(meta)) meta <- list()

  base_subtitle <- sprintf(
    "C = %s | D = %s | d = %s | T = %s | N = %s",
    format_subtitle_value(meta$C),
    format_subtitle_value(meta$D),
    format_subtitle_value(meta$d),
    format_subtitle_value(meta$T_steps),
    format_subtitle_value(meta$N_agents)
  )

  subtitle_final <- if (is.null(subtitle_context) || identical(subtitle_context, "")) {
    base_subtitle
  } else {
    paste(subtitle_context, base_subtitle, sep = " | ")
  }

  append_h0_to_subtitle(subtitle_final, meta)
}

# -----------------------------------------------------------------------------
# 2) Hypervigilance extraction + metrics
#    Goal: robustly compute HV rates from simulation agent_data even if column
#    names differ between models.
# -----------------------------------------------------------------------------
pick_first_column <- function(data, candidates) {
  # Return the first matching name in 'candidates' that exists in names(data)
  if (is.null(candidates) || length(candidates) == 0) return(NULL)
  for (candidate in candidates) {
    if (candidate %in% names(data)) return(candidate)
  }
  NULL
}

extract_hypervigilance_vectors <- function(agent_data) {
  # Flexible column naming across simulation schemas
  hv_col   <- pick_first_column(agent_data, c("hv", "Hypervigilance"))
  str_col  <- pick_first_column(agent_data, c("str", "stressor", "Str", "Stressor", "stressor_present"))
  time_col <- pick_first_column(agent_data, c("time", "Time"))
  ag_col   <- pick_first_column(agent_data, c("agent", "Agent"))

  n_rows <- nrow(agent_data)
  if (is.null(n_rows)) n_rows <- 0L

  hv_vec   <- if (!is.null(hv_col))  as.numeric(agent_data[[hv_col]]) else rep(NA_real_, n_rows)
  str_vec  <- if (!is.null(str_col)) as.integer(agent_data[[str_col]]) else rep(NA_integer_, n_rows)
  time_vec <- if (!is.null(time_col)) agent_data[[time_col]] else seq_len(n_rows)
  ag_vec   <- if (!is.null(ag_col))   agent_data[[ag_col]] else seq_len(n_rows)

  list(hv = hv_vec, str = str_vec, time = time_vec, agent = ag_vec)
}

compute_safe_hypervigilance_counts <- function(hv, str, agent, time, action = NULL) {
  n <- length(hv)
 
  if (n == 0) {
    return(list(safe_steps = 0L, safe_hv = 0L, prevent = 0L, spill = 0L))
  }
 
  # --- Standardize inputs ---------------------------------------------------
  hv_flag <- as.logical(hv)
  hv_flag[is.na(hv_flag)] <- FALSE
 
  str_int <- as.integer(str)
  if (any(is.na(str_int))) {
    stop("Stressor vector contains NA values; please correct missing stressor data.")
  }
 
  # Determine vigilance: use action column if available, otherwise infer
  # from hv + stressor (action=High when hv=TRUE OR when stressor=1 and prepared)
  if (!is.null(action)) {
    action_high <- as.integer(toupper(as.character(action)) == "HIGH")
    action_high[is.na(action_high)] <- 0L
  } else {
    # Fallback: hv is TRUE only for High+no_stressor, so we can't detect
    # High+stressor from hv alone. Warn and use old behavior.
    warning(
      "compute_safe_hypervigilance_counts: 'action' column not provided. ",
      "Cannot distinguish reactive vigilance (High during stressor). ",
      "Falling back to legacy behavior (prev_str-based split)."
    )
    # Legacy fallback (old code)
    action_high <- NULL
  }
  safe_steps <- sum(str_int == 0L, na.rm = TRUE)
 
  # --- Sort by (agent, time) ------------------------------------------------
  agent_chr <- as.character(agent)
  time_vals <- time
  if (length(time_vals) != n) time_vals <- seq_len(n)
 
  order_idx <- order(agent_chr, time_vals, na.last = TRUE)
 
  hv_ord    <- hv_flag[order_idx]
  str_ord   <- str_int[order_idx]
  agent_ord <- agent_chr[order_idx]
 
  if (!is.null(action_high)) {
    action_ord <- action_high[order_idx]
  }
 
  # --- Build next-step stressor context -------------------------------------
  same_agent <- c(agent_ord[-1L] == agent_ord[-n], FALSE)
 
  # next_str: stressor at t+1 for same agent
  next_str <- c(str_ord[-1L], NA_integer_)
  next_str[!same_agent] <- NA_integer_
 
  # --- Count hypervigilance using thesis definitions ------------------------
  if (!is.null(action_high)) {
    # NEW CORRECT DEFINITIONS:
    # Vigilance = action_high == 1 (agent chose "High" at time t)
    # Mismatch  = next_str == 0 (environment is safe at t+1)
    #
    # Anticipatory: vigilance while safe (str=0), next state also safe (next_str=0)
    # Reactive:     vigilance while danger (str=1), next state safe (next_str=0)
 
    vigilant <- action_ord == 1L
    mismatch <- !is.na(next_str) & next_str == 0L
 
    # Anticipatory mismatch: vigilant in safe state, followed by safe
    prevent <- sum(vigilant & str_ord == 0L & mismatch, na.rm = TRUE)
 
    # Reactive mismatch: vigilant in danger state, followed by safe
    spill <- sum(vigilant & str_ord == 1L & mismatch, na.rm = TRUE)
 
    # Total hypervigilance: vigilance followed by safe state (regardless of current)
    safe_hv <- prevent + spill
 
  } else {
    # LEGACY FALLBACK (old prev_str-based behavior)
    prev_str <- c(NA_integer_, str_ord[-n])
    prev_same <- c(FALSE, agent_ord[-1L] == agent_ord[-n])
    prev_str[!prev_same] <- NA_integer_
 
    safe_hv_idx <- hv_ord & (str_ord == 0L)
    safe_hv_idx[is.na(safe_hv_idx)] <- FALSE
 
    prevent <- sum(safe_hv_idx & prev_str == 0L, na.rm = TRUE)
    spill   <- sum(safe_hv_idx & prev_str == 1L, na.rm = TRUE)
    safe_hv <- sum(safe_hv_idx, na.rm = TRUE)
  }
 
  list(
    safe_steps = safe_steps,
    safe_hv    = safe_hv,
    prevent    = prevent,
    spill      = spill
  )
}

extract_hypervigilance_metadata <- function(agent_data) {
  hv_data <- extract_hypervigilance_vectors(agent_data)
 
  # Extract action column if available
  action_col <- pick_first_column(agent_data, c("action", "Action", "optimal_action"))
  action_vec <- if (!is.null(action_col)) agent_data[[action_col]] else NULL
 
  safe_counts <- compute_safe_hypervigilance_counts(
    hv_data$hv, hv_data$str, hv_data$agent, hv_data$time,
    action = action_vec
  )
 
  list(
    hv    = hv_data$hv,
    str   = hv_data$str,
    time  = hv_data$time,
    agent = hv_data$agent,
    safe_steps = safe_counts$safe_steps,
    safe_hv    = safe_counts$safe_hv,
    prevent    = safe_counts$prevent,
    spill      = safe_counts$spill
  )
}

compute_hv_rates_from_agent_data <- function(agent_data, T_steps = NULL, N_agents = NULL) {
  # Primary outputs:
  # - HypervigilanceRate_all: mean(hv) over all steps
  # - HypervigilanceRate_filtered: mean(hv | stressor==0)  [THIS is what you plot]
  # Additional diagnostics:
  # - Spread_no_stressor: sd(hv | stressor==0)
  # - coverage approximations

  hv_meta <- extract_hypervigilance_metadata(agent_data)

  hv_num <- as.numeric(hv_meta$hv)
  stressor_int <- as.integer(hv_meta$str)

  mask_no_stress <- !is.na(stressor_int) & stressor_int == 0L
  hv_filtered_vals <- hv_num[mask_no_stress & !is.na(hv_num)]

  m0 <- function(x) {
    m <- mean(x, na.rm = TRUE)
    ifelse(is.finite(m), m, 0)
  }
  s0 <- function(x) {
    s <- stats::sd(x, na.rm = TRUE)
    ifelse(is.finite(s), s, 0)
  }

  hyper_all      <- m0(hv_num)
  hyper_filtered <- m0(hv_filtered_vals)
  spread_no_stressor <- s0(hv_filtered_vals)

  # Diagnostics: how much of the horizon & agent-time grid did we observe?
  prop_timesteps_included <- NA_real_
  if (!is.null(T_steps) && is.numeric(T_steps) && T_steps > 0 && length(hv_meta$time) > 0) {
    unique_times <- unique(hv_meta$time[!is.na(hv_meta$time)])
    prop_timesteps_included <- if (length(unique_times) > 0) min(1, length(unique_times) / T_steps) else 0
  }

  prop_agent_timesteps <- NA_real_
  if (!is.null(N_agents) && is.numeric(N_agents) && N_agents > 0 &&
      !is.null(T_steps) && is.numeric(T_steps) && T_steps > 0) {
    prop_agent_timesteps <- min(1, nrow(agent_data) / (N_agents * T_steps))
  }

  list(
    HypervigilanceRate_all = hyper_all,
    HypervigilanceRate_filtered = hyper_filtered,
    Spread_no_stressor = spread_no_stressor,
    Prop_timesteps_included = prop_timesteps_included,
    Prop_agent_timesteps = prop_agent_timesteps,
    n_no_stressor_rows = sum(mask_no_stress, na.rm = TRUE),
    hv_meta = hv_meta,
    hv_filtered_values = hv_filtered_vals
  )
}

# -----------------------------------------------------------------------------
# 3) Model scenario definitions + environment “named scenarios”
# -----------------------------------------------------------------------------
default_model_specs <- function(h0 = 35) {
  # Each row defines:
  # - model_id: machine-friendly id
  # - model_label: display label
  # - model_type: dispatch key for DP/sim wrapper ("basic" or "health")
  # - policy_args: extra args for compute_optimal_policy_health
  # - sim_args: extra args for simulate_agents_forward_health
  data.frame(
    model_id = c("basic", "health_no_tr", "health_tr_w1", "health_power_a3", "health_thresh_tau60pct"),
    model_label = c(
      "Basic",
      "Health (phi = 0)",
      "Linear (phi = 1)",
      "Power (alpha = 3)",
      "Threshold (tau = 0.6*H0)"
    ),
    model_type = c("basic", "health", "health", "health", "health"),
    policy_args = I(list(
      list(),
      list(h0 = h0, health_step = 1, terminal_reward_weight = 0, terminal_reward_mode = "linear"),
      list(h0 = h0, health_step = 1, terminal_reward_weight = 1, terminal_reward_mode = "linear"),
      list(h0 = h0, health_step = 1, terminal_reward_weight = 1,
           terminal_reward_mode = "power", terminal_power_alpha = 3),
      list(h0 = h0, health_step = 1, terminal_reward_weight = 1,
           terminal_reward_mode = "threshold", terminal_threshold_tau = round(0.6 * h0))
    )),
    sim_args = I(list(
      list(),
      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE),
      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE),
      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE),
      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE)
    )),
    stringsAsFactors = FALSE
  )
}

default_health_env_scenarios <- function() {
  # These are your “named environments” used mainly for captions/keys.
  # They map to points in LA/LL space that correspond to L/M/H SSP and P/U.
  df <- data.frame(
    env_label = c("L-P", "L-U", "M-P", "M-U", "H-P", "H-U"),
    env_full = c(
      "low SSP, predictable",
      "low SSP, unpredictable",
      "medium SSP, predictable",
      "medium SSP, unpredictable",
      "high SSP, predictable",
      "high SSP, unpredictable"
    ),
    ssp_level = c("low", "low", "medium", "medium", "high", "high"),
    predictability = c(
      "predictable", "unpredictable",
      "predictable", "unpredictable",
      "predictable", "unpredictable"
    ),
    LA = c(0.005, 0.025, 0.05, 0.40, 0.095, 0.475),
    LL = c(0.095, 0.475, 0.05, 0.40, 0.005, 0.025),
    stringsAsFactors = FALSE
  )
  df$SSP <- with(df, LA / (LA + LL))
  df
}

env_label_note <- function(env_scenarios) {
  # Produces a readable caption mapping codes to LA/LL/SSP/autocorr
  if (is.null(env_scenarios) || !is.data.frame(env_scenarios) ||
      !"env_label" %in% names(env_scenarios) || !"env_full" %in% names(env_scenarios) ||
      !"LA" %in% names(env_scenarios) || !"LL" %in% names(env_scenarios)) {
    return(paste(
      "SSP = LA / (LA + LL) (stationary stressor probability)",
      "L-P = low SSP, predictable; L-U = low SSP, unpredictable;",
      "M-P = medium SSP, predictable; M-U = medium SSP, unpredictable;",
      "H-P = high SSP, predictable; H-U = high SSP, unpredictable.",
      sep = "\n"
    ))
  }

  ssp_vals <- if ("SSP" %in% names(env_scenarios)) env_scenarios$SSP else env_scenarios$LA / (env_scenarios$LA + env_scenarios$LL)
  autocorr_vals <- 1 - (env_scenarios$LA + env_scenarios$LL)

  parts <- sprintf(
    "%s = %s (LA = %.3f, LL = %.3f, SSP = %.3f, autocorr = %.3f)",
    env_scenarios$env_label,
    env_scenarios$env_full,
    env_scenarios$LA,
    env_scenarios$LL,
    ssp_vals,
    autocorr_vals
  )

  paste(
    "SSP = LA / (LA + LL) (stationary stressor probability)",
    paste(parts, collapse = "\n"),
    sep = "\n"
  )
}

# -----------------------------------------------------------------------------
# 4) Dispatch wrappers for DP + simulation (basic vs health)
# -----------------------------------------------------------------------------
assert_dp_sim_functions <- function() {
  missing <- character(0)
  if (!exists("compute_optimal_policy", mode = "function")) missing <- c(missing, "compute_optimal_policy")
  if (!exists("simulate_agents_forward", mode = "function")) missing <- c(missing, "simulate_agents_forward")
  if (!exists("compute_optimal_policy_health", mode = "function")) missing <- c(missing, "compute_optimal_policy_health")
  if (!exists("simulate_agents_forward_health", mode = "function")) missing <- c(missing, "simulate_agents_forward_health")

  if (length(missing) > 0) {
    stop(
      "Missing required DP/simulation functions: ",
      paste(missing, collapse = ", "),
      ". Load the model code (e.g., setup_project.R) and try again."
    )
  }
}

compute_policy_for_model <- function(model, K, C, D, d, LA, LL, T_steps, states, policy_args) {
  # Single interface: returns policy data.frame for either model class.
  if (identical(model, "basic")) {
    compute_optimal_policy(
      k_cost = K, c_cost = C, d_high = D, d_low = d,
      la_prob = LA, ll_prob = LL,
      horizon = T_steps, states = states
    )
  } else if (identical(model, "health")) {
    call_args <- c(
      list(
        k_cost = K, c_cost = C, d_high = D, d_low = d,
        la_prob = LA, ll_prob = LL,
        horizon = T_steps
      ),
      policy_args
    )
    do.call(compute_optimal_policy_health, call_args)
  } else {
    stop("Unknown model type: ", model)
  }
}

simulate_for_model <- function(model, policy_df, LA, LL, T_steps, N_agents, K, C, D, d, sim_args, seed) {
  # Single interface: returns simulation output that contains $agent_data.
  if (identical(model, "basic")) {
    simulate_agents_forward(
      policy_df = policy_df,
      la_prob = LA, ll_prob = LL,
      horizon = T_steps, n_agents = N_agents,
      k_cost = K, c_cost = C, d_high = D, d_low = d,
      seed = seed
    )
  } else if (identical(model, "health")) {
    call_args <- c(
      list(
        policy_df = policy_df,
        la_prob = LA, ll_prob = LL,
        horizon = T_steps, n_agents = N_agents,
        k_cost = K, c_cost = C, d_high = D, d_low = d,
        seed = seed
      ),
      sim_args
    )
    do.call(simulate_agents_forward_health, call_args)
  } else {
    stop("Unknown model type: ", model)
  }
}

# -----------------------------------------------------------------------------
# 5) Build environment grid + evaluate HV on each grid cell
# -----------------------------------------------------------------------------
build_env_grid <- function(env_grid = NULL, grid_step = 0.05, la_range = c(0, 0.5), ll_range = c(0, 0.5)) {
  # If user supplies env_grid, it must have LA and LL columns; otherwise build a full grid.
  if (!is.null(env_grid)) {
    if (!is.data.frame(env_grid) || !all(c("LA", "LL") %in% names(env_grid))) {
      stop("env_grid must be a data.frame with columns LA and LL.")
    }
    return(env_grid %>% select(LA, LL) %>% distinct())
  }

  expand.grid(
    LA = seq(la_range[1], la_range[2], by = grid_step),
    LL = seq(ll_range[1], ll_range[2], by = grid_step),
    KEEP.OUT.ATTRS = FALSE
  )
}

hypervigilance_grid_for_K <- function(
  env_grid,
  model,
  K, C, D, d,
  T_steps, states, N_agents,
  policy_args,
  sim_args,
  seed
) {
  # Sanity constraint used throughout the project:
  # if vigilant action is always dominated, the model is degenerate.
  if ((K + d) > (C + D)) stop("Require K + d <= C + D.")

  rows <- .maybe_parallel_lapply(seq_len(nrow(env_grid)), function(i) {
    LAi <- env_grid$LA[i]
    LLi <- env_grid$LL[i]

    # 1) Compute optimal policy for this environment and K
    pol <- compute_policy_for_model(
      model = model,
      K = K, C = C, D = D, d = d,
      LA = LAi, LL = LLi,
      T_steps = T_steps,
      states = states,
      policy_args = policy_args
    )

    # 2) Simulate agents forward under that policy
    sim <- simulate_for_model(
      model = model,
      policy_df = pol,
      LA = LAi, LL = LLi,
      T_steps = T_steps,
      N_agents = N_agents,
      K = K, C = C, D = D, d = d,
      sim_args = sim_args,
      seed = seed
    )

    # 3) Compute HV metrics from sim$agent_data
    ad <- sim$agent_data
    hv_rates <- compute_hv_rates_from_agent_data(ad, T_steps = T_steps, N_agents = N_agents)
    hv_meta <- hv_rates$hv_meta

    data.frame(
      LA = LAi,
      LL = LLi,
      K = K,
      D = D,
      autocorr = pmin(pmax(1 - (LAi + LLi), 0), 1),
      HypervigilanceRate_all = hv_rates$HypervigilanceRate_all,
      HypervigilanceRate_filtered = hv_rates$HypervigilanceRate_filtered,
      HypervigilanceSafeHV = dplyr::coalesce(hv_meta$safe_hv, 0),
      HypervigilanceSafeSteps = dplyr::coalesce(hv_meta$safe_steps, 0),
      HypervigilancePreventCount = dplyr::coalesce(hv_meta$prevent, 0),
      HypervigilanceSpillCount = dplyr::coalesce(hv_meta$spill, 0),
      Spread_no_stressor = hv_rates$Spread_no_stressor,
      Prop_timesteps_included = dplyr::coalesce(hv_rates$Prop_timesteps_included, 0),
      Prop_agent_timesteps = dplyr::coalesce(hv_rates$Prop_agent_timesteps, 0),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

build_env_heatmaps_for_models <- function(
  model_specs,
  env_grid,
  K_values,
  C, D, d,
  T_steps,
  states,
  N_agents,
  seed
) {
  message("Building full LA/LL environment heatmaps for all model variants...")

  rows <- lapply(seq_len(nrow(model_specs)), function(i) {
    spec <- model_specs[i, ]
    model_type <- spec$model_type
    policy_args <- spec$policy_args[[1]]
    sim_args <- spec$sim_args[[1]]

    per_k <- lapply(K_values, function(Ki) {
      hypervigilance_grid_for_K(
        env_grid = env_grid,
        model = model_type,
        K = Ki,
        C = C, D = D, d = d,
        T_steps = T_steps,
        states = states,
        N_agents = N_agents,
        policy_args = policy_args,
        sim_args = sim_args,
        seed = seed
      )
    })

    df_env <- dplyr::bind_rows(per_k)

    # Standardized output schema for plotting:
    df_env %>%
      transmute(
        model_id = spec$model_id,
        model = spec$model_label,
        model_type = model_type,
        K,
        LA,
        LL,
        # This is the plotted metric:
        hv = HypervigilanceRate_filtered,
        ssp = ifelse(LA + LL > 0, LA / (LA + LL), NA_real_),
        autocorr = 1 - (LA + LL),
        D = D
      )
  })

  result <- dplyr::bind_rows(rows)
  result$model <- factor(result$model, levels = model_specs$model_label)

  # Attach fixed params for subtitles/captions
  attr(result, "fixed_params") <- list(
    C = C, D = D, d = d,
    T_steps = T_steps,
    states = states,
    N_agents = N_agents,
    subtitle_meta = list(
      policy_args = model_specs$policy_args,
      sim_args = model_specs$sim_args
    )
  )

  result
}

# -----------------------------------------------------------------------------
# 6) Plotting theme + grob utilities for cleaner multi-facet layout
# -----------------------------------------------------------------------------
theme_supervisor_grid <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "gray80", size = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      plot.title = ggplot2::element_text(face = "plain", size = base_size + 4),
      plot.subtitle = ggplot2::element_text(face = "plain", size = base_size + 2),
      axis.title = ggplot2::element_text(face = "plain"),
      axis.text = ggplot2::element_text(size = base_size),
      strip.text = ggplot2::element_text(face = "plain", size = base_size + 1),
      strip.background = ggplot2::element_blank(),
      strip.background.y = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = "black", fill = "white", linewidth = 0.4)
    )
}

wrap_label_before_paren <- function(text) {
  if (is.null(text)) return(text)
  vapply(
    text,
    function(x) {
      if (is.na(x)) return(NA_character_)
      if (grepl("\\(", x)) sub(" \\(", "\n(", x) else x
    },
    character(1),
    USE.NAMES = FALSE
  )
}

map_model_label_values <- function(values) {
  # Optional normalization step used to keep facet labels consistent
  vapply(
    as.character(values),
    function(lbl) {
      if (grepl("Basic", lbl, ignore.case = TRUE)) return("basic")
      if (grepl("Health", lbl, ignore.case = TRUE)) return("health (phi = 0)")
      if (grepl("Linear", lbl, ignore.case = TRUE)) return("linear (phi = 1)")
      if (grepl("Power", lbl, ignore.case = TRUE)) return("power (alpha = 3)")
      if (grepl("Threshold", lbl, ignore.case = TRUE)) return("threshold (tau = 0.6*H0)")
      lbl
    },
    character(1)
  )
}

keep_left_bottom_axes <- function(p, preserve_bottom_axes = FALSE) {
  # For large facet grids: keep only one set of left and bottom axes
  g <- ggplot2::ggplotGrob(p)

  axis_l <- grep("axis-l", g$layout$name)
  axis_b <- grep("axis-b", g$layout$name)

  if (length(axis_l) > 0) {
    axis_left_df <- g$layout[axis_l, ]
    bottom_row <- max(axis_left_df$b)
    left_candidates <- axis_l[axis_left_df$b == bottom_row]
    keep_left <- left_candidates[which.min(axis_left_df$l[axis_left_df$b == bottom_row])]
    drop_left <- setdiff(axis_l, keep_left)
    if (length(drop_left) > 0) {
      g$grobs[drop_left] <- replicate(length(drop_left), grid::nullGrob(), simplify = FALSE)
    }
  }

  if (!preserve_bottom_axes && length(axis_b) > 0) {
    axis_bottom_df <- g$layout[axis_b, ]
    bottom_row <- max(axis_bottom_df$b)
    bottom_candidates <- axis_b[axis_bottom_df$b == bottom_row]
    keep_bottom <- bottom_candidates[which.min(axis_bottom_df$l[axis_bottom_df$b == bottom_row])]
    drop_bottom <- setdiff(axis_b, keep_bottom)
    if (length(drop_bottom) > 0) {
      g$grobs[drop_bottom] <- replicate(length(drop_bottom), grid::nullGrob(), simplify = FALSE)
    }
  }

  g
}

add_column_header_to_gtable <- function(g, header = "environment") {
  strip_rows <- grep("strip-t", g$layout$name)
  if (length(strip_rows) == 0) return(g)

  min_col <- min(g$layout$l[strip_rows])
  max_col <- max(g$layout$r[strip_rows])
  strip_top <- min(g$layout$t[strip_rows])

  g <- gtable_add_rows(g, heights = unit(1.8, "lines"), pos = strip_top - 1)
  g <- gtable_add_grob(
    g,
    textGrob(header, gp = gpar(fontface = "plain", fontsize = 18), x = 0.5, y = 0.5, just = "centre"),
    t = strip_top, l = min_col, r = max_col
  )

  g
}

add_row_header_to_gtable <- function(g, header = "model variant") {
  strip_cols <- grep("strip-l", g$layout$name)
  if (length(strip_cols) == 0) return(g)

  min_row <- min(g$layout$t[strip_cols])
  max_row <- max(g$layout$b[strip_cols])
  min_col <- min(g$layout$l[strip_cols])

  g <- gtable_add_cols(g, widths = unit(1.6, "lines"), pos = min_col - 1)
  g <- gtable_add_grob(
    g,
    textGrob(header, gp = gpar(fontface = "plain", fontsize = 16), rot = 90, just = "centre"),
    t = min_row, b = max_row, l = min_col, name = "row-header"
  )

  g
}

# -----------------------------------------------------------------------------
# 7) Overlays: threshold lines + SSP/predictability boundaries + region labels
# -----------------------------------------------------------------------------
add_threshold_layer <- function(p, segments) {
  if (is.null(segments)) return(p)
  p +
    geom_segment(
      data = segments,
      mapping = aes(x = x, y = y, xend = xend, yend = yend, color = color, linetype = linetype, size = size, alpha = alpha),
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_color_identity(guide = "none") +
    scale_linetype_identity(guide = "none") +
    scale_size_identity(guide = "none") +
    scale_alpha_identity(guide = "none")
}

build_standard_threshold_lines <- function(K_levels, D, x_range = c(0, 0.5), y_range = c(0, 0.5)) {
  # Draw reference lines at LA = K/D and LL = 1 - K/D when they fall in the plot range.
  if (length(K_levels) == 0) return(NULL)
  if (is.null(D) || !is.finite(D) || D == 0) stop("D must be a finite non-zero scalar")

  segments <- list()
  for (K in sort(unique(K_levels))) {
    lambda_a <- K / D
    if (lambda_a >= x_range[1] && lambda_a <= x_range[2]) {
      segments[[length(segments) + 1]] <- data.frame(
        K = K,
        x = lambda_a, y = y_range[1],
        xend = lambda_a, yend = y_range[2],
        color = "#777777", linetype = "dashed", size = 0.6, alpha = 0.45,
        stringsAsFactors = FALSE
      )
    }

    lambda_l <- 1 - (K / D)
    if (lambda_l >= y_range[1] && lambda_l <= y_range[2]) {
      segments[[length(segments) + 1]] <- data.frame(
        K = K,
        x = x_range[1], y = lambda_l,
        xend = x_range[2], yend = lambda_l,
        color = "#777777", linetype = "dashed", size = 0.6, alpha = 0.45,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(segments) == 0) return(NULL)
  seg_df <- do.call(rbind, segments)
  seg_df$K_fac <- factor(seg_df$K, levels = sort(unique(K_levels)))
  seg_df
}

build_ssp_predictability_boundaries <- function(x_range, y_range) {
  # SSP bands are lines in LA/LL space:
  # SSP = LA/(LA+LL) = const => LL = (1-SSP)/SSP * LA
  #
  # Here you draw two diagonal lines that approximate thresholds:
  # - low vs medium: SSP = 0.2 -> LL = 4*LA
  # - medium vs high: SSP = 0.8 -> LL = 0.25*LA
  #
  # Predictability boundary: total switching rate LA+LL relative to 0.5.
  # Your code uses: predictability = (LA+LL <= 0.5) => "P" else "U"
  # => boundary is LL = 0.5 - LA (within plot range)

  x_min <- x_range[1]; x_max <- x_range[2]
  y_min <- y_range[1]; y_max <- y_range[2]

  make_line <- function(max_la, slope) {
    if (max_la <= 0) return(data.frame(la = numeric(), ll = numeric()))
    la_vals <- seq(max(x_min, 0), max_la, length.out = 200)
    data.frame(la = la_vals, ll = slope * la_vals)
  }

  max_la_low_med <- min(x_max, y_max / 4)
  max_la_med_high <- min(x_max, y_max / 0.25)

  low_med_df <- make_line(max_la_low_med, 4)       # SSP ~ 0.2
  med_high_df <- make_line(max_la_med_high, 0.25)  # SSP ~ 0.8

  pred_la_start <- max(0, 0.5 - y_max)
  pred_la_end <- min(x_max, 0.5 - y_min)
  if (pred_la_end > pred_la_start) {
    pred_la_vals <- seq(pred_la_start, pred_la_end, length.out = 200)
    predictability_df <- data.frame(la = pred_la_vals, ll = 0.5 - pred_la_vals)
  } else {
    predictability_df <- data.frame(la = numeric(), ll = numeric())
  }

  list(low_med = low_med_df, med_high = med_high_df, predictability = predictability_df)
}

add_ssp_boundaries <- function(p, boundaries) {
  if (is.null(boundaries)) return(p)

  draw_line <- function(plot_obj, df) {
    if (is.null(df) || nrow(df) == 0) return(plot_obj)
    plot_obj +
      geom_line(data = df, aes(x = la, y = ll), inherit.aes = FALSE, colour = "white", linewidth = 1.2, alpha = 0.45) +
      geom_line(data = df, aes(x = la, y = ll), inherit.aes = FALSE, colour = "black", linewidth = 0.6, alpha = 0.35)
  }

  p <- draw_line(p, boundaries$low_med)
  p <- draw_line(p, boundaries$med_high)
  draw_line(p, boundaries$predictability)
}

build_env_region_labels <- function(df_plot) {
  # Labels each tile by region code based on:
  # - SSP bands: L (<=0.2), M (0.2-0.8), H (>=0.8)
  # - predictability: P if (LA+LL)<=0.5 else U
  if (nrow(df_plot) == 0) return(NULL)

  region_label_df <- df_plot %>%
    mutate(
      total = LA + LL,
      ssp = ifelse(total > 0, LA / total, NA_real_),
      ssp_band = case_when(
        is.na(ssp) ~ NA_character_,
        ssp <= 0.2 ~ "L",
        ssp >= 0.8 ~ "H",
        TRUE ~ "M"
      ),
      predictability = ifelse(total <= 0.5, "P", "U"),
      region_code = ifelse(is.na(ssp_band), NA_character_, paste0(ssp_band, "-", predictability))
    ) %>%
    filter(!is.na(region_code)) %>%
    group_by(region_code) %>%
    summarise(
      la_label = mean(LA, na.rm = TRUE),
      ll_label = mean(LL, na.rm = TRUE),
      hv_sample = hv_total[which.min((LA - mean(LA, na.rm = TRUE))^2 + (LL - mean(LL, na.rm = TRUE))^2)],
      .groups = "drop"
    ) %>%
    mutate(
      hjust = 0.5,
      vjust = 0.5,
      hv_sample = ifelse(is.na(hv_sample), 0, hv_sample),
      # If background is dark (high hv), use light text; else dark text
      lightness = pmin(pmax(hv_sample, 0), 1),
      label_colour = ifelse(lightness >= 0.5, "#f2f2f2", "#2b2b2b")
    ) %>%
    select(region_code, la_label, ll_label, hjust, vjust, label_colour)

  if (nrow(region_label_df) == 0) return(NULL)
  region_label_df
}

add_region_label_layer <- function(p, labels) {
  if (is.null(labels) || nrow(labels) == 0) return(p)
  p +
    geom_text(
      data = labels,
      mapping = aes(x = la_label, y = ll_label, label = region_code, hjust = hjust, vjust = vjust, colour = label_colour),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold",
      show.legend = FALSE,
      check_overlap = TRUE
    )
}

# -----------------------------------------------------------------------------
# 8) Heatmap plot builder (main faceted plot)
# -----------------------------------------------------------------------------
build_env_heatmap_base <- function(
  df,
  fill_column = "hv",
  fill_label = "hypervigilance",
  facet_rows = "model",
  facet_cols = "K",
  K_filter = NULL,
  title = "Proportion of hypervigilance across environments",
  subtitle = NULL,
  caption = NULL,
  env_scenarios = NULL,
  D = NULL,
  add_threshold_lines = FALSE,
  add_ssp_boundaries = TRUE,
  add_region_labels = TRUE,
  threshold_segments = NULL
) {
  required <- c("LA", "LL", facet_rows, facet_cols, fill_column)
  stopifnot(all(required %in% names(df)))

  if (!is.null(K_filter)) {
    df <- df %>% filter(.data[[facet_cols]] %in% K_filter)
  }

  # Harmonize facet labels (optional aesthetic step)
  row_raw <- df[[facet_rows]]
  col_raw <- df[[facet_cols]]

  row_decoded <- map_model_label_values(row_raw)
  row_values_raw <- unique(na.omit(row_decoded))
  if (length(row_values_raw) == 0) row_values_raw <- unique(as.character(row_raw))
  row_values <- wrap_label_before_paren(row_values_raw)

  col_values <- unique(na.omit(as.character(col_raw)))
  if (length(col_values) == 0) col_values <- unique(as.character(col_raw))

  df_plot <- df %>%
    mutate(
      fill_value = .data[[fill_column]],
      hv_total = .data[[fill_column]],
      facet_row_wrapped = wrap_label_before_paren(map_model_label_values(.data[[facet_rows]])),
      !!facet_rows := factor(facet_row_wrapped, levels = row_values),
      !!facet_cols := factor(.data[[facet_cols]],
                             levels = if (is.null(K_filter)) sort(unique(col_values)) else K_filter)
    )

  # Plot bounds / breaks: keep consistent across facets
  x_limits <- c(0, max(0.5, max(df_plot$LA[is.finite(df_plot$LA)], 0)))
  y_limits <- c(0, max(0.5, max(df_plot$LL[is.finite(df_plot$LL)], 0)))
  x_breaks <- seq(0, x_limits[2], by = 0.1)
  y_breaks <- seq(0, y_limits[2], by = 0.1)

  # Optional threshold lines if requested
  K_numeric <- suppressWarnings(as.numeric(col_values))
  K_numeric <- K_numeric[!is.na(K_numeric)]
  if (isTRUE(add_threshold_lines) && is.null(threshold_segments) && length(K_numeric) > 0 && !is.null(D)) {
    threshold_segments <- build_standard_threshold_lines(K_levels = K_numeric, D = D, x_range = x_limits, y_range = y_limits)
  }

  caption_final <- caption
  if (is.null(caption_final) && !is.null(env_scenarios)) {
    caption_final <- env_label_note(env_scenarios)
  }

  p <- ggplot(df_plot, aes(x = LA, y = LL, fill = fill_value)) +
    geom_tile(color = NA) +
    facet_grid(rows = vars(.data[[facet_rows]]), cols = vars(.data[[facet_cols]])) +
    scale_fill_gradient(
      limits = c(0, 1),
      low = "white",
      high = "black",
      name = fill_label,
      breaks = c(0, 0.02, 0.05, 0.1, 0.25, 0.5, 1),
      labels = scales::label_number(accuracy = 0.02)
    ) +
    guides(
      fill = guide_colourbar(
        title = fill_label,
        direction = "horizontal",
        title.position = "top",
        barwidth = unit(4, "in"),
        barheight = unit(0.35, "in"),
        frame.colour = "black",
        frame.linewidth = 0.6,
        ticks = TRUE,
        ticks.colour = "black",
        ticks.linewidth = 0.4,
        title.hjust = 0.5,
        label.position = "bottom"
      )
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "P(arrive)",
      y = "P(leave)",
      caption = caption_final
    ) +
    theme_supervisor_grid(base_size = 16) +
    theme(
      strip.text = element_text(size = 18, face = "plain"),
      strip.text.x = element_text(size = 18, face = "plain"),
      strip.text.y = element_text(size = 16, face = "plain"),
      panel.spacing = unit(0.45, "lines"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 8), size = 20),
      axis.title.y = element_text(margin = margin(r = 16), size = 20),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 4, r = 2, b = 8, l = 10, unit = "mm"),
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 16),
      plot.caption = element_text(hjust = 0, margin = margin(t = 30, unit = "pt"), size = 12)
    ) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = x_breaks, labels = sprintf("%.1f", x_breaks), limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(breaks = y_breaks, labels = sprintf("%.1f", y_breaks), limits = y_limits, expand = c(0, 0))

  if (isTRUE(add_threshold_lines) && !is.null(threshold_segments)) {
    p <- add_threshold_layer(p, threshold_segments)
  }
  if (isTRUE(add_ssp_boundaries)) {
    boundaries <- build_ssp_predictability_boundaries(x_limits, y_limits)
    p <- add_ssp_boundaries(p, boundaries)
  }
  if (isTRUE(add_region_labels)) {
    region_labels <- build_env_region_labels(df_plot)
    p <- add_region_label_layer(p, region_labels)
  }

  list(plot = p, df_plot = df_plot)
}

plot_env_heatmaps_matrix <- function(df, facet_rows = "model", facet_cols = "K") {
  stopifnot(all(c("LA", "LL", "hv", facet_rows, facet_cols) %in% names(df)))

  meta <- attr(df, "fixed_params")
  subtitle_final <- hv_subtitle_with_params(meta, NULL)
  D_value <- if ("D" %in% names(df)) unique(df$D)[1] else meta$D

  res <- build_env_heatmap_base(
    df = df,
    fill_column = "hv",
    fill_label = "hypervigilance (mean HV | stressor = 0)",
    facet_rows = facet_rows,
    facet_cols = facet_cols,
    title = "Proportion of hypervigilance across environments",
    subtitle = subtitle_final,
    env_scenarios = default_health_env_scenarios(),
    D = D_value,
    add_threshold_lines = FALSE,
    add_ssp_boundaries = TRUE,
    add_region_labels = TRUE
  )

  g <- keep_left_bottom_axes(res$plot, preserve_bottom_axes = FALSE)
  g <- add_column_header_to_gtable(g, header = "vigilance cost (K)")
  g <- add_row_header_to_gtable(g, header = "model variant")
  patchwork::wrap_elements(full = g)
}

plot_env_heatmaps <- function(real_data) {
  plot_env_heatmaps_matrix(real_data)
}

plot_env_heatmaps_with_region_key <- function(real_data, env_scenarios = default_health_env_scenarios()) {
  heatmap <- plot_env_heatmaps(real_data = real_data)
  key_caption <- env_label_note(env_scenarios)
  heatmap + patchwork::plot_annotation(caption = key_caption)
}

# -----------------------------------------------------------------------------
# 9) Side key panel: shows region boundaries and region labels in LA/LL space
# -----------------------------------------------------------------------------
build_env_key_panel <- function(df, k_value, D_fallback = NULL) {
  df_key <- dplyr::filter(df, K == k_value)

  # This is a "blank" background: we only want overlays + labels
  df_key$key_fill <- NA_real_
  df_key$hv_total <- 0

  x_limits <- c(0, 0.5)
  y_limits <- c(0, 0.5)

  d_value <- if ("D" %in% names(df_key)) unique(df_key$D)[1] else D_fallback

  boundaries <- build_ssp_predictability_boundaries(x_limits, y_limits)
  region_labels <- build_env_region_labels(df_key)
  if (!is.null(region_labels) && nrow(region_labels) > 0) region_labels$label_colour <- "black"

  base_plot <- ggplot(df_key, aes(x = LA, y = LL, fill = key_fill)) +
    geom_tile() +
    scale_fill_gradient(limits = c(0, 1), low = "white", high = "white", na.value = "white", guide = "none") +
    labs(title = NULL, subtitle = NULL, x = "P(arrive)", y = "P(leave)") +
    coord_equal(expand = FALSE) +
    scale_x_continuous(limits = x_limits, breaks = seq(0, 0.5, 0.1), expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, breaks = seq(0, 0.5, 0.1), expand = c(0, 0)) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18, face = "plain"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
    )

  base_plot <- add_ssp_boundaries(base_plot, boundaries)

  if (!is.null(region_labels) && nrow(region_labels) > 0) {
    base_plot <- base_plot +
      geom_text(
        data = region_labels,
        aes(x = la_label, y = ll_label, label = region_code, hjust = hjust, vjust = vjust),
        inherit.aes = FALSE,
        size = 4,
        colour = "black",
        fontface = "plain",
        lineheight = 0.9,
        show.legend = FALSE,
        check_overlap = TRUE
      )
  }

  base_plot
}

# -----------------------------------------------------------------------------
# 10) Composition: heatmap + side key + shared legend below the heatmaps only
# -----------------------------------------------------------------------------
.env_axis_breaks <- function(max_val = 0.5, by = 0.1) seq(0, max_val, by = by)

ENV_HEATMAP_STYLE <- list(
  axis_text_size = 30,
  axis_title_size = 30,
  strip_text_size = 26,
  key_axis_text = 28,
  key_axis_title = 30,
  legend_title_size = 24,
  legend_text_size = 22
)

make_env_key_header <- function(title = "Vigilance cost (K)", size = 18) {
  cowplot::ggdraw() +
    cowplot::draw_label(title, x = 0.5, y = 0.5, size = size) +
    theme(plot.margin = margin(0, 0, 0, 0, unit = "pt"))
}

make_env_heatmap_panel <- function(
  p_faceted,
  axis_max = 0.5,
  axis_by = 0.1,
  axis_text_size = 22,
  axis_title_size = 26,
  strip_text_size = 22,
  legend_title_size = 22,
  legend_text_size = 20,
  panel_spacing_x_lines = 0.20,
  right_margin_mm = 0.5
) {
  p_faceted +
    labs(title = NULL, subtitle = NULL) +
    scale_x_continuous(limits = c(0, axis_max), breaks = .env_axis_breaks(axis_max, axis_by), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, axis_max), breaks = .env_axis_breaks(axis_max, axis_by), expand = c(0, 0)) +
    coord_fixed(expand = FALSE) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        barwidth = unit(14, "cm"),
        barheight = unit(0.9, "cm"),
        frame.colour = "black",
        frame.linewidth = 0.9,
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    ) +
    theme(
      plot.margin = margin(t = 0, r = right_margin_mm, b = 0, l = 0, unit = "mm"),
      strip.text = element_text(size = strip_text_size, face = "plain", margin = margin(b = 1)),
      strip.background = element_blank(),
      panel.spacing.x = unit(panel_spacing_x_lines, "lines"),
      text = element_text(size = axis_text_size),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size, face = "plain"),
      legend.title = element_text(size = legend_title_size, face = "plain"),
      legend.text = element_text(size = legend_text_size),
      legend.justification = "center",
      legend.box.just = "center",
      legend.box.margin = margin(t = 2, r = 0, b = 0, l = 0, unit = "pt"),
      legend.margin = margin(0, 0, 0, 0, unit = "pt")
    )
}

make_env_key_panel <- function(
  df_env,
  k_value,
  D_fallback,
  axis_max = 0.5,
  axis_by = 0.1,
  axis_text_size = 20,
  axis_title_size = 26,
  left_margin_mm = 0.5
) {
  build_env_key_panel(df_env, k_value = k_value, D_fallback = D_fallback) +
    scale_x_continuous(limits = c(0, axis_max), breaks = .env_axis_breaks(axis_max, axis_by), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, axis_max), breaks = .env_axis_breaks(axis_max, axis_by), expand = c(0, 0)) +
    coord_fixed(expand = FALSE) +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 0, l = left_margin_mm, unit = "mm"),
      text = element_text(size = axis_text_size),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size, face = "plain")
    )
}

compose_env_heatmap_with_key <- function(
  heatmap_panel,
  key_panel,
  nK,
  title = "Vigilance cost (K)",
  title_size = 18,
  rel_heights = c(0.10, 1, 0.18)
) {
  # Extract the legend from the heatmap panel, then place it below ONLY the heatmaps.
  p_with_legend <- heatmap_panel + theme(legend.position = "bottom")
  legend_grob <- cowplot::get_legend(p_with_legend)
  p_heat_no_legend <- heatmap_panel + theme(legend.position = "none")

  row_main <- cowplot::plot_grid(
    p_heat_no_legend,
    key_panel,
    nrow = 1,
    rel_widths = c(nK, 1.11),
    align = "hv",
    axis = "tb"
  )

  row_legend <- cowplot::plot_grid(
    legend_grob,
    NULL,
    nrow = 1,
    rel_widths = c(nK, 1.11)
  )

  title_row <- make_env_key_header(title = title, size = title_size)

  cowplot::plot_grid(
    title_row,
    row_main,
    row_legend,
    ncol = 1,
    rel_heights = rel_heights
  )
}

# -----------------------------------------------------------------------------
# 11) Save helpers
# -----------------------------------------------------------------------------
save_plot_files <- function(plot_obj, stem, formats = c("png", "pdf"),
                            width = 18, height = 4.8, dpi = 450, bg = "white", overwrite = TRUE) {
  if (!dir.exists(dirname(stem))) dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)

  for (fmt in tolower(formats)) {
    out_path <- paste0(stem, ".", fmt)
    if (!overwrite && file.exists(out_path)) next

    ggplot2::ggsave(
      filename = out_path,
      plot = plot_obj,
      width = width,
      height = height,
      units = "in",
      dpi = dpi,
      bg = bg,
      device = fmt
    )
  }
}

# -----------------------------------------------------------------------------
# 12) Entry point
# -----------------------------------------------------------------------------
build_and_save_env_heatmaps_all_models <- function(
  models = NULL,
  K_values = 1:9,
  env_grid = NULL,
  params = list(
    C = 0, D = 10, d = 0,
    T = 10, h0 = 35,
    N = 1000L,
    states = c("K", "Kd", "C", "CD"),
    grid_step = 0.05
  ),
  out_dir = NULL,
  format = c("png", "pdf"),
  overwrite = TRUE,
  seed = 1
) {
  assert_dp_sim_functions()

  # Choose output root
  if (is.null(out_dir)) {
    if (exists("DIR_FIGURES", inherits = TRUE)) {
      out_dir <- get("DIR_FIGURES", inherits = TRUE)
    } else {
      out_dir <- file.path("outputs", "figures")
    }
  }

  # Validate key parameters
  if (length(K_values) == 0) stop("K_values must contain at least one value.")
  if (is.null(params$C) || is.null(params$D) || is.null(params$d) ||
      is.null(params$T) || is.null(params$N) || is.null(params$states)) {
    stop("params must include C, D, d, T, N, and states.")
  }

  # Build model specification table
  model_specs <- default_model_specs(h0 = params$h0)
  if (!is.null(models)) {
    keep <- model_specs$model_id %in% models | model_specs$model_label %in% models
    model_specs <- model_specs[keep, , drop = FALSE]
    if (nrow(model_specs) == 0) stop("No model_specs matched requested models.")
  }

  # Build environment grid (LA x LL)
  env_grid_df <- build_env_grid(env_grid = env_grid, grid_step = params$grid_step)

  # Evaluate HV on the full grid for each model x K
  env_df <- build_env_heatmaps_for_models(
    model_specs = model_specs,
    env_grid = env_grid_df,
    K_values = K_values,
    C = params$C,
    D = params$D,
    d = params$d,
    T_steps = params$T,
    states = params$states,
    N_agents = params$N,
    seed = seed
  )

  # Build plots
  env_heatmap_plot <- plot_env_heatmaps(real_data = env_df)
  env_heatmap_key_plot <- plot_env_heatmaps_with_region_key(real_data = env_df)

  # Apply “publication” sizing to the heatmap panel
  env_heatmap_panel <- make_env_heatmap_panel(
    p_faceted = env_heatmap_plot,
    axis_text_size = ENV_HEATMAP_STYLE$axis_text_size,
    axis_title_size = ENV_HEATMAP_STYLE$axis_title_size,
    strip_text_size = ENV_HEATMAP_STYLE$strip_text_size,
    legend_title_size = ENV_HEATMAP_STYLE$legend_title_size,
    legend_text_size = ENV_HEATMAP_STYLE$legend_text_size,
    panel_spacing_x_lines = 0.20,
    right_margin_mm = 2
  )

  # Side key panel uses the minimum K (just a visual map of regions)
  env_key_panel <- make_env_key_panel(
    df_env = env_df,
    k_value = min(env_df$K),
    D_fallback = params$D,
    axis_text_size = ENV_HEATMAP_STYLE$key_axis_text,
    axis_title_size = ENV_HEATMAP_STYLE$key_axis_title,
    left_margin_mm = 3
  )

  # Compose heatmap + key + legend
  nK_env <- length(sort(unique(env_df$K)))
  env_heatmap_with_key_side <- compose_env_heatmap_with_key(
    heatmap_panel = env_heatmap_panel,
    key_panel = env_key_panel,
    nK = nK_env,
    title = "Vigilance cost (K)",
    title_size = 18,
    rel_heights = c(0.10, 1, 0.18)
  )

  # Save
  figure_dir <- file.path(out_dir, "env")
  save_plot_files(env_heatmap_plot, file.path(figure_dir, "env_heatmap_by_model"),
                  formats = format, width = 18, height = 4.8, dpi = 450, overwrite = overwrite)

  save_plot_files(env_heatmap_key_plot, file.path(figure_dir, "env_heatmap_with_key"),
                  formats = format, width = 18, height = 4.8, dpi = 450, overwrite = overwrite)

  save_plot_files(env_heatmap_with_key_side, file.path(figure_dir, "env_heatmap_by_model_with_key_side"),
                  formats = format, width = 18, height = 4.8, dpi = 450, overwrite = overwrite)

  invisible(list(
    env_df = env_df,
    env_heatmap_plot = env_heatmap_plot,
    env_heatmap_key_plot = env_heatmap_key_plot,
    env_heatmap_with_key_side = env_heatmap_with_key_side
  ))
}
