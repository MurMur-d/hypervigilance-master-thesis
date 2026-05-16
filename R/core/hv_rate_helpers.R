# ------------------------------------------------------------------------------
# FILE: hv_rate_helpers.R
# ROLE:
#   Provide a single, canonical implementation of hypervigilance metrics computed
#   from raw simulation output (i.e., from mem_simulate_agents()).
#
# Why this file exists:
#   Many pipelines/figures need a “hypervigilance rate”, but it’s easy for slight
#   differences to creep in:
#     - different column names across models ("hv" vs "HV", "stressor" vs "str", ...)
#     - different filtering conventions (include stressor timesteps or not?)
#     - different handling of missing values / partial simulations
#
# The goal is: every script calls these helpers and gets *the same* definitions.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Depend on the vetted normalization helpers
# ------------------------------------------------------------------------------
# prep_hypervigilance_data.R is assumed to define functions like:
#   - extract_hypervigilance_vectors()
#   - extract_hypervigilance_metadata()
#
# These functions are responsible for “column normalization”:
# they take agent_data with potentially model-specific column names and return
# standardized fields such as:
#   hv:   hypervigilance indicator/score per row
#   str:  stressor indicator (0/1) per row
#   time: timestep column
#   agent: agent id column
#
# By sourcing this file, hv_rate_helpers.R stays focused on *metrics*, not on
# schema wrangling.
source("R/core/hypervigilance/00_data_prep_hypervigilance_data.R")  # reuse vetted normalisation helpers


# ------------------------------------------------------------------------------
# extract_hv_and_stressor()
# ------------------------------------------------------------------------------
# Lightweight extractor that:
#   - validates agent_data input
#   - calls extract_hypervigilance_vectors() to standardize columns
#   - returns a simple list of numeric/integer vectors for downstream computation
#
# This helper is useful when downstream code wants the vectors directly and does
# not need the full metadata object.
extract_hv_and_stressor <- function(agent_data) {

  # Defensive programming:
  # If caller provides NULL or a non-data.frame, return empty vectors instead of erroring.
  # This makes pipeline code more robust (it can still summarise and proceed).
  if (is.null(agent_data) || !is.data.frame(agent_data)) {
    return(list(
      hv_num = numeric(0),         # numeric hypervigilance vector
      stressor_int = integer(0),   # integer stressor indicator vector
      time_col = numeric(0)        # numeric timesteps (empty)
    ))
  }

  # Standardize column names and extract vectors.
  # extract_hypervigilance_vectors() should return a list-like object with fields:
  #   hv, str, time, agent (names inferred from the rest of this file).
  hv_data <- extract_hypervigilance_vectors(agent_data)

  # Explicit coercions:
  # - hv_num: numeric (ensures mean/sd behave consistently even if stored as logical)
  # - stressor_int: integer (ensures == 0L comparisons are stable)
  list(
    hv_num = as.numeric(hv_data$hv),
    stressor_int = as.integer(hv_data$str),
    time_col = hv_data$time,
    agent_col = hv_data$agent
  )
}


# ------------------------------------------------------------------------------
# compute_hv_rates_from_agent_data()
# ------------------------------------------------------------------------------
# This is the core canonical metric function.
#
# Inputs:
#   agent_data: the simulation output data frame (often sim$agent_data)
#   T_steps:    optional horizon length, used to compute coverage diagnostics
#   N_agents:   optional number of agents, used to compute coverage diagnostics
#
# Outputs:
#   A list containing:
#     - HypervigilanceRate_all: mean(hv) over *all* rows (including stressor rows)
#     - HypervigilanceRate_filtered: mean(hv) restricted to no-stressor rows (str==0)
#     - Spread_no_stressor: sd(hv) over no-stressor rows
#     - coverage diagnostics: Prop_timesteps_included, Prop_agent_timesteps
#     - counts and debug payload: n_no_stressor_rows, hv_meta, hv_filtered_values
#
# Important definition choice:
#   HypervigilanceRate_filtered is the “canonical” rate used across the project:
#     it pools *all rows* where stressor is absent (str==0), across agents and time.
#
# Interpretation:
#   This captures “preventative vigilance” / baseline vigilance in safe periods,
#   rather than reactive vigilance during stressor presence.
compute_hv_rates_from_agent_data <- function(agent_data, T_steps = NULL, N_agents = NULL) {

  # Extract standardized columns (hv, str, time, agent, etc.)
  # hv_meta is assumed to be a list-like object with at least:
  #   hv_meta$hv   : numeric/logical hv indicator per row
  #   hv_meta$str  : stressor indicator per row (0/1) or NA
  #   hv_meta$time : timestep per row (optional but used for coverage)
  hv_meta <- extract_hypervigilance_metadata(agent_data)

  # Pull out the raw vectors for readability.
  hv_num <- as.numeric(hv_meta$hv)
  stressor_int <- as.integer(hv_meta$str)

  # Define the “no stressor” mask:
  #   - require stressor value present (not NA)
  #   - and stressor == 0
  mask_no_stress <- !is.na(stressor_int) & stressor_int == 0L

  # Filter hypervigilance values to:
  #   - no-stressor rows
  #   - non-missing hv values
  hv_filtered_vals <- hv_num[mask_no_stress & !is.na(hv_num)]

  # --- Metric 1: hypervigilance over all rows ---------------------------------
  # mean(..., na.rm=TRUE) returns NaN if the vector is empty/all-NA.
  # dplyr::coalesce(x,0) replaces NA with 0, but not NaN.
  # Here, in practice, mean(empty, na.rm=TRUE) is NaN; depending on dplyr version
  # coalesce may not replace NaN. If you ever see NaN leaks, swap to:
  #   ifelse(is.finite(x), x, 0)
  #
  # Assumption in this project:
  #   if there is no data (or all missing), treat hypervigilance as 0.
  m0 <- function(x) {
    m <- mean(x, na.rm = TRUE)
    ifelse(is.finite(m), m, 0)
  }

  s0 <- function(x) {
    s <- stats::sd(x, na.rm = TRUE)
    ifelse(is.finite(s), s, 0)
  }

  hyper_all <- m0(hv_num)

  # --- Metric 2 (canonical): hypervigilance on no-stressor rows ----------------
  hyper_filtered <- m0(hv_filtered_vals)

  # --- Metric 3: spread (variability) on no-stressor rows ----------------------
  # This is useful for identifying heterogeneity in preventative vigilance:
  # - if everyone behaves the same, sd ≈ 0
  # - if some are vigilant and others relaxed, sd increases
  spread_no_stressor <- s0(hv_filtered_vals)

  # --- Coverage diagnostic 1: fraction of unique timesteps represented --------
  # Some pipelines may merge/aggregate simulation data or drop rows.
  # This diagnostic estimates whether we have a full time range.
  #
  # This uses hv_meta$time rather than agent_data$time directly because
  # extract_hypervigilance_metadata() should normalize time column naming.
  prop_timesteps_included <- NA_real_
  if (!is.null(T_steps) && is.numeric(T_steps) && T_steps > 0 && length(hv_meta$time) > 0) {
    unique_times <- unique(hv_meta$time[!is.na(hv_meta$time)])
    if (length(unique_times) > 0) {
      # cap at 1 in case time indexing is odd (e.g., repeated/extra timesteps)
      prop_timesteps_included <- min(1, length(unique_times) / T_steps)
    } else {
      prop_timesteps_included <- 0
    }
  }

  # --- Coverage diagnostic 2: fraction of agent×timestep rows present ----------
  # If agent_data is supposed to contain one row per agent per timestep,
  # then the maximum possible rows = N_agents * T_steps.
  #
  # This diagnostic flags partial simulations or filtered datasets.
  prop_agent_timesteps <- NA_real_
  if (!is.null(N_agents) && is.numeric(N_agents) && N_agents > 0 &&
      !is.null(T_steps) && is.numeric(T_steps) && T_steps > 0) {
    prop_agent_timesteps <- min(1, nrow(agent_data) / (N_agents * T_steps))
  }

  # Return a rich list so callers can:
  #   - use just HypervigilanceRate_filtered for plotting
  #   - also inspect coverage/spread when debugging pipeline differences
  list(
    HypervigilanceRate_all = hyper_all,
    HypervigilanceRate_filtered = hyper_filtered,
    Spread_no_stressor = spread_no_stressor,
    Prop_timesteps_included = prop_timesteps_included,
    Prop_agent_timesteps = prop_agent_timesteps,

    # Useful debugging / reporting fields:
    n_no_stressor_rows = sum(mask_no_stress, na.rm = TRUE),
    hv_meta = hv_meta,
    hv_filtered_values = hv_filtered_vals
  )
}


# ------------------------------------------------------------------------------
# check_hv_rate_consistency()
# ------------------------------------------------------------------------------
# Purpose:
#   Guardrail function to catch silent definition drift.
#
# Typical use case:
#   - You previously computed a “legacy” hv_rate in older scripts (maybe with a
#     slightly different filter/aggregation).
#   - You now want to ensure that the canonical implementation produces the same
#     result (within tolerance), or else warn that definitions differ.
#
# Inputs:
#   agent_data: raw simulation output
#   legacy_rate: previously computed hypervigilance rate to compare against
#   tolerance: numeric tolerance passed to all.equal()
#
# Output:
#   - invisibly returns the canonical HypervigilanceRate_filtered
#   - warns if canonical != legacy
check_hv_rate_consistency <- function(agent_data, legacy_rate = NULL, tolerance = 1e-6) {

  # If no legacy value supplied or it’s not finite, do nothing.
  # This makes it safe to call unconditionally.
  if (is.null(legacy_rate) || !is.finite(legacy_rate)) return(invisible(NULL))

  # Compute canonical metrics
  hv_rates <- compute_hv_rates_from_agent_data(agent_data)

  # Compare canonical filtered rate against legacy rate.
  # all.equal() returns TRUE or a descriptive message string.
  if (!isTRUE(all.equal(hv_rates$HypervigilanceRate_filtered, legacy_rate, tolerance = tolerance))) {
    warning(
      "Canonical HypervigilanceRate_filtered (pooled over no-stressor rows) differs from legacy hv rate"
    )
  }

  # Return the canonical rate invisibly so callers can capture it if needed.
  invisible(hv_rates$HypervigilanceRate_filtered)
}
