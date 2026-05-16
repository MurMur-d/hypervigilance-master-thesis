# ===================================================================================================
# File: prep_hypervigilance_data.R
# Purpose:
#   Normalize raw agent-level hypervigilance logging so that *all downstream*
#   prep/plotting code can rely on a consistent schema:
#       hv      : numeric/logical indicator of hypervigilance (1/TRUE = vigilant)
#       str     : integer indicator of stressor presence (1) vs absence (0)
#       time    : time index (integer-ish)
#       agent   : agent identifier (integer-ish)
#
#   In addition, this module computes a small set of *mechanism counters* used in
#   reporting tables and captions:
#       safe_steps : number of rows where stressor==0
#       safe_hv    : number of rows where stressor==0 AND hv==TRUE
#       prevent    : safe_hv rows that occur immediately after a no-stressor step
#       spill      : safe_hv rows that occur immediately after a stressor step
#
# Inputs:
#   data frames exported from `mem_simulate_agents()` (agent_data) or other logs
#   that may use different naming conventions:
#       snake_case  (hv, stressor, time, agent)
#       camelCase   (Hypervigilance, Stressor, Time, Agent)
#
# Outputs:
#   - extract_hypervigilance_vectors(): list(hv, str, time, agent)
#   - extract_hypervigilance_metadata(): list(hv, str, time, agent, safe_steps, safe_hv, prevent, spill)
#
# Notes:
#   - Pure transformation helpers only: no file I/O, no plotting, no dependencies
#     beyond base R.
#   - This file complements hv_rate_helpers.R:
#       * this file standardizes columns + computes prevent/spill counts
#       * hv_rate_helpers.R computes canonical HV rate summaries from those vectors
# ===================================================================================================


# ---------------------------------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------------------------------
# None beyond base R. We intentionally avoid dplyr/tidyr here:
# - This file is called in tight loops (grid simulations), so base operations reduce overhead
# - Minimal dependencies make it easy to source in many contexts


# ===================================================================================================
# 1) pick_first_column()
# ===================================================================================================

#' Pick the first matching column name from the candidates.
#'
#' Why this exists:
#'   Different parts of the project (or older cached outputs) may store the same concept
#'   under different column names (e.g., "hv" vs "Hypervigilance").
#'   Downstream code should not have to “guess” names repeatedly.
#'
#' @param data data.frame to search (must have names(data))
#' @param candidates character vector of preferred column names, in priority order
#' @return string: first candidate found in names(data); or NULL if none found
#'
#' @examples
#'   hv_col <- pick_first_column(agent_df, c("hv", "Hypervigilance"))
pick_first_column <- function(data, candidates) {
  # Guard: if caller provides NULL/empty candidates, we cannot match anything
  if (is.null(candidates) || length(candidates) == 0) {
    return(NULL)
  }

  # Scan candidates in order and return the first one that exists in the data frame
  for (candidate in candidates) {
    if (candidate %in% names(data)) {
      return(candidate)
    }
  }

  # If nothing matched, return NULL to signal “missing column”
  NULL
}


# ===================================================================================================
# 2) compute_safe_hypervigilance_counts()
# ===================================================================================================

#' Count safe hypervigilance transitions for each agent timeline.
#'
#' Core idea:
#'   We want to classify hypervigilance that occurs when no stressor is present (“safe HV”)
#'   into two mechanistic categories based on the immediately FOLLOWING time step *for the
#'   same agent*:
#'
#'     - prevent (preventative/anticipatory vigilance):
#'         hv==TRUE with stressor==0 AND the next step also has stressor==0
#'         → interpreted as proactive vigilance that correctly predicts safety
#'
#'     - spill (spillover/reactive vigilance):
#'         hv==TRUE with stressor==0 AND the next step has stressor==1
#'         → interpreted as vigilance chosen in safe times but followed by danger
#'
#'   This is intentionally a simple "one-step lookahead" mechanism definition,
#'   measuring whether vigilance at time t matches the safety of state t+1.
#'
#' @param hv    logical/numeric vector (TRUE/1 = hypervigilant) for rows ordered by time within agent
#' @param str   integer vector marking stressor presence (1) or absence (0)
#' @param agent vector identifying agent membership for each row
#' @param time  vector with the time stamp of each row (used to sort within agent)
#'
#' @return list(safe_steps, safe_hv, prevent, spill)
#'
#' @details
#'   - safe_steps counts *rows* with stressor==0, across all agents and times.
#'   - safe_hv counts *rows* with (stressor==0 AND hv==TRUE).
#'   - prevent/spill split safe_hv depending on the next row's stressor (within agent).
#'   - Sorting by (agent, time) ensures correct next-step context even if the input
#'     arrives shuffled.
#'   - Missing values are treated conservatively:
#'       hv NA   → FALSE (not vigilant)
#'       str NA  → 0     (assume no stressor)
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
 
  safe_steps <- sum(str_int == 0L, na.rm = TRUE)
 
  # --- Sort by (agent, time) ------------------------------------------------
  agent_chr <- as.character(agent)
  time_vals <- time
  if (length(time_vals) != n) time_vals <- seq_len(n)
 
  order_idx <- order(agent_chr, time_vals, na.last = TRUE)
 
  hv_ord    <- hv_flag[order_idx]
  str_ord   <- str_int[order_idx]
  agent_ord <- agent_chr[order_idx]
 
  # --- Build previous-step stressor context ---------------------------------
  # Legacy Figure 4 split: safe HV rows are classified by whether the same
  # agent's previous row was safe (anticipatory) or stressed (reactive).
  prev_str <- c(NA_integer_, str_ord[-n])
  prev_same <- c(FALSE, agent_ord[-1L] == agent_ord[-n])
  prev_str[!prev_same] <- NA_integer_

  safe_hv_idx <- hv_ord & (str_ord == 0L)
  safe_hv_idx[is.na(safe_hv_idx)] <- FALSE

  prevent <- sum(safe_hv_idx & prev_str == 0L, na.rm = TRUE)
  spill   <- sum(safe_hv_idx & prev_str == 1L, na.rm = TRUE)
  safe_hv <- sum(safe_hv_idx, na.rm = TRUE)
 
  list(
    safe_steps = safe_steps,
    safe_hv    = safe_hv,
    prevent    = prevent,
    spill      = spill
  )
}
 
# ===================================================================================================
# 3) extract_hypervigilance_vectors()
# ===================================================================================================

#' Extract canonical hv/stressor/time/agent vectors from legacy-friendly columns.
#'
#' Why this exists:
#'   Many downstream functions want numeric vectors and do not want to care about
#'   whether the simulation output used hv vs Hypervigilance, etc.
#'
#' What it does:
#'   - Detects “best” column names for hv/stressor/time/agent
#'   - Returns vectors in a canonical list:
#'       hv, str, time, agent
#'   - If a column is missing, returns a vector of NAs (or a fallback sequence)
#'
#' @param agent_data tibble/data.frame exported from DP/simulation scripts
#' @return list(hv, str, time, agent)
#'
#' @description
#'   Downstream callers can now assume these keys exist and are aligned in length.
extract_hypervigilance_vectors <- function(agent_data) {
  # --- Step 1: Find the “best” column name for each concept ------------------
  hv_col    <- pick_first_column(agent_data, c("hv", "Hypervigilance"))
  str_col   <- pick_first_column(agent_data, c("str", "stressor", "Str", "Stressor", "stressor_present"))
  time_col  <- pick_first_column(agent_data, c("time", "Time"))
  agent_col <- pick_first_column(agent_data, c("agent", "Agent"))

  # --- Step 2: Determine row count so we can build safe fallbacks ------------
  n_rows <- nrow(agent_data)
  if (is.null(n_rows)) n_rows <- 0L

  # --- Step 3: Extract vectors with safe defaults ----------------------------
  # hv: numeric (often 0/1); if missing, fill with NA
  hv_vec <- if (!is.null(hv_col)) {
    as.numeric(agent_data[[hv_col]])
  } else {
    rep(NA_real_, n_rows)
  }

  # str: integer (0/1); if missing, fill with NA
  str_vec <- if (!is.null(str_col)) {
    as.integer(agent_data[[str_col]])
  } else {
    rep(NA_integer_, n_rows)
  }

  # time: if missing, fall back to 1..n_rows so downstream ordering still works
  time_vec <- if (!is.null(time_col)) {
    agent_data[[time_col]]
  } else {
    seq_len(n_rows)
  }

  # agent: if missing, treat each row as its own “agent” so code doesn’t crash.
  # (This is conservative: it prevents cross-row transition logic from linking rows.)
  agent_vec <- if (!is.null(agent_col)) {
    agent_data[[agent_col]]
  } else {
    seq_len(n_rows)
  }

  # Return canonical vectors
  list(
    hv    = hv_vec,
    str   = str_vec,
    time  = time_vec,
    agent = agent_vec
  )
}


# ===================================================================================================
# 4) extract_hypervigilance_metadata()
# ===================================================================================================

#' Build the full hv metadata package for a single simulation run.
#'
#' This is the “main” convenience wrapper for downstream modules that need both:
#'   - the canonical vectors (hv/str/time/agent)
#'   - the mechanism counters (safe_steps/safe_hv/prevent/spill)
#'
#' @param agent_data data frame with hv/stressor/time/agent columns (legacy names accepted)
#' @return list(hv, str, time, agent, safe_steps, safe_hv, prevent, spill)
#'
#' @details
#'   The rest of the pipeline treats this output as a standard “hv meta object”.
#'   For example, hv_rate_helpers.R uses this to compute:
#'     - pooled HV rates
#'     - safe HV counts
#'     - prevention/spillover decomposition
extract_hypervigilance_metadata <- function(agent_data) {
  hv_data <- extract_hypervigilance_vectors(agent_data)
 
  safe_counts <- compute_safe_hypervigilance_counts(
    hv_data$hv, hv_data$str, hv_data$agent, hv_data$time
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
