# ------------------------------------------------------------------------------
# FILE: 00_plot_utils.R
# ROLE:
#   This is the *plotting backbone* for the project.
#
#   It serves three main roles:
#   (1) Plot styling: shared ggplot themes, palettes, and scales so figures look
#       consistent across scripts and pipelines.
#   (2) Model dispatch: a uniform interface so downstream code can call the same
#       function names regardless of whether we are using the basic or health model.
#   (3) Memoisation / caching anchors: persistent caches + file-backed caches for
#       expensive policy computation and forward simulations.
#
#   In practice, most plotting scripts source this file at the top and then assume:
#     - theme_vigilance(), scale_fill_grey01(), palette_actions exist
#     - get_model() exists (and returns compute_optimal_policy + simulate_agents_forward)
#     - mem_compute_policy() / mem_simulate_agents() exist for caching
#     - save_graphs() exists for consistent exports
# ------------------------------------------------------------------------------


# === STEP 0: Load the shared defaults/helpers =================================
# shared_config.R is expected to define:
#   - validate_default() (lookup of default parameter values)
#   - core default constants (e.g., default h0)
#   - possibly caches like persistent_cache / persistent_cache_health
#   - helpers like pick_first(), .use_cache(), etc.
#
# Sourcing it first ensures:
#   - defaults exist before we define functions that depend on them
#   - caching objects exist before wrappers refer to them
source("R/core/shared_config.R")


# === STEP 1: Load the packages used by plotting scripts (quietly) =============
# We suppress package startup messages so that when running pipelines the console
# isn’t flooded by library attachment notes.
#
# A key design choice here:
#   - even if some scripts do not use plotly/future.apply directly, we load them
#     centrally so all plotting scripts share a consistent environment.
suppressPackageStartupMessages({
  library(dplyr)        # used in most pipelines to wrangle plot-ready tables
  library(ggplot2)      # core plotting system
  library(plotly)       # OPTIONAL: convert ggplot to interactive (only some scripts)
  library(future.apply) # OPTIONAL: parallelized apply variants for grid sweeps
  library(tidyr)        # pivot_longer/complete/unnest etc. in some plot summaries
})


# === STEP 2: Model selector with a uniform interface ===========================
# PROBLEM THIS SOLVES:
#   The "basic" and "health" model implementations have different function names
#   and different argument signatures (health needs additional policy/sim args).
#
# GOAL:
#   Return a small list of functions with the SAME names regardless of model type:
#       m <- get_model("basic" or "health", ...)
#       pol <- m$compute_optimal_policy(K, C, D, d, LA, LL, T, states)
#       sim <- m$simulate_agents_forward(policy_df, LA, LL, T, N, K, C, D, d)
#
# This uniform interface keeps plotting/data-prep code clean and prevents lots of
# if/else logic scattered throughout the codebase.
get_model <- function(
    model = c("basic","health"),  # model family; enforced below via match.arg()
    policy_args = list(),         # extra args used ONLY by health-policy function
    sim_args    = list()          # extra args used ONLY by health-simulation function
) {
  # match.arg() standardizes input and throws a clear error if invalid
  model <- match.arg(model)

  if (model == "basic") {
    # --- STEP 2A: Basic model: simple passthrough -----------------------------
    # Basic model functions already have the desired signature, so we can
    # return them directly.
    #
    # Expected signature (documented informally):
    #   compute_optimal_policy(K,C,D,d,LA,LL,T,states)
    #   simulate_agents_forward(policy_df, LA,LL,T,N_agents,K,C,D,d)
    list(
      compute_optimal_policy  = compute_optimal_policy,
      simulate_agents_forward = simulate_agents_forward
    )

  } else {
    # --- STEP 2B: Health model: wrap to inject extra arguments ----------------
    # Health model functions typically have “*_health” names and use different
    # parameter naming (k_cost, la_prob, horizon, ...).
    #
    # To prevent every caller from remembering those names, we create wrappers:
    #   - cop(): "compute optimal policy" wrapper
    #   - saf(): "simulate agents forward" wrapper
    #
    # Each wrapper:
    #   - accepts the standard signature (same as basic)
    #   - translates/renames inputs into the health function’s expected names
    #   - splices in any additional user-supplied args via do.call()
    cop <- function(K, C, D, d, LA, LL, T, states) {
      do.call(
        compute_optimal_policy_health,
        c(
          # standard → health argument name mapping
          list(
            k_cost = K, c_cost = C, d_high = D, d_low = d,
            la_prob = LA, ll_prob = LL, horizon = T
          ),
          # user-provided extra health-only arguments
          policy_args
        )
      )
    }

    saf <- function(policy_df, LA, LL, T, N_agents, K, C, D, d) {
      do.call(
        simulate_agents_forward_health,
        c(
          # standard → health argument name mapping
          list(
            policy_df = policy_df,
            la_prob = LA, ll_prob = LL, horizon = T, n_agents = N_agents,
            k_cost = K, c_cost = C, d_high = D, d_low = d
          ),
          # user-provided extra health-only arguments
          sim_args
        )
      )
    }

    # Return a uniform interface with standard function names/signatures
    list(
      compute_optimal_policy  = cop,
      simulate_agents_forward = saf
    )
  }
}


# === STEP 2b: Subtitle helper for HEALTH terminal rewards =====================
# In the health model, “terminal rewards” can change the objective.
# For plotting, you often want the figure subtitle to include what terminal reward
# regime was used (none/linear/power/threshold + key parameters).
#
# health_reward_subtitle() returns a short human-facing suffix like:
#   " | no terminal reward"
#   " | with terminal reward | linear (*0.5)"
#   " | with terminal reward | power (^3)"
#   " | with terminal reward | threshold (τ=0.6·H0)"
#
# It pulls parameters from either policy_args or sim_args (some settings may live
# in either place depending on implementation).
health_reward_subtitle <- function(policy_args = list(), sim_args = list()) {

  # Normalize NULL → empty list so pick_first() calls are safe
  pa <- if (is.null(policy_args)) list() else policy_args
  sa <- if (is.null(sim_args)) list() else sim_args

  # Small formatting helper:
  # - convert to numeric if possible
  # - if not numeric / missing, return "?"
  # - otherwise format with a compact scientific/decimal style
  fmt_val <- function(x) {
    xv <- suppressWarnings(as.numeric(x))
    if (is.null(x) || !is.finite(xv)) return("?")
    formatC(xv, format = "g", digits = 4)
  }

  # pick_first() (expected from shared_config.R) picks the first non-null value
  # in the given sequence. This avoids having to remember where parameters live.
  w     <- pick_first(pa$terminal_reward_weight,   sa$terminal_reward_weight)
  mode  <- pick_first(pa$terminal_reward_mode,     sa$terminal_reward_mode)
  alpha <- pick_first(pa$terminal_power_alpha,     sa$terminal_power_alpha)
  tau   <- pick_first(pa$terminal_threshold_tau,   sa$terminal_threshold_tau)
  h0    <- pick_first(pa$h0, pa$H0, sa$h0, sa$H0)
  if (is.null(tau) && !is.null(h0)) {
    tau <- 0.6 * suppressWarnings(as.numeric(h0))
  }
  h0    <- pick_first(pa$h0, pa$H0, sa$h0, sa$H0)
  if (is.null(tau) && !is.null(h0)) {
    tau <- 0.6 * suppressWarnings(as.numeric(h0))
  }
  h0    <- pick_first(pa$h0, pa$H0, sa$h0, sa$H0)
  if (is.null(tau) && !is.null(h0)) {
    tau <- 0.6 * suppressWarnings(as.numeric(h0))
  }
  h0    <- pick_first(pa$h0, pa$H0, sa$h0, sa$H0)
  if (is.null(tau) && !is.null(h0)) {
    tau <- 0.6 * suppressWarnings(as.numeric(h0))
  }

  # If weight is missing, we can’t describe anything → no subtitle suffix
  if (is.null(w)) return("")

  # If weight is present but not parseable as numeric, also give nothing
  w_num <- suppressWarnings(as.numeric(w))
  if (!is.finite(w_num)) return("")

  suffix <- ""

  # Interpret weight:
  #   weight <= 0  → terminal reward effectively disabled
  #   weight > 0   → enabled
  if (w_num <= 0) {
    suffix <- " | no terminal reward"
  } else {
    suffix <- " | with terminal reward"

    # If weight differs from 1, label it explicitly as linear scaling
    # (the assumption: weight=1 is the default “fully on” setting)
    if (!identical(w_num, 1)) {
      suffix <- sprintf("%s | linear (*%s)", suffix, fmt_val(w_num))
    }

    # Mode controls the terminal reward transformation form
    mode_chr <- if (is.null(mode)) "linear" else as.character(mode)

    # If non-linear mode is active, annotate with the relevant parameter
    if (!identical(mode_chr, "linear")) {
      if (identical(mode_chr, "power")) {
        suffix <- sprintf("%s | power (^%s)", suffix, fmt_val(alpha))
      } else if (identical(mode_chr, "threshold")) {
        suffix <- sprintf("%s | threshold (\u03c4=%s)", suffix, fmt_val(tau))
      } else {
        # Fallback: unknown mode name, still show it
        suffix <- sprintf("%s | %s", suffix, mode_chr)
      }
    }
  }

  suffix
}


# health_terminal_reward_parts() does something similar to health_reward_subtitle(),
# but returns a *vector of pieces* rather than a single concatenated string.
#
# This is useful when you want:
#   - multi-line captions
#   - bullet lists
#   - facet labels with multiple fields
#
# Example output:
#   c("with terminal reward", "γ = 1", "power (α = 3)")
# or:
#   c("no terminal reward")
health_terminal_reward_parts <- function(policy_args = list(), sim_args = list()) {
  pa <- if (is.null(policy_args)) list() else policy_args
  sa <- if (is.null(sim_args)) list() else sim_args

  fmt_val <- function(x) {
    xv <- suppressWarnings(as.numeric(x))
    if (is.null(x) || !is.finite(xv)) return("?")
    formatC(xv, format = "g", digits = 4)
  }

  w     <- pick_first(pa$terminal_reward_weight,   sa$terminal_reward_weight)
  mode  <- pick_first(pa$terminal_reward_mode,     sa$terminal_reward_mode)
  alpha <- pick_first(pa$terminal_power_alpha,     sa$terminal_power_alpha)
  tau   <- pick_first(pa$terminal_threshold_tau,   sa$terminal_threshold_tau)

  parts <- character()

  # Only treat as "enabled" if weight exists, is numeric, and > 0
  if (!is.null(w)) {
    w_num <- suppressWarnings(as.numeric(w))
    if (is.finite(w_num) && w_num > 0) {

      # Always include the “enabled” message and the gamma value
      parts <- c(parts, "with terminal reward", sprintf("\u03B3 = %s", fmt_val(w_num)))

      # Mode controls how we describe the terminal function
      mode_chr <- if (is.null(mode) || !nzchar(mode)) "linear" else as.character(mode)

      if (identical(mode_chr, "power")) {
        parts <- c(parts, sprintf("power (\u03B1 = %s)", fmt_val(alpha)))
      } else if (identical(mode_chr, "threshold")) {
        parts <- c(parts, sprintf("threshold (\u03C4 = %s)", fmt_val(tau)))
      }

      return(parts)
    }
  }

  # Default if not enabled
  c("no terminal reward")
}


# === STEP 2c: Filename-safe tag for HEALTH terminal rewards ===================
# When saving plots, you want filenames to encode configuration so you don’t
# overwrite outputs from different reward settings.
#
# health_reward_tag() constructs an ASCII-safe tag like:
#   "h0-100_tr"                      (default terminal reward)
#   "h0-100_tr_w0.5"                 (scaled terminal reward)
#   "h0-100_tr_power-a3"             (power mode)
#   "h0-100_tr_thresh-tau60pct"      (threshold mode)
#   "h0-100_tr-none"                 (disabled)
#
# This tag is designed for filenames, not for human-readable captions.
health_reward_tag <- function(policy_args = list(), sim_args = list(), include_h0 = TRUE) {
  pa <- if (is.null(policy_args)) list() else policy_args
  sa <- if (is.null(sim_args)) list() else sim_args

  # Convert numerics to compact strings and remove odd characters
  fmt_num <- function(x) {
    xv <- suppressWarnings(as.numeric(x))
    if (is.null(xv) || !is.finite(xv)) return(NULL)
    gsub("[^0-9A-Za-z.+-]", "", formatC(xv, format = "g", digits = 6))
  }

  # Ensure mode strings etc. become filesystem-safe
  sanitize <- function(x) gsub("[^A-Za-z0-9._-]", "", x)

  # Pull potential parameters (h0 might appear as h0 or H0)
  h0    <- pick_first(pa$h0, pa$H0, sa$h0, sa$H0)
  w     <- pick_first(pa$terminal_reward_weight,   sa$terminal_reward_weight)
  mode  <- pick_first(pa$terminal_reward_mode,     sa$terminal_reward_mode)
  alpha <- pick_first(pa$terminal_power_alpha,     sa$terminal_power_alpha)
  tau   <- pick_first(pa$terminal_threshold_tau,   sa$terminal_threshold_tau)

  parts <- character()

  # Optionally encode the health ceiling in the filename tag
  if (include_h0 && !is.null(h0)) {
    h0_fmt <- fmt_num(h0)
    if (!is.null(h0_fmt)) parts <- c(parts, paste0("h0-", h0_fmt))
  }

  # Encode terminal reward configuration if weight is provided
  if (!is.null(w)) {
    w_num <- suppressWarnings(as.numeric(w))
    if (is.finite(w_num)) {

      # Disabled
      if (w_num <= 0) {
        parts <- c(parts, "tr-none")

      } else {
        # Enabled
        parts <- c(parts, "tr")

        # Weight (only include if not default 1)
        if (!identical(w_num, 1)) {
          w_fmt <- fmt_num(w_num)
          if (!is.null(w_fmt)) parts <- c(parts, paste0("w", w_fmt))
        }

        # Mode specifics
        mode_chr <- if (is.null(mode)) "linear" else as.character(mode)
        if (!identical(mode_chr, "linear")) {
          if (identical(mode_chr, "power")) {
            a_fmt <- fmt_num(alpha)
            parts <- c(parts, paste0("power", if (!is.null(a_fmt)) paste0("-a", a_fmt) else ""))
          } else if (identical(mode_chr, "threshold")) {
            tau_fmt <- fmt_num(tau)
            parts <- c(parts, paste0("thresh", if (!is.null(tau_fmt)) paste0("-tau", tau_fmt) else ""))
          } else {
            # Unknown mode: still encode it
            parts <- c(parts, paste0("mode-", sanitize(mode_chr)))
          }
        }
      }
    }
  }

  if (length(parts) == 0) return("")
  paste(vapply(parts, sanitize, character(1)), collapse = "_")
}


# Directory name for health plots based on reward settings
# Similar idea to health_reward_tag(), but this is meant to define a directory
# structure so outputs naturally group by health configuration.
#
# Examples:
#   "health_no_tr"
#   "health_tr"
#   "health_tr_w0.5"
#   "health_power_a3"
#   "health_threshold_tau60pct"
health_reward_dir <- function(policy_args = list(), sim_args = list()) {
  pa <- if (is.null(policy_args)) list() else policy_args
  sa <- if (is.null(sim_args)) list() else sim_args

  fmt_num <- function(x) {
    xv <- suppressWarnings(as.numeric(x))
    if (is.null(xv) || !is.finite(xv)) return(NULL)
    gsub("[^0-9A-Za-z.+-]", "", formatC(xv, format = "g", digits = 6))
  }

  w     <- pick_first(pa$terminal_reward_weight,   sa$terminal_reward_weight)
  mode  <- pick_first(pa$terminal_reward_mode,     sa$terminal_reward_mode)
  alpha <- pick_first(pa$terminal_power_alpha,     sa$terminal_power_alpha)
  tau   <- pick_first(pa$terminal_threshold_tau,   sa$terminal_threshold_tau)

  mode_chr <- if (is.null(mode)) "linear" else as.character(mode)
  w_num <- suppressWarnings(as.numeric(w))[1]

  # If missing or <=0, treat as "no terminal reward"
  if (length(w_num) == 0 || is.null(w_num) || is.na(w_num) || !is.finite(w_num) || w_num <= 0) {
    return("health_no_tr")
  }

  # Mode-specific directories
  if (identical(mode_chr, "power")) {
    a_fmt <- fmt_num(alpha)
    return(paste0("health_power", if (!is.null(a_fmt)) paste0("_a", a_fmt) else ""))
  }
  if (identical(mode_chr, "threshold")) {
    tau_fmt <- fmt_num(tau)
    return(paste0("health_threshold", if (!is.null(tau_fmt)) paste0("_tau", tau_fmt) else ""))
  }

  # If linear but weighted, encode weight
  if (!identical(w_num, 1)) {
    w_fmt <- fmt_num(w_num)
    return(paste0("health_tr_w", if (!is.null(w_fmt)) w_fmt else ""))
  }

  # Default terminal reward directory
  "health_tr"
}


# === STEP 3: A neutral, readable ggplot theme =================================
# theme_clean_minimal() is a “safe default” for general figures:
#   - minimal baseline
#   - removes minor grid noise
#   - adds a border so panels stand out when printed
#   - bold titles/axis titles for readability
theme_clean_minimal <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, colour = "black", linewidth = 0.5),
      plot.title       = element_text(face = "bold"),
      axis.title       = element_text(face = "bold"),
      legend.key       = element_rect(colour = "black")
    )
}


# === STEP 3b: Project-wide style helpers ======================================
# Everything below exists to make it hard to accidentally change aesthetics
# across plots; scripts should call these helpers rather than define their own.

# Discrete action palette:
#   - "High": vigilant action
#   - "Low":  relaxed action
#   - "Tie":  indifference region
palette_actions <- c(High = "#e02b35", Low = "#59a89c", Tie = "purple")

# Fill scale for geom_* that uses fill aesthetic
scale_action_fill <- function(drop = FALSE, na.value = "grey90", labels = NULL) {
  ggplot2::scale_fill_manual(
    values = palette_actions,
    drop = drop,
    na.value = na.value,
    labels = labels
  )
}

# Color scale for geoms that use colour aesthetic (lines/points/outlines)
scale_action_color <- function(drop = FALSE, na.value = NULL) {
  ggplot2::scale_color_manual(
    values = palette_actions,
    drop = drop,
    na.value = na.value
  )
}

# A tight theme optimized for:
#   - heatmaps (tile plots)
#   - small multiples / facet grids
#   - minimal clutter in dense panels
theme_vigilance <- function(base_size = 12, strip_size = 12, spacing_lines = 0.1) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid    = ggplot2::element_blank(),  # remove all gridlines (tiles already show structure)
      panel.spacing = grid::unit(spacing_lines, "lines"),
      strip.text    = ggplot2::element_text(size = strip_size),
      panel.border  = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title    = ggplot2::element_text(face = "bold", size = base_size + 2),
      axis.title    = ggplot2::element_text(face = "plain", margin = grid::unit(c(0, 0.5, 0.5, 0), "lines")),
      legend.key    = ggplot2::element_rect(colour = "black")
    )
}

# A “classic” theme with light major gridlines: useful for supervisor-facing
# line/point plots where exact reading is helpful.
theme_supervisor_grid <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "gray80", size = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border    = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      plot.title      = ggplot2::element_text(face = "plain", size = base_size + 4),
      plot.subtitle   = ggplot2::element_text(face = "plain", size = base_size + 2),
      axis.title      = ggplot2::element_text(face = "plain"),
      axis.text       = ggplot2::element_text(size = base_size),
      strip.text      = ggplot2::element_text(face = "plain", size = base_size + 1),
      strip.background = ggplot2::element_blank(),
      strip.background.y = ggplot2::element_blank(),
      legend.key      = ggplot2::element_rect(colour = "black", fill = "white", linewidth = 0.4)
    )
}

# A dedicated thesis-ready heatmap theme used by all environment and mechanism panels.
# This adds consistent spacing and margin behavior for publication-layout figures.
theme_thesis_heatmap <- function(base_size = 14, base_family = "sans") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border    = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      plot.title      = ggplot2::element_text(face = "bold", size = base_size + 4, hjust = 0.5),
      plot.subtitle   = ggplot2::element_text(face = "plain", size = base_size + 2, colour = "gray40", hjust = 0.5),
      axis.title      = ggplot2::element_text(face = "plain", size = base_size + 2),
      axis.text       = ggplot2::element_text(size = base_size),
      strip.text      = ggplot2::element_text(face = "plain", size = base_size + 2),
      legend.key      = ggplot2::element_rect(colour = "black", fill = "white", linewidth = 0.4),
      panel.spacing.x = grid::unit(0.45, "lines"),
      panel.spacing.y = grid::unit(0.45, "lines"),
      plot.margin     = ggplot2::margin(t = 3, r = 3, b = 3, l = 3, unit = "pt"),
      strip.background = ggplot2::element_blank(),
      strip.background.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.key.height = grid::unit(0.4, "in"),
      legend.key.width  = grid::unit(0.7, "in"),
      legend.text = ggplot2::element_text(size = base_size),
      legend.title = ggplot2::element_text(size = base_size + 1)
    )
}

# Overlay helper for integer grids:
# Adds faint v/h lines at half-integers so tile centers align with integer labels.
# Useful when x/y represent discrete levels but plotted on continuous axes.
unit_grid_xy <- function(xmin, xmax, ymin, ymax, step = 1,
                         v_col = "grey85", h_col = "grey90", size = 0.25) {
  vlines <- seq(xmin - 0.5, xmax + 0.5, by = step)
  hlines <- seq(ymin - 0.5, ymax + 0.5, by = step)
  list(
    ggplot2::geom_vline(xintercept = vlines, colour = v_col, size = size),
    ggplot2::geom_hline(yintercept = hlines, colour = h_col, size = size)
  )
}


# === STEP 4: Standard grayscale fill scale for 0–1 rates ======================
# Many of your heatmaps encode rates or proportions (0..1).
# Fixing the scale limits to [0,1] makes different panels *comparable*:
# otherwise ggplot rescales each panel independently and exaggerates differences.
scale_fill_grey01 <- function(title = "hypervigilance") {
  scale_fill_gradient(
    limits = c(0, 1),
    low    = "white",
    high   = "black",
    name   = title
  )
}


# === STEP 5: Default parallel plan for grid simulations =======================
# Some pipelines use future.apply to parallelize grid sweeps.
#
# This block sets a default plan only if the user hasn’t already configured one.
# It avoids stomping on a custom plan set by another script or interactive session.
if (!inherits(future::plan(), "multisession")) {
  future::plan(
    future::multisession,
    workers = max(1, parallel::detectCores() - 1) # keep one core free for responsiveness
  )
}


# === STEP 3 (later block): Model dispatch + deterministic caches ==============
# NOTE: numbering is slightly out of order because this file grew over time.
# Functionally, this is the “performance layer”:
#   - chooses which cache object to use
#   - selects “raw” compute/sim functions when available
#   - implements file-backed caching for reproducibility and speed


# cache_for(): choose the right persistent cache based on model family.
# - health caches often differ because policies/sims depend on extra args (h0, terminal reward etc.)
cache_for <- function(model){
  if (identical(model, "health")) .use_cache(persistent_cache_health) else .use_cache(persistent_cache)
}


# Prefer raw variants if present, otherwise fall back
# The project appears to expose both memoised and raw versions of functions in some contexts.
# These helpers pick the best available function in priority order.
#
# Why prefer *_raw?
#   - raw functions avoid nested memoisation layers
#   - ensures we control caching at THIS layer (file-backed cache)
.pick_policy_fun_raw <- function(m, model) {
  if (identical(model, "health") && !is.null(m$compute_optimal_policy_health_raw)) return(m$compute_optimal_policy_health_raw)
  if (!is.null(m$compute_optimal_policy_raw)) return(m$compute_optimal_policy_raw)
  if (identical(model, "health") && !is.null(m$compute_optimal_policy_health)) return(m$compute_optimal_policy_health)
  if (!is.null(m$compute_optimal_policy)) return(m$compute_optimal_policy)
  stop("No policy function found for model = ", model)
}
.pick_sim_fun_raw <- function(m, model) {
  if (identical(model, "health") && !is.null(m$simulate_agents_forward_health_raw)) return(m$simulate_agents_forward_health_raw)
  if (!is.null(m$simulate_agents_forward_raw)) return(m$simulate_agents_forward_raw)
  if (identical(model, "health") && !is.null(m$simulate_agents_forward_health)) return(m$simulate_agents_forward_health)
  if (!is.null(m$simulate_agents_forward)) return(m$simulate_agents_forward)
  stop("No simulation function found for model = ", model)
}


# --- File-backed cache helpers (atomic-ish) -----------------------------------
# These utilities implement a simple on-disk cache keyed by a hashed object.
#
# Key idea:
#   - Build a list describing the computation inputs ("key_obj")
#   - Hash that list deterministically (xxhash64)
#   - Store the result in cache/<something>/<hash>.rds
#
# “atomic-ish”:
#   - write to a temporary file
#   - rename into place (rename is atomic on most filesystems)
#   - if rename fails, fallback to copy
.safe_key   <- function(obj) digest::digest(obj, algo = "xxhash64")

.cache_path <- function(dir, key_obj) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.path(dir, paste0(.safe_key(key_obj), ".rds"))
}

.cache_get  <- function(dir, key_obj) {
  p <- .cache_path(dir, key_obj)
  if (!file.exists(p)) return(NULL)

  # If the RDS is corrupt or partially written, delete it and treat as a miss.
  tryCatch(
    readRDS(p),
    error = function(e) { try(unlink(p, force = TRUE), silent = TRUE); NULL }
  )
}

.cache_put  <- function(dir, key_obj, value) {
  p <- .cache_path(dir, key_obj)
  tmp <- paste0(p, ".", Sys.getpid(), ".", sample.int(1e9, 1), ".tmp")

  # Write to temp path first
  saveRDS(value, tmp)

  # Try to atomically swap temp into final path
  ok <- tryCatch(file.rename(tmp, p), warning = function(w) FALSE, error = function(e) FALSE)

  # Fallback: if rename fails (e.g., cross-filesystem), copy then cleanup temp
  if (!ok) {
    if (!file.exists(p)) file.copy(tmp, p, overwrite = TRUE)
    unlink(tmp, force = TRUE)
  }

  value
}


# --- Cached policy -------------------------------------------------------------
# mem_compute_policy(): cached wrapper around optimal policy computation.
#
# Inputs:
#   - model (basic/health)
#   - cost parameters K,C,D,d
#   - environment parameters LA,LL
#   - horizon T_steps + state set
#   - health-only: policy_args
#
# Output:
#   - a policy data.frame (schema depends on model implementation)
#
# Why cache?
#   Policy computation is expensive (DP), and many plots repeatedly call the same
#   computations across multiple scripts/pipelines.
mem_compute_policy <- function(model, K, C, D, d, LA, LL, T_steps, states, policy_args) {

  # Separate cache directories by model family
  cache_dir <- if (identical(model, "health")) "cache/policy_cache_health" else "cache/policy_cache"

  # The cache key fully describes the computation.
  # If any element changes, the hash changes and we recompute.
  key_obj <- list(
    kind="policy",
    model=model,
    args=list(K=K,C=C,D=D,d=d,LA=LA,LL=LL,T=T_steps,states=states),
    policy_args=policy_args
  )

  # Return cached result if available
  if (!is.null(hit <- .cache_get(cache_dir, key_obj))) return(hit)

  # Otherwise compute:
  # 1) obtain correct model interface
  # 2) pick raw policy function if possible
  # 3) compute policy
  m <- get_model(model, policy_args = policy_args, sim_args = list())
  f <- .pick_policy_fun_raw(m, model)
  out <- f(K, C, D, d, LA, LL, T_steps, states)

  # Save to cache and return
  .cache_put(cache_dir, key_obj, out)
}


# --- Cached simulation ---------------------------------------------------------
# mem_simulate_agents(): cached wrapper around forward simulation.
#
# Caching simulation is trickier than caching policy, because:
#   - simulation depends on the policy_df content, not just parameters
#   - health/basic policy data.frames may have different column names
#
# Strategy:
#   - build a "policy signature" (pol_sig) capturing:
#       * dimensions
#       * a short head of key columns (time/action/label)
#       * a hash of the action column (or entire policy_df if needed)
#   - include that signature into the cache key.
mem_simulate_agents <- function(model, policy_df, LA, LL, T_steps, N_agents, K, C, D, d, sim_args) {

  cache_dir <- if (identical(model, "health")) "cache/sim_cache_health" else "cache/sim_cache"

  # Handle policy_df schema differences:
  # - time column might be Time or time
  # - action column might be Optimal_Action or optimal_action
  # - label might be Label or label
  time_col  <- if ("Time" %in% names(policy_df)) "Time" else if ("time" %in% names(policy_df)) "time" else NULL
  act_col   <- if ("Optimal_Action" %in% names(policy_df)) "Optimal_Action" else if ("optimal_action" %in% names(policy_df)) "optimal_action" else NULL
  label_col <- if ("Label" %in% names(policy_df)) "Label" else if ("label" %in% names(policy_df)) "label" else NULL

  # Extract action values when possible (cheaper to hash than full df)
  act_vals <- if (!is.null(act_col)) policy_df[[act_col]] else NULL

  # Build robust policy signature
  pol_sig <- list(
    nrow  = nrow(policy_df),
    ncol  = ncol(policy_df),
    headT = if (!is.null(time_col))  utils::head(policy_df[[time_col]], 6) else NULL,
    headA = if (!is.null(act_col))   utils::head(act_vals, 6) else NULL,
    headL = if (!is.null(label_col)) utils::head(policy_df[[label_col]], 6) else NULL,

    # Hash action column if present (most important),
    # else hash full policy_df
    hash  = if (!is.null(act_vals)) {
      digest::digest(act_vals, algo = "xxhash64")
    } else {
      digest::digest(policy_df, algo = "xxhash64")
    }
  )

  # Full cache key: environment + costs + policy identity + sim args
  key_obj <- list(
    kind="simulation",
    model=model,
    env=list(LA=LA,LL=LL,T=T_steps,N=N_agents),
    costs=list(K=K,C=C,D=D,d=d),
    policy_sig=pol_sig,
    sim_args=sim_args
  )

  # Return cached simulation if present
  if (!is.null(hit <- .cache_get(cache_dir, key_obj))) return(hit)

  # Otherwise simulate:
  m <- get_model(model, policy_args = list(), sim_args = sim_args)
  f <- .pick_sim_fun_raw(m, model)
  out <- f(policy_df, LA, LL, T_steps, N_agents, K, C, D, d)

  # Save simulation output and return
  .cache_put(cache_dir, key_obj, out)
}


# === STEP 7: Save helper (PNG + PDF, publication grade) =======================
# save_graphs() standardizes plot export:
#   - always saves both PNG and PDF
#   - enforces consistent size/dpi defaults
#   - automatically places outputs in model-specific directories:
#       graphs/basic/...
#       graphs/health_tr/...
#       graphs/health_power_a3/...
#
# It also supports an optional suffix so you can save variants without manually
# changing the stem each time.
save_graphs <- function(plot_obj, stem, width = 14, height = 8, dpi = 300, bg = "white", suffix = NULL,
                        model = NULL, policy_args = list(), sim_args = list()) {
  stopifnot(inherits(plot_obj, "ggplot") || inherits(plot_obj, "patchwork"))

  # Split "stem" into directory + base name.
  # If user passes "graphs/foo/bar", stem_dir="graphs/foo", stem_base="bar".
  stem_dir  <- dirname(stem)
  stem_base <- basename(stem)

  # If no directory was supplied, default to "graphs"
  base_dir <- if (identical(stem_dir, ".") || stem_dir == "") "graphs" else stem_dir

  # Infer model type if not explicitly provided, based on name hints.
  # This is convenience for callers: they can pass a stem like "health_hv_heatmap"
  # and still get health-specific directories automatically.
  if (is.null(model)) {
    if (grepl("health", stem_base, ignore.case = TRUE) || grepl("health", stem_dir, ignore.case = TRUE)) {
      model <- "health"
    } else if (grepl("basic", stem_base, ignore.case = TRUE) || grepl("basic", stem_dir, ignore.case = TRUE)) {
      model <- "basic"
    }
  }

  # Choose subdirectory based on model and (for health) terminal reward regime
  subdir <- NULL
  if (!is.null(model)) {
    if (identical(model, "basic"))  subdir <- "basic"
    if (identical(model, "health")) subdir <- health_reward_dir(policy_args, sim_args)
  }

  # Final output directory = base_dir + optional subdir
  final_dir <- if (is.null(subdir)) base_dir else file.path(base_dir, subdir)
  dir.create(final_dir, showWarnings = FALSE, recursive = TRUE)

  # Optional suffix appended to filename
  if (!is.null(suffix) && nzchar(suffix)) stem_base <- paste0(stem_base, "_", suffix)

  # Construct output paths
  png_path <- file.path(final_dir, paste0(stem_base, ".png"))
  pdf_path <- file.path(final_dir, paste0(stem_base, ".pdf"))

  # Save both raster (PNG) and vector (PDF) formats
  ggsave(filename = png_path, plot = plot_obj, width = width, height = height, dpi = dpi, units = "in", bg = bg)
  ggsave(filename = pdf_path, plot = plot_obj, width = width, height = height, dpi = dpi, units = "in", bg = bg)

  message("Saved: ", png_path, " and ", pdf_path)
}

save_table <- function(table_obj, stem, format = c("csv", "tsv", "rds")) {
  stopifnot(is.data.frame(table_obj))
  fmt <- match.arg(format)
  target_dir <- dirname(stem)
  base_name <- basename(stem)

  if (!target_dir %in% c("", ".")) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ext <- switch(fmt,
    csv = ".csv",
    tsv = ".tsv",
    rds = ".rds"
  )

  out_path <- if (target_dir %in% c("", ".")) {
    paste0(base_name, ext)
  } else {
    file.path(target_dir, paste0(base_name, ext))
  }

  switch(fmt,
    csv = utils::write.csv(table_obj, out_path, row.names = FALSE),
    tsv = utils::write.table(table_obj, out_path, sep = "\t", row.names = FALSE),
    rds = saveRDS(table_obj, out_path)
  )

  message("Saved table: ", out_path)
  invisible(out_path)
}


# ------------------------------------------------------------------------------
# Default model variants to compare
# ------------------------------------------------------------------------------
# default_model_specs() defines the canonical set of model variants used in
# comparative pipelines and figure grids.
#
# It returns a data.frame with columns:
#   - model_id:    filename/ID-friendly string
#   - model_label: human-friendly label for plot facets/legends
#   - model_type:  "basic" or "health"
#   - policy_args: list-column of health policy settings
#   - sim_args:    list-column of health simulation settings
#
# Important:
#   - The basic model uses empty args.
#   - Health variants are parameterized by terminal reward settings.
default_model_specs <- function(h0 = validate_default("h0")) {

  data.frame(
    model_id    = c("basic", "health_no_tr", "health_tr_w1", "health_power_a3", "health_thresh_tau60pct"),

    model_label = c(
      "Basic",
      "Health (ω = 0)",
      "Linear (ω = 1)",
      "Power (α = 3)",
      "Threshold (τ = 0.6·H0)"
    ),

    model_type  = c("basic", "health", "health", "health", "health"),

    # policy_args is a list-column:
    # each row contains the argument list that will be passed into
    # compute_optimal_policy_health via get_model() wrappers.
    policy_args = I(list(
      list(),  # basic: ignored

      # Health variant 1: terminal reward disabled
      list(h0 = h0, health_step = 1, terminal_reward_weight = 0, terminal_reward_mode = "linear"),

      # Health variant 2: linear terminal reward enabled (ω=1)
      list(h0 = h0, health_step = 1, terminal_reward_weight = 1, terminal_reward_mode = "linear"),

      # Health variant 3: power terminal reward (α=3)
      list(h0 = h0, health_step = 1, terminal_reward_weight = 1,
           terminal_reward_mode = "power", terminal_power_alpha = 3),

      # Health variant 4: threshold terminal reward (τ=0.6·H0)
      list(h0 = h0, health_step = 1, terminal_reward_weight = 1,
           terminal_reward_mode = "threshold", terminal_threshold_tau = round(0.6 * h0))
    )),

    # sim_args is also a list-column:
    # these are passed into simulate_agents_forward_health via the wrapper.
    # Settings here appear to control initial health distribution + randomness.
    sim_args = I(list(
      list(),  # basic: ignored

      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE),
      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE),
      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE),
      list(h0 = h0, spread_initial_over_levels = FALSE, shuffle = TRUE)
    )),

    stringsAsFactors = FALSE
  )
}
