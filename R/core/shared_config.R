# ------------------------------------------------------------------------------
# FILE: 00_shared_config.R
# ROLE:
#   This is the “single source of truth” for small project-wide constants and
#   lightweight helpers that many scripts rely on:
#     - Directory paths (raw/processed/cache, outputs)
#     - Figure registry metadata (which script makes which figure)
#     - Global default model parameters (N, h0, T, D, d, c, etc.)
#     - Small utilities for defaults and config merging (validate_default, pick_first, merge_defaults)
#
# Why have this file?
#   - Prevents copy-pasting constants across scripts (which causes drift).
#   - Makes changing defaults safe: you edit ONE place and all pipelines align.
#   - Provides consistent logging/metadata when building many figures.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Dependency checks (fail fast)
# ------------------------------------------------------------------------------
# This file uses `here::here()` to construct project-root-relative paths.
# It also uses `tibble::tribble()` to build the figure registry.
#
# We explicitly check for these packages using requireNamespace() so:
#   - we avoid attaching packages globally (no side effects on search path)
#   - we produce a clean, informative error early if the environment is missing them
#
# call. = FALSE keeps the error message cleaner (no call stack printed by default).
if (!requireNamespace("here", quietly = TRUE)) {
  stop("The 'here' package is required by shared_config.R", call. = FALSE)
}
if (!requireNamespace("tibble", quietly = TRUE)) {
  stop("The 'tibble' package is required by shared_config.R", call. = FALSE)
}


# ------------------------------------------------------------------------------
# Directory constants
# ------------------------------------------------------------------------------
# These are canonical directories used throughout the project.
# We construct them using here::here() so they are robust to:
#   - different working directories
#   - running from RStudio vs command line
#   - Windows vs macOS/Linux path conventions
#
# Convention:
#   - data/... are inputs and intermediate artifacts
#   - outputs/... are paper-ready exports (figures/tables)
DIR_DATA_RAW       <- here::here("data", "raw")
DIR_DATA_PROCESSED <- here::here("data", "processed")
DIR_DATA_CACHE     <- here::here("data", "cache")
DIR_FIGURES        <- here::here("outputs", "figures")
DIR_TABLES         <- here::here("outputs", "tables")


# ------------------------------------------------------------------------------
# Structured figure registry
# ------------------------------------------------------------------------------
# FIGURE_REGISTRY is a human + machine readable mapping from:
#   figure_id  → script path → description
#
# This helps:
#   - logging: “Starting Fig4B …”
#   - automation: build all figures in order
#   - discoverability: quickly find which script generates a given figure
#
# tibble::tribble() is used for readability: it’s a “row-wise tibble literal”.
FIGURE_REGISTRY <- tibble::tribble(
  ~figure_id, ~script, ~description,

  # Basic model figure(s)
  "Fig1A", "R/plotting/basic_policy/03_call_basic_dp_policy.R",
  "Basic DP policy visual (full and compact)",

  # Environment figures
  "Fig2A", "R/plotting/env_heatmaps/03_call_env_heatmaps_by_model.R",
  "Environment heatmaps faceted by model variant",
  "Fig2B", "R/plotting/env_hv_matrix/03_call_env_hv_matrix.R",
  "Environment hypervigilance matrix summary",
  "Fig2C", "R/plotting/env_heatmaps/03_call_env_heatmaps_with_region_key.R",
  "Environment heatmaps with region key",

  # Health-model figures
  "Fig3A", "R/plotting/health_dp/03_call_health_dp.R",
  "Health DP comparison across policy variants",
  "Fig3B", "R/plotting/health_sim/03_call_health_sim.R",
  "Health simulation diagnostics (time/heatmaps)",
  "Fig3C", "R/plotting/health_policy_bars/03_call_policy_bars_by_model.R",
  "Health policy bars by model",
  "Fig3D", "R/plotting/health_policy_bars/03_call_policy_bars_grid.R",
  "Integrated health policy bars summary",

  # Mechanism figures
  "Fig4A", "R/plotting/mechanism_env/03_call_env_mechanisms.R",
  "Mechanism hypervigilance grid",
  "Fig4B", "R/plotting/mechanism_flow/03_call_vigilance_flow.R",
  "Vigilance flow grid examples",
  "Fig4C", "R/plotting/mechanism_thresholds/03_call_model_thresholds.R",
  "Model threshold behavior",
  "Fig4D", "R/plotting/mechanism_risk_autocorr/03_call_risk_autocorr_mechanisms.R",
  "Risk/autocorrelation mechanism heatmaps",
  "Fig4E", "R/tables/mechanism_threshold_analysis/03_call_threshold_analysis_table.R",
  "Threshold analysis panel",

  # SSP figures
  "Fig5A", "R/plotting/fig05_ssp_ranges/03_call_ssp_ranges_heatmap.R",
  "SSP range environment heatmap",
  "Fig5B", "R/plotting/fig05_ssp_ranges/03_call_ssp_ranges_heatmap_viridis.R",
  "SSP viridis environment heatmap",

  # Symmetry / risk-autocorr figures
  "Fig6A", "R/plotting/symmetry_ssp_autocorr_grid/03_call_ssp_autocorr_grid.R",
  "Hypervigilance rate vs autocorrelation grid",
  "Fig6B", "R/plotting/symmetry_autocorr/03_call_autocorr_heatmap_viridis.R",
  "Symmetry autocorr heatmap (viridis)",
  "Fig6C", "R/plotting/symmetry_autocorr/03_call_autocorr_heatmap.R",
  "Symmetry K autocorrelation heatmap",
  "Fig6D", "R/plotting/symmetry_risk/03_call_risk_heatmap.R",
  "Symmetry K risk plot",
  "Fig6E", "R/plotting/symmetry_risk/03_call_risk_heatmap_viridis.R",
  "Symmetry K risk plot (viridis)",
  "Fig6F", "R/plotting/symmetry_combo/03_call_combo.R",
  "Symmetry risk-autocorrelation combo"
)


# ------------------------------------------------------------------------------
# Path normalization helpers (Windows/Unix consistency)
# ------------------------------------------------------------------------------
# Problem:
#   - On Windows, paths often contain backslashes (\) and normalizePath behaves
#     differently than on Unix.
#   - Scripts may be referenced with relative or absolute paths.
#   - The registry stores script paths in a normalized “repo-relative” form.
#
# Solution:
#   normalize_script_path() converts an input path into a normalized, repo-relative
#   path with forward slashes (/). This allows consistent lookups into FIGURE_REGISTRY.
normalize_script_path <- function(script_path) {

  # Determine the project root (absolute path) in a consistent slash format
  root <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)

  # Try to normalize the script_path:
  #   - If script_path is already absolute and exists, normalizePath works.
  #   - If it is relative or doesn’t exist yet, normalizePath might error.
  #     In that case we attempt to interpret it relative to the project root.
  abs_path <- tryCatch(
    normalizePath(script_path, winslash = "/", mustWork = FALSE),
    error = function(e) normalizePath(file.path(root, script_path), winslash = "/", mustWork = FALSE)
  )

  # Convert absolute path to project-relative path by stripping the root prefix
  rel_path <- sub(paste0("^", root, "/?"), "", abs_path, perl = TRUE)

  # Ensure forward slashes even if something slipped through
  rel_path <- gsub("\\\\", "/", rel_path)

  # Strip leading "./" if present
  rel_path <- sub("^./", "", rel_path, perl = TRUE)

  rel_path
}


# ------------------------------------------------------------------------------
# Registry lookup utilities
# ------------------------------------------------------------------------------
normalize_script_path <- function(script_path) {
  find_project_root <- function(start = getwd()) {
    current <- normalizePath(start, winslash = "/", mustWork = TRUE)
    repeat {
      marker <- file.path(current, "R", "core", "shared_config.R")
      if (file.exists(marker)) {
        return(current)
      }
      parent <- dirname(current)
      if (identical(parent, current)) {
        stop("Could not find project root containing R/core/shared_config.R.")
      }
      current <- parent
    }
  }

  root <- find_project_root()
  script_path <- gsub("\\\\", "/", script_path)
  if (grepl("^R/", script_path)) {
    return(script_path)
  }
  is_absolute <- grepl("^[A-Za-z]:/", script_path) || grepl("^/", script_path)
  candidate <- if (is_absolute) script_path else file.path(root, script_path)
  abs_path <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
  rel_path <- sub(paste0("^", root, "/?"), "", abs_path, perl = TRUE)
  sub("^./", "", rel_path, perl = TRUE)
}

# These functions provide a small API around FIGURE_REGISTRY:
#   - figure_registry_entry(): return matching row(s) for a script path
#   - describe_figure_script(): return a friendly log string if script is known
#   - log_figure_start(): print a “Starting figure …” message and return metadata
#
# Keeping these helpers in shared_config.R means figure scripts can always do:
#   log_figure_start("R/plotting/.../03_call_*.R")
# without needing to reimplement any registry logic.
figure_registry_entry <- function(script_path) {
  rel_path <- normalize_script_path(script_path)

  # Subset by script path; drop = FALSE keeps the result a data.frame/tibble even
  # if there are zero or one matching rows (avoids accidental vector simplification).
  entry <- FIGURE_REGISTRY[FIGURE_REGISTRY$script == rel_path, , drop = FALSE]
  entry
}

describe_figure_script <- function(script_path) {
  entry <- figure_registry_entry(script_path)
  rel_path <- normalize_script_path(script_path)

  # If we have exactly one match, build a nice description.
  # Otherwise fall back to the normalized path.
  if (nrow(entry) == 1L) {
    paste0(entry$figure_id, " - ", entry$description, " (", entry$script, ")")
  } else {
    rel_path
  }
}

#' Log a figure script start and return its registry entry
#'
#' @param script_path relative path to the script inside the repo
#' @return tibble row describing the figure (invisible)
log_figure_start <- function(script_path) {

  # Lookup metadata (may be empty tibble if not registered)
  info <- figure_registry_entry(script_path)

  # Emit a stable, friendly message
  message("== Starting figure: ", describe_figure_script(script_path))

  # Return metadata without printing it by default
  invisible(info)
}


# === STEP 1: Global defaults ==================================================
# DEFAULT_MODEL_PARAMS is the canonical list of default parameters used across:
#   - DP policy computation (horizon length, damage parameters, etc.)
#   - simulations (agent count, spread flags)
#   - plotting grids (when a script doesn't override defaults)
#
# Centralizing them ensures:
#   - no “silent drift” where one script uses T=10 and another uses T=12
#   - easier reproducibility: a single lookup describes project-wide defaults
DEFAULT_MODEL_PARAMS <- list(
  N                     = 1000L,  # default number of simulated agents
  h0                    = 35L,    # default max integer health (health state ceiling)
  T                     = 10L,    # default horizon length / episode length
  D                     = 10,     # damage (or cost) when vigilant and stressor hits
  d                     = 0,      # damage (or cost) when relaxed and stressor hits
  c                     = 0,      # baseline relaxed cost (separate from damage)
  spread_initial_over_levels = FALSE   # whether to initialize agents spread across health levels
)

# validate_default():
#   small guardrail utility used throughout the project.
#   - ensures the requested key exists
#   - returns the corresponding default value
#
# Example use:
#   h0 <- validate_default("h0")
#   N  <- validate_default("N")
validate_default <- function(key) {
  stopifnot(key %in% names(DEFAULT_MODEL_PARAMS))
  DEFAULT_MODEL_PARAMS[[key]]
}


# === STEP 2: Helpers for merging configs ======================================
# Many functions accept arg lists (policy_args, sim_args) that may omit values.
# These helpers support a consistent pattern:
#   - pick_first(): choose the first non-NULL provided option
#   - merge_defaults(): fill in missing arg_list fields from DEFAULT_MODEL_PARAMS

# pick_first(...):
#   Returns the first argument that is not NULL, otherwise NULL.
#
# Used to implement “override precedence” logic such as:
#   w <- pick_first(policy_args$terminal_reward_weight, sim_args$terminal_reward_weight)
# meaning: prefer policy_args if present, else sim_args, else NULL.
pick_first <- function(...) {
  args <- list(...)
  for (value in args) if (!is.null(value)) return(value)
  NULL
}

# merge_defaults(arg_list, defaults):
#   Ensures arg_list contains entries for every name in defaults:
#     - if arg_list[[name]] is NULL, fill it with defaults[[name]]
#     - if arg_list[[name]] is already specified, keep user value
#
# This prevents missing-argument edge cases later, while still allowing overrides.
merge_defaults <- function(arg_list, defaults = DEFAULT_MODEL_PARAMS) {
  if (is.null(arg_list)) arg_list <- list()

  for (name in names(defaults)) {
    if (is.null(arg_list[[name]])) arg_list[[name]] <- defaults[[name]]
  }

  arg_list
}
