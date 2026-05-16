# ------------------------------------------------------------------------------
# File: R/core/setup_project.R
# PROJECT SETUP: packages, parallel plan, reproducibility, and on-disk caches
#
# This file is meant to be sourced early (often once per session or pipeline run).
# It establishes “global defaults” that many other scripts assume exist.
#
# What this file does (conceptually):
#   1) Loads packages used across the project (quietly).
#   2) Sets a portable parallel execution plan (future::multisession).
#   3) Fixes a global RNG seed so stochastic components are reproducible.
#   4) Ensures cache directories exist for on-disk caching.
#   5) Creates memoise filesystem backends (disk caches) if possible.
#   6) Defines a small helper (.use_cache) that only uses disk caches when valid.
#
# Why this matters:
#   - Without a consistent future plan, parallelized grid sweeps can behave
#     differently on different OSes (esp. Windows vs Linux).
#   - Without a fixed seed, “tie” resolution and Monte Carlo estimates shift
#     between runs, which is bad for paper figures.
#   - Without caches, repeated policy/simulation calls make iteration slow.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load shared configuration before packages
# ------------------------------------------------------------------------------
# shared_config.R is expected to provide:
#   - validate_default() and other default parameter lookups
#   - possibly helper functions like pick_first()
#   - shared constants used by both models and plotting pipelines
#
# Sourcing this first ensures that defaults exist before downstream code tries
# to use them. (It’s especially important when later files source plot_utils.R,
# which itself expects shared_config objects to exist.)
source("R/core/shared_config.R")


# === STEP 1: Load packages quietly ============================================
# suppressPackageStartupMessages():
#   - prevents clutter in console/log output when running long pipelines
#   - keeps RMarkdown / quarto output clean
#
suppressPackageStartupMessages({
  library(dplyr)        # core data manipulation verbs (select, mutate, summarise, ...)
  library(future.apply) # parallel versions of apply/lapply based on future framework
  library(memoise)      # caching/memoization utilities (RAM or filesystem)
  library(ggplot2)      # plotting framework (used broadly, even in “setup” contexts)
  library(tidyr)        # reshaping helpers: pivot_longer, pivot_wider, complete, ...
  library(forcats)      # factor tools: fct_reorder, fct_rev, etc.
  library(patchwork)    # optional ggplot layout compositor (plot + plot)
})


# ------------------------------------------------------------------------------
# Project-wide plotting helpers
# ------------------------------------------------------------------------------
# plot_utils.R typically defines:
#   - themes (theme_vigilance, theme_clean_minimal, ...)
#   - palettes and scales (palette_actions, scale_action_fill, ...)
#   - model dispatch helpers and caching wrappers (in your project)
#
# Why `try(..., silent = TRUE)`?
#   - This setup file should not crash the whole pipeline if plotting helpers
#     aren’t available in a particular run context (e.g., when running tests,
#     or when only doing pure computation).
#   - It also prevents warnings from spamming logs.
#
# Important tradeoff:
#   - If plot_utils.R fails to load when it *should* load, the failure is quiet.
#     That’s convenient for robustness but can hide real issues.
try(suppressWarnings(source("R/core/plot_utils.R")), silent = TRUE)


# === STEP 2: Choose a parallel plan (idempotent) ==============================
# The future ecosystem uses a “plan” to decide how future tasks run.
#
# multisession:
#   - works on Windows, macOS, Linux (unlike multicore, which is not Windows-safe)
#   - launches separate R sessions as workers
#
# Idempotent behavior:
#   - we ONLY set the plan if it is not already multisession
#   - this avoids overriding user/supervisor preferences if they set their own plan
#
# Why check class inheritance?
#   - future::plan() returns a plan object whose class indicates the backend type.
if (!inherits(future::plan(), "multisession")) {
  future::plan(
    future::multisession,

    # workers controls how many parallel R sessions to spawn.
    # Strategy here:
    #   - Use (logical cores - 1) so the machine remains responsive.
    #   - But never go below 2 workers, so “parallel” actually parallelizes.
    workers = max(
      2L,
      parallel::detectCores(logical = TRUE) - 1L
    )
  )
}


# === STEP 3: Reproducibility seed =============================================
# A global seed ensures deterministic randomness across:
#   - tie-breaking when multiple actions are equally optimal
#   - Monte Carlo forward simulations of agents
#   - any random shuffling or initialization used by the health model
#
# This makes figure regeneration stable for the paper.
#
# Caveat (important):
#   - If you use parallel futures, reproducibility may also require controlling
#     how RNG streams are handled across workers (e.g., future.seed = TRUE in calls).
#   - Still, setting a single session seed is a strong baseline.
set.seed(42)


# === STEP 4: Ensure cache directories exist ===================================
# The project uses on-disk caching to avoid recomputing expensive steps.
#
# Structure:
#   cache/
#     policy_cache/          (basic model policies)
#     policy_cache_health/   (health model policies)
#     sim_cache/             (basic simulations)
#     sim_cache_health/      (health simulations)
#
# Separate folders reduce accidental collisions:
#   - health policies depend on additional args (h0, terminal reward mode, etc.)
#   - simulations depend on both policy identity and simulation args
#
# dir.create() is safe to run repeatedly:
#   - showWarnings = FALSE avoids “already exists” warnings
#   - recursive = TRUE ensures intermediate directories are created as needed
dir.create("cache",                      showWarnings = FALSE)
dir.create("cache/policy_cache",         showWarnings = FALSE, recursive = TRUE)
dir.create("cache/policy_cache_health",  showWarnings = FALSE, recursive = TRUE)
dir.create("cache/sim_cache",            showWarnings = FALSE, recursive = TRUE)
dir.create("cache/sim_cache_health",     showWarnings = FALSE, recursive = TRUE)


# === STEP 5: Create memoise filesystem backends ===============================
# memoise caches function outputs keyed by inputs.
# cache_filesystem() creates a cache backend that stores values on disk.
#
# Why wrap in try()?
#   - If the process cannot write to disk (permissions, read-only environment),
#     cache_filesystem() can error.
#   - We store the try() result so the rest of the project can proceed and
#     fall back to in-memory caching instead of crashing.
#
# Note:
#   - These two caches are named like policy caches, but they’re “cache handles”
#     (objects memoise can use). The file-backed simulation caches in your project
#     are handled elsewhere via custom RDS hashing (in plot_utils.R).
persistent_cache <- try(
  memoise::cache_filesystem("cache/policy_cache"),
  silent = TRUE
)

persistent_cache_health <- try(
  memoise::cache_filesystem("cache/policy_cache_health"),
  silent = TRUE
)


# === STEP 6: Tiny helper to prefer disk cache when available ==================
# This helper centralizes one policy decision:
#   “Use disk cache only if it successfully initialized.”
#
# In your codebase, this is used like:
#   memoise::memoise(fun, cache = .use_cache(persistent_cache))
#
# Behavior:
#   - if cache_handle is a working cache_filesystem object: return it
#   - otherwise return NULL, letting memoise use its default (RAM) cache
#
# Why return NULL rather than error?
#   - The project remains runnable in restricted environments.
#   - You keep performance benefits where possible without making caching mandatory.
.use_cache <- function(cache_handle) {
  if (inherits(cache_handle, "cache_filesystem")) {
    cache_handle
  } else {
    NULL
  }
}
