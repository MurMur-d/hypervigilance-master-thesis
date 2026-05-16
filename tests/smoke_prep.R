#!/usr/bin/env Rscript
# Minimal public-repo smoke test. This is intentionally smaller than the full
# thesis reproduction pipeline so it is suitable for local checks and CI.

source("R/core/setup_project.R")
source("R/models/basic/basic_model_dp.R")
source("R/models/health/health_model_dp.R")
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")
source("R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R")
source("R/plotting/env_heatmaps/03_plot_env_heatmaps.R")

basic_policy <- compute_optimal_policy(
  k_cost = 1,
  c_cost = 0,
  d_high = 10,
  d_low = 0,
  la_prob = 0.1,
  ll_prob = 0.1,
  horizon = 3,
  states = c("Kd", "K", "CD", "C")
)
stopifnot(is.data.frame(basic_policy), nrow(basic_policy) > 0)

health_policy <- compute_optimal_policy_health(
  k_cost = 1,
  c_cost = 0,
  d_high = 10,
  d_low = 0,
  la_prob = 0.1,
  ll_prob = 0.1,
  horizon = 3,
  h0 = 10,
  health_step = 1,
  terminal_reward_mode = "linear"
)
stopifnot(is.data.frame(health_policy), nrow(health_policy) > 0)

stopifnot(file.exists("app/app.R"))

message("Smoke test complete: setup, basic model, health model, and app file are present.")
