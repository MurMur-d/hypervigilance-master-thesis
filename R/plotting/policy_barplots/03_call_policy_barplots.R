#!/usr/bin/env Rscript
# =============================================================================
# FILE: R/plotting/policy_barplots/03_call_policy_barplots.R
# PURPOSE:
#   Entry point for the new “policy barplots” pipeline. Source this file after
#   running `R/core/setup_project.R` and call run_policy_barplots_pipeline()
#   to rebuild the dataset and save the three policy bar plots plus the CSV.
# =============================================================================

# REPOSITORY REFERENCES (per request):
#   - Model execution:               `R/core/plot_utils.R` → `mem_compute_policy()` and `mem_simulate_agents()` provide cached DP + simulation engines.
#   - Policy extraction:             same file’s `get_model()` dispatches to the correct policy solver for basic/health variants.
#   - Environment grid definitions:  `R/plotting/_shared/utils_health_env_scenarios.R` → `default_health_env_scenarios()` lists the LA/LL mappings and env tags.
#   - Risk/predictability binning:   the `ssp_level` and `predictability` columns in that table encode low/medium/high risk + predictable/unpredictable.
#   - Cost binning:                  `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R` shows how `hypervigilance_grid_by_Kratio_by_model()` sweeps discrete K values to explore action cost regimes.

FIGURE_SCRIPT <- "R/plotting/policy_barplots/03_call_policy_barplots.R"

if (!exists("HV_SETUP_DONE", envir = .GlobalEnv)) {
  source("R/core/setup_project.R")
}
assign("HV_SETUP_DONE", TRUE, envir = .GlobalEnv)
log_figure_start(FIGURE_SCRIPT)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(readr)
  library(tibble)
  library(purrr)
})

source("R/core/hv_rate_helpers.R", encoding = "UTF-8")
source("R/core/hypervigilance/00_data_prep_hypervigilance_data.R", encoding = "UTF-8")
source("R/helpers/utils_model_scenarios.R", encoding = "UTF-8")

source_utf8 <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (length(lines) == 0) return(invisible(NULL))
  lines[[1]] <- sub("^\ufeff", "", lines[[1]])
  eval(parse(text = lines), envir = .GlobalEnv)
}

source_utf8("R/plotting/_shared/utils_health_env_scenarios.R")
source("R/models/basic/basic_model_dp.R", encoding = "UTF-8")
source("R/models/basic/basic_model_SIM.R", encoding = "UTF-8")
source("R/models/health/health_model_dp.R", encoding = "UTF-8")
source("R/models/health/health_model_SIM.R", encoding = "UTF-8")

# Default scenario assumption: use the health model with the linear terminal reward
# until another override is supplied.
DEFAULT_MODEL_SCENARIO_LABEL <- "terminal reward (ω = 1, linear)"

sanitize_label_for_path <- function(label) {
  normalized <- tolower(label)
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("_+", "_", normalized)
  normalized <- sub("^_+", "", normalized)
  normalized <- sub("_+$", "", normalized)
  if (nzchar(normalized)) normalized else "model_variant"
}

resolve_model_scenario <- function(model_label, model_family) {
  scenarios <- default_env_model_scenarios
  if (!is.null(model_label)) {
    matches <- Filter(function(scn) identical(scn$label, model_label), scenarios)
    if (length(matches) > 0) return(matches[[1]])
    stop(paste0("Cannot find a model scenario named '", model_label, "'."))
  }
  matches <- Filter(function(scn) identical(scn$model, model_family), scenarios)
  if (length(matches) > 0) return(matches[[1]])
  stop(paste0("No model scenario for family '", model_family, "' is defined."))
}

build_policy_barplot_dataset <- function(
  env_scenarios = default_health_env_scenarios(),
  cost_settings = c("low cost" = 3, "high cost" = 7),
  model_family = "health",
  model_scenario_label = DEFAULT_MODEL_SCENARIO_LABEL,
  base_policy_args = list(h0 = validate_default("h0")),
  C = 0,
  D = 10,
  d = 0,
  T_steps = 10,
  N_agents = 1000,
  states = c("K", "Kd", "C", "CD"),
  cache_path = NULL,
  force_refresh = FALSE
) {
  scenario <- resolve_model_scenario(model_scenario_label, model_family)
  policy_args <- merge_model_args(base_policy_args, scenario$policy_args)
  sim_args <- merge_model_args(list(), scenario$sim_args)

  if (!force_refresh && !is.null(cache_path) && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  if (is.null(cost_settings) || length(cost_settings) != 2) {
    stop("cost_settings must be a named numeric vector of length 2 (e.g. low/high).")
  }
  cost_names <- names(cost_settings)
  if (is.null(cost_names) || any(!nzchar(cost_names))) {
    stop("cost_settings entries must be named (e.g. c('low cost' = 3, 'high cost' = 7)).")
  }

  env_df <- env_scenarios %>%
    transmute(
      env_label,
      env_full,
      ssp_level = factor(ssp_level, levels = c("low", "medium", "high")),
      predictability = factor(predictability, levels = c("predictable", "unpredictable")),
      LA,
      LL
    )

  cost_df <- tibble(
    cost_bin = factor(cost_names, levels = cost_names),
    K_value = as.integer(cost_settings)
  )

  combos <- tidyr::crossing(env_df, cost_df)

  compute_one_row <- function(env_label, env_full, ssp_level, predictability, LA, LL, cost_bin, K_value) {
    pol <- mem_compute_policy(
      model = scenario$model,
      K = K_value, C = C, D = D, d = d,
      LA = LA, LL = LL,
      T_steps = T_steps,
      states = states,
      policy_args = policy_args
    )

    sim <- mem_simulate_agents(
      model = scenario$model,
      policy_df = pol,
      LA = LA, LL = LL,
      T_steps = T_steps,
      N_agents = N_agents,
      K = K_value, C = C, D = D, d = d,
      sim_args = sim_args
    )

    hv_rates <- compute_hv_rates_from_agent_data(sim$agent_data, T_steps = T_steps, N_agents = N_agents)
    hv_vecs <- extract_hypervigilance_vectors(sim$agent_data)

    safe_rows <- sum(hv_vecs$str == 0L, na.rm = TRUE)
    stressor_rows <- sum(hv_vecs$str == 1L, na.rm = TRUE)
    total_rows <- safe_rows + stressor_rows
    if (total_rows <= 0) total_rows <- safe_rows + stressor_rows + 1

    reactive_rate <- 0
    if (stressor_rows > 0) {
      reactive_vals <- hv_vecs$hv[hv_vecs$str == 1L]
      reactive_rate <- mean(reactive_vals, na.rm = TRUE)
    }
    reactive_rate <- coalesce(reactive_rate, 0)

    anticipatory_rate <- hv_rates$HypervigilanceRate_filtered
    overall_vigilance <- hv_rates$HypervigilanceRate_all

    safe_fraction <- safe_rows / total_rows
    stressor_fraction <- stressor_rows / total_rows

    anticipatory_contribution <- safe_fraction * anticipatory_rate
    reactive_contribution <- stressor_fraction * reactive_rate

    tibble(
      model_label = scenario$label,
      model_family = scenario$model,
      scenario_id = sanitize_label_for_path(scenario$label),
      env_label = env_label,
      env_full = env_full,
      ssp_level = ssp_level,
      predictability = predictability,
      LA = LA,
      LL = LL,
      cost_bin = cost_bin,
      K_value = K_value,
      anticipatory_rate = anticipatory_rate,
      reactive_rate = reactive_rate,
      overall_vigilance = overall_vigilance,
      safe_steps = safe_rows,
      stressor_steps = stressor_rows,
      total_steps = total_rows,
      safe_fraction = safe_fraction,
      stressor_fraction = stressor_fraction,
      anticipatory_contribution = anticipatory_contribution,
      reactive_contribution = reactive_contribution
    )
  }

  env_label_levels <- env_df$env_label

  dataset <- purrr::pmap_dfr(
    combos,
    compute_one_row
  ) %>%
    mutate(
      env_label = factor(env_label, levels = env_label_levels),
      cost_bin = factor(cost_bin, levels = levels(cost_df$cost_bin))
    ) %>%
    arrange(cost_bin, predictability, ssp_level)

  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    saveRDS(dataset, cache_path)
  }

  dataset
}

plot_hypervigilance_composition <- function(dataset) {
  component_palette <- c(anticipatory = "#e02b35", reactive = "#1f78b4")
  long_df <- dataset %>%
    mutate(env_label = factor(env_label, levels = unique(env_label))) %>%
    select(env_label, predictability, cost_bin, anticipatory_contribution, reactive_contribution) %>%
    tidyr::pivot_longer(
      cols = c(anticipatory_contribution, reactive_contribution),
      names_to = "component",
      values_to = "value"
    ) %>%
    mutate(
      component = recode(component,
                         anticipatory_contribution = "anticipatory",
                         reactive_contribution = "reactive"),
      component = factor(component, levels = c("reactive", "anticipatory"))
    )

  ggplot(long_df, aes(x = env_label, y = value, fill = component)) +
    geom_col(position = "stack", width = 0.75) +
    facet_grid(predictability ~ cost_bin, scales = "free_x", space = "free") +
    scale_fill_manual(values = component_palette, labels = c(anticipatory = "Anticipatory", reactive = "Reactive"), name = "Vigilance") +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "Composition of hypervigilance across canonical environments",
      subtitle = "Reactive vigilance carries most of the load; anticipatory contributions only show up in moderate/high risk unpredictable settings when cost is low (single deterministic policy evaluation).",
      x = "Environment (risk + predictability)",
      y = "Share of timesteps vigilant"
    ) +
    theme_vigilance(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.placement = "outside",
      panel.spacing = grid::unit(0.35, "lines")
    )
}

plot_anticipatory_presence <- function(dataset) {
  predictability_palette <- c(predictable = "#1f78b4", unpredictable = "#e31a1c")

  ggplot(dataset, aes(x = ssp_level, y = anticipatory_rate, fill = predictability)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray40") +
    facet_wrap(~ cost_bin, nrow = 1) +
    scale_fill_manual(name = "Predictability", values = predictability_palette) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(
      title = "Anticipatory vigilance presence",
      subtitle = "Appears only in medium/high risk + unpredictable environments when action cost is low (single deterministic policy evaluation).",
      x = "Risk level",
      y = "P(vigilant | safe)"
    ) +
    theme_vigilance(base_size = 12) +
    theme(panel.spacing = grid::unit(0.6, "lines"))
}

plot_reactive_shrinkage <- function(dataset) {
  predictability_palette <- c(predictable = "#1f78b4", unpredictable = "#e31a1c")

  ggplot(dataset, aes(x = ssp_level, y = reactive_rate, fill = predictability)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray40") +
    facet_wrap(~ cost_bin, nrow = 1) +
    scale_fill_manual(name = "Predictability", values = predictability_palette) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(
      title = "Reactive vigilance shrinkage with high cost",
      subtitle = "Reactive vigilance is omnipresent when cost is low but collapses in low/medium risk unpredictable worlds when cost is high while staying in predictable settings (single deterministic policy evaluation).",
      x = "Risk level",
      y = "P(vigilant | stressed)"
    ) +
    theme_vigilance(base_size = 12) +
    theme(panel.spacing = grid::unit(0.6, "lines"))
}

run_policy_barplots_pipeline <- function(
  model_family = "health",
  model_scenario_label = DEFAULT_MODEL_SCENARIO_LABEL,
  cost_settings = c("low cost" = 3, "high cost" = 7),
  env_scenarios = default_health_env_scenarios(),
  base_policy_args = list(h0 = validate_default("h0")),
  C = 0,
  D = 10,
  d = 0,
  T_steps = 10,
  N_agents = 1000,
  states = c("K", "Kd", "C", "CD"),
  force_refresh = FALSE,
  output_plot_dir = file.path("outputs", "plots"),
  output_data_dir = file.path("outputs", "data")
) {
  scenario <- resolve_model_scenario(model_scenario_label, model_family)
  sanitized_scenario <- sanitize_label_for_path(scenario$label)
  sanitized_cost <- paste(vapply(names(cost_settings), sanitize_label_for_path, FUN.VALUE = character(1)), collapse = "_")
  cache_root <- file.path(DIR_DATA_CACHE, "policy_barplots")
  cache_path <- file.path(cache_root, paste0("policy_barplots_", sanitized_scenario, "_", sanitized_cost, ".rds"))

  dataset <- build_policy_barplot_dataset(
    env_scenarios = env_scenarios,
    cost_settings = cost_settings,
    model_family = model_family,
    model_scenario_label = model_scenario_label,
    base_policy_args = base_policy_args,
    C = C,
    D = D,
    d = d,
    T_steps = T_steps,
    N_agents = N_agents,
    states = states,
    cache_path = cache_path,
    force_refresh = force_refresh
  )

  plots <- list(
    composition = plot_hypervigilance_composition(dataset),
    anticipatory = plot_anticipatory_presence(dataset),
    reactive = plot_reactive_shrinkage(dataset)
  )

  dir.create(output_plot_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_data_dir, showWarnings = FALSE, recursive = TRUE)

  save_graphs(plots$composition, file.path(output_plot_dir, "policy_bar_01_composition"), width = 14, height = 8, dpi = 300)
  save_graphs(plots$anticipatory, file.path(output_plot_dir, "policy_bar_02_anticipatory"), width = 12, height = 6, dpi = 300)
  save_graphs(plots$reactive, file.path(output_plot_dir, "policy_bar_03_reactive"), width = 12, height = 6, dpi = 300)

  save_table(dataset, file.path(output_data_dir, "policy_barplots_analysis"), format = "csv")

  invisible(list(dataset = dataset, plots = plots))
}
