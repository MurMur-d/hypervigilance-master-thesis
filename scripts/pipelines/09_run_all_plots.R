#!/usr/bin/env Rscript
# =============================================================================
# Script: scripts/pipelines/09_run_all_plots.R
# Purpose: Run every plot and table pipeline in a single, deterministic orchestration so the full figure suite can be generated with one command.
# =============================================================================

if (!exists("HV_SETUP_DONE", envir = .GlobalEnv)) {
  source("R/core/setup_project.R")
}
assign("HV_SETUP_DONE", TRUE, envir = .GlobalEnv)
log_figure_start("scripts/pipelines/09_run_all_plots.R")

dir.create(DIR_FIGURES, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_TABLES, showWarnings = FALSE, recursive = TRUE)

prep_scripts <- c(
  "R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R",
  "R/plotting/mechanism_env/00_data_prep_mechanism_data.R",
  "R/plotting/symmetry_risk/00_data_prep_symmetry_risk_autocorr.R",
  "R/plotting/health/00_data_prep_health_policy_mix.R",
  "R/plotting/compare_models/00_data_prep_compare_model_visuals.R"
)
for (script_path in prep_scripts) {
  message("Sourcing prep helper: ", script_path)
  source(script_path)
}

plot_scripts <- c(
  "R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R",
  "R/plotting/env_heatmaps/03_plot_env_heatmaps.R",
  "R/plotting/env_heatmaps/03_plot_env_hv_matrix.R",
  "R/plotting/mechanism_env/03_plot_mechanism_env.R",
  "R/plotting/mechanism_env/03_plot_mechanism_risk_autocorr.R",
  "R/plotting/compare_models/03_plot_compare_models.R",
  "R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R",
  "R/plotting/symmetry_risk/03_plot_symmetry_risk.R",
  "R/plotting/health/03_plot_health_policy_bars.R",
  "R/plotting/health/03_plot_health_simulation.R",
  "R/plotting/vigilance_flow/03_plot_vigilance_flow.R"
)
for (script_path in plot_scripts) {
  message("Sourcing plot module: ", script_path)
  source(script_path)
}

source("scripts/pipelines/09_compare_model_visuals.R")

write_plot <- function(name, plot_obj, model = NULL, suffix = NULL) {
  save_graphs(plot_obj, file.path(DIR_FIGURES, name), model = model, suffix = suffix)
}

write_table <- function(name, table_obj, fmt = "csv") {
  save_table(table_obj, file.path(DIR_TABLES, name), format = fmt)
}

health_policy <- list(
  h0 = validate_default("h0"),
  health_step = 1,
  terminal_reward_weight = 1,
  terminal_reward_mode = "linear"
)
health_sim <- list(
  h0 = validate_default("h0"),
  spread_initial_over_levels = FALSE,
  shuffle = FALSE
)

k_values <- c(1, 3, 5, 7, 9)
states <- c("K", "Kd", "C", "CD")
T_steps <- 10
N_agents <- 1000
C <- 0
d <- 0
deltaD <- 10

message(">>> Building environment heatmap grid")
env_grid <- build_env_heatmaps_for_models()

source("scripts/pipelines/09_env_heatmap_plots.R")
env_heatmap_outputs <- run_env_heatmap_grid_plots(
  env_grid = env_grid,
  states = states,
  T_steps = T_steps,
  N_agents = N_agents,
  C = C,
  D = deltaD,
  d = d,
  figure_dir = DIR_FIGURES
)
save_graphs(
  env_heatmap_outputs$env_heatmap_with_key,
  file.path(DIR_FIGURES, "env/env_heatmap_by_model_with_key_side"),
  width = 60, height = 21, dpi = 450, bg = "white", model = "basic"
)

basic_env_outputs <- run_basic_env_heatmap_plot(
  states = states,
  T_steps = T_steps,
  N_agents = N_agents,
  C = C,
  D = deltaD,
  d = d,
  k_values = k_values,
  figure_dir = DIR_FIGURES
)
save_graphs(
  basic_env_outputs$basic_env_with_key,
  file.path(DIR_FIGURES, "env/basic_hypervigilance_heatmap_by_K_with_key"),
  width = 30, height = 7.4, dpi = 450, bg = "white", model = "basic"
)

env_heatmap_plot <- plot_env_heatmaps(real_data = env_grid)

message(">>> Exporting environment HV matrix table")
hv_matrix <- hv_matrix_over_env()
write_table("env/env_hv_matrix", hv_matrix)

message(">>> Rendering mechanism panels")
mechanism_df <- prepare_mechanism_df(env_grid)
k_levels <- sort(unique(mechanism_df$K))
D_value <- if ("D" %in% names(mechanism_df)) unique(mechanism_df$D)[1] else deltaD
threshold_lines <- build_standard_threshold_lines(k_levels, D_value)

bivariate_mechanism <- plot_env_bivariate_hv_heatmap(mechanism_df, threshold_lines = threshold_lines)
print(bivariate_mechanism)
write_plot("mechanism/mechanism_bivariate", bivariate_mechanism)

split_mechanism <- plot_env_split_mechanism_heatmaps(mechanism_df, threshold_lines = threshold_lines)
print()
write_plot("mechanism/mechanism_split", split_mechanism)

stacked_mechanism <- plot_env_stacked_hv_bars(
  mechanism_df,
  value_mode = "absolute_rate"
)
print(stacked_mechanism)
write_plot("mechanism/mechanism_stacked", stacked_mechanism)

message(">>> Building symmetry risk/autocorrelation plots")
risk_grid <- risk_grid_K_vs_SSR(
  C = C, d = d, deltaD = deltaD,
  K_values = k_values,
  T_steps = T_steps, states = states, N_agents = N_agents
)
risk_plot <- plot_K_vs_SSR_heatmap(risk_grid)
print(risk_plot)
write_plot("symmetry/risk_k_ssr_heatmap", risk_plot)

risk_models <- risk_grid_K_vs_SSR_by_model(
  C = C, d = d, deltaD = deltaD,
  K_values = k_values,
  T_steps = T_steps, states = states, N_agents = N_agents
)

risk_mechanism <- plot_K_vs_SSR_mechanism_by_model(risk_models)
print(risk_mechanism)
write_plot("mechanism/ssr_mechanism_combo", risk_mechanism)

risk_split <- plot_K_vs_SSR_split_mechanism_by_model(risk_models)
print(risk_split)
write_plot("mechanism/ssr_mechanism_split", risk_split)


auto_grid <- symmetry_grid_K_vs_autocorr(
  C = C, d = d, deltaD = deltaD,
  K_values = k_values,
  T_steps = T_steps, states = states, N_agents = N_agents
)
auto_plot <- plot_K_vs_autocorr_heatmap(auto_grid)
write_plot("symmetry/autocorr_heatmap", auto_plot)

auto_models <- symmetry_grid_K_vs_autocorr_by_model(
  C = C, d = d, deltaD = deltaD,
  K_values = k_values,
  T_steps = T_steps, states = states, N_agents = N_agents
)

auto_mechanism <- plot_K_vs_autocorr_mechanism_by_model(auto_models)
print(auto_mechanism)
write_plot("mechanism/autocorr_mechanism_combo", auto_mechanism)

auto_split <- plot_K_vs_autocorr_split_mechanism_by_model(auto_models)
print(auto_split)
write_plot("mechanism/autocorr_mechanism_split", auto_split)


message(">>> Generating health policy summaries")
policy_grid <- plot_health_policy_action_bars_grid(
  K_values = k_values,
  policy_args = health_policy,
  sim_args = health_sim
)
print(policy_grid)
write_plot("health/health_policy_grid", policy_grid)

policy_by_model <- plot_health_policy_action_bars_by_model(
  base_policy_args = health_policy,
  sim_args = health_sim
)
print(policy_by_model)
write_plot("health/health_policy_by_model", policy_by_model)

message(">>> Running compare-model visual diagnostics")
run_compare_model_visuals(out_dir = DIR_FIGURES)

message(">>> Creating combo figure")
combo_plot <- env_heatmap_plot / risk_plot + patchwork::plot_layout(ncol = 1)
print(combo_plot)
write_plot("combo/env_and_risk", combo_plot)

message(">>> All plots generated and saved via save_graphs()/save_table()")








source("R/plotting/basic_policy/03_call_basic_dp_policy.R")
source("R/plotting/health/01_data_prep_policy_matrix.R")
source("R/plotting/00_apa_word_utils.R")

envs <- default_health_env_scenarios()
policy_matrix <- policy_matrix_over_env(
  env_scenarios = envs,
  K_values = 1:9,
  C = 0, D = 10, d = 0,
  T_steps = 10,
  states = c("K","Kd","C","CD"),
  model = "basic"
)
policy_plot <- plot_policy_matrix_over_env(policy_matrix)
policy_grob <- policy_plot$grob
policy_gg <- policy_plot$plot
print(policy_gg)

policy_dir <- file.path(DIR_FIGURES, "basic_model", "optimal_policy")
dir.create(policy_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = file.path(policy_dir, "basic_model_optimal_policy_by_environment.png"),
  plot = policy_grob,
  width = 11,
  height = 3.5,
  units = "in",
  dpi = 450,
  bg = "white"
)
ggsave(
  filename = file.path(policy_dir, "basic_model_optimal_policy_by_environment.pdf"),
  plot = policy_grob,
  width = 11,
  height = 3.5,
  units = "in",
  dpi = 450,
  device = "pdf",
  bg = "white"
)

save_word_doc <- identical(tolower(Sys.getenv("SAVE_POLICY_WORD_DOC", "")), "true")
if (save_word_doc) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Please install the 'officer' package to export the policy plot to Word.")
  }

  doc <- officer::read_docx()
  header_plot <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = "vigilance cost", size = 5) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = grid::unit(c(0, 0, 0, 0), "pt")
    )

  policy_gg_tight <- policy_gg +
    ggplot2::ggtitle(NULL) +
    ggplot2::labs(subtitle = NULL) +
    ggplot2::theme(
      plot.margin  = grid::unit(c(-4, 5, 5, 5), "pt"),
      strip.margin = ggplot2::margin(t = 0, b = 0),
      strip.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0, b = 0)),
      axis.text.x  = ggplot2::element_text(margin = ggplot2::margin(t = 2))
  ) +
  ggplot2::scale_x_discrete(
    labels = c("no stressor"="ns","stressor"="s","ns"="ns","s"="s")
  )

  policy_doc_plot <- patchwork::wrap_plots(
    header_plot,
    policy_gg_tight,
    ncol = 1,
    heights = c(0.025, 1)
  )

  doc <- apa_add_figure_block(
    doc = doc,
    plot = policy_doc_plot,
    fig_number = 1,
    title = "Basic model: optimal policy by environment",
    note = "C = 0 | D = 10 | d = 0 | T = 10 | model = basic. Matrix of vigilance policies across environments (ns = no stressor, s = stressor).",
    width = 6.5,
    height = 3.2,
    remove_plot_titles = TRUE
  )
  output_doc <- file.path(policy_dir, "basic_model_optimal_policy_by_environment.docx")
  dir.create(dirname(output_doc), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = output_doc)
  message("Word document saved to: ", output_doc)
}
