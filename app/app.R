# Hypervigilance Explorer (Thesis Version)
# Enhanced Shiny app for thesis submission with comprehensive documentation,
# parameter explanations, and additional simulation features.

library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyBS)  # For tooltips
library(bslib)    # For details/summary components
library(plotly)  # For interactive plots with hover

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

if (!dir.exists("R") && dir.exists("../R")) {
  setwd("..")
}

source("R/core/setup_project.R")
source("R/models/basic/basic_model_dp.R")
source("R/models/basic/basic_model_SIM.R")
source("R/models/health/health_model_dp.R")
source("R/models/health/health_model_SIM.R")
source("R/helpers/utils_model_scenarios.R")
source("R/plotting/_shared/utils_health_env_scenarios.R")
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")
source("R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R")
source("R/plotting/env_heatmaps/03_plot_env_heatmaps.R")
source("R/plotting/mechanism_env/03_plot_mechanism_env.R")
source("R/plotting/mechanism_env/03_plot_mechanism_risk_autocorr.R")
source("R/plotting/symmetry_risk/00_data_prep_symmetry_risk_autocorr.R")
source("R/plotting/symmetry_risk/03_plot_symmetry_risk.R")
source("R/plotting/symmetry_autocorr/03_plot_symmetry_autocorr.R")
source("R/plotting/health/01_data_prep_policy_matrix.R")
source("R/plotting/health/03_plot_health_policy_bars.R")
source("R/plotting/health/03_plot_health_simulation.R")  # For forward simulation plots
source("R/plotting/basic_policy/03_call_basic_dp_policy.R")
source("R/plotting/policy_grid_bars/01_plot_policy_health_slices_dp_only.R")

if (requireNamespace("future", quietly = TRUE)) {
  future::plan(future::sequential)
}

# ---- Constants and Defaults ----
.default_states <- c("K", "Kd", "C", "CD")
.default_k_values <- c(1, 3, 5, 7, 9)
.default_threshold_tau <- round(0.6 * validate_default("h0"))

# ---- Utility Functions ----
.parse_numeric_list <- function(x, default = .default_k_values) {
  if (is.null(x) || !nzchar(x)) return(default)
  vals <- suppressWarnings(as.numeric(unlist(strsplit(x, "[ ,;]+"))))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(default)
  unique(sort(vals))
}

.pick_first <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (!is.null(val) && length(val) > 0 && is.finite(val[1])) return(val[1])
  }
  NULL
}

.notice_plot <- function(message, title = "Not available for this configuration") {
  ggplot() +
    annotate("text", x = 0.5, y = 0.62, label = title, fontface = "bold", size = 5) +
    annotate("text", x = 0.5, y = 0.42, label = message, size = 4.2) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void()
}

# ---- Parameter Explanations ----
.parameter_explanations <- list(
  model = "Choose between the basic vigilance model or the extended health model that includes health state tracking.",
  k_values = "Detection cost values (K). Higher K means more costly vigilance. Range: 1-9. Example: '1, 3, 5, 7, 9'",
  focus_k = "Single K value for detailed health state analysis. Example: 5",
  C = "Baseline cost of vigilance (when no threat is present). Example: 0 (no cost)",
  D = "Damage if relaxed during a threat/stressor. Example: 10 (high damage)",
  d = "Damage if vigilant during a threat/stressor. Example: 0 (no additional damage)",
  T_steps = "Time horizon (number of steps to simulate). Example: 10",
  N_agents = "Number of agents to simulate per parameter combination. Example: 1000",
  grid_step = "Resolution of the environment grid (LA/LL probabilities). Example: 0.05",
  h0 = "Initial health level for health model. Example: 35",
  health_step = "Health change per time step. Example: 1",
  terminal_reward_mode = "Type of terminal reward: linear (proportional to health), power (exponential), or threshold (binary).",
  terminal_reward_weight = "Strength of terminal reward. Example: 1.0",
  terminal_power_alpha = "Exponent for power terminal reward. Example: 3.0",
  terminal_threshold_tau = "Threshold for threshold terminal reward. Example: 21",
  spread_initial_over_levels = "Distribute initial health across multiple levels instead of all starting at H0.",
  shuffle = "Randomize agent processing order to avoid ordering artifacts."
)

# ---- Hypervigilance Rules and Information ----
.hypervigilance_info <- HTML("
<div style='background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #007bff;'>
  <h4 style='margin-top: 0; color: #007bff;'>Hypervigilance Model Rules</h4>
  <p><strong>Hypervigilance</strong> occurs when an agent remains vigilant even when no threat is present, leading to unnecessary costs.</p>
  <ul>
    <li><strong>States:</strong> K (vigilant), Kd (vigilant+stressor), C (relaxed), CD (relaxed+stressor)</li>
    <li><strong>Actions:</strong> High (vigilant) or Low (relaxed)</li>
    <li><strong>Costs:</strong> C (vigilant baseline), D (relaxed during stressor), d (vigilant during stressor)</li>
    <li><strong>Constraint:</strong> K + d ≤ C + D (vigilance must be optimal when threats are certain)</li>
    <li><strong>Environment:</strong> LA (threat arrival probability), LL (threat likelihood)</li>
  </ul>
  <p><em>The model computes optimal policies using dynamic programming and simulates agent behavior to measure hypervigilance rates.</em></p>
</div>
")

# ---- App Plot Theme ----
.app_plot_theme <- function(base_size = 16) {
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 22, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 6)),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, margin = ggplot2::margin(b = 6)),
    axis.title = ggplot2::element_text(size = base_size, face = "plain"),
    axis.text = ggplot2::element_text(size = base_size - 2),
    strip.text = ggplot2::element_text(size = base_size, face = "plain"),
    legend.title = ggplot2::element_text(size = base_size - 2, face = "plain"),
    legend.text = ggplot2::element_text(size = base_size - 3),
    plot.margin = ggplot2::margin(t = 8, r = 8, b = 8, l = 8)
  )
}

.style_app_plot <- function(plot_obj, base_size = 16) {
  if (inherits(plot_obj, "ggplot") || inherits(plot_obj, "patchwork")) {
    plot_obj + .app_plot_theme(base_size)
  } else {
    plot_obj
  }
}

# ---- Parameter Validation ----
.render_parameter_summary <- function(input, k_values, focus_K) {
  health_bits <- if (identical(input$model, "health")) {
    paste0(
      "<br><b>Health settings:</b> H0 = ", input$h0,
      "; health step = ", input$health_step,
      "; terminal reward = ", input$terminal_reward_mode,
      " (weight ", input$terminal_reward_weight, ")",
      "; power α = ", input$terminal_power_alpha,
      "; threshold τ = ", input$terminal_threshold_tau
    )
  } else {
    ""
  }

  HTML(paste0(
    "<b>Current configuration</b><br>",
    "Model: ", input$model, "<br>",
    "K values: ", paste(k_values, collapse = ", "), "; focus K: ", focus_K, "<br>",
    "C: ", input$C, "; D: ", input$D, "; d: ", input$d, "; T: ", input$T_steps, "<br>",
    "Agents: ", input$N_agents, "; LA/LL grid step: ", input$grid_step,
    health_bits
  ))
}

.render_parameter_warnings <- function(input, k_values) {
  messages <- character()
  if (any(k_values <= input$C)) {
    messages <- c(messages, paste0("K should usually be greater than C. Problem K values: ", paste(k_values[k_values <= input$C], collapse = ", ")))
  }
  if (input$D <= input$d) {
    messages <- c(messages, "D should usually be greater than d so relaxed-during-stressor damage exceeds vigilant-during-stressor damage.")
  }
  invalid_K <- k_values[k_values + input$d > input$C + input$D]
  if (length(invalid_K) > 0) {
    messages <- c(messages, paste0("Invalid constraint K + d ≤ C + D fails for K values: ", paste(invalid_K, collapse = ", ")))
  }
  if (!is.finite(input$grid_step) || input$grid_step <= 0 || input$grid_step > 0.5) {
    messages <- c(messages, "LA/LL grid step must be between 0 and 0.5.")
  }
  if (identical(input$model, "health") && input$terminal_reward_mode == "threshold" && input$terminal_threshold_tau > input$h0) {
    messages <- c(messages, "Threshold τ is above H0; threshold terminal reward may never be reached from the initial health level.")
  }

  if (length(messages) == 0) return(HTML(""))

  HTML(paste0(
    "<div style='color:#8a4b00; background:#fff8e6; border:1px solid #f0d28a; padding:8px 10px; border-radius:4px; margin-bottom:10px;'>",
    "<b>Parameter warnings</b><br>",
    paste(messages, collapse = "<br>"),
    "</div>"
  ))
}

# ---- Model Functions ----
.selected_policy_args <- function(input, model) {
  if (!identical(model, "health")) return(list())
  list(
    h0 = input$h0,
    health_step = input$health_step,
    terminal_reward_weight = input$terminal_reward_weight,
    terminal_reward_mode = input$terminal_reward_mode,
    terminal_power_alpha = input$terminal_power_alpha,
    terminal_threshold_tau = input$terminal_threshold_tau
  )
}

.selected_sim_args <- function(input, model) {
  if (!identical(model, "health")) return(list())
  list(
    h0 = input$h0,
    spread_initial_over_levels = isTRUE(input$spread_initial_over_levels),
    shuffle = isTRUE(input$shuffle)
  )
}

.base_policy_args <- function(input) {
  list(
    h0 = .pick_first(input$h0, validate_default("h0")),
    health_step = .pick_first(input$health_step, 1)
  )
}

.base_sim_args <- function(input) {
  list(
    h0 = .pick_first(input$h0, validate_default("h0")),
    spread_initial_over_levels = isTRUE(input$spread_initial_over_levels),
    shuffle = isTRUE(input$shuffle)
  )
}

.build_env_grid_by_model <- function(
    model_scenarios,
    K_values,
    C, D, d, T_steps, states, N_agents, grid_step,
    base_policy_args = list(),
    base_sim_args = list()
) {
  rows <- lapply(seq_along(model_scenarios), function(i) {
    scenario <- model_scenarios[[i]]
    pol_args <- merge_model_args(base_policy_args, scenario$policy_args)
    sim_args <- merge_model_args(base_sim_args, scenario$sim_args)

    bind_rows(lapply(K_values, function(Ki) {
      df <- hypervigilance_grid(
        K = Ki,
        C = C,
        D = D,
        d = d,
        T_steps = T_steps,
        states = states,
        N_agents = N_agents,
        grid_step = grid_step,
        model = scenario$model,
        policy_args = if (identical(scenario$model, "health")) pol_args else list(),
        sim_args = if (identical(scenario$model, "health")) sim_args else list()
      )
      if (nrow(df) == 0) return(NULL)
      df$model_label <- scenario$label
      df$model <- scenario$label
      df$hv <- df$HypervigilanceRate_filtered
      df
    }))
  })

  bind_rows(rows)
}

.build_policy_tile_df <- function(
    model,
    K,
    C, D, d, T_steps, states, grid_step,
    policy_args = list()
) {
  env_grid <- expand.grid(
    LA = seq(0, 0.5, by = grid_step),
    LL = seq(0, 0.5, by = grid_step),
    KEEP.OUT.ATTRS = FALSE
  )

  bind_rows(lapply(seq_len(nrow(env_grid)), function(i) {
    cell <- env_grid[i, ]
    pol <- mem_compute_policy(
      model = model,
      K = K,
      C = C,
      D = D,
      d = d,
      LA = cell$LA,
      LL = cell$LL,
      T_steps = T_steps,
      states = states,
      policy_args = policy_args
    )
    pol$LA <- cell$LA
    pol$LL <- cell$LL
    if (!"health" %in% names(pol)) pol$health <- 1L
    pol
  }))
}

# ---- UI Definition ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Hypervigilance Model Explorer"),

  sidebarLayout(
    sidebarPanel(
      div(
        p("Configure model parameters below. Hover over inputs for detailed explanations.", style = "margin-bottom: 18px; font-weight: bold;"),
        style = "margin-bottom: 18px;"
      ),

      # Model Selection
      div(
        radioButtons("model", "Model Type:",
                    choices = c("Basic Model" = "basic", "Health Model" = "health"),
                    selected = "basic"),
        bsTooltip("model", .parameter_explanations$model, placement = "right", trigger = "hover")
      ),

      # K Values
      div(
        textInput("k_values", "Detection Costs (K):", value = paste(.default_k_values, collapse = ", ")),
        bsTooltip("k_values", .parameter_explanations$k_values, placement = "right", trigger = "hover")
      ),

      div(
        selectInput("focus_k", "Focus K for Details:", choices = as.character(.default_k_values), selected = "5"),
        bsTooltip("focus_k", .parameter_explanations$focus_k, placement = "right", trigger = "hover")
      ),

      # Cost Parameters
      fluidRow(
        column(6,
          div(
            numericInput("C", "C (Vigilance Cost):", value = 0, min = 0, step = 1),
            bsTooltip("C", .parameter_explanations$C, placement = "bottom", trigger = "hover")
          )
        ),
        column(6,
          div(
            numericInput("D", "D (Relaxed Damage):", value = 10, min = 0, step = 1),
            bsTooltip("D", .parameter_explanations$D, placement = "bottom", trigger = "hover")
          )
        )
      ),

      fluidRow(
        column(6,
          div(
            numericInput("d", "d (Vigilant Damage):", value = 0, min = 0, step = 1),
            bsTooltip("d", .parameter_explanations$d, placement = "bottom", trigger = "hover")
          )
        ),
        column(6,
          div(
            numericInput("T_steps", "Time Steps (T):", value = 10, min = 1, step = 1),
            bsTooltip("T_steps", .parameter_explanations$T_steps, placement = "bottom", trigger = "hover")
          )
        )
      ),

      # Simulation Parameters
      numericInput("N_agents", "Number of Agents:", value = 1000, min = 10, step = 10),
      bsTooltip("N_agents", .parameter_explanations$N_agents, placement = "right", trigger = "hover"),

      numericInput("grid_step", "Environment Grid Step:", value = 0.05, min = 0.01, max = 0.5, step = 0.01),
      bsTooltip("grid_step", .parameter_explanations$grid_step, placement = "right", trigger = "hover"),

      # Health Model Parameters
      conditionalPanel(
        condition = "input.model == 'health'",
        hr(),
        h4("Health Model Parameters"),

        numericInput("h0", "Initial Health (H₀):", value = 35, min = 1, step = 1),
        bsTooltip("h0", .parameter_explanations$h0, placement = "right", trigger = "hover"),

        numericInput("health_step", "Health Step:", value = 1, min = 1, step = 1),
        bsTooltip("health_step", .parameter_explanations$health_step, placement = "right", trigger = "hover"),

        selectInput("terminal_reward_mode", "Terminal Reward Mode:",
                   choices = c("linear", "power", "threshold"), selected = "linear"),
        bsTooltip("terminal_reward_mode", .parameter_explanations$terminal_reward_mode, placement = "right", trigger = "hover"),

        numericInput("terminal_reward_weight", "Terminal Reward Weight:", value = 1, step = 0.1),
        bsTooltip("terminal_reward_weight", .parameter_explanations$terminal_reward_weight, placement = "right", trigger = "hover"),

        conditionalPanel(
          condition = "input.terminal_reward_mode == 'power'",
          numericInput("terminal_power_alpha", "Power Alpha (α):", value = 3, step = 0.5),
          bsTooltip("terminal_power_alpha", .parameter_explanations$terminal_power_alpha, placement = "right", trigger = "hover")
        ),

        conditionalPanel(
          condition = "input.terminal_reward_mode == 'threshold'",
          numericInput("terminal_threshold_tau", "Threshold Tau (τ):", value = .default_threshold_tau, step = 1),
          bsTooltip("terminal_threshold_tau", .parameter_explanations$terminal_threshold_tau, placement = "right", trigger = "hover")
        ),

        checkboxInput("spread_initial_over_levels", "Spread Initial Health Levels", value = FALSE),
        bsTooltip("spread_initial_over_levels", .parameter_explanations$spread_initial_over_levels, placement = "right", trigger = "hover"),

        checkboxInput("shuffle", "Shuffle Agent Order", value = TRUE),
        bsTooltip("shuffle", .parameter_explanations$shuffle, placement = "right", trigger = "hover")
      ),

      br(),
      actionButton("run", "Run / Refresh All Figures", class = "btn-primary"),
      actionButton("reset", "Reset to Defaults", class = "btn-secondary", style = "margin-left: 10px;"),
      p("(Ctrl+R to run)", style = "color: #999; margin-left: 5px; display: inline-block;"),

      br(), br(),
      downloadButton("download_current_plot", "Download Current Plot (PDF)"),
      p("Runs after a plot has been generated.", style = "color: #777; font-size: 0.9em; margin-top: 6px;"),

      htmlOutput("parameter_warnings"),
      htmlOutput("summary_text"),

      br(),
      tags$details(
        tags$summary("Cache & Performance Info", style = "cursor: pointer; font-weight: bold;"),
        div(
          style = "margin-top: 10px; padding: 10px; background: #f5f5f5; border-radius: 4px; font-size: 0.85em;",
          verbatimTextOutput("cache_status")
        )
      )
    ),

    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Introduction",
          value = "Introduction",
          div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; margin-bottom: 20px;",
            h2("Welcome to the Hypervigilance Model Explorer", style = "text-align: center; margin-bottom: 20px;"),
            p("This interactive application allows you to explore mathematical models of hypervigilance - the phenomenon where individuals remain vigilant even when no threat is present, incurring unnecessary costs.", style = "font-size: 18px; text-align: center;")
          ),

          fluidRow(
            column(6,
              div(style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px;",
                h3("🎯 What is Hypervigilance?"),
                p("Hypervigilance occurs when an agent (representing an individual or system) maintains a vigilant state even in the absence of threats. This leads to wasted resources and potential health costs."),
                h4("Key Concepts:"),
                tags$ul(
                  tags$li(strong("States:"), " K (vigilant), Kd (vigilant+threat), C (relaxed), CD (relaxed+threat)"),
                  tags$li(strong("Actions:"), " High (vigilant) or Low (relaxed)"),
                  tags$li(strong("Environment:"), " LA (threat arrival probability), LL (threat likelihood)"),
                  tags$li(strong("Costs:"), " C (vigilance baseline), D (relaxed during threat), d (vigilant during threat)")
                )
              )
            ),
            column(6,
              div(style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px;",
                h3("🧮 How the App Works"),
                p("The app uses dynamic programming to compute optimal vigilance policies and agent-based simulations to measure hypervigilance rates across different environmental conditions."),
                h4("Navigation:"),
                tags$ul(
                  tags$li(strong("Manuscript:"), " Core figures from the research paper"),
                  tags$li(strong("Forward Simulation:"), " Detailed simulation trajectories and diagnostics"),
                  tags$li(strong("Appendix:"), " Additional analyses and comparisons")
                ),
                p("Use the sidebar controls to adjust model parameters. Hover over inputs for explanations, and hover over plots for detailed information.")
              )
            )
          ),

          .hypervigilance_info,

          div(style = "text-align: center; margin-top: 30px;",
            actionButton("start_exploring", "Start Exploring →", class = "btn-primary btn-lg",
                         style = "padding: 15px 30px; font-size: 18px;")
          )
        ),

        tabPanel(
          "Manuscript",
          tabsetPanel(
            id = "manuscript_tabs",
            tabPanel("Policy",
              p("Optimal vigilance policy across different model configurations. Hover over the plot for detailed information.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("policy_plot", height = "680px"), type = 8)
            ),
            tabPanel("Environment",
              p("Hypervigilance rates as a function of threat arrival (LA) and threat likelihood (LL).", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("hv_plot", height = "820px"), type = 8)
            ),
            tabPanel("Mechanism Map",
              p("Bivariate analysis of hypervigilance mechanisms.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("mech_bivariate_plot", height = "920px"), type = 8)
            ),
            tabPanel("Mechanism Split",
              p("Detailed mechanism decomposition across vigilance costs.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("mech_split_plot", height = "1200px"), type = 8)
            ),
            tabPanel("Mechanism Bars",
              p("Stacked bars showing hypervigilance composition by environment.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("mech_bars_plot", height = "900px"), type = 8)
            ),
            tabPanel("Health Slices",
              p("Policy surface across time, health, and environment dimensions.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("health_slice_plot", height = "920px"), type = 8)
            ),
            tabPanel("Health Bars",
              p("Health-state policy summaries by environment and cost.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("health_policy_grid_plot", height = "920px"), type = 8)
            ),
            tabPanel("Model Comparison",
              p("Cross-model comparison of hypervigilance patterns.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("env_compare_plot", height = "980px"), type = 8)
            )
          )
        ),

        tabPanel(
          "Forward Simulation",
          tabsetPanel(
            tabPanel("Agent Trajectories",
              p("Individual agent simulation trajectories showing health and vigilance over time.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              fluidRow(
                column(4, numericInput("sim_LA", "LA (Threat Arrival):", value = 0.2, min = 0, max = 0.5, step = 0.05)),
                column(4, numericInput("sim_LL", "LL (Threat Likelihood):", value = 0.3, min = 0, max = 0.5, step = 0.05)),
                column(4, numericInput("n_agents_display", "Agents to Display:", value = 10, min = 1, max = 50))
              ),
              actionButton("run_simulation", "Run Forward Simulation", class = "btn-success"),
              br(), br(),
              withSpinner(plotlyOutput("agent_trajectories_plot", height = "600px"), type = 8)
            ),
            tabPanel("Population Summary",
              p("Population-level statistics from forward simulation.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("population_summary_plot", height = "600px"), type = 8)
            ),
            tabPanel("Health Distribution",
              p("Evolution of health distribution over time.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("health_distribution_plot", height = "600px"), type = 8)
            )
          )
        ),

        tabPanel(
          "Appendix",
          tabsetPanel(
            tabPanel("Risk",
              p("Risk analysis across vigilance costs and environment.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("risk_plot", height = "760px"), type = 8)
            ),
            tabPanel("Risk by Model",
              p("Cross-model risk comparison.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("risk_models_plot", height = "980px"), type = 8)
            ),
            tabPanel("Risk Mechanisms",
              p("Mechanistic decomposition of risk patterns.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("risk_mechanism_plot", height = "920px"), type = 8)
            ),
            tabPanel("Autocorrelation",
              p("Environment autocorrelation analysis.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("autocorr_plot", height = "760px"), type = 8)
            ),
            tabPanel("Autocorr by Model",
              p("Cross-model autocorrelation comparison.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("autocorr_models_plot", height = "980px"), type = 8)
            ),
            tabPanel("Autocorr Mechanisms",
              p("Mechanistic autocorrelation analysis.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("autocorr_mechanism_plot", height = "920px"), type = 8)
            ),
            tabPanel("Policy by Model",
              p("Policy comparison across models.", style = "font-style: italic; color: #555; margin-bottom: 15px;"),
              withSpinner(plotlyOutput("health_policy_compare_plot", height = "980px"), type = 8)
            )
          )
        )
      )
    )
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {

  # Navigation from Introduction
  observeEvent(input$start_exploring, {
    updateTabsetPanel(session, "main_tabs", selected = "Manuscript")
  })

  # Update focus_k choices when k_values changes
  observe({
    k_values <- .parse_numeric_list(input$k_values)
    selected <- as.character(input$focus_k)
    if (!selected %in% as.character(k_values)) {
      selected <- as.character(k_values[ceiling(length(k_values) / 2)])
    }
    updateSelectInput(session, "focus_k", choices = as.character(k_values), selected = selected)
  })

  # Reset to defaults
  observeEvent(input$reset, {
    updateRadioButtons(session, "model", selected = "basic")
    updateTextInput(session, "k_values", value = paste(.default_k_values, collapse = ", "))
    updateSelectInput(session, "focus_k", selected = "5")
    updateNumericInput(session, "C", value = 0)
    updateNumericInput(session, "D", value = 10)
    updateNumericInput(session, "d", value = 0)
    updateNumericInput(session, "T_steps", value = 10)
    updateNumericInput(session, "N_agents", value = 1000)
    updateNumericInput(session, "grid_step", value = 0.05)
    updateNumericInput(session, "h0", value = 35)
    updateNumericInput(session, "health_step", value = 1)
    updateSelectInput(session, "terminal_reward_mode", selected = "linear")
    updateNumericInput(session, "terminal_reward_weight", value = 1)
    updateNumericInput(session, "terminal_power_alpha", value = 3)
    updateNumericInput(session, "terminal_threshold_tau", value = .default_threshold_tau)
    updateCheckboxInput(session, "spread_initial_over_levels", value = FALSE)
    updateCheckboxInput(session, "shuffle", value = TRUE)
  })

  # Reactive parameter validation
  output$parameter_warnings <- renderUI({
    k_values <- .parse_numeric_list(input$k_values)
    .render_parameter_warnings(input, k_values)
  })

  output$summary_text <- renderUI({
    k_values <- .parse_numeric_list(input$k_values)
    focus_K <- suppressWarnings(as.numeric(input$focus_k))
    if (!is.finite(focus_K) || !focus_K %in% k_values) {
      focus_K <- k_values[ceiling(length(k_values) / 2)]
    }
    .render_parameter_summary(input, k_values, focus_K)
  })

  # Cache status
  output$cache_status <- renderText({
    paste("Cache status monitoring would be implemented here.")
  })

  # Main results computation
  results <- eventReactive(input$run, {
    k_values <- .parse_numeric_list(input$k_values)
    focus_K <- suppressWarnings(as.numeric(input$focus_k))
    if (!is.finite(focus_K) || !focus_K %in% k_values) {
      focus_K <- k_values[ceiling(length(k_values) / 2)]
    }

    C <- input$C
    D <- input$D
    d <- input$d
    T_steps <- input$T_steps
    N_agents <- input$N_agents
    grid_step <- input$grid_step

    if (any(k_values + d > (C + D))) {
      stop("Constraint violated: require K + d <= C + D for all K values.")
    }

    model <- input$model
    policy_args <- .selected_policy_args(input, model)
    sim_args <- .selected_sim_args(input, model)
    base_policy_args <- .base_policy_args(input)
    base_sim_args <- .base_sim_args(input)
    env_scenarios <- default_health_env_scenarios()

    withProgress(message = "Computing manuscript figures...", value = 0, {
      incProgress(0.08, detail = "Optimal policy matrix")
      policy_df <- policy_matrix_over_env(
        env_scenarios = env_scenarios,
        K_values = k_values,
        C = C,
        D = D,
        d = d,
        T_steps = T_steps,
        states = .default_states,
        model = model,
        policy_args = policy_args
      )
      policy_plot <- plot_policy_matrix_over_env(policy_df)$plot

      incProgress(0.18, detail = "Single-model environment grid")
      env_grid <- bind_rows(lapply(k_values, function(Ki) {
        hypervigilance_grid(
          K = Ki,
          C = C,
          D = D,
          d = d,
          T_steps = T_steps,
          states = .default_states,
          N_agents = N_agents,
          grid_step = grid_step,
          model = model,
          policy_args = policy_args,
          sim_args = sim_args
        )
      }))
      env_grid$hv <- env_grid$HypervigilanceRate_filtered
      env_grid$model <- if (identical(model, "basic")) "Basic" else "Health"
      env_grid$model_label <- env_grid$model

      threshold_lines <- build_standard_threshold_lines(sort(unique(env_grid$K)), D)
      hv_plot <- plot_env_heatmaps(
        real_data = env_grid,
        facet_rows = "model_label",
        add_ssp_boundaries = TRUE,
        add_region_labels = TRUE
      )

      incProgress(0.28, detail = "Main-paper mechanism figures")
      mech_bivariate_plot <- plot_env_bivariate_hv_heatmap(
        env_grid,
        threshold_lines = threshold_lines
      )
      mech_split_plot <- plot_env_split_mechanism_heatmaps(
        env_grid,
        threshold_lines = threshold_lines
      )
      mech_bars_plot <- plot_env_stacked_hv_bars(
        env_grid,
        env_scenarios = env_scenarios,
        row_facet = "model_label",
        value_mode = "absolute_rate"
      )

      incProgress(0.38, detail = "Health-state figures")
      health_slice_plot <- if (identical(model, "health")) {
        pol_tiles <- .build_policy_tile_df(
          model = model,
          K = focus_K,
          C = C,
          D = D,
          d = d,
          T_steps = T_steps,
          states = .default_states,
          grid_step = grid_step,
          policy_args = policy_args
        )
        plot_policy_grid_tiles_by_health(
          policy_df = pol_tiles,
          model_type = paste0("health_", policy_args$terminal_reward_mode),
          K = focus_K,
          C = C,
          D = D,
          d = d,
          T = T_steps,
          T_steps = T_steps,
          include_time0 = FALSE,
          la_vals = seq(0, 0.5, by = grid_step),
          ll_vals = seq(0, 0.5, by = grid_step),
          health_break_step = 5L,
          show_grid = TRUE,
          save_output = FALSE
        )
      } else {
        .notice_plot(
          "Switch the single-model control to 'health' to inspect the policy surface across time, health, and environment.",
          title = "Health slices are only defined for the health model"
        )
      }

      health_policy_grid_plot <- if (identical(model, "health")) {
        plot_health_policy_action_bars_grid(
          K_values = k_values,
          env_scenarios = env_scenarios,
          C = C,
          D = D,
          d = d,
          T_steps = T_steps,
          states = .default_states,
          policy_args = policy_args,
          sim_args = sim_args
        )
      } else {
        .notice_plot(
          "The health-policy bar panel summarizes the health-state policy surface. Switch to the health model to generate it.",
          title = "Health bars are only defined for the health model"
        )
      }

      incProgress(0.50, detail = "Cross-model environment comparison")
      env_compare_df <- .build_env_grid_by_model(
        model_scenarios = default_env_model_scenarios,
        K_values = k_values,
        C = C,
        D = D,
        d = d,
        T_steps = T_steps,
        states = .default_states,
        N_agents = N_agents,
        grid_step = grid_step,
        base_policy_args = base_policy_args,
        base_sim_args = base_sim_args
      )
      env_compare_plot <- plot_env_heatmaps(
        real_data = env_compare_df,
        facet_rows = "model_label",
        add_ssp_boundaries = TRUE,
        add_region_labels = TRUE
      )

      incProgress(0.64, detail = "Risk appendix figures")
      risk_df <- risk_grid_K_vs_SSR(
        C = C,
        d = d,
        deltaD = D,
        K_values = k_values,
        T_steps = T_steps,
        states = .default_states,
        N_agents = N_agents,
        model = model,
        policy_args = policy_args,
        sim_args = sim_args
      )
      risk_plot <- plot_K_vs_SSR_heatmap(risk_df)

      risk_models_df <- risk_grid_K_vs_SSR_by_model(
        model_scenarios = default_risk_model_scenarios,
        C = C,
        d = d,
        deltaD = D,
        K_values = k_values,
        T_steps = T_steps,
        states = .default_states,
        N_agents = N_agents,
        base_policy_args = base_policy_args,
        base_sim_args = base_sim_args
      )
      risk_models_plot <- plot_K_vs_SSR_heatmap(
        risk_models_df,
        row_facet = "model_label",
        row_labels = attr(risk_models_df, "row_levels"),
        add_row_title = TRUE,
        row_title = "model variant"
      )
      risk_mechanism_plot <- plot_K_vs_SSR_mechanism_by_model(risk_models_df)

      incProgress(0.78, detail = "Autocorrelation appendix figures")
      autocorr_df <- symmetry_grid_K_vs_autocorr(
        C = C,
        d = d,
        deltaD = D,
        K_values = k_values,
        T_steps = T_steps,
        states = .default_states,
        N_agents = N_agents,
        model = model,
        policy_args = policy_args,
        sim_args = sim_args
      )
      autocorr_plot <- plot_K_vs_autocorr_heatmap(autocorr_df)

      autocorr_models_df <- symmetry_grid_K_vs_autocorr_by_model(
        model_scenarios = default_symmetry_model_scenarios,
        C = C,
        d = d,
        deltaD = D,
        K_values = k_values,
        T_steps = T_steps,
        states = .default_states,
        N_agents = N_agents,
        base_policy_args = base_policy_args,
        base_sim_args = base_sim_args
      )
      autocorr_models_plot <- plot_K_vs_autocorr_heatmap(
        autocorr_models_df,
        row_facet = "model_label",
        row_labels = attr(autocorr_models_df, "row_levels"),
        add_row_title = TRUE,
        row_title = "model variant"
      )
      autocorr_mechanism_plot <- plot_K_vs_autocorr_mechanism_by_model(autocorr_models_df)

      incProgress(0.90, detail = "Model-comparison policy bars")
      health_policy_compare_plot <- plot_health_policy_action_bars_by_model(
        K_value = focus_K,
        env_scenarios = env_scenarios,
        C = C,
        D = D,
        d = d,
        T_steps = T_steps,
        states = .default_states,
        base_policy_args = base_policy_args,
        sim_args = base_sim_args
      )

      incProgress(1, detail = "Done")
      list(
        policy_plot = policy_plot,
        hv_plot = hv_plot,
        mech_bivariate_plot = mech_bivariate_plot,
        mech_split_plot = mech_split_plot,
        mech_bars_plot = mech_bars_plot,
        health_slice_plot = health_slice_plot,
        health_policy_grid_plot = health_policy_grid_plot,
        env_compare_plot = env_compare_plot,
        risk_plot = risk_plot,
        risk_models_plot = risk_models_plot,
        risk_mechanism_plot = risk_mechanism_plot,
        autocorr_plot = autocorr_plot,
        autocorr_models_plot = autocorr_models_plot,
        autocorr_mechanism_plot = autocorr_mechanism_plot,
        health_policy_compare_plot = health_policy_compare_plot
      )
    })
  })

  # Forward simulation results
  simulation_results <- eventReactive(input$run_simulation, {
    model <- input$model
    policy_args <- .selected_policy_args(input, model)
    sim_args <- .selected_sim_args(input, model)

    withProgress(message = "Running forward simulation...", value = 0, {
      incProgress(0.5, detail = "Simulating agents")
      sim <- simulate_agents_forward_health(
        K = as.numeric(input$focus_k),
        C = input$C,
        D = input$D,
        d = input$d,
        LA = input$sim_LA,
        LL = input$sim_LL,
        T_steps = input$T_steps,
        states = .default_states,
        N_agents = input$n_agents_display,
        policy_args = policy_args,
        sim_args = sim_args
      )
      incProgress(1, detail = "Done")
      sim
    })
  })

  # Convert ggplot to plotly with hover info
  .ggplot_to_plotly <- function(gg_plot, hovertemplate = NULL) {
    p <- plotly::ggplotly(gg_plot, tooltip = "text")
    if (!is.null(hovertemplate)) {
      p <- p %>% plotly::layout(hovertemplate = hovertemplate)
    }
    p
  }

  # Manuscript plots with hover
  output$policy_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$policy_plot)
    .ggplot_to_plotly(gg_plot, "Optimal Action: %{text}<br>Environment: LA=%{x:.2f}, LL=%{y:.2f}<extra></extra>")
  })

  output$hv_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$hv_plot)
    .ggplot_to_plotly(gg_plot, "Hypervigilance Rate: %{z:.3f}<br>LA: %{x:.2f}<br>LL: %{y:.2f}<extra></extra>")
  })

  output$mech_bivariate_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$mech_bivariate_plot)
    .ggplot_to_plotly(gg_plot, "Mechanism: %{text}<br>LA: %{x:.2f}, LL: %{y:.2f}<extra></extra>")
  })

  output$mech_split_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$mech_split_plot)
    .ggplot_to_plotly(gg_plot, "Hypervigilance: %{z:.3f}<br>LA: %{x:.2f}, LL: %{y:.2f}<extra></extra>")
  })

  output$mech_bars_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$mech_bars_plot)
    .ggplot_to_plotly(gg_plot, "Rate: %{y:.3f}<br>%{text}<extra></extra>")
  })

  output$health_slice_plot <- renderPlotly({
    req(results())
    if (inherits(results()$health_slice_plot, "ggplot")) {
      gg_plot <- .style_app_plot(results()$health_slice_plot)
      .ggplot_to_plotly(gg_plot, "Action: %{text}<br>Health: %{x}, Time: %{y}<extra></extra>")
    } else {
      plotly_empty() %>% add_annotations(text = "Health slices only available for health model", showarrow = FALSE)
    }
  })

  output$health_policy_grid_plot <- renderPlotly({
    req(results())
    if (inherits(results()$health_policy_grid_plot, "ggplot")) {
      gg_plot <- .style_app_plot(results()$health_policy_grid_plot)
      .ggplot_to_plotly(gg_plot, "Proportion: %{y:.2f}<br>%{text}<extra></extra>")
    } else {
      plotly_empty() %>% add_annotations(text = "Health bars only available for health model", showarrow = FALSE)
    }
  })

  output$env_compare_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$env_compare_plot)
    .ggplot_to_plotly(gg_plot, "Hypervigilance: %{z:.3f}<br>LA: %{x:.2f}, LL: %{y:.2f}<br>Model: %{text}<extra></extra>")
  })

  # Forward simulation plots
  output$agent_trajectories_plot <- renderPlotly({
    req(simulation_results())
    sim <- simulation_results()

    if (is.null(sim$agent_data) || nrow(sim$agent_data) == 0) {
      return(plotly_empty() %>% add_annotations(text = "No simulation data available", showarrow = FALSE))
    }

    # Create agent trajectory plot
    plot_data <- sim$agent_data %>%
      mutate(
        agent_id = as.factor(agent),
        action_label = ifelse(action == "High", "Vigilant", "Relaxed"),
        state_label = case_when(
          state == "K" ~ "Vigilant",
          state == "Kd" ~ "Vigilant+Threat",
          state == "C" ~ "Relaxed",
          state == "CD" ~ "Relaxed+Threat",
          TRUE ~ as.character(state)
        )
      )

    p <- plot_ly(data = plot_data, x = ~time, y = ~health, color = ~agent_id,
                 type = 'scatter', mode = 'lines+markers',
                 hovertemplate = paste(
                   "Agent: %{color}<br>",
                   "Time: %{x}<br>",
                   "Health: %{y}<br>",
                   "Action: %{text}<br>",
                   "State: %{customdata}<br>",
                   "Stressor: %{meta}<extra></extra>"
                 ),
                 text = ~action_label,
                 customdata = ~state_label,
                 meta = ~stressor) %>%
      layout(title = "Agent Health Trajectories",
             xaxis = list(title = "Time Step"),
             yaxis = list(title = "Health Level"),
             showlegend = FALSE)

    p
  })

  output$population_summary_plot <- renderPlotly({
    req(simulation_results())
    sim <- simulation_results()

    if (is.null(sim$agent_stats)) {
      return(plotly_empty() %>% add_annotations(text = "No population statistics available", showarrow = FALSE))
    }

    # Create population summary plot
    stats_data <- sim$agent_stats %>%
      mutate(
        hv_rate = ifelse(is.na(hv_rate), 0, hv_rate),
        final_health = ifelse(is.na(final_health), 0, final_health)
      )

    p <- plot_ly(data = stats_data, x = ~hv_rate, y = ~final_health,
                 type = 'scatter', mode = 'markers',
                 marker = list(size = 8, opacity = 0.7),
                 hovertemplate = paste(
                   "Agent: %{customdata}<br>",
                   "Hypervigilance Rate: %{x:.3f}<br>",
                   "Final Health: %{y:.1f}<br>",
                   "Total Actions: %{meta}<extra></extra>"
                 ),
                 customdata = ~agent,
                 meta = ~total_actions) %>%
      layout(title = "Population Summary: Hypervigilance vs Final Health",
             xaxis = list(title = "Hypervigilance Rate"),
             yaxis = list(title = "Final Health"))

    p
  })

  output$health_distribution_plot <- renderPlotly({
    req(simulation_results())
    sim <- simulation_results()

    if (is.null(sim$agent_data)) {
      return(plotly_empty() %>% add_annotations(text = "No health distribution data available", showarrow = FALSE))
    }

    # Create health distribution over time
    dist_data <- sim$agent_data %>%
      group_by(time, health) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(proportion = count / sum(count), .by = time)

    p <- plot_ly(data = dist_data, x = ~time, y = ~health, z = ~proportion,
                 type = 'heatmap',
                 hovertemplate = paste(
                   "Time: %{x}<br>",
                   "Health: %{y}<br>",
                   "Proportion: %{z:.3f}<extra></extra>"
                 )) %>%
      layout(title = "Health Distribution Over Time",
             xaxis = list(title = "Time Step"),
             yaxis = list(title = "Health Level"))

    p
  })

  # Appendix plots with hover
  output$risk_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$risk_plot)
    .ggplot_to_plotly(gg_plot, "Risk: %{z:.3f}<br>LA: %{x:.2f}, LL: %{y:.2f}<extra></extra>")
  })

  output$risk_models_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$risk_models_plot)
    .ggplot_to_plotly(gg_plot, "Risk: %{z:.3f}<br>LA: %{x:.2f}, LL: %{y:.2f}<br>Model: %{text}<extra></extra>")
  })

  output$risk_mechanism_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$risk_mechanism_plot)
    .ggplot_to_plotly(gg_plot, "Risk: %{y:.3f}<br>%{text}<extra></extra>")
  })

  output$autocorr_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$autocorr_plot)
    .ggplot_to_plotly(gg_plot, "Autocorr: %{z:.3f}<br>LA: %{x:.2f}, LL: %{y:.2f}<extra></extra>")
  })

  output$autocorr_models_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$autocorr_models_plot)
    .ggplot_to_plotly(gg_plot, "Autocorr: %{z:.3f}<br>LA: %{x:.2f}, LL: %{y:.2f}<br>Model: %{text}<extra></extra>")
  })

  output$autocorr_mechanism_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$autocorr_mechanism_plot)
    .ggplot_to_plotly(gg_plot, "Autocorr: %{y:.3f}<br>%{text}<extra></extra>")
  })

  output$health_policy_compare_plot <- renderPlotly({
    req(results())
    gg_plot <- .style_app_plot(results()$health_policy_compare_plot)
    .ggplot_to_plotly(gg_plot, "Proportion: %{y:.2f}<br>%{text}<extra></extra>")
  })

  # Download handler
  output$download_current_plot <- downloadHandler(
    filename = function() {
      paste0("hypervigilance_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      # This would need to be implemented based on current tab
      # For now, just create a placeholder
      pdf(file)
      plot(1:10, 1:10, main = "Placeholder - implement based on current tab")
      dev.off()
    }
  )
}

# ---- Run the App ----
shinyApp(ui, server)
