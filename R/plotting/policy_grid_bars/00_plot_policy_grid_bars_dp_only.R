# =============================================================================
# File: R/plotting/policy_grid_bars/00_plot_policy_grid_bars_dp_only.R
#
# Purpose:
#   DP-only policy-by-environment bar grid (no simulation). Each panel shows
#   stacked proportions of optimal actions over time, aggregated across health
#   states and labels within each context (ns vs s).
#
# Requirements:
#   - Expects project setup to be done (setup_project.R), so save_graphs() and
#     DIR_FIGURES are available.
#   - Expects palette_actions and theme_vigilance() to exist (project style).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

#' DP-only policy grid bars over LA x LL.
#'
#' @param policy_df data.frame with columns: LA, LL, time, label, optimal_action,
#'   and optionally health.
#' @param model_type character label for the model (e.g., "health_threshold").
#' @param K numeric vigilance cost (for subtitle + filename).
#' @param C numeric relaxed cost (for subtitle).
#' @param D numeric damage cost (for subtitle).
#' @param d numeric additional damage modifier (for subtitle).
#' @param T_steps horizon length used in DP generation (for subtitle).
#' @param include_time0 logical; include time==0 / PRIOR entries (default TRUE).
#' @param drop_last_time logical; drop the final time step (default TRUE).
#'   Useful when your DP table includes time==T but no action is defined there.
#' @param la_vals optional ordering for LA facet values (numeric vector).
#' @param ll_vals optional ordering for LL facet values (numeric vector).
#'
#' @return ggplot object (visible).
plot_policy_grid_bars <- function(
    policy_df,
    model_type,
    K,
    C = 0,
    D = 10,
    d = 0,
    T_steps = 10L,
    include_time0 = TRUE,
    drop_last_time = TRUE,
    la_vals = NULL,
    ll_vals = NULL
) {
  # ---- validate ----
  required_cols <- c("LA", "LL", "time", "label", "optimal_action")
  missing_cols <- setdiff(required_cols, names(policy_df))
  if (length(missing_cols) > 0) {
    stop("policy_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # ---- standardize types early ----
  df <- policy_df %>%
    mutate(
      LA = .data$LA,
      LL = .data$LL,
      time = as.integer(.data$time),
      label = as.character(.data$label),
      optimal_action = as.character(.data$optimal_action)
    )

  # ---- drop PRIOR / time 0 unless requested ----
  if (!include_time0) {
    df <- df %>%
      filter(.data$time != 0) %>%
      filter(!tolower(.data$label) %in% c("prior"))
  }

  # ---- infer T if needed ----
  if (is.null(T_steps) || !is.finite(T_steps)) {
    T_steps <- max(df$time, na.rm = TRUE)
  }

  # ---- drop last time step (typically T) because no policy/action exists there ----
  # If T_steps = 10, keep 1..9 by default.
  if (isTRUE(drop_last_time)) {
    df <- df %>% filter(.data$time < T_steps)
  }

  # ---- define context from DP labels, then abbreviate ----
  df <- df %>%
    mutate(
      context = case_when(
        .data$label %in% c("Kd", "CD") ~ "s",
        .data$label %in% c("K", "C")   ~ "ns",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(.data$context))

  # ---- facet ordering ----
  if (is.null(la_vals)) la_vals <- sort(unique(df$LA))
  if (is.null(ll_vals)) ll_vals <- sort(unique(df$LL))

  df <- df %>%
    mutate(
      LA = factor(.data$LA, levels = la_vals),
      LL = factor(.data$LL, levels = ll_vals),
      context = factor(.data$context, levels = c("ns", "s"))
    )

  # ---- fix time spacing: time as factor with GLOBAL levels + complete() ----
  time_levels <- sort(unique(df$time))
  # If drop_last_time=TRUE and T_steps exists, enforce full sequence.
  if (isTRUE(drop_last_time) && is.finite(T_steps) && T_steps >= 1) {
    start_t <- if (isTRUE(include_time0)) 0L else 1L
    end_t <- as.integer(T_steps) - 1L
    time_levels <- seq.int(start_t, end_t)
  }

  # ---- aggregate: proportion across health states (and across the 2 labels per context) ----
  summary_df <- df %>%
    group_by(.data$LA, .data$LL, .data$context, .data$time) %>%
    summarise(
      p_high = mean(.data$optimal_action == "High", na.rm = TRUE),
      p_low  = mean(.data$optimal_action == "Low",  na.rm = TRUE),
      p_tie  = mean(.data$optimal_action == "Tie",  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      p_total = .data$p_high + .data$p_low + .data$p_tie,
      p_high = ifelse(.data$p_total > 0, .data$p_high / .data$p_total, 0),
      p_low  = ifelse(.data$p_total > 0, .data$p_low  / .data$p_total, 0),
      p_tie  = ifelse(.data$p_total > 0, .data$p_tie  / .data$p_total, 0)
    ) %>%
    pivot_longer(
      cols = c("p_high", "p_low", "p_tie"),
      names_to = "action",
      values_to = "proportion"
    ) %>%
    mutate(
      action = recode(.data$action, p_high = "High", p_low = "Low", p_tie = "Tie"),
      action = factor(.data$action, levels = c("High", "Low", "Tie")),
      time   = factor(.data$time, levels = time_levels)
    ) %>%
    tidyr::complete(
      LA, LL, context, time, action,
      fill = list(proportion = 0)
    )

  # ---- labellers: LA on top, LL on right, context in strip as ns/s ----
  facet_labeller <- labeller(
    LA = function(x) paste0("LA = ", x),
    LL = function(x) paste0("LL = ", x),
    context = function(x) x
  )

  # ---- subtitle formatting (exact style requested) ----
  subtitle_txt <- paste0(
    "K = ", K,
    " | C = ", C,
    " | D = ", D,
    " | d = ", d,
    " | T = ", T_steps,
    " | model = ", model_type
  )

  # ---- plot ----
  p <- ggplot(summary_df, aes(x = .data$time, y = .data$proportion, fill = .data$action)) +
    # width < 1 gives a tiny gap so grid lines can be seen above (panel.ontop=TRUE)
    geom_col(width = 0.92, linewidth = 0) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1"),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(
      values = palette_actions,              # KEEP YOUR EXISTING COLORS
      limits = names(palette_actions),
      breaks = names(palette_actions),
      drop = FALSE
    ) +
    labs(
      x = "time",
      y = "proportion of health states",
      fill = "optimal action"
      #title = "DP-only policy summary by environment",
      #subtitle = subtitle_txt
    ) +
    facet_grid(
      LL ~ LA + context,
      labeller = facet_labeller,
      switch = "y"  # put LL strips on the RIGHT
    ) +
    theme_vigilance() +
    theme(
      # put gridlines ABOVE bars (you asked for grid inside plot)
      panel.ontop = TRUE,

      # slightly more whitespace between panels
      panel.spacing.x = unit(0.55, "lines"),
      panel.spacing.y = unit(0.5, "lines"),

      # grid lines visible but light
      panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.25),
      panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.30),
      panel.grid.minor = element_blank(),

      # remove panel border (cleaner)
      panel.border = element_blank(),

      # facet strips: outside + no grey boxes + NOT bold
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.x = element_text(size = 12, face = "plain"),
      strip.text.y.right = element_text(size = 12, face = "plain", angle = 0),

      # axis text: horizontal time labels
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x  = element_text(size = 11, angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text.y  = element_text(size = 11),

      # title/subtitle styling (match your desired clean style)
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "plain"),

      # legend
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 14, face = "plain"),
      legend.text  = element_text(size = 12),
      legend.key.height = unit(0.55, "lines"),
      legend.key.width  = unit(1.0, "lines"),

      plot.margin = margin(10, 12, 10, 12)
    )

  # ---- save (project save function) ----
  stem <- file.path(
    DIR_FIGURES,
    "policy",
    paste0("policy_grid_bars__model-", model_type, "__K-", K)
  )
  save_graphs(
    p,
    stem,
    model = if (grepl("health", model_type, ignore.case = TRUE)) "health" else "basic"
  )

  return(p)
}

# -----------------------------------------------------------------------------
# Batch runner: all HEALTH model variants × K values
# -----------------------------------------------------------------------------
# Assumes setup_project.R has been sourced so:
#   - default_model_scenarios
#   - merge_model_args
#   - validate_default
#   - mem_compute_policy
# are available.
run_policy_grid_bars_all_health <- function(
    K_values = c(1, 3, 5, 7, 9),
    C = 0,
    D = 10,
    d = 0,
    T_steps = 10L,
    states = c("K", "Kd", "C", "CD"),
    la_vals = seq(0, 0.5, by = 0.1),
    ll_vals = seq(0, 0.5, by = 0.1),
    base_policy_args = list(h0 = validate_default("h0")),
    include_time0 = TRUE,
    drop_last_time = TRUE
) {
  if (!exists("default_model_scenarios", mode = "function") &&
      !exists("default_model_scenarios", mode = "list")) {
    stop("default_model_scenarios not found; source R/helpers/utils_model_scenarios.R or setup_project.R first.")
  }

  scenarios <- if (is.function(default_model_scenarios)) default_model_scenarios() else default_model_scenarios
  health_scenarios <- Filter(function(s) !is.null(s$model) && s$model == "health", scenarios)
  if (length(health_scenarios) == 0) stop("No health model scenarios found in default_model_scenarios().")

  grid <- expand.grid(LA = la_vals, LL = ll_vals, KEEP.OUT.ATTRS = FALSE)

  sanitize_label <- function(x) {
    gsub("[^A-Za-z0-9._-]+", "_", tolower(as.character(x)))
  }

  plots <- list()
  for (scenario in health_scenarios) {
    scenario_label <- if (!is.null(scenario$label)) scenario$label else "health"
    model_tag <- sanitize_label(scenario_label)
    policy_args <- merge_model_args(base_policy_args, scenario$policy_args)

    for (K in K_values) {
      pol_df <- dplyr::bind_rows(lapply(seq_len(nrow(grid)), function(i) {
        cell <- grid[i, ]
        pol <- mem_compute_policy(
          model = "health",
          K = K, C = C, D = D, d = d,
          LA = cell$LA, LL = cell$LL,
          T_steps = T_steps,
          states = states,
          policy_args = policy_args
        )
        pol$LA <- cell$LA
        pol$LL <- cell$LL
        pol
      }))

      plot_obj <- plot_policy_grid_bars(
        policy_df = pol_df,
        model_type = model_tag,
        K = K, C = C, D = D, d = d, T_steps = T_steps,
        la_vals = la_vals, ll_vals = ll_vals,
        include_time0 = include_time0,
        drop_last_time = drop_last_time
      )

      plots[[paste0(model_tag, "__K-", K)]] <- plot_obj
    }
  }

  plots
}

# ---------------------------------------------------------------------------
# Example usage (run after setup_project.R + after computing policy_df)
# ---------------------------------------------------------------------------
if (FALSE) {
  la_vals <- seq(0, 0.5, by = 0.1)
  ll_vals <- seq(0, 0.5, by = 0.1)
  grid <- expand.grid(LA = la_vals, LL = ll_vals, KEEP.OUT.ATTRS = FALSE)

  pol_health_thresh <- dplyr::bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    cell <- grid[i, ]
    pol <- mem_compute_policy(
      model = "health",
      K = 5, C = 0, D = 10, d = 0,
      LA = cell$LA, LL = cell$LL,
      T_steps = 10,
      states = c("K", "Kd", "C", "CD"),
      policy_args = list(
        h0 = 35,
        terminal_reward_weight = 1,
        terminal_reward_mode = "threshold",
        terminal_threshold_tau = round(0.6 * 35)
      )
    )
    pol$LA <- cell$LA
    pol$LL <- cell$LL
    pol
  }))

  plot_policy_grid_bars(
    pol_health_thresh,
    model_type = "health_threshold",
    K = 5, C = 0, D = 10, d = 0, T_steps = 10,
    la_vals = la_vals,
    ll_vals = ll_vals,
    drop_last_time = TRUE
  )
}


run_policy_grid_bars_all_health(
  K_values = c(1, 3, 5, 7, 9),
  T_steps = 10,
  la_vals = seq(0, 0.5, by = 0.1),
  ll_vals = seq(0, 0.5, by = 0.1),
  base_policy_args = list(h0 = 35)
)
