# =============================================================================
# File: R/plotting/policy_grid_tiles/00_plot_policy_grid_tiles_dp_by_health.R
#
# Purpose:
#   DP-only policy-by-environment plot that shows the *optimal action per health*
#   across time. Each small panel is one environment cell (LA x LL) split into
#   ns vs s (no-stressor vs stressor).
#
# What it shows:
#   - x-axis: time (equal-width steps; discrete)
#   - y-axis: health (readable ticks only; full range still present)
#   - fill: optimal_action (High / Low / Tie) using palette_actions
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

#' DP-only policy tiles by health over LA x LL.
#'
#' @param policy_df data.frame with columns: LA, LL, time, label, optimal_action, health
#' @param model_type character label for the model (e.g., "health_threshold")
#' @param K numeric vigilance cost (for subtitle/filename)
#' @param C,D,d,T numeric model params for subtitle (optional but recommended)
#' @param T_steps optional horizon length. If NULL, inferred from policy_df$time.
#'   Note: this function *excludes the last timestep* (time == T_steps) because
#'   there is typically no policy defined for the terminal step.
#' @param include_time0 logical; include time==0 / PRIOR entries (default FALSE)
#' @param la_vals optional ordering for LA facet values (numeric vector)
#' @param ll_vals optional ordering for LL facet values (numeric vector)
#' @param health_break_step integer; show a y tick every this many health units (default 5)
#' @param show_grid logical; draw grid lines on top of tiles (default TRUE)
#'
#' @return ggplot object (visible)
plot_policy_grid_tiles_by_health <- function(
    policy_df,
    model_type,
    K,
    C = NA, D = NA, d = NA, T = NA,
    T_steps = NULL,
    include_time0 = FALSE,
    la_vals = NULL,
    ll_vals = NULL,
    health_break_step = 5L,
    show_grid = TRUE,
    save_output = TRUE
) {

  required_cols <- c("LA", "LL", "time", "label", "optimal_action", "health")
  missing_cols <- setdiff(required_cols, names(policy_df))
  if (length(missing_cols) > 0) {
    stop("policy_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # --- Standardize types early ---
  df <- policy_df %>%
    mutate(
      LA = .data$LA,
      LL = .data$LL,
      label = as.character(.data$label),
      optimal_action = as.character(.data$optimal_action),
      time = as.integer(.data$time),
      health = as.integer(.data$health)
    )

  # --- Drop PRIOR/time0 by default ---
  if (!include_time0) {
    df <- df %>%
      filter(.data$time != 0) %>%
      filter(!tolower(.data$label) %in% c("prior"))
  }

  # --- Infer T_steps if needed ---
  if (is.null(T_steps)) {
    T_steps <- suppressWarnings(max(df$time, na.rm = TRUE))
  }

  # --- Exclude terminal step (no policy usually exists for that one) ---
  # If times run 0..T, keep 0..(T-1). If already 0..(T-1), this is harmless.
  df <- df %>%
    filter(.data$time <= (T_steps - 1L))

  # --- Define DP context from DP labels, and abbreviate ---
  df <- df %>%
    mutate(
      context = case_when(
        .data$label %in% c("Kd", "CD") ~ "s",   # stressor
        .data$label %in% c("K",  "C")  ~ "ns",  # no stressor
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(.data$context))

  # --- Factor ordering for facets ---
  if (is.null(la_vals)) la_vals <- sort(unique(df$LA))
  if (is.null(ll_vals)) ll_vals <- sort(unique(df$LL))

  df <- df %>%
    mutate(
      LA = factor(.data$LA, levels = la_vals),
      LL = factor(.data$LL, levels = ll_vals),
      context = factor(.data$context, levels = c("ns", "s")),
      optimal_action = factor(.data$optimal_action, levels = c("High", "Low", "Tie"))
    )

  # --- Force equal-width timesteps: use a discrete time factor with full levels ---
  time_levels <- seq.int(min(df$time, na.rm = TRUE), max(df$time, na.rm = TRUE))
  df <- df %>%
    mutate(time_f = factor(.data$time, levels = time_levels))

  # --- If multiple labels exist within a context, collapse to a single action per health/time
  #     while preserving "Tie" meaning:
  #     - If any "Tie" exists across labels => "Tie"
  #     - Else if both High and Low appear => "Tie"
  #     - Else unanimous => that action
  df2 <- df %>%
    group_by(.data$LA, .data$LL, .data$context, .data$time_f, .data$health) %>%
    summarise(
      has_high = any(.data$optimal_action == "High"),
      has_low  = any(.data$optimal_action == "Low"),
      has_tie  = any(.data$optimal_action == "Tie"),
      .groups = "drop"
    ) %>%
    mutate(
      action = case_when(
        has_tie ~ "Tie",
        has_high & has_low ~ "Tie",
        has_high ~ "High",
        has_low  ~ "Low",
        TRUE ~ NA_character_
      ),
      action = factor(action, levels = c("High", "Low", "Tie"))
    ) %>%
    filter(!is.na(.data$action))

  # --- Health axis: keep full range but show fewer labels ---
  h_min <- min(df2$health, na.rm = TRUE)
  h_max <- max(df2$health, na.rm = TRUE)
  health_breaks <- seq(h_min, h_max, by = as.integer(health_break_step))
  # Make sure extremes are included
  health_breaks <- sort(unique(c(h_min, health_breaks, h_max)))

  # --- Facet labeller (NOT bold) ---
  facet_labeller <- labeller(
    LA = function(x) paste0("LA = ", x),
    LL = function(x) paste0("LL = ", x),
    context = function(x) x
  )

  # --- Subtitle in your preferred style ---
  # Keep it robust if C/D/d/T are not supplied
  sub_parts <- c(
    if (!is.na(K)) paste0("K = ", K) else NULL,
    if (!is.na(C)) paste0("C = ", C) else NULL,
    if (!is.na(D)) paste0("D = ", D) else NULL,
    if (!is.na(d)) paste0("d = ", d) else NULL,
    if (!is.na(T)) paste0("T = ", T) else NULL,
    paste0("model = ", model_type)
  )
  subtitle_txt <- paste(sub_parts, collapse = " | ")

  # --- Plot ---
  p <- ggplot(df2, aes(x = .data$time_f, y = .data$health, fill = .data$action)) +
    geom_tile(width = 0.98, height = 0.98) +

    # grid lines ON TOP (optional)
    { if (isTRUE(show_grid)) {
      list(
        geom_hline(yintercept = health_breaks, colour = "grey85", linewidth = 0.25),
        geom_vline(xintercept = seq_along(time_levels) + 0.5, colour = "grey90", linewidth = 0.20)
      )
    } else {
      NULL
    }} +

    scale_fill_manual(
      values = palette_actions,      # KEEP YOUR EXISTING COLORS
      limits = names(palette_actions),
      breaks = names(palette_actions),
      drop = FALSE
    ) +

    # equal-width discrete steps + horizontal tick labels
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(
      breaks = health_breaks,
      labels = health_breaks,
      expand = expansion(mult = c(0.01, 0.01))
    ) +

    labs(
      x = "time",
      y = "health",
      fill = "optimal action"
      #title = "DP-only policy by environment and health",
      #subtitle = subtitle_txt
    ) +

    # NOTE: LL row strips on the right
    facet_grid(
      LL ~ LA + context,
      labeller = facet_labeller,
      switch = "y"
    ) +

    theme_vigilance(base_size = 12, strip_size = 11) +
    theme(
      panel.ontop = isTRUE(show_grid),

      # tighter panel packing for thesis export
      panel.spacing.x = unit(if (isTRUE(show_grid)) 0.30 else 0.18, "lines"),
      panel.spacing.y = unit(if (isTRUE(show_grid)) 0.24 else 0.16, "lines"),

      # remove strip boxes/grey outlines
      strip.background = element_blank(),
      strip.placement  = "outside",

      # make strip text NOT bold
      strip.text.x = element_text(size = 10.5, face = "plain"),
      strip.text.y.right = element_text(size = 10.5, face = "plain", angle = 0),

      # axes
      axis.title.x = element_text(size = 13, face = "bold"),
      axis.title.y = element_text(size = 13, face = "bold"),
      axis.text.x  = element_text(size = 9, angle = 0, hjust = 0.5),
      axis.text.y  = element_text(size = 8.5),

      # legend
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 10),
      legend.key.height = unit(0.55, "lines"),
      legend.key.width  = unit(0.90, "lines"),

      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 11, face = "plain"),

      plot.margin = margin(4, 6, 4, 6)
    )

  # --- Save (project save function) ---
  if (isTRUE(save_output)) {
    stem <- file.path(
      DIR_FIGURES,
      "policy",
      paste0("policy_grid_tiles_by_health__model-", model_type, "__K-", K)
    )
    save_graphs(
      p,
      stem,
      model = if (grepl("health", model_type, ignore.case = TRUE)) "health" else "basic"
    )
  }

  return(p)
}

# -----------------------------------------------------------------------------
# Example usage (run after setup_project.R and after computing policy_df)
# -----------------------------------------------------------------------------
if (FALSE) {
  la_vals <- seq(0, 0.5, by = 0.1)
  ll_vals <- seq(0, 0.5, by = 0.1)

  # Suppose pol_health is your combined policy_df over env grid with columns:
  # LA, LL, time, label, health, optimal_action
  p <- plot_policy_grid_tiles_by_health(
    policy_df  = pol_health,
    model_type = "health_threshold",
    K = 5, C = 0, D = 10, d = 0, T = 10,
    T_steps = 10,
    include_time0 = FALSE,
    la_vals = la_vals,
    ll_vals = ll_vals,
    health_break_step = 5,
    show_grid = TRUE
  )
  print(p)
}

# -----------------------------------------------------------------------------
# Batch runner: all model types × K values (basic handled via pseudo-health)
# -----------------------------------------------------------------------------
# Assumes setup_project.R has been sourced so:
#   - default_model_scenarios
#   - merge_model_args
#   - validate_default
#   - mem_compute_policy
# are available.
run_policy_grid_tiles_all_models <- function(
    K_values = c(1, 3, 5, 7, 9),
    C = 0,
    D = 10,
    d = 0,
    T_steps = 10L,
    states = c("K", "Kd", "C", "CD"),
    la_vals = seq(0, 0.5, by = 0.1),
    ll_vals = seq(0, 0.5, by = 0.1),
    base_policy_args = list(h0 = validate_default("h0")),
    include_time0 = FALSE,
    health_break_step = 5L,
    show_grid = TRUE
) {
  if (!exists("default_model_scenarios", mode = "function") &&
      !exists("default_model_scenarios", mode = "list")) {
    stop("default_model_scenarios not found; source R/helpers/utils_model_scenarios.R or setup_project.R first.")
  }

  scenarios <- if (is.function(default_model_scenarios)) default_model_scenarios() else default_model_scenarios
  grid <- expand.grid(LA = la_vals, LL = ll_vals, KEEP.OUT.ATTRS = FALSE)

  sanitize_label <- function(x) {
    gsub("[^A-Za-z0-9._-]+", "_", tolower(as.character(x)))
  }

  plots <- list()
  for (scenario in scenarios) {
    scenario_label <- if (!is.null(scenario$label)) scenario$label else as.character(scenario$model)
    model_tag <- sanitize_label(scenario_label)

    policy_args <- if (identical(scenario$model, "health")) {
      merge_model_args(base_policy_args, scenario$policy_args)
    } else {
      list()
    }

    for (K in K_values) {
      pol_df <- dplyr::bind_rows(lapply(seq_len(nrow(grid)), function(i) {
        cell <- grid[i, ]
        pol <- mem_compute_policy(
          model = scenario$model,
          K = K, C = C, D = D, d = d,
          LA = cell$LA, LL = cell$LL,
          T_steps = T_steps,
          states = states,
          policy_args = policy_args
        )
        pol$LA <- cell$LA
        pol$LL <- cell$LL
        if (!"health" %in% names(pol)) pol$health <- 1L
        pol
      }))

      plot_obj <- plot_policy_grid_tiles_by_health(
        policy_df  = pol_df,
        model_type = model_tag,
        K = K, C = C, D = D, d = d, T = T_steps,
        T_steps = T_steps,
        include_time0 = include_time0,
        la_vals = la_vals,
        ll_vals = ll_vals,
        health_break_step = health_break_step,
        show_grid = show_grid
      )

      plots[[paste0(model_tag, "__K-", K)]] <- plot_obj
    }
  }

  plots
}
if (FALSE) {
  run_policy_grid_tiles_all_models(
    K_values = c(1, 3, 5, 7, 9),
    health_break_step = 5
  )
}
