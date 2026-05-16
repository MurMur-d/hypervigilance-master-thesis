# ============================================================
# File: R/plotting/health/03_plot_health_simulation.R
#
# Purpose: Shared plotting helpers for health simulation diagnostics.
#
# What this module is:
#   A pure plotting/visualization module that consumes the output of the
#   forward simulator (e.g., simulate_agents_forward_health()) and produces
#   diagnostic panels used during development and figure construction.
#
# What this module is NOT:
#   - It does not run DP / policy computation itself (except the small demo
#     block at the bottom).
#   - It does not save files (saving is handled by higher-level scripts via
#     save_graphs() or similar utilities in plot_utils.R).
#
# Expected input object (`sim`):
#   A list with at least:
#     - sim$agent_data  : long panel with one row per (agent, time)
#     - sim$agent_stats : per-agent summary (optional for some plots)
#
# agent_data columns commonly used here:
#   agent, time, stressor (0/1), action (High/Low/NA if dead),
#   state (K/Kd/C/CD/DEAD), health (numeric/int), alive (logical),
#   hv (logical: hypervigilant on no-stressor steps)
#
# Notes:
#   Many â€œdiagnostic plotsâ€ are meant to be robust to partial/missing columns
#   because simulation outputs evolve during development. Youâ€™ll see defensive
#   checks (e.g., creating alive when missing).
# ============================================================

# ---- Sources ----------------------------------------------------------------
# plot_utils.R:
#   - theme_vigilance() (house style)
#   - possibly theme_clean_minimal() (optional)
# utils_palette_health.R:
#   - pal_states_health: mapping state -> color (including DEAD)
#   - scale_action_fill(): action color scale helper (High vs Low)
source("R/core/plot_utils.R")
source("R/plotting/_shared/utils_palette_health.R")

# ---- Packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggforce)   # used for arc/pie geometry (geom_arc_bar)
  library(patchwork) # used to stack plots vertically (p1 / p2)
  library(tidyr)
})

# =============================================================================
# Plots for inspection of the forward simulated agents
# =============================================================================
# The functions below are designed as â€œsmall multiplesâ€ diagnostics:
#   - health distribution and survival curve over time,
#   - per-agent state and action heatmaps,
#   - hypervigilance rate trend,
#   - and a more complex environment-flow visualization combining pies and ribbons.
#
# Convention:
#   - "High" action = vigilant
#   - "Low"  action = relaxed
#   - "hv" is typically defined as High when stressor==0 (i.e., vigilant in safe moments)
# =============================================================================


# =============================================================================
# 12) VISUALIZATION: COLOR PALETTES (external)
# =============================================================================
# We do not define palettes in this file. Instead:
#   - `pal_states_health` is expected to be defined in utils_palette_health.R
#   - `scale_action_fill()` is also expected to come from utils_palette_health.R
#
# Why:
#   Centralizing palettes ensures consistent color semantics across:
#   - state heatmaps,
#   - action heatmaps,
#   - and any publication figures reusing these diagnostics.
# =============================================================================


# =============================================================================
# 13) PLOT 1 â€” Health summary over time + survival
# =============================================================================
# plot_health_over_time(sim)
#
# What it shows:
#   Panel A: health trajectory summary across agents at each time step:
#     - solid line  : mean health
#     - dashed line : median health
#     - ribbon      : interquartile range (25thâ€“75th percentiles)
#   Panel B: survival curve:
#     - proportion alive at each time
#
# Why itâ€™s useful:
#   - Quickly checks whether health is decaying as expected.
#   - Reveals whether deaths are abrupt or gradual (via alive_prop).
#   - Ribbon width indicates heterogeneity across agents.
#
# Implementation details:
#   - We defensively ensure an `alive` column exists.
#   - Summaries are computed by grouping agent_data by time.
#   - Returns a patchwork stack if patchwork is installed; otherwise returns
#     a list of plots.
plot_health_over_time <- function(sim) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required for plotting.")

  # --- Pull agent_data --------------------------------------------------------
  df <- sim$agent_data
  # Some pipelines store nested lists; accept sim$agent_data$agent_data.
  if (is.list(df) && "agent_data" %in% names(df)) df <- df$agent_data

  # --- Ensure `alive` exists and is non-missing -------------------------------
  # Motivation:
  #   Some simulations may not explicitly store alive; we assume alive=TRUE
  #   for all rows if missing, then explicitly coerce NA->TRUE for robustness.
  if (!"alive" %in% names(df)) df <- dplyr::mutate(df, alive = TRUE)
  if (!"alive" %in% names(df)) {
    df <- df %>% dplyr::mutate(alive = TRUE)
  } else {
    df <- df %>% dplyr::mutate(alive = dplyr::if_else(is.na(alive), TRUE, alive))
  }

  # --- Aggregate across agents per time --------------------------------------
  # mean_health / median_health: central tendency
  # p25 / p75: distribution spread (IQR)
  # alive_prop: survival fraction
  summ <- df %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      mean_health   = mean(health, na.rm = TRUE),
      median_health = stats::median(health, na.rm = TRUE),
      p25           = stats::quantile(health, 0.25, na.rm = TRUE, names = FALSE),
      p75           = stats::quantile(health, 0.75, na.rm = TRUE, names = FALSE),
      alive_prop    = mean(alive, na.rm = TRUE),
      .groups = "drop"
    )

  # --- Panel A: health summary ------------------------------------------------
  p_health <- ggplot2::ggplot(summ, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p25, ymax = p75),
      fill = "#cccccc", alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_health),
      linewidth = 1.1, colour = "#2c7fb8"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = median_health),
      linewidth = 0.9, linetype = "dashed", colour = "#7fcdbb"
    ) +
    ggplot2::labs(
      title = "Health over time",
      subtitle = "mean (solid), median (dashed); ribbon = 25â€“75%",
      x = "time",
      y = "health"
    ) +
    theme_vigilance(base_size = 12) +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$time)))

  # --- Panel B: survival curve ------------------------------------------------
  # alive_prop already in [0,1]; we clamp visually with limits.
  p_surv <- ggplot2::ggplot(summ, ggplot2::aes(x = time, y = alive_prop)) +
    ggplot2::geom_line(linewidth = 1.1, colour = "#3c8d40") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = "Survival (alive proportion)", x = "time", y = "alive (%)") +
    theme_vigilance(base_size = 12) +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$time)))

  # --- Return combined or separate plots --------------------------------------
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_health / p_surv + patchwork::plot_layout(heights = c(1, 0.7))
  } else {
    warning("patchwork not installed; returning a list of plots")
    list(health = p_health, survival = p_surv)
  }
}


# =============================================================================
# 14) PLOT 2 â€” State heatmap by agent/time (includes DEAD)
# =============================================================================
# plot_state_heatmap_health(sim, n_agents = 100)
#
# What it shows:
#   A raster/heatmap where:
#     x = time
#     y = agent id (first n_agents)
#     fill = discrete state label (K, Kd, C, CD, DEAD)
#
# Why itâ€™s useful:
#   - Checks that state transitions are sensible.
#   - Visually reveals clustering of death times (DEAD bands).
#   - Quickly diagnoses â€œstuckâ€ dynamics (e.g., never leaving stressor states).
#
# Implementation details:
#   - Subsets to first n_agents for readability.
#   - Forces factor ordering so legend and palette align.
#   - Uses pal_states_health palette and keeps DEAD even if absent (drop = FALSE).
plot_state_heatmap_health <- function(sim, n_agents = 100) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required for plotting.")

  df <- sim$agent_data %>% dplyr::filter(agent <= n_agents)

  # Ensure consistent legend order and presence of DEAD.
  df$state <- factor(df$state, levels = c("K", "Kd", "C", "CD", "DEAD"))

  ggplot2::ggplot(df, ggplot2::aes(x = time, y = factor(agent), fill = state)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = pal_states_health, drop = FALSE, name = "state") +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$time))) +
    ggplot2::labs(
      title = paste0("States by agent (first ", n_agents, ")"),
      x = "time",
      y = "agent"
    ) +
    theme_vigilance(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}


# =============================================================================
# 15) PLOT 3 â€” Action heatmap by agent/time
# =============================================================================
# plot_action_heatmap_health(sim, n_agents = 100)
#
# What it shows:
#   Similar to the state heatmap, but fill = action (High vs Low).
#
# Important:
#   Dead rows typically have action == NA in your simulator.
#   Those rows will appear as missing tiles; if you want them explicit,
#   you could map NA -> "DEAD" before plotting.
#
# Uses:
#   - Verifies that the policy produces coherent time structure (e.g., early
#     vigilance then relaxation later, or vice versa).
#   - Highlights whether agents synchronize (banding) vs diverge (speckling).
plot_action_heatmap_health <- function(sim, n_agents = 100) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required for plotting.")

  df <- sim$agent_data %>% dplyr::filter(agent <= n_agents)

  # Force ordering: High then Low (matches other figures).
  df$action <- factor(df$action, levels = c("High", "Low"))

  ggplot2::ggplot(df, ggplot2::aes(x = time, y = factor(agent), fill = action)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    scale_action_fill(drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$time))) +
    ggplot2::labs(
      title = paste0("Actions by agent (first ", n_agents, ")"),
      x = "time",
      y = "agent"
    ) +
    theme_vigilance(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}


# =============================================================================
# 16) PLOT 4 â€” Hypervigilance rate over time (trend line)
# =============================================================================
# plot_hv_rate_over_time(sim)
#
# Definition used here:
#   hypervigilance = (hv == TRUE) on no-stressor steps (stressor == 0),
#   and only counting agents that are alive at that step.
#
# Why this definition:
#   It matches the conceptual idea of â€œunnecessary vigilanceâ€: staying vigilant
#   when the environment is actually safe.
#
# Implementation details:
#   - n0 counts eligible rows: (stressor==0 AND alive==TRUE)
#   - hv_count counts hv==TRUE among alive rows (note: your hv flag is already
#     only TRUE for High+no-stressor in the simulator)
#   - hv_rate = hv_count / n0, undefined if n0==0
plot_hv_rate_over_time <- function(sim) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required for plotting.")

  df <- sim$agent_data
  if (is.list(df) && "agent_data" %in% names(df)) df <- df$agent_data

  # If alive missing, assume alive=TRUE everywhere (development robustness).
  if (!"alive" %in% names(df)) df <- dplyr::mutate(df, alive = TRUE)

  # Summarise hv rate per time step.
  summ <- df %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      n0       = sum(stressor == 0 & alive == TRUE, na.rm = TRUE),
      hv_count = sum(hv & alive == TRUE, na.rm = TRUE),
      hv_rate  = dplyr::if_else(n0 > 0, hv_count / n0, as.numeric(NA)),
      .groups = "drop"
    )

  ggplot2::ggplot(summ, ggplot2::aes(x = time, y = hv_rate)) +
    ggplot2::geom_line(linewidth = 1.1, colour = "#8B0000") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = "Hypervigilance rate over time",
      subtitle = "High action on no-stressor steps (alive)",
      x = "time",
      y = "hypervigilance rate"
    ) +
    theme_vigilance(base_size = 12) +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$time)))
}


# =============================================================================
# 17) EXAMPLE USAGE â€” QUICK DEMO (optional / typically commented out)
# =============================================================================
# This block demonstrates a minimal end-to-end workflow:
#   1) compute a DP policy (mem_compute_policy),
#   2) simulate agents forward (mem_simulate_agents),
#   3) call plotting helpers.
#
# In production figure scripts, you typically:
#   - do the compute/sim in a pipeline script,
#   - save `sim` objects or data frames,
#   - then source this module and plot.
pol_h <- mem_compute_policy(
  model = "health", K = 3, C = 0, D = 10, d = 0,
  LA = 0.2, LL = 0.3, T_steps = 12,
  states = c("Kd","K","CD","C"), policy_args = list(h0 = 35)
)
sim_h <- mem_simulate_agents(
  model = "health", policy_df = pol_h,
  LA = 0.2, LL = 0.3, T_steps = 12, N_agents = 1000,
  K = 3, C = 0, D = 10, d = 0, sim_args = list(h0 = 35, seed = 42)
)

# # Produce example plots
# print(plot_health_over_time(sim_h))
# print(plot_state_heatmap_health(sim_h, n_agents = 100))
# print(plot_action_heatmap_health(sim_h, n_agents = 100))
# print(plot_hv_rate_over_time(sim_h))


# =============================================================================
# 18) MAIN VISUALISATION â€” ENVIRONMENT Ã— TIME PIE MAP WITH TRANSITION RIBBONS
# =============================================================================
# plot_env_flow_pies(sim)
#
# What it shows:
#   A 2-row Ã— (T+1)-column â€œflowâ€ diagram where:
#     - Rows correspond to environment state:
#         row 1: No stressor (stressor==0)
#         row 2: Stressor     (stressor==1)
#     - Columns correspond to time steps, plus a prior column (â€œ-â€) at t_prior.
#
# Each node (env state Ã— time) is a pie chart showing composition of:
#   - High (vigilant),
#   - Low  (relaxed),
#   - DEAD (dead / no action recorded).
#
# Between nodes are â€œribbonsâ€:
#   - Ribbons connect (env state at time t) -> (env state at time t+1),
#   - Ribbon thickness is proportional to the number of alive agents making that
#     environment transition.
#
# Key design choices:
#   - Pies use ALL rows including dead agents so the DEAD slice appears.
#   - Ribbons use only alive rows (action non-NA) so we donâ€™t show flows â€œto deathâ€
#     as environment transitions.
#
# Dependencies:
#   - ggforce::geom_arc_bar for pie slices as arcs.
#   - Custom polygon generation for ribbons.
# =============================================================================


# ---- Small helper for ribbon polygons ---------------------------------------
# Given:
#   start point (xs, ys), end point (xe, ye)
#   normal direction (nx, ny) (unit normal to the segment)
#   offsets oL and oR defining the ribbon thickness on each side
#   optional start/end shifts along x (for spacing)
#
# Return:
#   A 4-point polygon (rectangle in the segment-normal frame) used by geom_polygon.
.rect_poly <- function(xs, ys, xe, ye, nx, ny, oL, oR, start_shift, end_shift) {
  data.frame(
    x = c(xs + nx*oL + start_shift,
          xe + nx*oL + end_shift,
          xe + nx*oR + end_shift,
          xs + nx*oR + start_shift),
    y = c(ys + ny*oL,
          ye + ny*oL,
          ye + ny*oR,
          ys + ny*oR)
  )
}

# ---- Main function: plot_env_flow_pies --------------------------------------
plot_env_flow_pies <- function(
  sim,
  max_diam = 0.32,       # maximum node diameter in plot units
  band_shrink = 0.90,    # shrink factor to prevent ribbons overlapping node edges
  inner_under = 0.02,    # small offset to keep ribbons slightly under pies
  start_shift = -0.01,   # horizontal shift for ribbon start
  end_shift   = 0.15     # horizontal shift for ribbon end
) {
  # Dependency checks (explicit, so the error messages are clear).
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required for plotting.")
  if (!requireNamespace("ggforce", quietly = TRUE)) stop("ggforce is required (install.packages('ggforce')).")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr is required for data prep.")

  # ---------------------------------------------------------------------------
  # Data preparation: two views
  #   df_all   : all rows, including dead rows (for pie slices)
  #   df_alive : alive-only rows (for transitions/ribbons)
  # ---------------------------------------------------------------------------
  df_all   <- sim$agent_data

  # Alive-only for ribbons: your simulator encodes death as action==NA (and/or alive==FALSE).
  df_alive <- df_all[!is.na(df_all$action), , drop = FALSE]

  stopifnot(all(c("agent","time","stressor","action","state","alive") %in% names(df_all)))

  # ---------------------------------------------------------------------------
  # Define pie slices:
  #   - If action is NA OR state is DEAD OR alive==FALSE -> slice = "DEAD"
  #   - Otherwise slice = action ("High" or "Low")
  # This ensures every node pie has a DEAD slice when deaths accumulate.
  # ---------------------------------------------------------------------------
  df_all$slice <- ifelse(
    is.na(df_all$action) | df_all$state == "DEAD" | df_all$alive == FALSE,
    "DEAD",
    as.character(df_all$action)
  )

  # ---------------------------------------------------------------------------
  # Layout constants:
  #   y_pos maps stressor 0/1 to fixed y coordinates.
  #   y_labels define axis text.
  #   time range gives us prior column position.
  # ---------------------------------------------------------------------------
  y_pos <- c(`0` = 1, `1` = 2)
  y_labels <- c("No stressor", "Stressor")
  t_min <- min(df_all$time, na.rm = TRUE)
  t_max <- max(df_all$time, na.rm = TRUE)

  # Prior column placed one step left of the first observed time.
  t_prior <- t_min - 1L

  # ---------------------------------------------------------------------------
  # Node sizing:
  #   - Count how many rows (agents) exist in each (time, stressor) node.
  #   - Scale radius by sqrt(n_node / n_max) so area ~ count.
  #     (i.e., radius âˆ sqrt(count) gives proportional area perception)
  # ---------------------------------------------------------------------------
  nodes <- df_all |>
    dplyr::count(time, stressor, name = "n_node") |>
    dplyr::mutate(x = time, y = y_pos[as.character(stressor)])

  n_max <- if (nrow(nodes)) max(nodes$n_node, na.rm = TRUE) else 0
  nodes$radius <- if (n_max > 0) (max_diam/2) * sqrt(nodes$n_node / n_max) else (max_diam/10)

  # ---------------------------------------------------------------------------
  # Prior node:
  #   We build a special node at t_prior (between stressor rows) summarizing the
  #   first-time composition.
  #   - y = 1.5 centers between the two environment rows.
  #   - radius scaled the same way as other nodes.
  # ---------------------------------------------------------------------------
  prior_counts <- df_all[df_all$time == t_min, , drop = FALSE]
  n_prior <- nrow(prior_counts)

  prior_node <- data.frame(
    time = t_prior, stressor = NA, n_node = n_prior,
    x = t_prior, y = 1.5,
    radius = if (n_max > 0) (max_diam/2) * sqrt(n_prior / n_max) else (max_diam/10)
  )

  # ---------------------------------------------------------------------------
  # Composition per node:
  #   comp_main: per (time, stressor) counts of each slice (High/Low/DEAD)
  #   comp_prior: similar but for the single prior node (stressor = NA)
  #   prop = n_slice / n_node
  # ---------------------------------------------------------------------------
  comp_main <- df_all |>
    dplyr::count(time, stressor, slice, name = "n_slice") |>
    dplyr::left_join(nodes[, c("time","stressor","n_node")], by = c("time","stressor")) |>
    dplyr::mutate(prop = ifelse(n_node > 0, n_slice / n_node, 0))

  comp_prior <- if (n_prior > 0) {
    pc <- prior_counts |>
      dplyr::count(slice, name = "n_slice")
    pc$time <- t_prior
    pc$stressor <- NA
    pc$n_node <- n_prior
    pc$prop <- pc$n_slice / n_prior
    pc
  } else {
    data.frame(time = integer(), stressor = integer(), slice = character(),
               n_slice = integer(), n_node = integer(), prop = numeric())
  }

  comp_all <- dplyr::bind_rows(comp_main, comp_prior)
  comp_all$slice <- factor(comp_all$slice, levels = c("High","Low","DEAD"))

  # ---------------------------------------------------------------------------
  # Convert slice proportions into arc geometry:
  #   - Within each node, compute cumulative proportions.
  #   - Convert to angles on [0, 2Ï€].
  #   start = 2Ï€*(cum - prop)
  #   end   = 2Ï€*cum
  #
  # Then attach x, y, radius for drawing via ggforce::geom_arc_bar.
  # ---------------------------------------------------------------------------
  pie_df <- comp_all |>
    dplyr::group_by(time, stressor) |>
    dplyr::arrange(time, stressor, slice, .by_group = TRUE) |>
    dplyr::mutate(
      cum = cumsum(prop),
      start = 2*pi*(cum - prop),
      end   = 2*pi*cum
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      rbind(
        nodes[, c("time","stressor","x","y","radius")],
        prior_node[, c("time","stressor","x","y","radius")]
      ),
      by = c("time","stressor")
    )

  # ---------------------------------------------------------------------------
  # Edges / transitions (ribbons):
  #   We only use alive-only rows (df_alive) so we do not draw â€œdeath flowsâ€.
  #
  # Two edge sets:
  #   1) edges_prior: from the prior node "P" -> stressor state at first time.
  #   2) df_trans:    from stressor(t) -> stressor(t+1) for t in [t_min, t_max-1]
  #
  # Each edge record has:
  #   t      : origin time (prior uses t_prior)
  #   s0, s1 : origin and destination environment state labels
  #   n_edge : number of agents making that transition
  # ---------------------------------------------------------------------------
  edges_prior <- if (nrow(df_alive[df_alive$time == t_min, , drop = FALSE])) {
    df_alive[df_alive$time == t_min, , drop = FALSE] |>
      dplyr::count(stressor, name = "n_edge") |>
      dplyr::mutate(t = t_prior, s0 = "P", s1 = as.character(stressor)) |>
      dplyr::select(t, s0, s1, n_edge)
  } else {
    dplyr::tibble(t = integer(), s0 = character(), s1 = character(), n_edge = integer())
  }

  # Compute within-agent consecutive stressor states.
  df_trans <- df_alive |>
    dplyr::arrange(agent, time) |>
    dplyr::group_by(agent) |>
    dplyr::mutate(s0 = stressor, s1 = dplyr::lead(stressor)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(s1)) |>
    dplyr::count(time, s0, s1, name = "n_edge") |>
    dplyr::transmute(t = time, s0 = as.character(s0), s1 = as.character(s1), n_edge)

  edges <- dplyr::bind_rows(edges_prior, df_trans)

  # ---------------------------------------------------------------------------
  # Attach geometry to edges:
  #   We need (xs,ys,r0,n0) for origins and (xe,ye,...) for destinations.
  #
  # nodes_aug includes a pseudo-environment "P" for the prior node.
  # dest_nodes shifts time by -1 so that edges at time t map to destination node at time t+1.
  # ---------------------------------------------------------------------------
  nodes_aug <- rbind(
    transform(nodes,
              env = as.character(stressor), t = time,
              x0 = x, y0 = y, r0 = radius, n0 = n_node)[, c("t","env","x0","y0","r0","n0")],
    data.frame(t = t_prior, env = "P",
               x0 = t_prior, y0 = 1.5,
               r0 = prior_node$radius, n0 = prior_node$n_node)
  )

  dest_nodes <- transform(nodes,
                          env = as.character(stressor),
                          t = time - 1L,
                          x1 = x, y1 = y, r1 = radius, n1 = n_node)[, c("t","env","x1","y1","r1","n1")]

  edges_geom <- edges |>
    dplyr::left_join(nodes_aug,  by = c("t" = "t", "s0" = "env")) |>
    dplyr::left_join(dest_nodes, by = c("t" = "t", "s1" = "env")) |>
    dplyr::mutate(
      # Thickness proportional to fraction of origin node represented by this transition.
      # (2*r0) is node diameter; multiply by n_edge/n0 gives a band that "fits" around node.
      thick_total = (2 * r0) * (n_edge / pmax(n0, 1L)),
      thick_draw  = band_shrink * thick_total,

      xs = x0, ys = y0,
      xe = x1, ye = y1
    ) |>
    dplyr::filter(is.finite(xs), is.finite(ys), is.finite(xe), is.finite(ye), thick_draw > 0)

  # ---------------------------------------------------------------------------
  # Ribbon stacking logic:
  #   For each origin node (t, s0), multiple outgoing transitions exist.
  #   We:
  #     - compute a unit normal (nx, ny) to the segment direction,
  #     - assign each outgoing band a non-overlapping offset interval [u_right, u_left]
  #       so bands stack side-by-side around the node.
  #
  # The ordering (arrange) is a heuristic so bands are visually stable.
  # ---------------------------------------------------------------------------
  route <- edges_geom |>
    dplyr::group_by(t, s0, s1) |>
    dplyr::summarise(
      xs = dplyr::first(xs), ys = dplyr::first(ys),
      xe = dplyr::first(xe), ye = dplyr::first(ye),
      r0 = dplyr::first(r0),
      thick_draw = dplyr::first(thick_draw),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dx = xe - xs, dy = ye - ys,
      L = sqrt(dx^2 + dy^2),
      nx = ifelse(L > 0, -dy / L, 0),
      ny = ifelse(L > 0,  dx / L, 0)
    ) |>
    dplyr::group_by(t, s0) |>
    dplyr::arrange(dplyr::if_else(s0 == "1", -ye, ye), .by_group = TRUE) |>
    dplyr::mutate(
      cum     = cumsum(thick_draw),

      # u_right/u_left define the band position across the node radius.
      # The if_else(s0=="1") flip just keeps ordering consistent between rows.
      u_right = dplyr::if_else(s0 == "1", r0 - cum, -r0 + (cum - thick_draw)),
      u_left  = u_right + thick_draw
    ) |>
    dplyr::ungroup()

  # Build polygon definitions for each ribbon band.
  tmp <- route |>
    dplyr::transmute(
      poly_id = dplyr::row_number(),
      xs, ys, xe, ye, nx, ny,
      oL = u_left  - inner_under,
      oR = u_right - inner_under
    )

  # NOTE: you redefine .rect_poly below; it matches the helper above.
  # Keeping it here makes plot_env_flow_pies self-contained.
  .rect_poly <- function(xs, ys, xe, ye, nx, ny, oL, oR, start_shift, end_shift) {
    data.frame(
      x = c(xs + nx*oL + start_shift,
            xe + nx*oL + end_shift,
            xe + nx*oR + end_shift,
            xs + nx*oR + start_shift),
      y = c(ys + ny*oL,
            ye + ny*oL,
            ye + ny*oR,
            ys + ny*oR)
    )
  }

  poly_list <- Map(
    f = function(pid, xs, ys, xe, ye, nx, ny, oL, oR) {
      pg <- .rect_poly(xs, ys, xe, ye, nx, ny, oL, oR, start_shift, end_shift)
      pg$poly_id <- pid
      pg
    },
    pid = tmp$poly_id,
    xs = tmp$xs, ys = tmp$ys, xe = tmp$xe, ye = tmp$ye,
    nx = tmp$nx, ny = tmp$ny, oL = tmp$oL, oR = tmp$oR
  )
  poly_df <- dplyr::bind_rows(poly_list)

  # ---------------------------------------------------------------------------
  # Palettes:
  #   - Prefer pal_actions (if defined globally) for High/Low.
  #   - DEAD uses pal_states_health["DEAD"] if available.
  #   - fills is used for the pie slices (action/status legend).
  # ---------------------------------------------------------------------------
  pal_actions_local <- if (exists("pal_actions")) pal_actions else c(High = "#e41a1c", Low = "#377eb8")
  pal_dead <- if (exists("pal_states_health")) pal_states_health["DEAD"] else "#666666"
  fills <- c(pal_actions_local, DEAD = unname(pal_dead))

  # ---------------------------------------------------------------------------
  # Axes: include prior tick labeled "-"
  # ---------------------------------------------------------------------------
  x_breaks <- c(t_prior, seq.int(t_min, t_max))
  x_labels <- c("-", as.character(seq.int(t_min, t_max)))

  # ---------------------------------------------------------------------------
  # Final plot assembly:
  #   - ribbons as grey polygons (behind pies),
  #   - pies as arc bars,
  #   - fixed y rows, time on x,
  #   - coord_equal to keep circles circular.
  # ---------------------------------------------------------------------------
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = poly_df,
      ggplot2::aes(x, y, group = poly_id),
      fill = "grey75", colour = NA, alpha = 0.9
    ) +
    ggforce::geom_arc_bar(
      data = pie_df,
      ggplot2::aes(
        x0 = x, y0 = y,
        r0 = 0, r = radius,
        start = start, end = end,
        fill = slice
      ),
      colour = NA
    ) +
    ggplot2::scale_fill_manual(values = fills, drop = FALSE, name = "action / status") +
    ggplot2::scale_y_continuous(breaks = c(1, 2), labels = y_labels) +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0.05, 0.05)) +
    ggplot2::coord_equal(
      xlim = c(t_prior - 0.6, t_max + 0.6),
      ylim = c(0.5, 2.5),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = "Actions by environment and time (with transitions & death slice)",
      x = "time",
      y = "environment"
    ) +
    (if (exists("theme_clean_minimal")) theme_clean_minimal() else ggplot2::theme_minimal(base_size = 12)) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "top")
}

# =============================================================================
# Summary notes (human-readable)
# =============================================================================
# - plot_health_over_time():
#     Aggregates health and survival per time step; shows mean/median/IQR + alive fraction.
# - plot_state_heatmap_health():
#     Shows the discrete state path for many agents; includes DEAD.
# - plot_action_heatmap_health():
#     Shows High/Low decisions for many agents (dead rows appear as missing).
# - plot_hv_rate_over_time():
#     Computes hv_rate = (# hv among alive) / (# no-stressor among alive) per time.
# - plot_env_flow_pies():
#     Builds an environment-by-time flow diagram:
#       pies encode action/status composition (High/Low/DEAD),
#       ribbons encode stressor-state transitions for alive agents.
# =============================================================================

