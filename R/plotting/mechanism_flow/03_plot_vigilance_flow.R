# ============================================================
# File: R/plotting/vigilance_flow/03_plot_vigilance_flow.R
#
# Purpose: Pure helpers for the vigilance-flow panels used in Fig4B.
# Notes:
#   - Runs the cached DP/policy + agent simulation per (K, LA, LL) scenario.
#   - Builds pies + ribbon polygons so the consuming script only saves the ggplot.
# ============================================================

# ---- Dependencies ----------------------------------------------------------
# These sources provide *repo-local* functionality that this module builds on.
# Keeping them here (rather than in scripts) makes the plotting helpers portable.
source("R/core/plot_utils.R")              # theme_vigilance(), theme helpers, grid constants
source("R/models/basic/basic_model_dp.R")  # mem_compute_policy() - cached DP solver (basic)
source("R/models/basic/basic_model_SIM.R") # mem_simulate_agents() - cached simulation (basic)
source("R/models/health/health_model_dp.R")# mem_compute_policy() - cached DP solver (health)
source("R/models/health/health_model_SIM.R")# mem_simulate_agents() - cached simulation (health)

suppressPackageStartupMessages({
  library(dplyr)      # mutate, filter, join, count, summarise
  library(tidyr)      # complete(), pivot_longer(), replace_na()
  library(ggplot2)    # ggplot objects + layers
  library(ggforce)    # geom_arc_bar() for pie-slices
  library(ggnewscale) # new_scale_fill() so ribbons and pies can have separate legends
})

# =============================================================================
# Internal helper: .rect_poly()
# =============================================================================
# A "ribbon" in this figure is drawn as a 4-vertex polygon connecting:
#   - a start point (xs, ys) on the origin node
#   - an end point   (xe, ye) on the destination node
# The polygon's thickness is controlled by offsets (oL, oR) applied along the
# *normal* direction (nx, ny) to the route (xs,ys)->(xe,ye).
#
# The offsets are precomputed elsewhere; this helper only translates "centerline"
# geometry + offsets into a closed polygon.
#
# Kept private (leading dot) because it is tightly coupled to this layoutâ€™s math.
.rect_poly <- function(xs, ys, xe, ye, nx, ny, oL, oR, start_shift, end_shift) {
  data.frame(
    # Note the x "shift" terms:
    #   - start_shift nudges polygons slightly left at the origin
    #   - end_shift   nudges polygons slightly right at the destination
    # This creates a visible gap between node pies and ribbons so edges look cleaner.
    x = c(xs + nx * oL + start_shift,
          xe + nx * oL + end_shift,
          xe + nx * oR + end_shift,
          xs + nx * oR + start_shift),
    y = c(ys + ny * oL,
          ye + ny * oL,
          ye + ny * oR,
          ys + ny * oR)
  )
}

# =============================================================================
# plot_vigilance_flow()
# =============================================================================
# This is the main â€œsingle panelâ€ builder:
#   1) compute the optimal policy for the scenario (via cached DP)
#   2) simulate N agents forward under that policy (via cached simulation)
#   3) summarise the simulation into:
#        - node sizes (how many agents in env=0/1 at each time)
#        - pie slices (share relaxed vs vigilant, derived from State labels)
#        - ribbons (agent transitions between env states over time, split into
#          three "bands" depending on what state the agent moves into)
#   4) draw:
#        - ribbons first (background)
#        - pies second (foreground)
#        - separate legends for ribbons and pies using ggnewscale
#
# Interpreting the plot:
#   - y-axis has two rows:
#       0 = no stressor, 1 = stressor
#   - each time step has up to two nodes (one per env row)
#   - node radius âˆ sqrt(n agents) so area corresponds to agent counts
#   - pie slices show relaxed vs vigilant composition within that node
#   - ribbons show flows between env rows (and are shaded by â€œbandâ€ type)
#
# Band semantics (as coded in cat_band()):
#   - "light": transitions into C/CD/Kd (non-K-ish outcomes; often â€œless vigilant/less harmfulâ€ states)
#   - "mid"  : transitions into K from C/CD (a â€œswitch into vigilanceâ€ pattern)
#   - "dark" : transitions to/within K (vigilant persistence or continued K-like)
#
# @return ggplot object for the scenario.
# @export
plot_vigilance_flow <- function(
    K, C, D, d,
    LA, LL,
    T_steps,
    states,
    N_agents,
    model = c("basic", "health"),
    policy_args = list(),
    sim_args    = list(),
    max_diam   = 0.32,
    band_shrink = 0.9,
    inner_under = 0.02,
    start_shift = -0.01,
    end_shift = 0.15
) {
  # ---- Validate + normalize model choice -----------------------------------
  model <- match.arg(model, c("basic", "health"))
  # mem_compute_policy / mem_simulate_agents expect "basic" or "health"
  model_key <- if (model == "basic") "basic" else "health"

  # ---- Step 1: policy computation (cached) ---------------------------------
  # This should be deterministic given:
  #   (model_key, K,C,D,d, LA,LL, T_steps, states, policy_args)
  # and because it is cached, repeated calls in pipelines are cheap.
  policy_df <- mem_compute_policy(
    model = model_key, K = K, C = C, D = D, d = d,
    LA = LA, LL = LL, T_steps = T_steps, states = states,
    policy_args = policy_args
  )

  # ---- Step 2: forward simulation (cached) ---------------------------------
  # Returns a list that includes sim$agent_data (long panel of agent-time rows).
  sim <- mem_simulate_agents(
    model = model_key,
    policy_df = policy_df,
    LA = LA, LL = LL, T_steps = T_steps, N_agents = N_agents,
    K = K, C = C, D = D, d = d,
    sim_args = sim_args
  )

  # ---- Step 3: standardise column names ------------------------------------
  # Different SIM modules sometimes use different capitalization conventions.
  # This block normalises to Agent/Time/Stressor/State so plotting code is stable.
  panel <- sim$agent_data
  if (!"Agent" %in% names(panel) && "agent" %in% names(panel)) panel$Agent <- panel$agent
  if (!"Time"  %in% names(panel) && "time"  %in% names(panel)) panel$Time  <- panel$time
  if (!"Stressor" %in% names(panel) && "stressor" %in% names(panel)) panel$Stressor <- panel$stressor
  if (!"State" %in% names(panel) && "state" %in% names(panel)) panel$State <- panel$state
  panel <- panel %>% dplyr::select(Agent, Time, Stressor, State)

  # ---- Plot constants / style ----------------------------------------------
  # We place the two environments on fixed y positions so ribbons have stable geometry.
  y_position <- c("0" = 0.15, "1" = 0.85)

  # Action colors (pies)
  color_relaxed  <- "#59a89c"
  color_vigilant <- "#e02b35"

  # Ribbon band colors (background)
  color_light <- "grey75"
  color_mid   <- "grey55"
  color_dark  <- "grey25"

  # ==========================================================================
  # Step 4: Build node table (counts + pie components)
  # ==========================================================================
  # nodes represent each (Time, env) combination.
  # - n_node: how many agent rows are in that environment at that time
  # - n_relaxed: count of relaxed-coded states (C/CD) in that node
  # - n_vigilant: remainder
  #
  # Important: â€œrelaxed vs vigilantâ€ is inferred from State, not Action.
  # Here the operational definition is:
  #   relaxed if State âˆˆ {C, CD}, else vigilant (including K, Kd, etc.)
  nodes <- panel %>%
    mutate(env = as.character(Stressor)) %>%
    count(Time, env, name = "n_node") %>%
    left_join(
      panel %>%
        mutate(env = as.character(Stressor)) %>%
        filter(State %in% c("C", "CD")) %>%
        count(Time, env, name = "n_relaxed"),
      by = c("Time", "env")
    ) %>%
    mutate(
      # complete() below creates zeros; join introduces NAs for missing relaxed counts
      n_relaxed  = tidyr::replace_na(n_relaxed, 0L),
      n_vigilant = n_node - n_relaxed
    ) %>%
    # Ensure every time step has both env rows (0 and 1),
    # so the layout remains a consistent 2-row grid even if nobody visits env=1.
    tidyr::complete(
      Time = sort(unique(panel$Time)),
      env = c("0", "1"),
      fill = list(n_node = 0L, n_relaxed = 0L, n_vigilant = 0L)
    ) %>%
    mutate(
      # Radius scales with sqrt(count) so the node *area* is proportional to n_node.
      radius = if (max(n_node) > 0) (max_diam / 2) * sqrt(n_node / max(n_node)) else 0,
      x = Time,
      y = unname(y_position[env])
    )

  # ==========================================================================
  # Step 5: Build transition table (agent-level edges)
  # ==========================================================================
  # We want transitions from time t to t+1.
  # For each agent, we compute:
  #   - origin env/state at t  (s0/st0)
  #   - destination env/state at t+1 (s1/st1)
  # Then we aggregate counts per (t, s0, s1) and also split into â€œbandâ€ classes.
  transitions <- panel %>%
    arrange(Agent, Time) %>%
    group_by(Agent) %>%
    mutate(
      t  = Time,
      t1 = dplyr::lead(Time),
      s0 = as.character(Stressor),
      s1 = as.character(dplyr::lead(Stressor)),
      st0 = State,
      st1 = dplyr::lead(State)
    ) %>%
    ungroup() %>%
    # Keep only valid consecutive steps (no gaps, no final NA).
    filter(!is.na(t1), t1 == t + 1)

  # ---- Helper: classify transition "band" ----------------------------------
  # This is a *visual encoding* choice: it lets you see different transition
  # â€œtypesâ€ in the background ribbons. The rule is based on the destination state
  # (and sometimes origin state).
  cat_band <- function(st0, st1) {
    # light: destination is C, CD, or Kd
    if (st1 %in% c("C", "CD", "Kd")) "light"
    # mid: arriving at K specifically from a relaxed state (C/CD)
    else if (st1 == "K" && st0 %in% c("C", "CD")) "mid"
    # dark: everything else (e.g., staying in K, moving into K from non-relaxed)
    else "dark"
  }

  # Aggregate transitions to counts per band.
  # n_band: count for this band
  # n_edge: total count for the route (t,s0,s1), summed across bands
  edges <- transitions %>%
    mutate(band = mapply(cat_band, st0, st1, USE.NAMES = FALSE)) %>%
    count(t, s0, s1, band, name = "n_band") %>%
    group_by(t, s0, s1) %>%
    mutate(n_edge = sum(n_band)) %>%
    ungroup() %>%
    filter(n_band > 0, n_edge > 0)

  # ==========================================================================
  # Step 6: Attach geometry to edges (origin/destination coordinates)
  # ==========================================================================
  # For each edge (t,s0,s1):
  #   - origin node is at time t, env s0
  #   - destination node is at time t+1, env s1
  # Because nodes store (Time, env), we join them twice:
  #   - origin join directly on (t == Time, s0 == env)
  #   - destination join uses Time-1 trick so we can join on current t:
  #       destination node has Time = t+1
  #       so we store it as t = Time-1 for matching
  edges_geom <- edges %>%
    left_join(
      nodes %>%
        select(Time, env, x0 = x, y0 = y, r0 = radius, n0 = n_node) %>%
        rename(t = Time, s0 = env),
      by = c("t", "s0")
    ) %>%
    left_join(
      nodes %>%
        select(Time, env, x1 = x, y1 = y, r1 = radius, n1 = n_node) %>%
        transmute(t = Time - 1L, s1 = env, x1, y1, r1, n1),
      by = c("t", "s1")
    ) %>%
    mutate(
      # Total drawable thickness allocated to this route, relative to node diameter.
      # thick_total is proportional to (route share of nodeâ€™s outgoing mass) Ã— (node diameter).
      thick_total = (2 * r0) * (n_edge / pmax(n0, 1L)),

      # shrink overall thickness slightly (visual padding between stacked ribbons)
      thick_draw = band_shrink * thick_total,

      # Split thickness across bands proportionally.
      thick_band = thick_total * (n_band / n_edge),
      thick_band_draw = thick_draw * (n_band / n_edge),

      # Convenience renames: start/end coordinates for centerline.
      xs = x0, ys = y0, xe = x1, ye = y1
    )

  # ==========================================================================
  # Step 7: Compute stacking offsets so ribbons donâ€™t overlap
  # ==========================================================================
  # The â€œstackingâ€ problem:
  #   multiple routes leave the same origin node (t,s0), and they all need a
  #   distinct vertical offset so their polygons donâ€™t overlap.
  #
  # Approach:
  #   - compute a normal vector (nx,ny) for each route
  #   - for each origin node group (t,s0), stack routes along the normal direction
  #   - store left/right offsets (u_left, u_right) describing the band boundaries
  #
  route_offsets <- edges_geom %>%
    group_by(t, s0, s1) %>%
    summarise(
      xs = dplyr::first(xs), ys = dplyr::first(ys),
      xe = dplyr::first(xe), ye = dplyr::first(ye),
      r0 = dplyr::first(r0),
      thick_draw = dplyr::first(thick_draw),
      .groups = "drop"
    ) %>%
    mutate(
      # Route direction vector
      dx = xe - xs,
      dy = ye - ys,
      L  = sqrt(dx^2 + dy^2),

      # Unit normal vector (perpendicular to the route)
      # Using (-dy, dx) gives a 90Â° rotation; divide by length for unit vector.
      nx = ifelse(L > 0, -dy / L, 0),
      ny = ifelse(L > 0,  dx / L, 0)
    ) %>%
    group_by(t, s0) %>%
    # Order routes so the stacking is stable and visually consistent.
    # If s0=="1" (top row), we reverse the order so â€œupperâ€ routes appear in a consistent direction.
    arrange(if_else(s0 == "1", -ye, ye), .by_group = TRUE) %>%
    mutate(
      cum = cumsum(thick_draw),

      # Offsets define the *outer boundary* of each stacked ribbon at the origin.
      # These formulas place ribbons within [-r0, +r0] around the node.
      u_right = if_else(s0 == "1", r0 - cum, -r0 + (cum - thick_draw)),
      u_left  = u_right + thick_draw
    ) %>%
    ungroup() %>%
    # Only keep what we need downstream; rename to avoid collisions when joining back.
    select(-r0, -thick_draw, -dx, -dy, -L) %>%
    rename_with(~ paste0(.x, "_offset"), c(xs, ys, xe, ye, nx, ny, u_left, u_right))

  # ==========================================================================
  # Step 8: Expand route offsets into band-specific offsets
  # ==========================================================================
  # Now we have â€œroute-levelâ€ offsets for each (t,s0,s1), but each route may have
  # 1â€“3 bands (light/mid/dark). We want *band-level* polygons stacked within the route.
  #
  # Strategy:
  #   - join route offsets back to edges_geom
  #   - within each route group, stack bands in a fixed order (lightâ†’midâ†’dark)
  #   - compute band-specific o_left/o_right offsets
  edges_banded <- edges_geom %>%
    left_join(
      route_offsets %>% select(t, s0, s1, ends_with("_offset")),
      by = c("t", "s0", "s1")
    ) %>%
    mutate(
      # bring back the route geometry fields under standard names
      xs = xs_offset, ys = ys_offset,
      xe = xe_offset, ye = ye_offset,
      nx = nx_offset, ny = ny_offset,

      # start from route-level left/right boundaries
      o_left  = u_left_offset,
      o_right = u_right_offset
    ) %>%
    group_by(t, s0, s1) %>%
    arrange(factor(band, levels = c("light", "mid", "dark")), .by_group = TRUE) %>%
    mutate(
      # cumulative stacking inside the route:
      cum_sub = cumsum(thick_band_draw),

      # band spans [o_right, o_left] in normal-offset space
      o_right = u_right_offset + (cum_sub - thick_band_draw),
      o_left  = u_right_offset + cum_sub
    ) %>%
    ungroup() %>%
    filter(thick_band_draw > 0)

  # Safety net: recompute normals if they ended up missing or non-finite after joins.
  if (!"nx" %in% names(edges_banded) || !"ny" %in% names(edges_banded) ||
      any(!is.finite(edges_banded$nx)) || any(!is.finite(edges_banded$ny))) {
    dx <- edges_banded$xe - edges_banded$xs
    dy <- edges_banded$ye - edges_banded$ys
    L  <- sqrt(dx^2 + dy^2)
    edges_banded$nx <- ifelse(L > 0, -dy / L, 0)
    edges_banded$ny <- ifelse(L > 0,  dx / L, 0)
  }

  # ==========================================================================
  # Step 9: Materialize polygon vertices for ggplot
  # ==========================================================================
  # ribbon_df: one row per band polygon, with its geometric parameters
  ribbon_df <- edges_banded %>%
    transmute(
      poly_id = dplyr::row_number(),
      band,
      xs, ys, xe, ye, nx, ny,
      # subtract inner_under so ribbons appear tucked slightly behind pies
      o_left  = o_left  - inner_under,
      o_right = o_right - inner_under
    )

  # Build the actual vertices using .rect_poly() for each ribbon polygon.
  polygons <- Map(
    f = function(pid, b, xs, ys, xe, ye, nx, ny, oL, oR) {
      pg <- .rect_poly(xs, ys, xe, ye, nx, ny, oL, oR, start_shift, end_shift)
      pg$poly_id <- pid
      pg$band <- b
      pg
    },
    pid = ribbon_df$poly_id,
    b   = ribbon_df$band,
    xs  = ribbon_df$xs,
    ys  = ribbon_df$ys,
    xe  = ribbon_df$xe,
    ye  = ribbon_df$ye,
    nx  = ribbon_df$nx,
    ny  = ribbon_df$ny,
    oL  = ribbon_df$o_left,
    oR  = ribbon_df$o_right
  )
  poly_df <- dplyr::bind_rows(polygons)
  poly_df$band <- factor(poly_df$band, levels = c("light", "mid", "dark"))

  # ==========================================================================
  # Step 10: Build pie-slice geometry
  # ==========================================================================
  # For each node, compute relaxed/vigilant proportions, then convert to arc angles.
  pie_df <- nodes %>%
    filter(n_node > 0) %>%
    transmute(
      Time, env, x, y, r = radius,
      p_relaxed  = n_relaxed / n_node,
      p_vigilant = n_vigilant / n_node
    ) %>%
    pivot_longer(
      cols = c(p_relaxed, p_vigilant),
      names_to = "slice",
      values_to = "p"
    ) %>%
    mutate(
      slice = ifelse(slice == "p_relaxed", "relaxed", "vigilant")
    ) %>%
    group_by(Time, env) %>%
    arrange(slice, .by_group = TRUE) %>%
    mutate(
      cum   = cumsum(p),
      start = 2 * pi * (cum - p),
      end   = 2 * pi * cum
    ) %>%
    ungroup()

  # ==========================================================================
  # Step 11: Draw plot (ribbons first, then pies)
  # ==========================================================================
  ggplot() +
    # ---- Background ribbons -------------------------------------------------
    geom_polygon(
      data = poly_df,
      aes(x, y, group = poly_id, fill = band),
      colour = NA
    ) +
    scale_fill_manual(
      name = "transitions",
      values = c(light = color_light, mid = color_mid, dark = color_dark),
      breaks = c("light", "mid", "dark"),
      labels = c("to C/CD/Kd", "from C/CD to K", "to/within K")
    ) +

    # Reset fill so pies get their own scale + legend (otherwise ggplot merges legends).
    ggnewscale::new_scale_fill() +

    # ---- Foreground pies ----------------------------------------------------
    ggforce::geom_arc_bar(
      data = pie_df,
      aes(x0 = x, y0 = y, r0 = 0, r = r, start = start, end = end, fill = slice),
      colour = NA
    ) +
    scale_fill_manual(
      name = "agent action",
      values = c(relaxed = color_relaxed, vigilant = color_vigilant),
      breaks = c("relaxed", "vigilant"),
      labels = c("Low vigilance", "High vigilance")
    ) +

    # ---- Labels / axes ------------------------------------------------------
    labs(
      title = "Agent actions and transitions over time",
      x = "time",
      y = "environment"
    ) +
    scale_y_continuous(
      breaks = c(0.15, 0.85),
      labels = c("no stressor", "stressor")
    ) +

    # ---- Geometry / aspect --------------------------------------------------
    # Fixed aspect ratio ensures circles look like circles, not ellipses.
    coord_fixed(
      xlim = c(min(nodes$Time) - 0.6, max(nodes$Time) + 0.6),
      ylim = c(-0.05, 1.05),
      expand = FALSE
    ) +

    # ---- Theme --------------------------------------------------------------
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      legend.position = "right",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# =============================================================================
# plot_vigilance_flow_grid()
# =============================================================================
# Batch helper that returns a list of ggplots keyed by scenario.
#
# Important behavior:
#   - LA_values and LL_values are paired by index (vectorized â€œenvironment listâ€)
#   - K_values is crossed with those pairs (so total panels = length(LA_values)*length(K_values))
#   - Each panel is created by calling plot_vigilance_flow() once (policy+sim cached)
#
# @return named list of ggplots (scripts can arrange/save as desired)
# @export
plot_vigilance_flow_grid <- function(
    K_values, LA_values, LL_values,
    C, D, d, T_steps, states, N_agents,
    model = c("basic", "health"),
    policy_args = list(),
    sim_args = list(),
    max_diam = 0.30,
    band_shrink = 0.9,
    inner_under = 0.02,
    start_shift = -0.01,
    end_shift = 0.15
) {
  # Ensure LA/LL are paired correctly.
  stopifnot(length(LA_values) == length(LL_values))

  # Normalize model string (for plot_vigilance_flow()).
  model <- match.arg(model, c("basic", "health"))

  # Local closure that produces one panel plot (keeps loop clean).
  make_panel <- function(K, LA, LL) {
    plot_vigilance_flow(
      K = K,
      C = C,
      D = D,
      d = d,
      LA = LA,
      LL = LL,
      T_steps = T_steps,
      states = states,
      N_agents = N_agents,
      model = model,
      policy_args = policy_args,
      sim_args = sim_args,
      max_diam = max_diam,
      band_shrink = band_shrink,
      inner_under = inner_under,
      start_shift = start_shift,
      end_shift = end_shift
    ) +
      # Consumers usually add a shared title outside the grid,
      # so suppress individual panel titles.
      ggtitle(NULL)
  }

  # Build plots as a named list so downstream scripts can:
  #   - pick a subset by key
  #   - order panels deterministically
  #   - feed into patchwork/cowplot for arrangement
  plots <- list()
  for (i in seq_along(LA_values)) {
    for (K in K_values) {
      key <- paste0("LA=", LA_values[i], "_LL=", LL_values[i], "_K=", K)
      plots[[key]] <- make_panel(K, LA_values[i], LL_values[i]) +
        labs(subtitle = paste0("K=", K, " | LA=", LA_values[i], ", LL=", LL_values[i]))
    }
  }
  plots
}

# =============================================================================
# Transformation summary (high-level)
# =============================================================================
# - Step C1: Normalize selected model (basic vs health) and ensure policy inputs are defined.
# - Step C2: Compute the optimal policy (cached DP) and replay agents (cached simulation).
# - Step C3: Summarise simulation into node counts per (time, env) and infer relaxed/vigilant shares via State.
# - Step C4: Derive agent transitions (t->t+1), aggregate into routes and band types.
# - Step C5: Compute route normals + stacking offsets so ribbons are non-overlapping and stable.
# - Step C6: Convert offsets into polygon vertex tables and compute pie arc angles.
# - Step C7: Draw ribbons (first fill scale) then pies (second fill scale) with separate legends.

