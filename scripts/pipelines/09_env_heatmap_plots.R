#!/usr/bin/env Rscript
# =============================================================================
# Script: scripts/pipelines/09_env_heatmap_plots.R
# Purpose: Build and save environment heatmap figures with side key + legend.
# Key change: facets are made physically large by sizing the output device
#             based on a target facet size (inches).
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
  library(cowplot)
  library(patchwork)
})

source("R/plotting/env_heatmaps/03_plot_env_heatmaps.R")

.env_axis_breaks <- function(max_val = 0.5, by = 0.1) seq(0, max_val, by = by)

# Helper: call plot_env_heatmaps with optional args (in case another version masks it)
.call_plot_env_heatmaps <- function(real_data, x_label, y_label, add_ssp_boundaries, add_region_labels) {
  fn <- get("plot_env_heatmaps", mode = "function")
  fml <- names(formals(fn))
  args <- list(real_data = real_data)
  if ("x_label" %in% fml) args$x_label <- x_label
  if ("y_label" %in% fml) args$y_label <- y_label
  if ("add_ssp_boundaries" %in% fml) args$add_ssp_boundaries <- add_ssp_boundaries
  if ("add_region_labels" %in% fml) args$add_region_labels <- add_region_labels
  do.call(fn, args)
}

ENV_HEATMAP_STYLE <- list(
  strip_text_size   = 26,
  axis_title_size   = 30,
  axis_text_size    = 30,
  key_axis_text     = 28,
  key_axis_title    = 30,
  key_region_text   = 10,
  legend_title_size = 24,
  legend_text_size  = 22
)

# -----------------------------------------------------------------------------#
# Helper: bump region label text size in key plot WITHOUT editing build_env_key_panel()
# -----------------------------------------------------------------------------#
.bump_key_region_labels <- function(p, new_size = 10) {
  if (!inherits(p, "ggplot")) return(p)
  if (length(p$layers) == 0) return(p)

  for (i in seq_along(p$layers)) {
    geom_name <- tryCatch(class(p$layers[[i]]$geom)[1], error = function(e) NA_character_)
    if (identical(geom_name, "GeomText") || identical(geom_name, "GeomLabel")) {
      p$layers[[i]]$aes_params$size <- new_size
    }
  }
  p
}

# -----------------------------------------------------------------------------#
# Helper: robust legend extraction (works for ggplot and patchwork)
# -----------------------------------------------------------------------------#
.extract_legend_safe <- function(p) {
  # If it's patchwork, collect guides then extract
  if (inherits(p, "patchwork")) {
    p_leg <- p + plot_layout(guides = "collect") & theme(legend.position = "bottom")
    g <- patchwork::patchworkGrob(p_leg)
    guides <- tryCatch(
      cowplot::get_plot_component(g, "guide-box", return_all = TRUE),
      error = function(e) NULL
    )
  } else {
    p_leg <- p + theme(legend.position = "bottom")
    guides <- tryCatch(
      cowplot::get_plot_component(p_leg, "guide-box", return_all = TRUE),
      error = function(e) NULL
    )
  }

  if (is.null(guides) || length(guides) == 0) return(NULL)

  non_empty <- vapply(guides, function(x) {
    n <- tryCatch(length(x$grobs), error = function(e) 0L)
    n > 1L
  }, logical(1))

  if (any(non_empty)) {
    idx <- which(non_empty)
    return(guides[[idx[length(idx)]]])  # last non-empty
  }

  guides[[length(guides)]]
}

# -----------------------------------------------------------------------------#
# NEW: choose output dimensions so each facet is physically large
# (all units in inches; tune facet_size_in to "way larger")
# -----------------------------------------------------------------------------#
.calc_env_plot_dims <- function(
  nK,
  facet_size_in = 3.2,     # increase this for "way larger" facets (e.g., 3.6 or 4.0)
  gap_in        = 0.20,
  right_pad_in  = 0.18,
  title_band_in = 0.55,
  legend_band_in= 0.95,
  outer_pad_in  = 0.10     # small breathing room
) {
  key_size_in <- facet_size_in  # key ≈ one facet

  width_in  <- (nK * facet_size_in) + gap_in + key_size_in + right_pad_in + outer_pad_in
  height_in <- title_band_in + facet_size_in + legend_band_in + outer_pad_in

  list(
    width_in       = width_in,
    height_in      = height_in,
    facet_size_in  = facet_size_in,
    key_size_in    = key_size_in,
    gap_rel        = gap_in / facet_size_in,
    right_pad_rel  = right_pad_in / facet_size_in,
    key_rel        = key_size_in / facet_size_in
  )
}

.calc_env_plot_dims_grid <- function(
  ncol,
  nrow,
  facet_w_in = 2.2,
  facet_h_in = 2.2,
  key_w_in   = 2.2,
  title_in   = 1.0,
  legend_in  = 1.0,
  gap_in     = 0.25,
  outer_in   = 0.25
) {
  width_in  <- (ncol * facet_w_in) + gap_in + key_w_in + outer_in
  height_in <- title_in + (nrow * facet_h_in) + legend_in + outer_in

  list(width_in = width_in, height_in = height_in)
}


# -----------------------------------------------------------------------------#
# Heatmap panel styling
# - If ggplot: safe to add scales/coord
# - If patchwork/composite: DO NOT add scales/coord (breaks panels); use patchwork theme only
# -----------------------------------------------------------------------------#
make_env_heatmap_panel <- function(
  p_faceted,
  axis_max = 0.5,
  axis_by  = 0.1,
  panel_spacing_x_lines = 0.20,
  panel_spacing_y_lines = 0.20,
  right_margin_mm = 2
) {
  th <- theme(
    plot.margin = margin(t = 4, r = right_margin_mm, b = 2, l = 2, unit = "mm"),

    # Facet strips
    strip.text = element_text(size = ENV_HEATMAP_STYLE$strip_text_size, face = "plain", margin = margin(b = 2)),
    strip.text.x = element_text(size = ENV_HEATMAP_STYLE$strip_text_size, face = "plain", margin = margin(b = 2)),
    strip.text.y = element_text(size = ENV_HEATMAP_STYLE$strip_text_size, face = "plain"),
    strip.background = element_blank(),

    # Spacing between facets
    panel.spacing.x = unit(panel_spacing_x_lines, "lines"),
    panel.spacing.y = unit(panel_spacing_y_lines, "lines"),

    # Axes
    axis.text.x  = element_text(size = ENV_HEATMAP_STYLE$axis_text_size, margin = margin(t = 6)),
    axis.text.y  = element_text(size = ENV_HEATMAP_STYLE$axis_text_size),
    axis.title.x = element_text(size = ENV_HEATMAP_STYLE$axis_title_size, face = "plain", margin = margin(t = 10)),
    axis.title.y = element_text(size = ENV_HEATMAP_STYLE$axis_title_size, face = "plain", margin = margin(r = 10)),

    # Panel border
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),

    # Legend typography
    legend.title = element_text(size = ENV_HEATMAP_STYLE$legend_title_size, face = "plain"),
    legend.text  = element_text(size = ENV_HEATMAP_STYLE$legend_text_size),

    # Hide plot-level title/subtitle/caption to match basic layout
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank()
  )

  # Patchwork: just apply theme, don't add coord/scales here
  if (inherits(p_faceted, "patchwork")) return(p_faceted & th)

  # ggplot: set consistent axes + legend format
  p_faceted +
    labs(x = "P(arrive)", y = "P(leave)", title = NULL, subtitle = NULL, caption = NULL) +
    scale_x_continuous(
      breaks = .env_axis_breaks(axis_max, axis_by),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = .env_axis_breaks(axis_max, axis_by),
      expand = expansion(mult = 0)
    ) +
    coord_fixed(
      ratio = 1,
      xlim = c(0, axis_max),
      ylim = c(0, axis_max),
      expand = FALSE,
      clip = "on"
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        barwidth  = unit(14, "cm"),
        barheight = unit(0.9, "cm"),
        frame.colour = "black",
        frame.linewidth = 0.9,
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    ) +
    th
}

# -----------------------------------------------------------------------------#
# Key panel styling (keep square; keep look; region labels bigger)
# -----------------------------------------------------------------------------#
make_env_key_panel <- function(
  df_env,
  k_value,
  D_fallback,
  axis_max = 0.5,
  axis_by  = 0.1,
  left_margin_mm  = 3,
  right_margin_mm = 0
) {
  # build_env_key_panel() is assumed to exist elsewhere in your project
  p <- build_env_key_panel(
    df_env,
    k_value = k_value,
    axis_max_breaks = 4,
    D_fallback = D_fallback
  )

  p <- .bump_key_region_labels(p, new_size = ENV_HEATMAP_STYLE$key_region_text)

  p +
    labs(x = "P(arrive)", y = "P(leave)") +
    scale_x_continuous(
      breaks = .env_axis_breaks(axis_max, axis_by),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = .env_axis_breaks(axis_max, axis_by),
      expand = expansion(mult = 0)
    ) +
    coord_fixed(
      ratio = 1,
      xlim = c(0, axis_max),
      ylim = c(0, axis_max),
      expand = FALSE,
      clip = "on"
    ) +
    theme(
      plot.margin = margin(t = 0, r = right_margin_mm, b = 0, l = left_margin_mm, unit = "mm"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),
      axis.text.x  = element_text(size = ENV_HEATMAP_STYLE$key_axis_text, margin = margin(t = 12)),
      axis.text.y  = element_text(size = ENV_HEATMAP_STYLE$key_axis_text),
      axis.title.x = element_text(
        size = ENV_HEATMAP_STYLE$key_axis_title,
        face = "plain",
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        size = ENV_HEATMAP_STYLE$key_axis_title,
        face = "plain",
        margin = margin(r = 18)
      )
    )
}

# -----------------------------------------------------------------------------#
# Compose: heatmaps + key + legend under HEATMAPS only
# This version works whether heatmap_panel is ggplot OR patchwork.
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# Compose: heatmaps + key + legend under HEATMAPS only
# FIX: Use PATCHWORK for layout (cowplot can drop patchwork/gtables => blank)
# -----------------------------------------------------------------------------#
compose_env_heatmap_with_key_flex <- function(
  heatmap_panel,
  key_panel,
  title = NULL,               # optional external title (usually keep NULL since plot already has one)
  heat_w = 5.5,
  key_w  = 1.11
) {
  # Remove legend from key, keep only from heatmap
  key_panel <- key_panel + theme(legend.position = "none")

  # Put heatmap + key side-by-side, collect legend at bottom
  p <- (heatmap_panel | key_panel) +
    plot_layout(widths = c(heat_w, key_w), guides = "collect") &
    theme(legend.position = "bottom")

  # Optional external title if you want one unified title
  if (!is.null(title)) {
    p <- p + plot_annotation(
      title = title,
      theme = theme(
        plot.title = element_text(size = 26, hjust = 0.5, margin = margin(b = 10))
      )
    )
  }

  p
}

# =============================================================================
# Independent plot builders + savers
# =============================================================================

# --- Build (no saving): env grid heatmap (by model / K facets) ---------------
build_env_grid_heatmap_with_key <- function(
  env_grid,
  D_fallback,
  axis_max = 0.5,
  axis_by  = 0.1,
  facet_size_in = 3.2
) {
  env_heatmap_plot  <- .call_plot_env_heatmaps(
    real_data = env_grid,
    x_label = "P(arrive)",
    y_label = "P(leave)",
    add_ssp_boundaries = FALSE,
    add_region_labels = FALSE
  )
  env_heatmap_panel <- make_env_heatmap_panel(
    env_heatmap_plot,
    axis_max = axis_max,
    axis_by  = axis_by
  )

  env_key_panel <- make_env_key_panel(
    df_env     = env_grid,
    k_value    = min(env_grid$K),
    D_fallback = D_fallback,
    axis_max   = axis_max,
    axis_by    = axis_by
  )

  nK_env   <- length(sort(unique(env_grid$K)))
  nModel   <- length(sort(unique(env_grid$model)))  # <-- adjust to your actual model column name
  dims     <- .calc_env_plot_dims_grid(ncol = nK_env, nrow = nModel, facet_w_in = 2.2, facet_h_in = 2.2)


  composed <- compose_env_heatmap_with_key_flex(
    heatmap_panel = env_heatmap_panel,
    key_panel     = env_key_panel,
    heat_w        = nK_env,
    key_w         = 1.1
  )

  list(plot = composed, dims = dims, nK = nK_env)
}

# --- Save wrapper: env grid heatmap -----------------------------------------
save_env_grid_heatmap_with_key <- function(
  env_grid,
  D_fallback,
  figure_dir = DIR_FIGURES,
  axis_max = 0.5,
  axis_by  = 0.1,
  facet_size_in = 3.2,
  dpi = 450,
  bg  = "white",
  out_stem = "env/env_heatmap_by_model_with_key_side"
) {
  message(">>> Saving environment heatmap (env_grid)")

  res <- build_env_grid_heatmap_with_key(
    env_grid      = env_grid,
    D_fallback    = D_fallback,
    axis_max      = axis_max,
    axis_by       = axis_by,
    facet_size_in = facet_size_in
  )

  print(res$plot)

  save_graphs(
    res$plot,
    file.path(figure_dir, out_stem),
    width  = res$dims$width_in,
    height = res$dims$height_in,
    dpi    = dpi,
    bg     = bg,
    model  = "basic"
  )

  invisible(res)
}

# --- Build (no saving): basic-model-only heatmap -----------------------------
build_basic_env_heatmap_with_key <- function(
  states,
  T_steps,
  N_agents,
  C,
  D,
  d,
  k_values,
  axis_max = 0.5,
  axis_by  = 0.1,
  facet_size_in = 3.2,
  # pipeline knobs (keep defaults you had)
  K = 5,
  grid_step  = 0.025,
  ratio_step = 0.2,
  axis_max_breaks = 4
) {
  basic_env_res <- run_hypervigilance_pipeline(
    model           = "basic",
    states          = states,
    T_steps         = T_steps,
    N_agents        = N_agents,
    K               = K,
    C               = C,
    D               = D,
    d               = d,
    grid_step       = grid_step,
    ratio_step      = ratio_step,
    K_values        = k_values,
    axis_max_breaks = axis_max_breaks
  )

  basic_env_panel <- make_env_heatmap_panel(
    basic_env_res$p_faceted,
    axis_max = axis_max,
    axis_by  = axis_by
  )

  basic_key_panel <- make_env_key_panel(
    df_env     = basic_env_res$df_env2,
    k_value    = min(basic_env_res$df_env2$K),
    D_fallback = D,
    axis_max   = axis_max,
    axis_by    = axis_by
  )

  nK_basic <- length(sort(unique(basic_env_res$df_env2$K)))
  dims     <- .calc_env_plot_dims(nK_basic, facet_size_in = facet_size_in)

  composed <- compose_env_heatmap_with_key_flex(
    heatmap_panel = basic_env_panel,
    key_panel     = basic_key_panel,
    heat_w        = nK_basic,
    key_w         = 1.1
  )

  list(plot = composed, dims = dims, nK = nK_basic, raw = basic_env_res)
}

# --- Save wrapper: basic-model-only heatmap ----------------------------------
save_basic_env_heatmap_with_key <- function(
  states,
  T_steps,
  N_agents,
  C,
  D,
  d,
  k_values,
  figure_dir = DIR_FIGURES,
  axis_max = 0.5,
  axis_by  = 0.1,
  facet_size_in = 3.2,
  dpi = 450,
  bg  = "white",
  out_stem = "env/basic_hypervigilance_heatmap_by_K_with_key"
) {
  message(">>> Saving basic-model-only environment heatmap")

  res <- build_basic_env_heatmap_with_key(
    states        = states,
    T_steps       = T_steps,
    N_agents      = N_agents,
    C             = C,
    D             = D,
    d             = d,
    k_values      = k_values,
    axis_max      = axis_max,
    axis_by       = axis_by,
    facet_size_in = facet_size_in
  )

  print(res$plot)

  save_graphs(
    res$plot,
    file.path(figure_dir, out_stem),
    width  = res$dims$width_in,
    height = res$dims$height_in,
    dpi    = dpi,
    bg     = bg,
    model  = "basic"
  )

  invisible(res)
}

# =============================================================================
# Optional: combined wrapper (still independent under the hood)
# =============================================================================
run_env_heatmap_plots <- function(
  env_grid,
  states,
  T_steps,
  N_agents,
  C,
  D,
  d,
  k_values,
  figure_dir = DIR_FIGURES,
  facet_size_in = 3.2
) {
  message(">>> Saving environment heatmaps (both)")

  res1 <- save_env_grid_heatmap_with_key(
    env_grid      = env_grid,
    D_fallback    = D,
    figure_dir    = figure_dir,
    facet_size_in = facet_size_in
  )

  res2 <- save_basic_env_heatmap_with_key(
    states        = states,
    T_steps       = T_steps,
    N_agents      = N_agents,
    C             = C,
    D             = D,
    d             = d,
    k_values      = k_values,
    figure_dir    = figure_dir,
    facet_size_in = facet_size_in
  )

  invisible(list(env_grid = res1, basic = res2))
}

# =============================================================================
# Convenience: run basic + by-model plots for multiple LA/LL grid steps
# =============================================================================
run_env_heatmaps_for_grid_steps <- function(
  grid_steps = c(0.10, 0.05, 0.025),
  ratio_step = 0.2,
  k_values,
  states,
  T_steps,
  N_agents,
  C,
  D,
  d,
  figure_dir = DIR_FIGURES,
  facet_size_in = 3.2,
  axis_max = 0.5,
  axis_by = 0.1
) {
  results <- list()

  for (gs in grid_steps) {
    message(">>> Running env heatmaps with grid_step = ", gs)

    env_grid <- build_env_heatmaps_for_models(
      C = C,
      D = D,
      d = d,
      T_steps = T_steps,
      states = states,
      N_agents = N_agents,
      grid_step = gs,
      ratio_step = ratio_step
    )

    res_env <- save_env_grid_heatmap_with_key(
      env_grid      = env_grid,
      D_fallback    = D,
      figure_dir    = figure_dir,
      axis_max      = axis_max,
      axis_by       = axis_by,
      facet_size_in = facet_size_in,
      out_stem      = sprintf("env/env_heatmap_by_model_with_key_side_gs_%s", gs)
    )

    res_basic <- save_basic_env_heatmap_with_key(
      states        = states,
      T_steps       = T_steps,
      N_agents      = N_agents,
      C             = C,
      D             = D,
      d             = d,
      k_values      = k_values,
      figure_dir    = figure_dir,
      axis_max      = axis_max,
      axis_by       = axis_by,
      facet_size_in = facet_size_in,
      out_stem      = sprintf("env/basic_hypervigilance_heatmap_by_K_with_key_gs_%s", gs)
    )

    results[[as.character(gs)]] <- list(env_grid = res_env, basic = res_basic)
  }

  invisible(results)
}

# =============================================================================
# Convenience: run env-by-model plot for multiple H0 values
# =============================================================================
run_env_heatmap_by_model_for_h0 <- function(
  h0_values = c(35),
  grid_step = 0.05,
  ratio_step = 0.2,
  states,
  T_steps,
  N_agents,
  C,
  D,
  d,
  figure_dir = DIR_FIGURES,
  facet_size_in = 3.2,
  axis_max = 0.5,
  axis_by = 0.1
) {
  results <- list()

  for (h0 in h0_values) {
    message(">>> Running env-by-model heatmap with H0 = ", h0)

    env_grid <- build_env_heatmaps_for_models(
      model_specs = default_model_specs(h0 = h0),
      C = C,
      D = D,
      d = d,
      T_steps = T_steps,
      states = states,
      N_agents = N_agents,
      grid_step = grid_step,
      ratio_step = ratio_step
    )

    res_env <- save_env_grid_heatmap_with_key(
      env_grid      = env_grid,
      D_fallback    = D,
      figure_dir    = figure_dir,
      axis_max      = axis_max,
      axis_by       = axis_by,
      facet_size_in = facet_size_in,
      out_stem      = sprintf("env/env_heatmap_by_model_with_key_side_h0_%s", h0)
    )

    results[[as.character(h0)]] <- res_env
  }

  invisible(results)
}

# =============================================================================
# Function calls (independent)
# =============================================================================
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")


# 1) Save env-grid heatmap only (does NOT run the basic pipeline)
# Comment/uncomment as needed.
save_env_grid_heatmap_with_key(
  env_grid      = env_grid,
  D_fallback    = D,
  figure_dir    = DIR_FIGURES,
  facet_size_in = 3.2
)

# 2) Save basic-model-only heatmap only (runs the pipeline)
# Comment/uncomment as needed.
save_basic_env_heatmap_with_key(
  states        = states,
  T_steps       = T_steps,
  N_agents      = N_agents,
  C             = C,
  D             = D,
  d             = d,
  k_values      = k_values,
  figure_dir    = DIR_FIGURES,
  facet_size_in = 3.2
)

# 3) Run both env-grid + basic for two grid steps (0.05 and 0.025)
# Comment/uncomment as needed.
run_env_heatmaps_for_grid_steps(
  grid_steps = c(0.10, 0.05, 0.025),
  ratio_step = 0.2,
  k_values   = k_values,
  states     = states,
  T_steps    = T_steps,
  N_agents   = N_agents,
  C          = C,
  D          = D,
  d          = d,
  figure_dir = DIR_FIGURES,
  facet_size_in = 3.2
)

# 4) Run env-by-model heatmap for different H0 values
# Comment/uncomment as needed.
run_env_heatmap_by_model_for_h0(
  h0_values = c(35),
  grid_step = 0.025,
  ratio_step = 0.2,
  states     = states,
  T_steps    = T_steps,
  N_agents   = N_agents,
  C          = C,
  D          = D,
  d          = d,
  figure_dir = DIR_FIGURES,
  facet_size_in = 3.2
)

# 3) Or run both (uses the two functions above)
# run_env_heatmap_plots(
#   env_grid      = env_grid,
#   states        = states,
#   T_steps       = T_steps,
#   N_agents      = N_agents,
#   C             = C,
#   D             = D,
#   d             = d,
#   k_values      = k_values,
#   figure_dir    = DIR_FIGURES,
#   facet_size_in = 3.2
# )

# =============================================================================
# Debug (optional): check the class of plot_env_heatmaps output
# =============================================================================
# p <- plot_env_heatmaps(real_data = env_grid)
# print(class(p))


print(env_heatmap_plot)
print(class(env_heatmap_plot))
