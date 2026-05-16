# ===================================================================================================
# FILE: R/plotting/env_heatmaps/02_plot_setup_env_heatmaps.R
#
# ROLE:
#   Shared plot/layout/pipeline helpers that wrap the hypervigilance grid data from
#   `R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R`.
#
#   What this file does (big picture):
#   - It does NOT simulate anything itself. It *calls* grid-builders (which may run DP/sims)
#     and then standardises how the resulting LA/LL grids are plotted.
#   - It centralises the “house style” for environment heatmaps:
#       * LA on x-axis, LL on y-axis, hypervigilance rate as fill
#       * Facet by vigilance cost K (columns)
#       * Optionally facet by model scenario (rows)
#   - It offers two convenience wrappers:
#       * `run_hypervigilance_pipeline()` for a single model
#       * `run_hypervigilance_pipeline_by_model()` for a set of scenarios (stacked rows)
#
#   Why centralise this here?
#   - Every figure/table pipeline can reuse identical plotting logic
#   - Any change in aesthetics (legend, axis breaks, facet layout) is applied consistently
# ===================================================================================================

# ---------------------------------------------------------------------------------------------------
# Dependencies / sourcing
# ---------------------------------------------------------------------------------------------------
# We source the grid builders first: these functions produce the LA/LL × K data frame(s)
# containing hypervigilance rates (and often additional metadata like prevent/spill counts).
source("R/plotting/env_heatmaps/00_data_prep_environment_heatmap_data.R")

# Shared plotting theme helpers (fonts, base theme, subtitle helpers like append_h0_to_subtitle()).
source("R/core/plot_utils.R")

# Layout wrapper used to add a left-hand row title outside the facet strips (paper-style panels).
source("R/plotting/_shared/utils_plot_layout.R")


# ===================================================================================================
# 4) plot_env_heatmap_faceted_by_K(): standard plot builder for environment grids
# ===================================================================================================

#' Render LA/LL heatmaps faceted by cost (K) and optional scenario rows.
#'
#' Key idea:
#'   Input is already a “grid” data frame:
#'     - One row per (LA, LL, K) cell (and possibly also per model scenario)
#'     - A hypervigilance rate column used as tile fill (defaults to HypervigilanceRate_filtered)
#'
#' This function:
#'   1) Infers plotting ranges and nice axis breaks from the grid
#'   2) Builds a ggplot tile heatmap (geom_tile)
#'   3) Facets across K (columns), optionally scenarios (rows)
#'   4) Applies the project theme and consistent legend styling
#'
#' @param df data frame created by the grid builders (must include LA, LL, K)
#' @param rate_column name of the fill column (default: HypervigilanceRate_filtered, i.e., safe-state HV)
#' @param subtitle_context subtitle text (often includes C,D,d,T,N, and model reward details)
#' @param row_facet optional column name used for row facets (e.g., "model_label")
#' @param row_labels optional explicit ordering of row facet levels
#' @param add_row_title add a left-hand row-title annotation (paper-style panels)
#' @param row_title title string for row title annotation
#' @param gutter_row_title spacing between row title and heatmaps
#' @param gutter_row_labels spacing between row labels and heatmaps
#' @param row_label_width wrapping width for long row labels
#' @param axis_max_breaks max breaks for x/y “pretty” axes
#' @return ggplot (or wrapped grob) ready for saving
plot_env_heatmap_faceted_by_K <- function(
    df,
    rate_column = "HypervigilanceRate_filtered",
    subtitle_context = NULL,
    row_facet = NULL,
    row_labels = NULL,
    add_row_title = FALSE,
    row_title = "model scenario",
    gutter_row_title = grid::unit(1.6, "lines"),
    gutter_row_labels = grid::unit(1.4, "lines"),
    row_label_width = 18,
    axis_max_breaks = 6
) {

  # -----------------------------------------------------------------------------------------------
  # 1) Subtitle handling
  # -----------------------------------------------------------------------------------------------
  # Many figure scripts pass a subtitle string that encodes the sweep configuration
  # (e.g., C/D/d/T/N plus any health reward parameters).
  # If NULL, ggplot will simply omit the subtitle.
  subtitle_text <- if (is.null(subtitle_context)) NULL else as.character(subtitle_context)

  # -----------------------------------------------------------------------------------------------
  # 2) Basic input validation
  # -----------------------------------------------------------------------------------------------
  # Plotting an empty data frame is usually a sign that the upstream grid builder failed
  # (e.g., scenario list produced no feasible rows).
  if (nrow(df) == 0) stop("df is empty; nothing to plot")

  # -----------------------------------------------------------------------------------------------
  # 3) Decide whether we have a row facet
  # -----------------------------------------------------------------------------------------------
  # If `row_facet` is provided and exists as a column, we will facet_grid(rows=..., cols=K).
  # Otherwise we facet only by K (facet_wrap over K).
  has_row_facet <- !is.null(row_facet) && row_facet %in% names(df)

  # -----------------------------------------------------------------------------------------------
  # 4) Infer grid spacing and axis ranges from the data
  # -----------------------------------------------------------------------------------------------
  # We compute unique LA/LL values so we can:
  #   - infer grid step sizes (used only for sanity / potential later use)
  #   - infer min/max to set axis breaks and tile extents consistently
  #
  # Note:
  #   xmin/xmax and ymin/ymax are clamped into [0, 0.5] ranges where relevant,
  #   because the project treats LA,LL in that canonical region.
  x_vals <- sort(unique(df$LA))
  y_vals <- sort(unique(df$LL))
  x_step <- if (length(x_vals) > 1) min(diff(x_vals)) else 0.1
  y_step <- if (length(y_vals) > 1) min(diff(y_vals)) else 0.1
  tile_step <- min(x_step, y_step) # enforce square tiles even if steps differ
  tile_pad <- tile_step / 2        # pad limits to avoid clipping at 0.0
  xmin <- min(x_vals, 0); xmax <- max(x_vals, 0.5)
  ymin <- min(y_vals, 0); ymax <- max(y_vals, 0.5)
  x_limits <- c(xmin - tile_pad, xmax + tile_pad) # add half-step padding
  y_limits <- c(ymin - tile_pad, ymax + tile_pad)

  # Prevent weird user input (e.g., axis_max_breaks = 0) from breaking scales::breaks_pretty().
  axis_max_breaks <- max(1, as.integer(axis_max_breaks))

  # -----------------------------------------------------------------------------------------------
  # 5) Build “pretty” axis breaks
  # -----------------------------------------------------------------------------------------------
  # We use pretty breaks rather than fixed breaks so the same plotting function adapts
  # gracefully if the grid resolution changes (e.g., grid_step 0.05 vs 0.1).
  pretty_range <- function(range_vals) {
    breaks <- scales::breaks_pretty(n = axis_max_breaks)(range_vals)
    if (length(breaks) == 0) return(c(range_vals[1], range_vals[2]))
    breaks
  }
  x_breaks <- pretty_range(c(xmin, xmax))
  y_breaks <- pretty_range(c(ymin, ymax))
  x_breaks <- seq(0, 0.5, 0.1) # restore 0.1 tick spacing
  y_breaks <- seq(0, 0.5, 0.1)

  # -----------------------------------------------------------------------------------------------
  # 6) Construct K factor for consistent facet ordering
  # -----------------------------------------------------------------------------------------------
  # Facet ordering is important: we want K columns to appear from low→high cost.
  # Converting to a factor ensures ggplot respects that order.
  k_levels <- sort(unique(df$K))
  df$K_fac <- factor(df$K, levels = k_levels)

  # -----------------------------------------------------------------------------------------------
  # 7) Facet specification
  # -----------------------------------------------------------------------------------------------
  # Two cases:
  #   A) With row facet: facet_grid(rows = row_facet, cols = K)
  #      - `switch="y"` moves row strips to the left (useful for paper layout)
  #      - label_wrap_gen wraps long scenario labels so they don't overflow
  #
  #   B) Without row facet: facet_wrap over K, one row (nrow=1)
  if (has_row_facet) {
    # Establish explicit row ordering.
    # If row_labels is provided, trust it; otherwise infer from the data.
    row_levels <- if (is.null(row_labels)) unique(as.character(df[[row_facet]])) else as.character(row_labels)
    row_levels <- row_levels[!is.na(row_levels)]
    if (length(row_levels) == 0) row_levels <- unique(as.character(df[[row_facet]]))

    # Filter the requested ordering down to only levels actually present in df
    # (prevents factor levels that never appear).
    row_levels <- row_levels[row_levels %in% as.character(df[[row_facet]])]
    if (length(row_levels) == 0) row_levels <- unique(as.character(df[[row_facet]]))

    # Create a dedicated “facet_row” column so we don't mutate the original column in-place.
    df$facet_row <- factor(as.character(df[[row_facet]]), levels = row_levels)

    facet_layer <- ggplot2::facet_grid(
      rows = ggplot2::vars(facet_row),
      cols = ggplot2::vars(K_fac),
      switch = "y",
      labeller = ggplot2::labeller(
        facet_row = ggplot2::label_wrap_gen(row_label_width),
        K_fac = ggplot2::label_value
      )
    )
  } else {
    facet_layer <- ggplot2::facet_wrap(~ K_fac, nrow = 1, scales = "fixed",
                                       labeller = ggplot2::labeller(K_fac = ggplot2::label_value))
  }

  # -----------------------------------------------------------------------------------------------
  # 8) Facet strip styling logic
  # -----------------------------------------------------------------------------------------------
  # If we are going to add a separate left-hand “row title” panel (via wrapping),
  # we often want to hide the default facet strip text on the y side to avoid redundancy.
  #
  # Otherwise, keep row strip labels prominent.
  strip_y_text <- if (has_row_facet && add_row_title) ggplot2::element_blank()
                 else ggplot2::element_text(size = 12, face = "plain")
  strip_y_bg <- ggplot2::element_blank()

  # -----------------------------------------------------------------------------------------------
  # 9) Resolve the fill column via tidy evaluation
  # -----------------------------------------------------------------------------------------------
  # This allows callers to specify `rate_column` as a string.
  # We then use `!!rate_sym` inside aes(fill=...).
  rate_sym <- rlang::sym(rate_column)

  # -----------------------------------------------------------------------------------------------
  # 10) Core heatmap construction
  # -----------------------------------------------------------------------------------------------
  # - geom_tile draws one rectangle per (LA,LL) cell in the grid
  # - scale_fill_gradient maps 0→white and 1→black (fixed limits ensure comparability)
  # - coord_fixed ensures squares (important because LA and LL share comparable ranges)
  #
  # IMPORTANT:
  # - Because limits are fixed (0..1), differences between plots remain meaningful
  #   across models and across scripts.
  p <- ggplot2::ggplot(df, aes(x = LA, y = LL, fill = !!rate_sym)) +
    ggplot2::geom_tile(width = tile_step, height = tile_step) + # fixed square tiles
    ggplot2::scale_fill_gradient(
      limits = c(0, 1),
      low = "white",
      high = "black",
      name = "hypervigilance",
      breaks = c(0, 0.25, 0.5, 0.75, 1), # readable legend ticks
      labels = scales::label_number(accuracy = 0.01),
      guide = ggplot2::guide_colorbar(
        direction = "horizontal", # horizontal legend under panels
        frame.colour = "black",
        frame.linewidth = 0.3,
        ticks.colour = "black",
        barheight = grid::unit(0.4, "cm"),
        barwidth = grid::unit(5, "cm"),
        ticks.linewidth = 0.3,
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom"
      )
    ) +
    facet_layer +
    ggplot2::labs(
      title    = "Proportion of hypervigilance across environments",
      subtitle = subtitle_text,
      x = "P(arrive)", # plain axis labels
      y = "P(leave)"
    ) +
    theme_thesis_heatmap(base_size = 14) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = sprintf("%.1f", x_breaks),
      limits = x_limits,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = sprintf("%.1f", y_breaks),
      limits = y_limits,
      expand = c(0, 0)
    ) +
    ggplot2::theme(
      # Strip styling: general strip text size, plus special y-strip behaviour
      strip.text = ggplot2::element_text(size = 14, face = "plain"), # no bold strip text
      strip.text.y = strip_y_text,
      strip.background = ggplot2::element_blank(), # no strip boxes
      strip.background.y = strip_y_bg,

      # Place row strips outside the panels when we have row facets
      strip.placement = if (has_row_facet) "outside" else "inside",

      # Spacing between facet panels (x = between K columns, y = between scenario rows)
      panel.spacing.x = grid::unit(0.45, "lines"),
      panel.spacing.y = grid::unit(0.45, "lines"),

      # Axis title placement: small tweaks so labels align with paper layout
      axis.title.x = ggplot2::element_text(hjust = 0.5, face = "plain", size = 14),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8), hjust = 0.5, vjust = 0.5, face = "plain", size = 14),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 6)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = 8)),

      # Title/subtitle sizing
      plot.title = ggplot2::element_text(size = 16, face = "plain", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, colour = "grey40", margin = ggplot2::margin(b = 6)),

      # Keep each panel square
      aspect.ratio = 1,

      # Legend formatting (bottom legend is manuscript-friendly)
      legend.title = ggplot2::element_text(size = 12, face = "plain"),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box.just = "center",
      legend.key.width = grid::unit(12, "pt"),
      legend.key.height = grid::unit(12, "pt"),
      legend.key = ggplot2::element_rect(colour = "black", fill = "white", linewidth = 0.3),
      legend.background = ggplot2::element_blank(), # no legend container box
      legend.box.background = ggplot2::element_blank(),

      # Remove grey backgrounds; keep only black panel borders
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),

      # Reduce extra whitespace around panels
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")
    ) +
    ggplot2::theme(strip.clip = "off")

  # -----------------------------------------------------------------------------------------------
  # 11) Optional top label annotation (“vigilance cost (K)”)
  # -----------------------------------------------------------------------------------------------
  # This is a “paper-ish” annotation that sits above the top row of panels,
  # used when the plot has multiple row facets.
  #
  # Note:
  # - The label is placed at y=Inf and we turn clipping off so it can render.
  # -----------------------------------------------------------------------------------------------
  # 11) Store row level metadata on the plot object
  # -----------------------------------------------------------------------------------------------
  # Downstream layout wrappers sometimes need access to the row ordering
  # after ggplot has been built. We attach it as an attribute.
  if (has_row_facet) {
    attr(p, "row_levels") <- if (exists("row_levels")) row_levels else levels(df$facet_row)
  }

  # -----------------------------------------------------------------------------------------------
  # 13) Optional wrapper: add a dedicated left-hand row title panel
  # -----------------------------------------------------------------------------------------------
  # `wrap_env_heatmap_with_row_title()` typically:
  #   - Converts the ggplot to a grob
  #   - Adds a rotated title on the left
  #   - Adds gutters so row labels don’t clash with panels
  if (has_row_facet && add_row_title) {
    p <- wrap_env_heatmap_with_row_title(
      p,
      row_title = row_title,
      gutter_row_title = gutter_row_title,
      gutter_row_labels = gutter_row_labels,
      row_levels = attr(p, "row_levels"),
      row_label_width = row_label_width
    )
  }

  p
}


# ===================================================================================================
# 6) run_hypervigilance_pipeline(): convenience wrapper for a single model
# ===================================================================================================

#' Run the full pipeline for a single model and return both data and plot
#'
#' What it returns:
#'   - df_env  : a “single K” grid (mainly useful for debugging/diagnostics)
#'   - df_env2 : a multi-K grid (facetable by K)
#'   - p_faceted: the standard faceted heatmap for df_env2
#'
#' Why two grids?
#'   - Some scripts still want a “single K” grid for quick checks or for overlays.
#'   - The primary figure-ready output is df_env2 (multi-K) + p_faceted.
#'
#' @return list(df_env, df_env2, p_faceted)
run_hypervigilance_pipeline <- function(
    model = c("basic", "health"),
    states, T_steps, N_agents,
    K, C, D, d,
    grid_step = 0.1,
    ratio_step = 0.2,
    rate_column = "HypervigilanceRate_filtered",
    K_values = NULL,
    policy_args = list(),
    sim_args = list(),
    axis_max_breaks = 6
) {
  # Ensure we only accept known model keys; prevents typos silently falling through.
  model <- match.arg(model)

  # -----------------------------------------------------------------------------------------------
  # 1) Build a single-K grid (diagnostic)
  # -----------------------------------------------------------------------------------------------
  # hypervigilance_grid() typically:
  #   - Sweeps LA/LL for a *single* vigilance cost K
  #   - Computes policies/simulations to estimate hypervigilance at each cell
  df_env <- hypervigilance_grid(
    K, C, D, d,
    T_steps, states, N_agents,
    grid_step,
    model = model,
    policy_args = policy_args,
    sim_args = sim_args
  )

  # -----------------------------------------------------------------------------------------------
  # 2) Build a multi-K grid (primary plot data)
  # -----------------------------------------------------------------------------------------------
  # hypervigilance_grid_by_Kratio() typically:
  #   - Converts ratio steps into actual K values (e.g., K/D ratios)
  #   - Produces a stacked data frame with an explicit K column
  df_env2 <- hypervigilance_grid_by_Kratio(
    D = D, C = C, d = d,
    T_steps = T_steps,
    states = states,
    N_agents = N_agents,
    grid_step = grid_step,
    ratio_step = ratio_step,
    model = model,
    policy_args = policy_args,
    sim_args = sim_args
  )

  # Optional filter: keep only selected K levels (useful for paper figures).
  if (!is.null(K_values)) {
    df_env2 <- dplyr::filter(df_env2, K %in% K_values)
  }

  # -----------------------------------------------------------------------------------------------
  # 3) Construct a standard subtitle describing the sweep configuration
  # -----------------------------------------------------------------------------------------------
  # This makes figure panels self-documenting when exported.
  subtitle_text <- sprintf(
    "C = %d | D = %d | d = %d | T = %d | N = %d | mode = %s",
    C, D, d, T_steps, N_agents, model
  )

  # Append health initial conditions (h0 etc.) when present in policy_args or sim_args.
  subtitle_text <- append_h0_to_subtitle(subtitle_text, list(policy_args = policy_args, sim_args = sim_args))

  # For health model, append terminal reward details so different reward variants are distinguishable.
  if (identical(model, "health")) {
    subtitle_text <- paste0(subtitle_text, health_reward_subtitle(policy_args, sim_args))
  }

  # -----------------------------------------------------------------------------------------------
  # 4) Build the faceted plot
  # -----------------------------------------------------------------------------------------------
  p_faceted <- plot_env_heatmap_faceted_by_K(
    df_env2,
    rate_column = rate_column,
    subtitle_context = subtitle_text,
    axis_max_breaks = axis_max_breaks
  )

  # Return everything so callers can both plot and reuse the underlying data frame.
  list(
    df_env = df_env,
    df_env2 = df_env2,
    p_faceted = p_faceted
  )
}


# ===================================================================================================
# 7) run_hypervigilance_pipeline_by_model(): compare scenarios (stacked rows)
# ===================================================================================================

#' Compare multiple model scenarios via stacked LA/LL grids.
#'
#' This is the main “multi-scenario” entry point used by paper figures:
#'   - Calls the by-model grid builder (which loops over scenarios internally)
#'   - Produces a single stacked df with a model_label column
#'   - Plots it as a facet grid: rows = model_label, cols = K
#'
#' @return list(df_env_models, p_faceted)
run_hypervigilance_pipeline_by_model <- function(
    model_scenarios = default_env_model_scenarios,
    D, C, d, T_steps, states, N_agents,
    grid_step = 0.1,
    ratio_step = 0.2,
    rate_column = "HypervigilanceRate_filtered",
    base_policy_args = list(),
    base_sim_args = list(),
    subtitle_context = NULL,
    add_row_title = TRUE,
    row_title = "model variant",
    axis_max_breaks = 6
) {

  # -----------------------------------------------------------------------------------------------
  # 1) Build stacked grids across scenarios
  # -----------------------------------------------------------------------------------------------
  # hypervigilance_grid_by_Kratio_by_model() typically:
  #   - Iterates over model_scenarios (label/model/policy_args/sim_args)
  #   - Merges base args with scenario overrides
  #   - Returns one combined df with columns including:
  #       LA, LL, K, HypervigilanceRate_all, model_label, model_id, model
  #   - Attaches attr(..., "row_levels") so plots can keep consistent ordering
  df_env_models <- hypervigilance_grid_by_Kratio_by_model(
    model_scenarios = model_scenarios,
    D = D, C = C, d = d,
    T_steps = T_steps,
    states = states,
    N_agents = N_agents,
    grid_step = grid_step,
    ratio_step = ratio_step,
    base_policy_args = base_policy_args,
    base_sim_args = base_sim_args
  )
  if (nrow(df_env_models) == 0) stop("No rows generated for provided model_scenarios")

  # -----------------------------------------------------------------------------------------------
  # 2) Establish row ordering for the facet grid
  # -----------------------------------------------------------------------------------------------
  # Prefer the explicit ordering attached by the grid builder; otherwise fall back to factor levels.
  row_levels <- attr(df_env_models, "row_levels")
  if (is.null(row_levels)) row_levels <- levels(df_env_models$model_label)

  # -----------------------------------------------------------------------------------------------
  # 3) Subtitle construction
  # -----------------------------------------------------------------------------------------------
  subtitle_text <- if (is.null(subtitle_context)) {
    sprintf("C = %s | D = %s | d = %s | T = %s | N = %s", C, D, d, T_steps, N_agents)
  } else {
    as.character(subtitle_context)
  }

  # Include health initial condition info when relevant (even for mixed scenarios, the base args matter).
  subtitle_text <- append_h0_to_subtitle(subtitle_text, list(policy_args = base_policy_args, sim_args = base_sim_args))

  # -----------------------------------------------------------------------------------------------
  # 4) Plot: rows = scenario/model_label, cols = K
  # -----------------------------------------------------------------------------------------------
  p_grid <- plot_env_heatmap_faceted_by_K(
    df_env_models,
    rate_column = rate_column,
    subtitle_context = subtitle_text,
    row_facet = "model_label",
    row_labels = row_levels,
    add_row_title = add_row_title,
    row_title = row_title,
    axis_max_breaks = axis_max_breaks
  )

  list(
    df_env_models = df_env_models,
    p_faceted = p_grid
  )
}


# ===================================================================================================
# Summary (what to use when)
# ===================================================================================================
# - hypervigilance_grid():
#     Compute an LA/LL grid at a *single* vigilance cost K.
#     Useful for debugging or inspecting a specific cost slice.
#
# - hypervigilance_grid_by_Kratio():
#     Compute an LA/LL grid for multiple K values (derived via ratio_step), suitable for faceting.
#
# - hypervigilance_grid_by_Kratio_by_model():
#     Repeat the multi-K grid for a list of model scenarios and return one stacked data frame
#     with model labels for row facets (used for model-comparison figures).
#
# - plot_env_heatmap_faceted_by_K():
#     The standard “house style” tile plot for these grids (consistent legend/axes/facets).
#
# - run_hypervigilance_pipeline() / run_hypervigilance_pipeline_by_model():
#     Convenience wrappers that return both the tidy data and a ready-to-save plot.
# ===================================================================================================


