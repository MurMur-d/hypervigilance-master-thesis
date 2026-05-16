# ==================================================================================================
# File: R/plotting/mechanism_env/00_data_prep_mechanism_data.R
# Purpose:
#   Provide shared helpers used by â€œmechanismâ€ and â€œriskâ€ plots, specifically:
#     1) A *mechanism color encoding* that simultaneously communicates:
#          - whether HV is primarily preventative vs spillover (hue)
#          - how much HV there is overall (lightness/whitening)
#     2) Consistent plotting layers for:
#          - threshold line overlays (vertical/horizontal reference lines)
#          - SSP/predictability boundary overlays
#          - region code labels (e.g., L-P, M-U) placed inside the heatmap
#     3) A canonical â€œmechanism-readyâ€ data frame constructor that:
#          - clamps and cleans HV values to [0, 1]
#          - turns prevention/spill counts into shares
#          - computes derived rates (preventative_rate, spillover_rate)
#          - computes â€œautocorrâ€ if missing
#
# Inputs:
#   - Grid tables from the environment pipeline (one row per LAÃ—LLÃ—K cell)
#   - Columns such as HypervigilanceRate_all, HypervigilancePreventCount,
#     HypervigilanceSpillCount, HypervigilanceSafeHV, and/or LA/LL/autocorr.
#
# Outputs:
#   - Functions returning vectors/data frames ready to be plugged into ggplot.
#
# Design notes:
#   - This file is intentionally â€œplot-adjacentâ€: it does not draw final plots by itself,
#     but it creates reusable, consistent layers and aesthetics for plots elsewhere.
#   - We keep the mechanism color logic centralized so all figures encode mechanism the same way.
# ==================================================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# --------------------------------------------------------------------------------------------------
# Mechanism color encoding (core aesthetic)
# --------------------------------------------------------------------------------------------------
# We encode â€œpreventative vs spilloverâ€ as hue along a three-point ramp:
#   prevent_share = 0   â†’ blue   (pure spillover)
#   prevent_share = 0.5 â†’ purple (mixed)
#   prevent_share = 1   â†’ red    (pure preventative)
#
# Later, we *mix with white* depending on hv_value:
#   hv_value = 0 â†’ fully white (no HV; mechanism hue irrelevant)
#   hv_value = 1 â†’ full saturated ramp color (strong HV)
#
# This yields a visually intuitive combined encoding:
#   - hue answers â€œwhat kind of HV is this?â€
#   - lightness answers â€œhow much HV is there?â€
mechanism_color_ramp <- grDevices::colorRamp(c("#2c7fb8", "#2b2b2b", "#d95f0e"))


#' Map preventivity vs HV value to a combined color gradient.
#'
#' Interpretation:
#'   - prevent_prop sets hue (blueâ†’purpleâ†’red)
#'   - hv_value scales saturation by blending toward white when hv is low
#'
#' @param prevent_prop numeric vector in [0,1] (share of safe HV classified as preventative)
#' @param hv_value     numeric vector in [0,1] (overall HV magnitude to scale color intensity)
#' @return character vector of colors (hex-like strings from grDevices::rgb)
mechanism_color_map <- function(prevent_prop, hv_value) {
  # Quick exit for empty inputs (useful in pipelines that sometimes pass empty frames)
  if (length(prevent_prop) == 0L) return(character(0))

  # We require 1:1 correspondence between prevent_share and hv_value per row
  stopifnot(length(prevent_prop) == length(hv_value))

  # Clamp to [0, 1] to avoid colorRamp surprises and prevent NaNs from leaking
  prevent_prop <- pmin(pmax(prevent_prop, 0), 1)
  hv_value     <- pmin(pmax(hv_value, 0), 1)

  # base_rgb is an Nx3 matrix (R,G,B) from 0..255 as a function of prevent_prop
  base_rgb <- mechanism_color_ramp(prevent_prop)

  # hv_mat is Nx3, repeating hv_value across RGB channels
  # This makes it easy to do per-row linear interpolation
  # Slight gamma lift improves color visibility at mid-range HV values.
  hv_vis <- hv_value^0.85
  hv_mat <- matrix(hv_vis, nrow = length(hv_vis), ncol = 3)

  # Mix rule:
  #   hv = 1 â†’ mix_rgb = base_rgb
  #   hv = 0 â†’ mix_rgb = 255 (white)
  # Linear interpolation between white and the hue color.
  mix_rgb <- hv_mat * base_rgb + (1 - hv_mat) * 255

  # Clamp again just to be safe (floating point and weird edge cases)
  mix_rgb <- pmin(pmax(mix_rgb, 0), 255)

  # Convert to color strings (rgb() expects 0..1)
  grDevices::rgb(
    red   = mix_rgb[, 1] / 255,
    green = mix_rgb[, 2] / 255,
    blue  = mix_rgb[, 3] / 255
  )
}


# --------------------------------------------------------------------------------------------------
# Threshold line helpers
# --------------------------------------------------------------------------------------------------
# Some plots overlay theoretical thresholds (e.g., Î»A* or Î»L*) as vertical/horizontal lines.
# These helpers standardize how threshold segments are represented and added to ggplot.
#
# Key idea:
#   "segments" is a data.frame with x,y,xend,yend and optional aesthetics.
#   We sanitize it once, then add it as a geom_segment layer.
# --------------------------------------------------------------------------------------------------

##' Prepare threshold segments so ggplot can draw them with consistent aesthetics.
##'
##' @param threshold_lines data.frame with required columns:
##'        x, y, xend, yend
##'   Optional columns (if absent, defaults are inserted):
##'        color/colour, linetype, size, alpha
##'
##' @return data.frame ready for geom_segment() with standard columns:
##'   x, y, xend, yend, color, linetype, size, alpha
prepare_threshold_segments <- function(threshold_lines) {
  # Allow callers to pass NULL (means â€œno threshold overlayâ€)
  if (is.null(threshold_lines)) return(NULL)

  # Ensure required coordinate columns exist
  required <- c("x", "y", "xend", "yend")
  if (!all(required %in% names(threshold_lines))) {
    stop("threshold_lines must contain columns: ", paste(required, collapse = ", "))
  }

  # Normalize spelling: some code may use British â€œcolourâ€
  if ("colour" %in% names(threshold_lines)) {
    threshold_lines$color <- threshold_lines$colour
    threshold_lines$colour <- NULL
  }

  # Provide defaults if aesthetics are missing
  if (!"colour" %in% names(threshold_lines) && !"color" %in% names(threshold_lines)) {
    threshold_lines$color <- "black"
  }
  if (!"linetype" %in% names(threshold_lines)) threshold_lines$linetype <- "dashed"
  if (!"size"     %in% names(threshold_lines)) threshold_lines$size     <- 0.6
  if (!"alpha"    %in% names(threshold_lines)) threshold_lines$alpha    <- 0.8

  threshold_lines
}

##' Add threshold segments to a ggplot, respecting aesthetics embedded in `segments`.
##'
##' Why we use scale_*_identity():
##'   Because weâ€™re providing literal aesthetics per-segment (not mapping to data categories).
##'   Identity scales tell ggplot â€œuse these values as-is and donâ€™t create legends.â€
##'
##' @param p ggplot object
##' @param segments data.frame returned by prepare_threshold_segments()
##' @return ggplot object with additional segment layer (or unchanged if segments is NULL)
add_threshold_layer <- function(p, segments) {
  if (is.null(segments)) return(p)

  p +
    geom_segment(
      data = segments,
      mapping = aes(
        x = x, y = y, xend = xend, yend = yend,
        color = color, linetype = linetype, size = size, alpha = alpha
      ),
      inherit.aes = FALSE,  # do not inherit the main plotâ€™s mappings
      show.legend = FALSE   # keep overlays out of the legend
    ) +
    scale_color_identity(guide = "none") +
    scale_linetype_identity(guide = "none") +
    scale_size_identity(guide = "none") +
    scale_alpha_identity(guide = "none")
}


# --------------------------------------------------------------------------------------------------
# Standard threshold builders
# --------------------------------------------------------------------------------------------------
# These are convenience constructors that generate â€œtheory referenceâ€ lines for each K level.
# In many figures, K is facetted and we want the same threshold overlaid in each facet.
# --------------------------------------------------------------------------------------------------

#' Build vertical/horizontal dashed reference lines for each K based on D.
#'
#' Concept:
#'   Given theoretical thresholds of the form:
#'     Î»A* = K / D          (vertical line in LA)
#'     Î»L* = 1 - (K / D)    (horizontal line in LL)
#'
#' we generate segment rows restricted to the plotting ranges.
#'
#' @param K_levels vector of K values present in the plotted data
#' @param D scalar damage parameter used in the threshold formulas
#' @param x_range plotting range for LA axis
#' @param y_range plotting range for LL axis
#' @return data.frame of segment coordinates + aesthetics, tagged with K_fac for faceting
build_standard_threshold_lines <- function(
  K_levels, D,
  x_range = c(0, 0.5),
  y_range = c(0, 0.5)
) {
  if (length(K_levels) == 0) return(NULL)

  # Be tolerant to factors/character inputs coming from facet metadata.
  D_num <- suppressWarnings(as.numeric(as.character(D))[1])
  if (is.na(D_num) || !is.finite(D_num) || D_num == 0) {
    stop("D must be a finite non-zero scalar")
  }

  K_num <- suppressWarnings(as.numeric(as.character(K_levels)))
  K_num <- K_num[is.finite(K_num)]
  if (length(K_num) == 0) return(NULL)

  segments <- list()

  for (K in sort(unique(K_num))) {

    # --- Vertical line at Î»A = K/D ------------------------------------------
    lambda_a <- K / D_num
    if (lambda_a >= x_range[1] && lambda_a <= x_range[2]) {
      segments[[length(segments) + 1]] <- data.frame(
        K = K,
        x = lambda_a, y = y_range[1],
        xend = lambda_a, yend = y_range[2],
        color = "#777777",
        linetype = "dashed",
        size = 0.6,
        alpha = 0.45,
        stringsAsFactors = FALSE
      )
    }

    # --- Horizontal line at Î»L = 1 - (K/D) ----------------------------------
    lambda_l <- 1 - (K / D_num)
    if (lambda_l >= y_range[1] && lambda_l <= y_range[2]) {
      segments[[length(segments) + 1]] <- data.frame(
        K = K,
        x = x_range[1], y = lambda_l,
        xend = x_range[2], yend = lambda_l,
        color = "#777777",
        linetype = "dashed",
        size = 0.6,
        alpha = 0.45,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(segments) == 0) return(NULL)

  seg_df <- do.call(rbind, segments)

  # K_fac is a stable factor for faceting and ordering
  seg_df$K_fac <- factor(seg_df$K, levels = sort(unique(K_num)))
  seg_df
}


# --------------------------------------------------------------------------------------------------
# SSP and predictability boundaries
# --------------------------------------------------------------------------------------------------
# In your environment grid, you often label regions like L-P, M-U, etc.
# These boundary curves are the lines separating those region definitions.
#
# The returned list is â€œplot-readyâ€: each element is a data.frame with columns la, ll
# so it can be passed to geom_line().
# --------------------------------------------------------------------------------------------------

#' Build boundary curves for SSP bands and predictability split.
#'
#' @param x_range LA axis range (min,max)
#' @param y_range LL axis range (min,max)
#' @param overlay_step (unused right now) kept for API compatibility / future refinement
#' @return list(low_med, med_high, predictability) each a data.frame(la, ll)
build_ssp_predictability_boundaries <- function(x_range, y_range, overlay_step = 0.025) {
  x_min <- x_range[1]; x_max <- x_range[2]
  y_min <- y_range[1]; y_max <- y_range[2]

  # --- SSP boundaries --------------------------------------------------------
  # Your SSP definition (as used elsewhere) appears to be:
  #   ssp = LA / (LA + LL)
  # with thresholds around 0.2 and 0.8.
  #
  # Solving ssp = 0.2 gives:  LA / (LA+LL) = 0.2  ->  LL = 4*LA
  # Solving ssp = 0.8 gives:  LA / (LA+LL) = 0.8  ->  LL = 0.25*LA
  #
  # We then clip those lines to the plot window (x_range, y_range).
  max_la_low_med  <- min(x_max, y_max / 4)
  max_la_med_high <- min(x_max, y_max / 0.25)

  make_line <- function(max_la, slope) {
    if (max_la <= 0) return(data.frame(la = numeric(), ll = numeric()))
    la_vals <- seq(max(x_min, 0), max_la, length.out = 200) # start at axis corner
    data.frame(la = la_vals, ll = slope * la_vals)
  }

  ssp_low_med_df  <- make_line(max_la_low_med, 4)
  ssp_med_high_df <- make_line(max_la_med_high, 0.25)

  # --- Predictability boundary ----------------------------------------------
  # Your predictability split is based on total rate:
  #   predictable if LA + LL <= 0.5
  # So the boundary is:
  #   LL = 0.5 - LA
  #
  # Again we clip to plot bounds.
  pred_la_start <- max(0, 0.5 - y_max)
  pred_la_end   <- min(x_max, 0.5 - y_min)

  if (pred_la_end > pred_la_start) {
    pred_la_vals <- seq(pred_la_start, pred_la_end, length.out = 200)
    predictability_df <- data.frame(la = pred_la_vals, ll = 0.5 - pred_la_vals)
  } else {
    predictability_df <- data.frame(la = numeric(), ll = numeric())
  }

  list(
    low_med        = ssp_low_med_df,
    med_high       = ssp_med_high_df,
    predictability = predictability_df
  )
}

#' Add SSP/predictability boundary lines to a ggplot.
#'
#' Style:
#'   We draw each boundary twice (white thick underlay + black thin overlay)
#'   so it remains visible on both dark and light heatmap tiles.
#'
#' @param p ggplot object
#' @param boundaries list as returned by build_ssp_predictability_boundaries()
#' @return ggplot with boundary layers added
add_ssp_boundaries <- function(p, boundaries) {
  if (is.null(boundaries)) return(p)

  draw_line <- function(plot_obj, df) {
    if (is.null(df) || nrow(df) == 0) return(plot_obj)
    plot_obj +
      geom_line(data = df, aes(x = la, y = ll), inherit.aes = FALSE,
                colour = "white", linewidth = 1.2, alpha = 0.45) +
      geom_line(data = df, aes(x = la, y = ll), inherit.aes = FALSE,
                colour = "black", linewidth = 0.6, alpha = 0.35)
  }

  p <- draw_line(p, boundaries$low_med)
  p <- draw_line(p, boundaries$med_high)
  draw_line(p, boundaries$predictability)
}


# --------------------------------------------------------------------------------------------------
# Region label placement (L-P, L-U, M-P, ...)
# --------------------------------------------------------------------------------------------------
# These helpers compute â€œniceâ€ label positions inside each region and choose label text color
# based on the background HV level (light text on dark tiles, dark text on light tiles).
# --------------------------------------------------------------------------------------------------

#' Compute label coordinates for each SSPÃ—predictability region.
#'
#' @param df_plot data.frame that includes LA, LL, and hv_total columns
#' @param x_range,y_range plot bounds (currently unused for filtering, but useful to keep API stable)
#' @param overlay_step (unused) retained for compatibility
#' @return data.frame(region_code, la_label, ll_label, hjust, vjust, label_colour)
build_env_region_labels <- function(df_plot, x_range, y_range, overlay_step = 0.025) {
  if (nrow(df_plot) == 0) return(NULL)

  region_label_df <- df_plot %>%
    mutate(
      # Total â€œrate massâ€ used in SSP definition
      total = LA + LL,

      # SSP share: LA / (LA + LL)
      # If total==0, SSP is undefined -> NA
      ssp = ifelse(total > 0, LA / total, NA_real_),

      # Convert continuous SSP into bands:
      #   <= 0.2: Low
      #   >= 0.8: High
      #   else:   Medium
      ssp_band = dplyr::case_when(
        is.na(ssp) ~ NA_character_,
        ssp <= 0.2 ~ "L",
        ssp >= 0.8 ~ "H",
        TRUE ~ "M"
      ),

      # Predictability split:
      #   P if LA+LL <= 0.5, else U
      predictability = ifelse(total <= 0.5, "P", "U"),

      # Region code like â€œL-Pâ€, â€œH-Uâ€
      region_code = ifelse(is.na(ssp_band), NA_character_, paste0(ssp_band, "-", predictability))
    ) %>%
    filter(!is.na(region_code)) %>%

    # For each region, place the label at the mean (LA,LL) of points in the region.
    # We also sample an hv_total value near the centroid to decide label color.
    group_by(region_code) %>%
    summarise(
      la_label = mean(LA, na.rm = TRUE),
      ll_label = mean(LL, na.rm = TRUE),

      # Pick a representative hv_total near the centroid:
      # choose the point minimizing squared distance to (meanLA, meanLL)
      hv_sample = hv_total[which.min((LA - mean(LA, na.rm = TRUE))^2 + (LL - mean(LL, na.rm = TRUE))^2)],
      .groups = "drop"
    ) %>%
    mutate(
      hjust = 0.5,
      vjust = 0.5,
      hv_sample = ifelse(is.na(hv_sample), 0, hv_sample)
    ) %>%
    mutate(
      # Determine label text color based on background darkness:
      # hv_total ~ 1 â†’ dark tile â†’ use light text
      # hv_total ~ 0 â†’ light tile â†’ use dark text
      lightness = pmin(pmax(hv_sample, 0), 1),
      label_colour = dplyr::case_when(
        lightness >= 0.5 ~ "#f2f2f2",
        TRUE ~ "#2b2b2b"
      ),

      # Small manual nudges for specific labels that tend to overlap boundaries or edges
      la_label = la_label + dplyr::case_when(
        region_code == "H-P" ~ 0.02,
        region_code == "L-P" ~ 0.02,
        TRUE ~ 0
      ),
      ll_label = ll_label + dplyr::case_when(
        region_code == "L-P" ~ 0.05,
        region_code == "H-U" ~ 0.02,
        region_code == "H-P" ~ 0.02,
        TRUE ~ 0
      )
    ) %>%
    select(region_code, la_label, ll_label, hjust, vjust, label_colour)

  if (nrow(region_label_df) == 0) return(NULL)
  region_label_df
}

#' Add region-code labels as a geom_text layer.
#'
#' @param p ggplot object
#' @param labels output of build_env_region_labels()
#' @return ggplot with label layer added
add_region_label_layer <- function(p, labels) {
  if (is.null(labels) || nrow(labels) == 0) return(p)

  p +
    geom_text(
      data = labels,
      mapping = aes(
        x = la_label, y = ll_label, label = region_code,
        hjust = hjust, vjust = vjust, colour = label_colour
      ),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold",
      show.legend = FALSE,
      check_overlap = TRUE
    )
}


# --------------------------------------------------------------------------------------------------
# prepare_mechanism_df(): the main â€œmake it mechanism-readyâ€ transformer
# --------------------------------------------------------------------------------------------------
# This is the workhorse that:
#   - chooses the right columns (or falls back safely)
#   - computes prevent_share and spill_share
#   - computes mechanism_fill colors with mechanism_color_map()
#   - computes derived rates for plotting
#
# It is intentionally tolerant of missing columns so it can be reused across:
#   - environment heatmaps
#   - mechanism heatmaps
#   - risk/autocorr plots
# --------------------------------------------------------------------------------------------------

prepare_mechanism_df <- function(
    df,
    hv_column      = "HypervigilanceRate_all",
    prevent_column = "HypervigilancePreventCount",
    spill_column   = "HypervigilanceSpillCount",
    safe_column    = "HypervigilanceSafeHV"
) {
  n <- nrow(df)

  # --- Step 1: Hypervigilance values (clean + clamp) -------------------------
  hv_vals <- if (hv_column %in% names(df)) df[[hv_column]] else rep(0, n)
  hv_vals[is.na(hv_vals)] <- 0
  hv_vals[!is.finite(hv_vals)] <- 0
  hv_vals <- pmin(pmax(hv_vals, 0), 1)

  # --- Step 2: Prevention/spill counts (clean) -------------------------------
  prevent_vals <- if (prevent_column %in% names(df)) df[[prevent_column]] else rep(0, n)
  prevent_vals[is.na(prevent_vals)] <- 0
  prevent_vals[!is.finite(prevent_vals)] <- 0

  spill_vals <- if (spill_column %in% names(df)) df[[spill_column]] else rep(0, n)
  spill_vals[is.na(spill_vals)] <- 0
  spill_vals[!is.finite(spill_vals)] <- 0

  # --- Step 3: Safe HV denominator (robust fallback) -------------------------
  # safe_vals should represent â€œsafe HV total eventsâ€ used to define shares.
  # If HypervigilanceSafeHV is missing or entirely NA, fall back to prevent+spill.
  safe_vals <- rep(NA_real_, n)
  if (safe_column %in% names(df)) safe_vals <- df[[safe_column]]

  # If the column exists but is entirely NA, treat safe_vals as prevent+spill
  if (all(is.na(safe_vals))) safe_vals <- prevent_vals + spill_vals

  # For partial NAs, fill them rowwise with prevent+spill
  safe_na <- which(is.na(safe_vals))
  if (length(safe_na) > 0) {
    safe_vals[safe_na] <- prevent_vals[safe_na] + spill_vals[safe_na]
  }

  # Ensure non-negative denominator
  safe_vals <- pmax(safe_vals, 0)

  # --- Step 4: Shares (prevent vs spill) ------------------------------------
  # If safe_vals==0, we set prevent_share = 0.5 (neutral/mixed),
  # because â€œno safe HV eventsâ€ means mechanism is undefined.
  prevent_share <- ifelse(safe_vals > 0, prevent_vals / safe_vals, 0.5)
  prevent_share[!is.finite(prevent_share)] <- 0.5
  prevent_share <- pmin(pmax(prevent_share, 0), 1)

  spill_share <- 1 - prevent_share

  # --- Step 5: Autocorrelation (compute if missing) --------------------------
  autocorr_vals <- rep(NA_real_, n)
  if ("autocorr" %in% names(df)) {
    autocorr_vals <- df[["autocorr"]]
  } else if (all(c("LA", "LL") %in% names(df))) {
    # Project convention: autocorr â‰ˆ 1 - (LA + LL), clamped to [0, 1]
    autocorr_vals <- 1 - (df$LA + df$LL)
  }

  autocorr_vals[is.na(autocorr_vals)] <- 0
  autocorr_vals <- pmin(pmax(autocorr_vals, 0), 1)

  # --- Step 6: Attach derived columns to the original df ---------------------
  # mechanism_fill:
  #   combined hue/lightness color encoding for plotting
  # preventative_rate / spillover_rate:
  #   decompose total HV into two components (useful for bivariate plots)
  df %>%
    mutate(
      autocorr = autocorr_vals,
      hv_total = hv_vals,

      prevent_share = prevent_share,
      spill_share   = spill_share,

      mechanism_fill = mechanism_color_map(prevent_share, hv_vals),

      preventative_rate = hv_vals * prevent_share,
      spillover_rate    = hv_vals * spill_share
    )
}

# Shared summary for the documentation to pass into the mechanism plots.
# (Downstream scripts can cite:
#   - hue encodes mechanism (prevent vs spill)
#   - lightness encodes HV magnitude
#   - threshold/boundary overlays are standardized here
# )
