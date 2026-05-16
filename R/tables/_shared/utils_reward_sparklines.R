# ==============================================================================
# File: R/tables/_shared/utils_reward_sparklines.R
# Purpose: Add tiny inline reward-shape sparklines to flextable tables.
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(flextable)
})

# Map free-text reward labels to the three supported canonical sparkline types.
normalize_reward_type <- function(x) {
  x_low <- tolower(trimws(as.character(x)))

  if (grepl("threshold", x_low, fixed = TRUE)) return("threshold")
  if (grepl("convex", x_low, fixed = TRUE) || grepl("power", x_low, fixed = TRUE)) return("convex")
  if (grepl("linear", x_low, fixed = TRUE)) return("linear")

  # Fallback keeps rendering resilient for unknown labels.
  "linear"
}

# Build normalized [0,1] sparkline points for a reward type.
build_reward_sparkline_data <- function(
  reward_type,
  n = 48L,
  threshold_x = 0.6,
  convex_power = 3,
  convex_mode = c("exp", "power"),
  convex_k = 2,
  convex_shift = 0.25
) {
  type <- normalize_reward_type(reward_type)
  x <- seq(0, 1, length.out = as.integer(n))
  convex_mode <- match.arg(convex_mode)

  convex_y <- if (identical(convex_mode, "exp")) {
    k <- as.numeric(convex_k)
    if (!is.finite(k) || k <= 0) k <- 4
    s <- as.numeric(convex_shift)
    if (!is.finite(s) || s < 0) s <- 0
    # Shifted, normalized exponential for earlier visual lift while preserving convexity.
    (exp(k * (x + s)) - exp(k * s)) / (exp(k * (1 + s)) - exp(k * s))
  } else {
    x^as.numeric(convex_power)
  }

  y <- switch(
    type,
    linear = x,
    convex = convex_y,
    threshold = ifelse(x < threshold_x, 0, 1),
    x
  )

  data.frame(x = x, y = y, reward_type = type, stringsAsFactors = FALSE)
}

# Render one tiny sparkline PNG with fixed axes/size across reward types.
write_reward_sparkline_png <- function(
  reward_type,
  file,
  width_px = 180L,
  height_px = 34L,
  threshold_x = 0.6,
  convex_power = 3,
  convex_mode = c("exp", "power"),
  convex_k = 2,
  convex_shift = 0.25,
  color = "#202020",
  size = 0.6
) {
  dat <- build_reward_sparkline_data(
    reward_type = reward_type,
    threshold_x = threshold_x,
    convex_power = convex_power,
    convex_mode = convex_mode,
    convex_k = convex_k,
    convex_shift = convex_shift
  )

  p <- ggplot(dat, aes(x = x, y = y))

  if (identical(dat$reward_type[1], "threshold")) {
    p <- p + geom_step(direction = "hv", linewidth = size, color = color)
  } else {
    p <- p + geom_line(linewidth = size, color = color)
  }

  p <- p +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA)
    )

  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)

  ggplot2::ggsave(
    filename = file,
    plot = p,
    width = as.numeric(width_px) / 96,
    height = as.numeric(height_px) / 96,
    dpi = 96,
    units = "in",
    bg = "transparent"
  )

  normalizePath(file, winslash = "/", mustWork = TRUE)
}

# Export standalone sparkline files (one per reward type).
# Returns a named character vector of absolute file paths.
export_reward_sparklines <- function(
  reward_types = c("Linear", "Convex", "Threshold"),
  output_dir = file.path("outputs", "figures", "reward_sparklines"),
  file_prefix = "spark",
  width_px = 180L,
  height_px = 34L,
  threshold_x = 0.6,
  convex_power = 3,
  convex_mode = c("exp", "power"),
  convex_k = 2,
  convex_shift = 0.25,
  color = "#202020",
  size = 0.6
) {
  if (length(reward_types) == 0) {
    stop("`reward_types` must contain at least one value.", call. = FALSE)
  }

  canonical_types <- unique(vapply(reward_types, normalize_reward_type, character(1)))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  out_paths <- vapply(
    canonical_types,
    function(rt) {
      write_reward_sparkline_png(
        reward_type = rt,
        file = file.path(output_dir, paste0(file_prefix, "_", rt, ".png")),
        width_px = width_px,
        height_px = height_px,
        threshold_x = threshold_x,
        convex_power = convex_power,
        convex_mode = convex_mode,
        convex_k = convex_k,
        convex_shift = convex_shift,
        color = color,
        size = size
      )
    },
    character(1)
  )

  setNames(out_paths, canonical_types)
}

# Add a rightmost sparkline column to an existing table data.frame and render as flextable.
# Existing table content is left unchanged; this only appends the new visual column.
render_reward_table_with_sparklines <- function(
  table_data,
  reward_col = "Reward Type",
  spark_col = "Shape",
  threshold_x = 0.6,
  convex_power = 3,
  convex_mode = c("exp", "power"),
  convex_k = 2,
  convex_shift = 0.25,
  spark_width_in = 0.85,
  spark_height_in = 0.16,
  spark_dir = file.path(tempdir(), "reward_sparklines")
) {
  stopifnot(is.data.frame(table_data))
  if (!reward_col %in% names(table_data)) {
    stop(sprintf("Missing reward column: %s", reward_col), call. = FALSE)
  }

  df <- table_data
  df[[spark_col]] <- ""

  reward_types <- unique(vapply(df[[reward_col]], normalize_reward_type, character(1)))
  spark_paths <- setNames(
    vapply(
      reward_types,
      function(rt) {
        write_reward_sparkline_png(
          reward_type = rt,
          file = file.path(spark_dir, paste0("spark_", rt, ".png")),
          threshold_x = threshold_x,
          convex_power = convex_power,
          convex_mode = convex_mode,
          convex_k = convex_k,
          convex_shift = convex_shift
        )
      },
      character(1)
    ),
    reward_types
  )

  row_spark_paths <- unname(spark_paths[vapply(df[[reward_col]], normalize_reward_type, character(1))])

  ft <- flextable::flextable(df)
  ft <- flextable::align(ft, j = spark_col, align = "center", part = "all")
  ft <- flextable::width(ft, j = spark_col, width = spark_width_in + 0.05)

  for (i in seq_len(nrow(df))) {
    ft <- flextable::compose(
      x = ft,
      i = i,
      j = spark_col,
      value = flextable::as_paragraph(
        flextable::as_image(
          src = row_spark_paths[[i]],
          width = spark_width_in,
          height = spark_height_in
        )
      ),
      part = "body"
    )
  }

  ft
}

# Inject sparkline images into an existing flextable body column.
# Use this when you already build the table with nice_table() and only want to
# extend rendering with inline sparkline visuals.
add_reward_sparklines_inline <- function(
  ft,
  table_data,
  reward_col = "Reward Type",
  spark_col = "Shape",
  threshold_x = 0.6,
  convex_power = 3,
  convex_mode = c("exp", "power"),
  convex_k = 2,
  convex_shift = 0.25,
  spark_width_in = 0.85,
  spark_height_in = 0.16,
  spark_dir = file.path(tempdir(), "reward_sparklines")
) {
  if (!inherits(ft, "flextable")) {
    stop("`ft` must be a flextable object.", call. = FALSE)
  }
  stopifnot(is.data.frame(table_data))
  if (!reward_col %in% names(table_data)) {
    stop(sprintf("Missing reward column: %s", reward_col), call. = FALSE)
  }
  if (!spark_col %in% ft$col_keys) {
    stop(
      sprintf(
        "Column `%s` not found in flextable. Add this column to table_data before creating `ft`.",
        spark_col
      ),
      call. = FALSE
    )
  }

  reward_types <- unique(vapply(table_data[[reward_col]], normalize_reward_type, character(1)))
  spark_paths <- setNames(
    vapply(
      reward_types,
      function(rt) {
        write_reward_sparkline_png(
          reward_type = rt,
          file = file.path(spark_dir, paste0("spark_", rt, ".png")),
          threshold_x = threshold_x,
          convex_power = convex_power,
          convex_mode = convex_mode,
          convex_k = convex_k,
          convex_shift = convex_shift
        )
      },
      character(1)
    ),
    reward_types
  )

  row_spark_paths <- unname(spark_paths[vapply(table_data[[reward_col]], normalize_reward_type, character(1))])

  ft <- flextable::align(ft, j = spark_col, align = "center", part = "all")
  ft <- flextable::width(ft, j = spark_col, width = spark_width_in + 0.05)

  for (i in seq_len(nrow(table_data))) {
    ft <- flextable::compose(
      x = ft,
      i = i,
      j = spark_col,
      value = flextable::as_paragraph(
        flextable::as_image(
          src = row_spark_paths[[i]],
          width = spark_width_in,
          height = spark_height_in
        )
      ),
      part = "body"
    )
  }

  ft
}

# Create the reward-summary table data used in manuscript text.
# This preserves the provided table content and only appends sparklines at render time.
create_reward_table_df <- function() {
  data.frame(
    "Reward Type" = c("Linear", "Convex", "Threshold"),
    "Functional Form Φ(h)" = c(
      "αh",
      "αhγ, γ > 1",
      "{ α, if h≥h* ; 0, otherwise }"
    ),
    "Key Parameter(s)" = c(
      "α > 0",
      "α > 0,\nγ > 1",
      "α > 0,\nh*"
    ),
    "Parameter Values Used" = c(
      "α = 1",
      "α = 1,\nγ = 3",
      "α = 1,\nh = 0.6·h0*"
    ),
    "Interpretation" = c(
      "Proportional valuation of health",
      "Increasing marginal value of health",
      "Predefined minimum health cut-off"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# One-call constructor for a flextable with inline reward sparklines.
build_reward_table_with_sparklines <- function(
  table_data = create_reward_table_df(),
  reward_col = "Reward Type",
  spark_col = "Shape",
  threshold_x = 0.6,
  convex_power = 3,
  convex_mode = c("exp", "power"),
  convex_k = 2,
  convex_shift = 0.25
) {
  df <- table_data
  if (!spark_col %in% names(df)) {
    df[[spark_col]] <- ""
  }

  ft <- flextable::flextable(df)
  ft <- flextable::align(ft, j = names(df), align = "left", part = "all")
  ft <- flextable::align(ft, j = spark_col, align = "center", part = "all")

  add_reward_sparklines_inline(
    ft = ft,
    table_data = df,
    reward_col = reward_col,
    spark_col = spark_col,
    threshold_x = threshold_x,
    convex_power = convex_power,
    convex_mode = convex_mode,
    convex_k = convex_k,
    convex_shift = convex_shift
  )
}
