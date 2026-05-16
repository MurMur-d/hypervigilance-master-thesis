# ===================================================================================================
# File: utils_checks.R
# Purpose: Provide lightweight column- and structure-validation helpers for the data prep modules.
#
# Why this file exists:
#   Many prep_* scripts assume that incoming data frames already conform to a specific schema
#   (e.g., must contain env_label / LA / LL, or hv / stressor / time columns).
#   Rather than letting those assumptions fail later with cryptic subscript errors,
#   this helper centralizes *explicit checks* with clear error messages.
#
# Design philosophy:
#   - Minimal, dependency-free helpers (base R only).
#   - Fail fast and loudly when required structure is missing.
#   - Never mutate data; validation only.
#   - Return the input invisibly so checks can be chained inside pipelines.
#
# Typical usage pattern:
#   check_required_cols(df, c("env_label", "LA", "LL"))
#   df %>% ...   # safe to proceed knowing the schema is valid
# ===================================================================================================


#' Ensure the supplied data frame contains the expected columns.
#'
#' @param df data.frame under inspection
#'   The object whose column names will be validated.
#'
#' @param cols character vector of required column names
#'   Each entry must appear in `names(df)` for the check to pass.
#'
#' @return
#'   The original data frame `df`, returned invisibly.
#'   (This allows the function to be inserted into pipelines without changing outputs.)
#'
#' @details
#'   - Computes the set difference between the required column names and the actual columns.
#'   - If *any* required columns are missing, execution stops immediately with a clear message.
#'   - The error message lists all missing columns at once to avoid iterative debugging.
#'
#' @examples
#'   # Will pass silently
#'   check_required_cols(
#'     data.frame(a = 1, b = 2),
#'     c("a", "b")
#'   )
#'
#'   # Will error with: "Missing required columns: c"
#'   check_required_cols(
#'     data.frame(a = 1, b = 2),
#'     c("a", "b", "c")
#'   )
#'
#' @rationale
#'   Returning invisibly (rather than returning TRUE/FALSE) allows idioms like:
#'
#'     df <- check_required_cols(df, c("LA", "LL", "K"))
#'
#'   or:
#'
#'     df %>%
#'       check_required_cols(c("env_label", "LA", "LL")) %>%
#'       mutate(...)
#'
#'   which keeps validation colocated with the code that depends on it.
check_required_cols <- function(df, cols) {

  # Identify which required columns are missing from the data frame
  missing <- setdiff(cols, names(df))

  # If any required columns are absent, stop immediately with a readable error
  if (length(missing) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Return the original data frame invisibly so this function can be used inline
  invisible(df)
}

