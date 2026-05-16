# ===================================================================================================
# File: utils_model_labels.R
# Purpose: Normalize raw model descriptors to the shared labels used in plots and tables.
#
# Why this file exists:
#   Throughout the project, model variants can appear under many slightly different textual
#   descriptions depending on where they originate (scenario lists, DP outputs, filenames,
#   simulation metadata, etc.). For example:
#     - "Basic"
#     - "basic_model"
#     - "Health threshold"
#     - "Linear terminal reward"
#
#   Allowing these raw labels to flow directly into figures and tables would:
#     - Fragment legends and table rows
#     - Make cross-figure comparisons harder
#     - Force ad-hoc relabeling in many scripts
#
#   This helper centralizes *all* label normalization so that:
#     - Every figure/table uses the same reader-facing names
#     - Changes to naming conventions propagate globally
#     - Downstream scripts never need to guess how to format model labels
#
# Design philosophy:
#   - Accept *messy* or inconsistent input labels.
#   - Use permissive pattern matching (grepl) rather than exact equality.
#   - Return clean, publication-ready labels.
#   - Fall back gracefully to the original label if no rule matches.
# ===================================================================================================


#' Map simulation model identifiers to reader-ready labels.
#'
#' @param values character vector from scenarios or policy outputs
#'   These are typically raw model descriptors such as:
#'     - scenario labels
#'     - DP metadata fields
#'     - simulation annotations
#'
#' @return character vector
#'   Normalized labels used consistently across all tables and plots:
#'     - "Basic"
#'     - "Health (β = 0)"
#'     - "Linear (β = 1)"
#'     - "Power (a = 3)"
#'     - "Threshold (τ = 0.6·H0)"
#'
#' @details
#'   - Matching is case-insensitive so inputs like "basic", "BASIC", or "Basic model"
#'     all map to the same output.
#'   - Matching is substring-based, not exact, to tolerate descriptive labels
#'     (e.g. "Linear terminal reward").
#'   - The first matching rule wins; ordering therefore matters.
#'   - If no rule matches, the original label is returned unchanged.
#'
#' @examples
#'   map_model_label_values(c(
#'     "Basic",
#'     "Health threshold",
#'     "Linear terminal reward",
#'     "Power utility",
#'     "Threshold policy",
#'     "Custom experimental model"
#'   ))
#'
#'   # Returns:
#'   # [1] "Basic"
#'   # [2] "Health (β = 0)"
#'   # [3] "Linear (β = 1)"
#'   # [4] "Power (a = 3)"
#'   # [5] "Threshold (τ = 0.6·H0)"
#'   # [6] "Custom experimental model"
#'
#' @rationale
#'   This function is intentionally *simple and explicit* rather than data-driven:
#'   the labels encode theoretical meaning (β, a, τ) that belongs in the paper,
#'   not in scattered plotting code.
map_model_label_values <- function(values) {

  # vapply is used instead of sapply for strict type safety:
  # we guarantee a character vector of the same length as `values`.
  vapply(
    as.character(values),

    function(lbl) {

      # --- Basic model --------------------------------------------------------
      # Matches any label containing "Basic" (case-insensitive).
      if (grepl("Basic", lbl, ignore.case = TRUE)) {
        return("Basic")
      }

      # --- Health model -------------------------------------------------------
      # Health models correspond to β = 0 in the paper.
      # We deliberately ignore more detailed substrings (e.g. "Health threshold")
      # and collapse them into a single canonical label.
      if (grepl("Health", lbl, ignore.case = TRUE)) {
        return("Health (β = 0)")
      }

      # --- Linear terminal reward model ---------------------------------------
      # Linear damage/utility scaling corresponds to β = 1.
      if (grepl("Linear", lbl, ignore.case = TRUE)) {
        return("Linear (β = 1)")
      }

      # --- Power utility model ------------------------------------------------
      # Power curvature parameter a = 3 is fixed in this project,
      # so we encode it directly in the label.
      if (grepl("Power", lbl, ignore.case = TRUE)) {
        return("Power (a = 3)")
      }

      # --- Threshold model ----------------------------------------------------
      # Threshold parameter τ defaults to 0.6·H0 and is documented here.
      if (grepl("Threshold", lbl, ignore.case = TRUE)) {
        return("Threshold (τ = 0.6·H0)")
      }

      # --- Fallback -----------------------------------------------------------
      # If the label does not match any known model family,
      # return it unchanged so nothing is silently lost.
      lbl
    },

    # Enforce that each element of the output is length-1 character
    character(1)
  )
}
