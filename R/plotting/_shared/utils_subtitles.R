# ------------------------------------------------------------------------------
# FILE: R/plotting/_shared/utils_subtitles.R
#
# ROLE:
#   Central helpers for discovering active `h0` values and appending them to
#   comparison-grid subtitles/filenames in a consistent, reusable way.
#
# WHY THIS EXISTS:
#   In this repo, "health" runs often depend on an initial-health parameter
#   (commonly named `h0` or `H0`, depending on where it appears).
#   That parameter can be tucked inside:
#     - policy argument bundles (policy_args)
#     - simulation argument bundles (sim_args)
#     - nested meta lists attached as attributes to data frames / plots
#
#   When plotting many scenarios (model variants Ã— environment Ã— K, etc.), you
#   want subtitles and filenames to make that initial-health configuration visible
#   *without* manually repeating the same extraction logic everywhere.
#
# OVERVIEW OF FUNCTIONS:
#   1) find_h0_from_entry(entry)
#        - Extracts h0/H0 from ONE entry, but also supports "list of entries"
#          by recursing and returning unique values.
#
#   2) collect_h0_values(entries)
#        - Applies find_h0_from_entry() over many entries (typically a list of
#          argument lists) and returns unique h0/H0 values.
#
#   3) find_h0_from_meta(meta)
#        - Knows the typical places where h0/H0 might live inside a "meta" bundle
#          (e.g., df attributes used to build subtitles).
#
#   4) append_h0_to_subtitle(subtitle, meta)
#        - Uses find_h0_from_meta() to attach a standardized "H0 = ..." tag to a
#          subtitle string (only when it finds values).
#
#   5) append_meta_h0_subtitle(subtitle, meta)
#        - A thin alias wrapper kept for readability/consistency in call sites.
# ------------------------------------------------------------------------------


# ==============================================================================
# 1) Extract `h0` / `H0` from a single entry (or a list-of-entries)
# ==============================================================================

find_h0_from_entry <- function(entry) {
  # Accept either:
  #   - NULL            â†’ return NULL
  #   - a single list   â†’ search for h0/H0 keys
  #   - a list of lists â†’ recurse into each sub-entry and merge results
  #
  # This flexibility is important because some places store argument bundles as:
  #   policy_args = list(h0 = 35)
  # while others store multiple bundles as:
  #   policy_args = list(list(h0 = 35), list(h0 = 80))
  if (is.null(entry)) return(NULL)

  # Detect whether `entry` is a "list of lists":
  #   - is.list(entry) ensures it's list-like
  #   - length(entry) > 0 avoids edge-case empty list
  #   - all(vapply(..., is.list)) ensures each element is itself a list
  is_list_of_lists <- is.list(entry) && length(entry) > 0 &&
    all(vapply(entry, is.list, logical(1), USE.NAMES = FALSE))

  if (is_list_of_lists) {
    # Recursively extract h0/H0 from each child entry.
    # unlist(..., use.names = FALSE) ensures we return a plain vector.
    values <- unlist(lapply(entry, find_h0_from_entry), use.names = FALSE)

    # If nothing found anywhere, return NULL rather than an empty vector
    # to keep downstream "is.null" checks simple.
    if (length(values) == 0) return(NULL)

    # Remove duplicates because multiple bundles might reuse the same h0.
    return(unique(values))
  }

  # For a single argument-list entry:
  # Try both conventional key spellings:
  #   - "h0" (common in code)
  #   - "H0" (common in papers / notation)
  # and return the first one found.
  for (key in c("h0", "H0")) {
    if (!is.null(entry[[key]])) {
      return(entry[[key]])
    }
  }

  # No h0/H0 found in this entry.
  NULL
}


# ==============================================================================
# 2) Extract h0/H0 across a list of entries
# ==============================================================================

collect_h0_values <- function(entries) {
  # Typical input:
  #   entries = model_specs$policy_args
  # where each element is a list of policy args for one scenario.
  #
  # Returns:
  #   - NULL if nothing found
  #   - otherwise unique vector of h0/H0 values
  if (is.null(entries)) return(NULL)

  # Apply find_h0_from_entry() over each element and collapse.
  values <- unlist(lapply(entries, find_h0_from_entry), use.names = FALSE)

  # Standardize empty â†’ NULL for cleaner downstream logic.
  if (length(values) == 0) return(NULL)

  unique(values)
}


# ==============================================================================
# 3) Extract h0/H0 from a standard "meta" bundle
# ==============================================================================

find_h0_from_meta <- function(meta) {
  # Many plot/data prep functions attach metadata like:
  #   attr(df, "meta") <- list(policy_args = ..., sim_args = ..., ...)
  #
  # This helper centralizes the convention of where to look.
  if (is.null(meta)) return(NULL)

  # Small wrapper purely for readability at call sites.
  extract_from <- function(arg_entry) {
    if (is.null(arg_entry)) return(NULL)
    find_h0_from_entry(arg_entry)
  }

  # Candidate locations where h0/H0 might live.
  #
  # We include both direct args and nested "subtitle_meta" mirrors, because
  # some pipelines store separate metadata specifically for subtitle generation.
  candidate_args <- list(
    meta$policy_args,
    meta$sim_args,
    meta$subtitle_meta$policy_args,
    meta$subtitle_meta$sim_args
  )

  # Extract from all candidate places and merge into one vector.
  values <- unlist(lapply(candidate_args, extract_from), use.names = FALSE)

  if (length(values) == 0) return(NULL)

  unique(values)
}


# ==============================================================================
# 4) Append an "H0 = ..." tag onto a subtitle string
# ==============================================================================

append_h0_to_subtitle <- function(subtitle, meta) {
  # Pull any discovered initial-health values from the meta bundle.
  h0_vals <- find_h0_from_meta(meta)

  # If no values exist, return subtitle unchanged.
  # (No additional separators, no trailing whitespace, etc.)
  if (is.null(h0_vals) || length(h0_vals) == 0) return(subtitle)

  # Compose the tag as a standardized string.
  #
  # NOTE: The prefix uses "H0" in the label even if the underlying key was "h0",
  # because "H0" tends to match the paper/math notation and looks consistent in plots.
  #
  # NOTE: The leading "! " is a visual cue; it makes the h0 note stand out in long
  # subtitles (e.g., when subtitles are mostly C/D/d/T/N metadata).
  h0_part <- paste0("! H0 = ", paste(unique(h0_vals), collapse = ", "))

  # If the existing subtitle is empty/null, just return the tag.
  # This avoids returning " | ! H0 = ..." with a leading separator.
  if (is.null(subtitle) || subtitle == "") return(h0_part)

  # Otherwise, append using the repo's standard separator.
  paste(subtitle, h0_part, sep = " | ")
}


# ==============================================================================
# 5) Alias: keep older/more explicit name for call sites
# ==============================================================================

append_meta_h0_subtitle <- function(subtitle, meta) {
  # This is intentionally just a passthrough.
  # It reads nicely in plot builders:
  #   subtitle <- append_meta_h0_subtitle(subtitle, attr(df, "meta"))
  append_h0_to_subtitle(subtitle, meta)
}

