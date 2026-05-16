# ------------------------------------------------------------------------------
# FILE: R/plotting/_shared/utils_health_env_scenarios.R
#
# ROLE:
#   Defines the *canonical* health-environment scenarios used throughout the repoâ€™s
#   health-policy visualisations (matrices, bar panels, captions, etc.).
#
# WHY THIS EXISTS:
#   Many plots need a small, interpretable set of representative environments rather
#   than a full LAâ€“LL grid. If each script re-created these environments independently,
#   youâ€™d risk subtle inconsistencies (different LA/LL values, swapped labels, missing
#   metadata), which makes figures harder to compare.
#
# WHAT AN "ENVIRONMENT" MEANS HERE:
#   Each environment is defined by two transition rates for a 2-state stressor process:
#     - LA: lambda_A = probability the stressor *appears* in the next step
#           (i.e., transition from "no stressor" -> "stressor")
#     - LL: lambda_L = probability the stressor *leaves* in the next step
#           (i.e., transition from "stressor" -> "no stressor")
#
#   Given LA and LL, the stationary stressor probability (SSP) is:
#     SSP = LA / (LA + LL)
#
#   (Interpretation: long-run fraction of time the stressor is present, assuming the
#   process has mixed and LA+LL>0.)
#
#   A second quantity often used in captions/interpretation is *predictability*:
#   when LA + LL is small, the stressor state is more persistent (higher autocorrelation),
#   making the environment more predictable. When LA + LL is large, switching is frequent,
#   making it less predictable. In some parts of the repo youâ€™ll see:
#     autocorr ~ 1 - (LA + LL)
#   (This is a simple heuristic/summary rather than a full Markov autocorrelation derivation.)
#
# DESIGN OF THE CANONICAL SET:
#   We define 6 scenarios crossing:
#     SSP level âˆˆ {low, medium, high}
#     predictability âˆˆ {predictable, unpredictable}
#
#   They are encoded with short row labels:
#     - "L-P" = low SSP, predictable
#     - "L-U" = low SSP, unpredictable
#     - "M-P" = medium SSP, predictable
#     - "M-U" = medium SSP, unpredictable
#     - "H-P" = high SSP, predictable
#     - "H-U" = high SSP, unpredictable
#
# NOTE ON VALUES:
#   The LA/LL pairs are chosen so that:
#     - low SSP:   LA << LL  (stressor rare, tends to leave quickly)
#     - high SSP:  LA >> LL  (stressor common, tends to persist / reappear quickly)
#     - medium SSP: LA â‰ˆ LL  (balanced)
#   and predictability is manipulated by the *magnitude* of LA+LL:
#     - "predictable": smaller total rate (slower switching)
#     - "unpredictable": larger total rate (faster switching)
# ------------------------------------------------------------------------------
default_health_env_scenarios <- function() {

  # Build the canonical environment table as a plain data.frame.
  # (Using data.frame here keeps dependencies minimal; downstream code often turns this into a tibble.)
  df <- data.frame(
    # Short labels used for facet strips and compact figure annotations.
    env_label = c("L-P", "L-U", "M-P", "M-U", "H-P", "H-U"),

    # Human-readable descriptors suitable for captions or supplementary tables.
    env_full = c(
      "low SSP, predictable",
      "low SSP, unpredictable",
      "medium SSP, predictable",
      "medium SSP, unpredictable",
      "high SSP, predictable",
      "high SSP, unpredictable"
    ),

    # SSP category (often used for ordering or grouping in plots).
    ssp_level = c("low", "low", "medium", "medium", "high", "high"),

    # Predictability category used in figure narratives.
    # Here it is *defined operationally* by how quickly the stressor flips states
    # (i.e., by the total switching probability LA + LL).
    predictability = c(
      "predictable", "unpredictable",
      "predictable", "unpredictable",
      "predictable", "unpredictable"
    ),

    # --- Core parameters: LA and LL -----------------------------------------
    # LA: probability the stressor appears next step (0 -> 1)
    # LL: probability the stressor leaves next step   (1 -> 0)
    #
    # Low SSP:
    #   - predictable: LA very small, LL small-ish => stressor rare AND states persist
    #   - unpredictable: LA bigger, LL big => stressor still rare-ish but switching frequent
    #
    # Medium SSP:
    #   - predictable: LA == LL and both modest => balanced and moderately persistent
    #   - unpredictable: LA == LL and both large => balanced but flips often
    #
    # High SSP:
    #   - predictable: LA modest, LL very small => stressor common AND persistent
    #   - unpredictable: LA big, LL bigger-ish => stressor common but flips frequently
    LA = c(
      0.005, 0.025,
      0.05,  0.40,
      0.095, 0.475
    ),
    LL = c(
      0.095, 0.475,
      0.05,  0.40,
      0.005, 0.025
    ),

    # Prevents R from auto-converting strings to factors (important for facet ordering/labels).
    stringsAsFactors = FALSE
  )

  # Derive SSP from LA and LL.
  # SSP is well-defined as long as LA + LL > 0.
  # (All rows here satisfy that, but this is why SSP is computed rather than hard-coded.)
  df$SSP <- with(df, LA / (LA + LL))

  # Return the enriched table.
  df
}

# Backwards-compatible alias:
# Some scripts/modules may refer to `default_env_scenarios()` historically.
# Making it an alias avoids breaking older code while keeping a single source of truth.
default_env_scenarios <- default_health_env_scenarios

