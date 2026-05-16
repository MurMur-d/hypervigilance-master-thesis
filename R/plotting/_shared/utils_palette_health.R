# ------------------------------------------------------------------------------
# FILE: R/plotting/_shared/utils_palette_health.R
#
# ROLE:
#   Centralised colour palette definitions for health-related plots.
#
#   This file exists purely to *standardise visual semantics* across the project.
#   By defining all colours in one place, we ensure that:
#     - The same state/action always has the same colour across figures
#     - Changes to colour choices propagate consistently
#     - Plotting code remains focused on logic, not aesthetics
#
#   These palettes are intentionally simple named vectors so they can be passed
#   directly into ggplot2 scale functions (e.g., scale_fill_manual()).
# ------------------------------------------------------------------------------


# ==============================================================================
# STATE COLOUR PALETTE (HEALTH MODEL)
# ==============================================================================

# pal_states_health:
#   Maps internal *state labels* used by the health model to fixed colours.
#
#   These colours are used in:
#     - State heatmaps by agent/time
#     - Environment flow pies
#     - Any diagnostic plot that visualises the agent's internal state
#
#   State semantics (typical interpretation in the project):
#     - K    : Vigilant state (high vigilance, no damage yet)
#     - Kd   : Vigilant state with accumulated damage
#     - C    : Calm / relaxed state
#     - CD   : Calm but damaged state
#     - DEAD : Absorbing terminal state (agent has died)
#
#   Colour logic:
#     - Reds (K, Kd): high vigilance / threat-focused states
#     - Blues (C, CD): relaxed / calm states
#     - Black (DEAD): terminal, absorbing state
#
#   The lighter shades (Kd, CD) visually indicate â€œdamagedâ€ variants of the base
#   states without introducing a completely new hue.
pal_states_health <- c(
  K    = "#8B0000",  # dark red: vigilant, no damage
  Kd   = "#F1696E",  # light red: vigilant with damage
  C    = "#08306B",  # dark blue: calm / relaxed
  CD   = "#9ECAE1",  # light blue: calm with damage
  DEAD = "#000000"   # black: terminal absorbing state
)


# ==============================================================================
# ACTION COLOUR PALETTE
# ==============================================================================

# pal_actions:
#   Maps *action labels* (policy outputs) to colours.
#
#   Used in:
#     - Action heatmaps
#     - Flow diagrams (pies and ribbons)
#     - Any plot that visualises what the agent chooses to do
#
#   Action semantics:
#     - High : high vigilance action
#     - Low  : low vigilance (relax) action
#
#   Colour logic:
#     - Red   â†’ vigilance / defensive behaviour
#     - Blue  â†’ relaxation / low vigilance
#
#   These colours intentionally align with the state palette:
#     High vigilance actions are red-toned, calm actions are blue-toned,
#     reinforcing interpretation across different figure types.
pal_actions <- c(
  High = "#e41a1c",  # red: high vigilance action
  Low  = "#377eb8"   # blue: low vigilance / relax action
)


