# App Guide

Run the app with:

```r
shiny::runApp("app")
```

The sidebar controls model type, vigilance cost, damage parameters, time horizon, environment probabilities, simulation size, and health-model settings.

Main app areas:

- **Introduction:** conceptual summary and model overview.
- **Manuscript:** main thesis-style figures.
- **Forward Simulation:** simulated trajectories and population summaries.
- **Appendix:** additional risk, autocorrelation, policy, and mechanism analyses.

For reproducible scripted outputs, use `scripts/reproduce/00_reproduce_everything.R` rather than relying on interactive app state.
