# Shiny App

`app.R` is the canonical thesis app.

Run it from the repository root:

```r
shiny::runApp("app")
```

The app provides interactive access to the Basic Model, Health Model, terminal-reward variants, forward simulations, and the main thesis visualizations.

The app expects the repository root as the working directory so that relative paths such as `R/core/setup_project.R` resolve correctly.
