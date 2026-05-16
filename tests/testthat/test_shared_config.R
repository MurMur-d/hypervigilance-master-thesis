find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "R", "core", "shared_config.R")) &&
        file.exists(file.path(current, "README.md"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate public repository root.")
    }
    current <- parent
  }
}

project_root <- find_project_root()
setwd(project_root)
source("R/core/shared_config.R")

test_that("normalize_script_path keeps figure scripts relative", {
  rel_path <- "R/plotting/basic_policy/03_call_basic_dp_policy.R"
  expect_equal(normalize_script_path(rel_path), rel_path)
})

test_that("figure registry entry returns metadata", {
  entry <- figure_registry_entry("R/plotting/basic_policy/03_call_basic_dp_policy.R")
  expect_true(is.data.frame(entry))
  expect_equal(entry$figure_id, "Fig1A")
})

test_that("describe_figure_script references figure id", {
  msg <- describe_figure_script("R/plotting/basic_policy/03_call_basic_dp_policy.R")
  expect_true(grepl("Fig1A", msg))
  expect_true(grepl("Basic DP policy visual", msg))
})

test_that("log_figure_start returns registry row and messages", {
  expect_message(
    info <- log_figure_start("R/plotting/basic_policy/03_call_basic_dp_policy.R"),
    "Fig1A"
  )
  expect_equal(info$figure_id, "Fig1A")
})
