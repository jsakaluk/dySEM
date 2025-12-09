#' Package Summary Statistics
#'
#' This script provides a quick summary of package statistics including
#' the number of exported functions and unit tests.
#'
#' Usage: source("package_summary.R") or run individual commands

# Count exported functions from NAMESPACE
count_exported_functions <- function() {
  if (!file.exists("NAMESPACE")) {
    stop("NAMESPACE file not found. Are you in the package root directory?")
  }

  exports <- readLines("NAMESPACE")
  export_lines <- grep("^export\\(", exports, value = TRUE)
  # Exclude the pipe operator if present
  export_lines <- export_lines[!grepl('^export\\("%>%"\\)', export_lines)]

  return(length(export_lines))
}

# Count test files
count_test_files <- function() {
  test_dir <- "tests/testthat"
  if (!dir.exists(test_dir)) {
    return(0)
  }

  test_files <- list.files(test_dir, pattern = "^test-.*\\.R$", full.names = FALSE)
  return(length(test_files))
}

# Count total functions (exported + internal)
count_total_functions <- function() {
  r_dir <- "R"
  if (!dir.exists(r_dir)) {
    return(0)
  }

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  total_funcs <- 0

  for (f in r_files) {
    content <- readLines(f, warn = FALSE)
    # Match function definitions: name <- function( or name <- function (
    func_lines <- grep("^[a-zA-Z_][a-zA-Z0-9_.]*\\s*<-\\s*function", content)
    total_funcs <- total_funcs + length(func_lines)
  }

  return(total_funcs)
}

# Count total test cases (test_that blocks)
count_test_cases <- function() {
  test_dir <- "tests/testthat"
  if (!dir.exists(test_dir)) {
    return(0)
  }

  test_files <- list.files(test_dir, pattern = "^test-.*\\.R$", full.names = TRUE)

  total_tests <- 0
  for (f in test_files) {
    # Count test_that blocks in each file
    content <- readLines(f, warn = FALSE)
    test_blocks <- grep("^test_that\\(", content)
    total_tests <- total_tests + length(test_blocks)
  }

  return(total_tests)
}

# Print summary
print_package_summary <- function() {
  cat("\n=== dySEM Package Summary ===\n\n")

  # Total functions
  n_total <- count_total_functions()
  cat("Total Functions:", n_total, "\n")

  # Exported functions
  n_exports <- count_exported_functions()
  cat("A) Exported Functions:", n_exports, "\n")

  # Test files
  n_test_files <- count_test_files()
  cat("B) Test Files:", n_test_files, "\n")

  # Test cases
  n_test_cases <- count_test_cases()
  cat("   Test Cases (test_that blocks):", n_test_cases, "\n")

  cat("\n")

  return(invisible(list(
    total_functions = n_total,
    exported_functions = n_exports,
    test_files = n_test_files,
    test_cases = n_test_cases
  )))
}

# Run summary if script is executed directly
if (!interactive()) {
  print_package_summary()
}
