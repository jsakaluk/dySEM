#' Launch the interactive dySEM GUI
#'
#' Opens a Shiny application that guides researchers through selecting a dyadic
#' model, configuring invariance constraints and variable naming patterns,
#' previewing a path diagram, and generating the corresponding lavaan syntax.
#'
#' @param launch.browser Logical; if \code{TRUE} (the default), the app opens
#'   in the system's default web browser. Set to \code{FALSE} to use the
#'   RStudio viewer pane instead.
#'
#' @details
#' The app supports uni-construct (unidimensional, correlated, hierarchical,
#' bifactor), bi-construct (L-APIM, Common Fate, Mutual Influence, Two-Construct),
#' and multi-construct (multiple correlated factors) models.
#'
#' No data upload is required. The app constructs variable names from naming
#' patterns you specify, generates lavaan syntax via dySEM scripters, and
#' provides a path diagram preview, copy-to-clipboard, download, and
#' reproducible R code.
#'
#' Requires the \pkg{shiny}, \pkg{bslib}, and \pkg{shinyjs} packages
#' (listed in Suggests).
#'
#' @return Called for its side effect (launching the app). Returns the value
#'   from \code{\link[shiny]{runApp}} invisibly.
#'
#' @export
#' @examples
#' \dontrun{
#' launchScripter()
#' }
launchScripter <- function(launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The 'shiny' package is required to run the dySEM Scripter app.\n",
      "Install it with: install.packages('shiny')",
      call. = FALSE
    )
  }
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop(
      "The 'bslib' package is required to run the dySEM Scripter app.\n",
      "Install it with: install.packages('bslib')",
      call. = FALSE
    )
  }
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop(
      "The 'shinyjs' package is required to run the dySEM Scripter app.\n",
      "Install it with: install.packages('shinyjs')",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny-scripter", package = "dySEM")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Try reinstalling dySEM.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = launch.browser)
}
