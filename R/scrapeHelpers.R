#' @name scrapeHelpers
#' @rdname scrapeHelpers
#'
#' @title Helper-functions for scraping variable names and data set properties
#'
#' @param dat input data frame of indicators of a particular LV
#' @param stem input character stem of indicator variables for LV
#' @param delim1 optional input character of first delimiting character
#' @param item_num optional input character to indicate range of variable numbers (defaults to scrape any digits)
#' @param delim2 optional input character of second delimiting character
#' @param distinguish input character used as the identifier a particular partner
#' @param delim3 optional input character of second delimiting character
#' @param wave input character for the particular wave of a longitudinal design
#' @family helpers

#' @noRd
sipExtractor <- function(dat, stem, delim1, item_num, delim2, distinguish) {
  stem_delim <- stringr::str_c(stem, delim1)
  delim_distinguish <- stringr::str_c(delim2, distinguish)

  vars <- stringr::str_extract(names(dat), stringr::str_c("^", stem_delim, item_num, delim_distinguish))
  vars <- vars[!is.na(vars)]
  return(vars)
}

#' @rdname scrapeHelpers
#' @noRd
spiExtractor <- function(dat, stem, delim1, item_num, delim2, distinguish) {
  stem_delim <- stringr::str_c(stem, delim1, distinguish)
  vars <- stringr::str_extract(names(dat), stringr::str_c("^", stem_delim, delim2, item_num))
  vars <- vars[!is.na(vars)]
  return(vars)
}

#' @rdname scrapeHelpers
#' @noRd
psiExtractor <- function(dat, stem, delim1, item_num, delim2, distinguish) {
  stem_delim <- stringr::str_c(distinguish, delim1, stem)
  vars <- stringr::str_extract(names(dat), stringr::str_c("^", stem_delim, delim2, item_num))
  vars <- vars[!is.na(vars)]
  return(vars)
}

#' @rdname scrapeHelpers
#' @noRd
spitExtractor <- function(dat, stem, delim1, item_num, delim2, distinguish, delim3, wave) {
  stem_delim_distinguish <- stringr::str_c(stem, delim1, distinguish)

  vars <- stringr::str_extract(names(dat), stringr::str_c("^", stem_delim_distinguish, delim2, item_num, delim3, wave))
  vars <- vars[!is.na(vars)]
  return(vars)
}

#' @rdname scrapeHelpers
#' @noRd
siptExtractor <- function(dat, stem, delim1, item_num, delim2, distinguish, delim3, wave) {
  stem_delim <- stringr::str_c(stem, delim1)
  delim_distinguish <- stringr::str_c(delim2, distinguish)
  delim_wave <- stringr::str_c(delim3, wave)

  vars <- stringr::str_extract(names(dat), stringr::str_c("^", stem_delim, item_num, delim_distinguish, delim_wave))
  vars <- vars[!is.na(vars)]
  return(vars)
}

# Helper function for CLI messages in scrapeVarCross
print_scrape_cross <- function(varlist, var_list = NULL, x_stem = NULL, y_stem = NULL, distinguish_1 = "1", distinguish_2 = "2") {
  if (!is.null(var_list)) {
    # Multi-LV summary
    cli::cli_h2("Variable Scraping Summary")
    cli::cli_alert_success("Successfully scraped {length(var_list$lvnames)} latent variable{?s}")
    for (i in 1:length(var_list$lvnames)) {
      lv_name <- var_list$lvnames[i]
      p1_count <- length(varlist$p1xvarnames[[lv_name]])
      p2_count <- length(varlist$p2xvarnames[[lv_name]])
      cli::cli_alert_info("{.strong {lv_name}}: {p1_count} indicators for P1 ({distinguish_1}), {p2_count} indicators for P2 ({distinguish_2})")
    }
    cli::cli_alert_info("Total indicators: {varlist$indnum}")
  } else {
    # Single LV summary
    cli::cli_h2("Variable Scraping Summary")
    if (!is.null(y_stem)) {
      cli::cli_alert_success("Successfully scraped 2 latent variables: {x_stem} and {y_stem}")
      cli::cli_alert_info("{.strong {x_stem}}: {varlist$xindper} indicators for P1 ({distinguish_1}), {varlist$xindper} indicators for P2 ({distinguish_2})")
      cli::cli_alert_info("{.strong {y_stem}}: {varlist$yindper} indicators for P1 ({distinguish_1}), {varlist$yindper} indicators for P2 ({distinguish_2})")
      cli::cli_alert_info("Total indicators: {varlist$indnum}")
    } else {
      cli::cli_alert_success("Successfully scraped 1 latent variable: {x_stem}")
      cli::cli_alert_info("{.strong {x_stem}}: {varlist$xindper} indicators for P1 ({distinguish_1}), {varlist$xindper} indicators for P2 ({distinguish_2})")
      cli::cli_alert_info("Total indicators: {varlist$indnum}")
    }
  }
}
