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
sipExtractor <- function(dat, stem, delim1, item_num,delim2, distinguish){
  stem_delim <-stringr::str_c(stem, delim1)
  delim_distinguish <- stringr::str_c(delim2, distinguish)

  vars <- stringr::str_extract(names(dat), stringr::str_c(stem_delim, item_num, delim_distinguish))
  vars <- vars[!is.na(vars)]
  return(vars)
}

#' @rdname scrapeHelpers
#' @noRd
spiExtractor <- function(dat, stem, delim1, item_num,delim2, distinguish){
  stem_delim <-stringr::str_c(stem, delim1, distinguish)
  vars <- stringr::str_extract(names(dat), stringr::str_c(stem_delim, delim2, item_num))
  vars <- vars[!is.na(vars)]
  return(vars)
}

#' @rdname scrapeHelpers
#' @noRd
spitExtractor <- function(dat, stem, delim1, item_num,delim2, distinguish, delim3, wave){
  stem_delim_distinguish <-stringr::str_c(stem, delim1, distinguish)

  vars <- stringr::str_extract(names(dat), stringr::str_c(stem_delim_distinguish, delim2, item_num, delim3, wave))
  vars <- vars[!is.na(vars)]
  return(vars)
}

#' @rdname scrapeHelpers
#' @noRd
siptExtractor <- function(dat, stem, delim1, item_num,delim2, distinguish, delim3, wave){
  stem_delim <-stringr::str_c(stem, delim1)
  delim_distinguish <- stringr::str_c(delim2, distinguish)
  delim_wave <- stringr::str_c(delim3, wave)

  vars <- stringr::str_extract(names(dat), stringr::str_c(stem_delim, item_num, delim_distinguish, delim_wave))
  vars <- vars[!is.na(vars)]
  return(vars)
}
