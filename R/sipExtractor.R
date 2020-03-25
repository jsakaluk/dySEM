#' A helper function for scraping variable names from (S)tem-(I)tem-(P)artner
#' formatted variables
#'
#' This function helps dvn extract variable names according to the
#' "sip" order type.
#' @param dat input data frame of indicators of a particular LV
#' @param stem input character stem of indicator variables for LV
#' @param delim1 optional input character of first delimiting character
#' @param item_num optional input character to indicate range of variable numbers (defaults to scrape any digits)
#' @param delim2 optional input character of second delimiting character
#' @param distinguish input character used as the identifier a particular partner
#' @return vector containing variable names for a particular partner, for
#' a particular LV,
#' @export

sipExtractor <- function(dat, stem, delim1, item_num,delim2, distinguish){
  stem_delim <-stringr::str_c(stem, delim1)
  delim_distinguish <- stringr::str_c(delim2, distinguish)

  vars <- stringr::str_extract(names(dat), stringr::str_c(stem_delim, item_num, delim_distinguish))
  vars <- vars[!is.na(vars)]
  return(vars)
}
