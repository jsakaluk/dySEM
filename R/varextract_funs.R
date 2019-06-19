#' @name varextract_funs
#' @rdname varextract_funs
#'
#' @title Functions for dyadVarNames() for extracting item information
#'
#' @param dat input data frame of indicators of a particular LV
#' @param xvar input character stem of indicator variables for LV X
#' @param yvar input character stem of indicator variables for LV Y
#' @param index1 combined character of sep and identifier for the first partner
#' @param index2 combined character of sep and identifier for the first partner
#'
#' @seealso \code{\link{dyadVarNames}} which this function supplies
#' @export

#' @rdname varextract_funs
x1vars <- function(dat, index1, xvar){
  info1 <- tidyselect::vars_select(names(dat), tidyselect::contains(index1)) %>%
    tidyselect::vars_select(tidyselect::starts_with(xvar))
  return(info1)
}

#' @rdname varextract_funs
x2vars <- function(dat, index2, xvar){
  info2 <- tidyselect::vars_select(names(dat), tidyselect::contains(index2)) %>%
    tidyselect::vars_select(tidyselect::starts_with(xvar))
  return(info2)
}

#' @rdname varextract_funs
y1vars <- function(dat, index1, yvar){
  info3 <- tidyselect::vars_select(names(dat), tidyselect::contains(index1)) %>%
    tidyselect::vars_select(tidyselect::starts_with(yvar))
  return(info3)
}

#' @rdname varextract_funs
y2vars <- function(dat, index2, yvar){
  info4 <- tidyselect::vars_select(names(dat), tidyselect::contains(index2)) %>%
    tidyselect::vars_select(tidyselect::starts_with(yvar))
  return(info4)
}

