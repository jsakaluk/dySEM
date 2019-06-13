#' A Variable Name-Scraping and Indexing Function
#'
#' This function scrapes the names of indicator variables in a
#' wide-format data set used for dyadic analyses of two latent variables (LV; X and Y),
#' and indexes which indicators correspond to which partner, for which LV.
#' It is used primarily to guide the syntax-writing of the other dySEM functions.
#' @param dat input data frame of indicators of a particular LV
#' @param xvar input character stem of indicator variables for LV X
#' @param yvar input character stem of indicator variables for LV Y
#' @param sep input character use to separate variable names from the partner identifier
#' @param distinguish1 input character used as the identifier for the first partner
#' @param distinguish2 input character used as the identifier for the first partner
#' @return list containing variable names for p1, p2, # of items per LV,
#'characters distinguishing partners, and total number of indicators
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", yvar="Y", sep = ".",distinguish1 = "1", distinguish2 = "2")

dyadVarNames = function(dat, xvar, yvar, sep, distinguish1, distinguish2){
  index1 = gsub(" ", "", paste(sep, distinguish1), fixed = T)
  index2 = gsub(" ", "", paste(sep, distinguish2), fixed = T)
  p1xvars = tidyselect::vars_select(names(dat), tidyselect::contains(index1)) %>%
    tidyselect::vars_select(tidyselect::starts_with(xvar))
  p2xvars = tidyselect::vars_select(names(dat), tidyselect::contains(index2)) %>%
    tidyselect::vars_select(tidyselect::starts_with(xvar))
  p1yvars = tidyselect::vars_select(names(dat), tidyselect::contains(index1)) %>%
    tidyselect::vars_select(tidyselect::starts_with(yvar))
  p2yvars = tidyselect::vars_select(names(dat), tidyselect::contains(index2)) %>%
    tidyselect::vars_select(tidyselect::starts_with(yvar))
  items.x = length(p1xvars)
  items.y = length(p1yvars)
  indicator.num = 2*(items.x+items.y)
  varinfo = list(p1xvars, p2xvars, items.x, distinguish1, distinguish2,
                 p1yvars, p2yvars, items.y, indicator.num)
  return(varinfo)
}

