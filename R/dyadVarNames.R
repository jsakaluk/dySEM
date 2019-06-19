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

dyadVarNames = function(dat, xvar=NULL, yvar=NULL, sep, distinguish1, distinguish2){
  if(!is.null(xvar)&!is.null(yvar)){
    #Create indices with sep and distinguish
    index1 = gsub(" ", "", paste(sep, distinguish1), fixed = T)
    index2 = gsub(" ", "", paste(sep, distinguish2), fixed = T)

    #Extract X/Y indicators for P1 and P2
    p1xvars <- x1vars(dat, index1, xvar)
    p2xvars <- x2vars(dat, index2, xvar)
    p1yvars <- y1vars(dat, index1, yvar)
    p2yvars <- y2vars(dat, index2, yvar)

    #Item #s for X, Y, and total
    items.x = length(p1xvars)
    items.y = length(p1yvars)
    indicator.num = 2*(items.x+items.y)

    #Compile in list
    varinfo = list(p1xvars, p2xvars, items.x, distinguish1, distinguish2,
                   p1yvars, p2yvars, items.y, indicator.num)
  }
  else if(!is.null(xvar)&is.null(yvar)){
    #Create indices with sep and distinguish
    index1 = gsub(" ", "", paste(sep, distinguish1), fixed = T)
    index2 = gsub(" ", "", paste(sep, distinguish2), fixed = T)

    #Extract X indicators for P1 and P2
    p1xvars <- x1vars(dat, index1, xvar)
    p2xvars <- x2vars(dat, index2, xvar)

    #Item #s for X and total
    items.x = length(p1xvars)
    indicator.num = 2*(items.x)

    #Compile in list
    varinfo = list(p1xvars, p2xvars, items.x, distinguish1, distinguish2,indicator.num)

  }
  else if(is.null(xvar)&!is.null(yvar)){
    #Create indices with sep and distinguish
    index1 = gsub(" ", "", paste(sep, distinguish1), fixed = T)
    index2 = gsub(" ", "", paste(sep, distinguish2), fixed = T)

    #Extract Y indicators for P1 and P2
    p1yvars <- y1vars(dat, index1, yvar)
    p2yvars <- y2vars(dat, index2, yvar)

    #Item #s for Y and total
    items.y = length(p1yvars)
    indicator.num = 2*(items.y)

    #Compile in list
    varinfo = list(p1yvars, p2yvars, items.y, distinguish1, distinguish2,indicator.num)
  }
  return(varinfo)
}

