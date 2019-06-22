#' @name lavaanscript_funs
#' @rdname lavaanscript_funs
#'
#' @title Functions for scripting portions of lavaan() code.
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
x1_loads <- function(dvn, lvxname=lvname){
  if(model=="configural"){
    stem_x1 = sprintf("%s%s =~ NA*",lvxname, dvn[[4]])
    loadscript.x1 = gsub(" ", "",paste(eta_x1,paste(dvn[[1]], collapse = "+")), fixed = T)
  }
  else if(model=="loading"|model=="intercept"){

  }
  return()
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

