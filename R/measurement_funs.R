#' @name measurement_funs
#' @rdname measurement_funs
#'
#' @title Functions for extracting measurement model output
#'
#' @param dvn input object from dyadVarNames()
#' @param fit input object from fitted lavaan model
#'
#' @seealso \code{\link{dyOutput}} which this function supplies
#' @export
#'

#' @rdname measurement_funs
xlamda1 <- function(dvn, fit){
  #Extract loadings
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~" & rhs %in% dvn[[1]]) %>%
    dplyr::select(Std.Loading=.data$std.all)
  return(load)
}

#' @rdname measurement_funs
xlamda2 <- function(dvn, fit){
  #Extract loadings
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~" & rhs %in% dvn[[2]]) %>%
    dplyr::select(Std.Loading=.data$std.all)
  return(load)
}

#' @rdname measurement_funs
loadings <- function(dvn, fit){
  #Extract loadings, SEs, Z, p,
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~") %>%
    dplyr::select('Latent Factor'=.data$lhs, Indicator=.data$rhs, Loading=.data$est, SE=.data$se, Z=.data$z,
                  'p-value'=.data$pvalue, Std.Loading=.data$std.all)
  return(load)
}

#' @rdname measurement_funs
xintercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1")
  x.int.list <- c()
  x.int.num <- dvn[[3]]*2#number of x intercepts (*2 for dyads)
  #Only keep indicator intercepts
  for(i in 1:x.int.num){
    x.int.list[[i]]<- intercept.param$est[i]
  }
  return(x.int.list)
}

#' @rdname measurement_funs
yintercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1")
  y.int.list <- c()
  x.int.num <- dvn[[3]]*2#number of x intercepts (*2 for dyads)
  ystart<- (x.int.num+1) #intercept where y begins
  #Only keep indicator intercepts
  for(i in ystart:dvn[[9]]){
    y.int.list[[i]]<- intercept.param$est[i]
  }
  y.int.list = y.int.list[!is.na(y.int.list)]
  return(y.int.list)
}
