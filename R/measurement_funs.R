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
xtheta1 <- function(dvn, fit){
  #Extract resicual variances
  error = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~~" & lhs %in% dvn[[1]] & rhs %in% dvn[[1]]) %>%
    dplyr::select(Std.Error=.data$std.all)
  return(error)
}

#' @rdname measurement_funs
xtheta2 <- function(dvn, fit){
  #Extract residual variances
  error = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~~" & lhs %in% dvn[[2]] & rhs %in% dvn[[2]]) %>%
    dplyr::select(Std.Error=.data$std.all)
  return(error)
}

#' @rdname measurement_funs
xtheta12 <- function(dvn, fit){
  #Extract loadings
  corerror = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~~" & lhs %in% dvn[[1]] & rhs %in% dvn[[2]]) %>%
    dplyr::select(Std.Error=.data$std.all)
  return(corerror)
}

#' @rdname measurement_funs
loadings <- function(dvn, fit){
  #Extract loadings, SEs, Z, p,
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~") %>%
    dplyr::select('Latent Factor'=.data$lhs, Indicator=.data$rhs, Loading=.data$est, SE=.data$se, Z=.data$z,
                  'p-value'=.data$pvalue, 'Std. Loading'=.data$std.all)
  return(load)
}

#' @rdname measurement_funs
xintercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1") %>%
    dplyr::select(.data$est)

  intercept.param <- dplyr::rename(intercept.param, intercept = est)
  x.int.num <- dvn[[3]]*2#number of x intercepts (*2 for dyads)
  intercept.param <- intercept.param[1:x.int.num,]

  return(intercept.param)
}

#' @rdname measurement_funs
xyintercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1") %>%
    dplyr::select(.data$est)

  intercept.param <- intercept.param$est[1:(length(intercept.param$est)-4)]

  return(intercept.param)
}
