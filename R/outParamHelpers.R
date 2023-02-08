#' @name outParamHelpers
#' @rdname outParamHelpers
#'
#' @title Functions for extracting measurement model parameter output
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param fit input object from fitted lavaan model
#' @family helpers

#' @export
#'

xlamda1 <- function(dvn, fit){
  #Extract loadings
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~" & .data$rhs %in% dvn[[1]]) %>%
    dplyr::select(Std.Loading=.data$std.all)
  return(load)
}

#' @rdname outParamHelpers
xlamda2 <- function(dvn, fit){
  #Extract loadings
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~" & .data$rhs %in% dvn[[2]]) %>%
    dplyr::select(Std.Loading=.data$std.all)
  return(load)
}

#' @rdname outParamHelpers
xtheta1 <- function(dvn, fit){
  #Extract resicual variances
  error = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~~" & .data$lhs %in% dvn[[1]] & .data$rhs %in% dvn[[1]]) %>%
    dplyr::select(Std.Error=.data$std.all)
  return(error)
}

#' @rdname outParamHelpers
xtheta2 <- function(dvn, fit){
  #Extract residual variances
  error = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~~" & .data$lhs %in% dvn[[2]] & .data$rhs %in% dvn[[2]]) %>%
    dplyr::select(Std.Error=.data$std.all)
  return(error)
}

#' @rdname outParamHelpers
xtheta12 <- function(dvn, fit){
  #Extract loadings
  corerror = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~~" & .data$lhs %in% dvn[[1]] & .data$rhs %in% dvn[[2]]) %>%
    dplyr::select(Std.Error=.data$std.all)
  return(corerror)
}

#' @rdname outParamHelpers
loadings <- function(dvn, fit){
  #Extract loadings, SEs, Z, p,
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~") %>%
    dplyr::select('Latent Factor'=.data$lhs, Indicator=.data$rhs, Loading=.data$est, SE=.data$se, Z=.data$z,
                  'p-value'=.data$pvalue, 'Std. Loading'=.data$std.all)
  load <- load[1:dvn[["indnum"]],]

  return(load)
}

#' @rdname outParamHelpers
bidyLoadings <- function(dvn, fit){
  #Extract loadings, SEs, Z, p,
  load = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "=~") %>%
    dplyr::select('Latent Factor'=.data$lhs, Indicator=.data$rhs, Loading=.data$est, SE=.data$se, Z=.data$z,
                  'p-value'=.data$pvalue, 'Std. Loading'=.data$std.all)

  return(load)
}

#' @rdname outParamHelpers
xintercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1") %>%
    dplyr::select(.data$est)

  intercept.param <- dplyr::rename(intercept.param, intercept = .data$est)
  x.int.num <- dvn[[3]]*2#number of x intercepts (*2 for dyads)
  intercept.param <- intercept.param[1:x.int.num,]

  return(intercept.param)
}

#' @rdname outParamHelpers
xbidyIntercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1") %>%
    dplyr::select(.data$est)

  intercept.param <- dplyr::rename(intercept.param, intercept = .data$est)
  x.int.num <- dvn[[3]]*2#number of x intercepts (*2 for dyads)
  intercept.param <- intercept.param[1:x.int.num,]

  half <- x.int.num/2
  inthalf1 <- intercept.param[1:half]
  inthalf2 <- intercept.param[(half+1):x.int.num]
  missing <- rep(NA, x.int.num)
  intercept.param <- c(inthalf1, missing, inthalf2)
  return(intercept.param)
}
#' @rdname outParamHelpers
xyintercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1") %>%
    dplyr::select(.data$est)

  intercept.param <- intercept.param$est[1:dvn[["indnum"]]]

  return(intercept.param)
}

#' @rdname outParamHelpers
xybidyIntercepts <- function(dvn, fit){
  #Extract intercepts
  intercept.param <- lavaan::parameterEstimates(fit, standardized=TRUE) %>%
    dplyr::filter(.data$op == "~1") %>%
    dplyr::filter(.data$lhs %in% dvn[["p1xvarnames"]]|
                    .data$lhs %in% dvn[["p2xvarnames"]]|
                    .data$lhs %in% dvn[["p1yvarnames"]]|
                    .data$lhs %in% dvn[["p2yvarnames"]]) %>%
    dplyr::select(.data$est)

  intx1 <- intercept.param$est[1:dvn[["xindper"]]]
  intx2 <- intercept.param$est[(dvn[["xindper"]]+1):(dvn[["xindper"]]*2)]
  inty1 <- intercept.param$est[((dvn[["xindper"]]*2)+1):((dvn[["xindper"]]*2)+dvn[["yindper"]])]
  inty2 <- intercept.param$est[((dvn[["xindper"]]*2)+dvn[["yindper"]]+1):((dvn[["xindper"]]*2)+2*dvn[["yindper"]])]
  missing1 <- rep(NA, (2*dvn[["xindper"]]))
  missing2 <- rep(NA, (2*dvn[["yindper"]]))

  intercept.param <- c(intx1, missing1, intx2, inty1, missing2, inty2)

  return(intercept.param)
}
