#' Performs random permutation tests of  invariance for ostensibly indistinguishable  dyads
#'
#' This function fits  dyadic CFA models to randomly permuted data frames
#' generated via dyRand() and dyRandPermFit(), and presents a numeric and visual
#' comparison of the fit of the observed invariance model against
#' the fit of the empirical null sampling distribtuion
#' @param dvn input object from dyadVarNames()
#' @param dat input data frame
#' @param n input numeric argument for number of randomly permuted samples to use
#' @param AFIs Optional character vector indicating which alternative fit indices (or chi-squared itself)
#' @param model Character argument indicating type of invariance model to evaluate
#' are to be used to be extracted from the model fit in each permuted sample
#' @return A randomly permuted data frame
#' @seealso \code{\link{dyadVarNames}} \code{\link{dyRand}} \code{\link{dyRandPermFit}} which this function relies on
#' @export
#' @examples
#' dat = dplyr::select(DRES, sexsat1.1:sexsat5.2)
#' dvn = dyadVarNames(dat, xvar = "sexsat", sep = ".", distinguish1 = "1", distinguish2 = "2")
#' config.test <- dyRandPermTest(dvn, dat, n = 10, model = "configural")

dyRandPermTest<-function(dvn, dat, n, AFIs = NULL, model = "configural"){
  if(model == "configural"){
    permout = dyRandPermFit(dvn, dat, n, model = "configural")
    script = dyadCFA(dvn, model = "configural")
  }else if(model == "loading"){
    permout = dyRandPermFit(dvn, dat, n, model = "loading")
    script = dyadCFA(dvn, model = "loading")
  }else if(model == "intercept"){
    permout = dyRandPermFit(dvn, dat, n, model = "intercept")
    script = dyadCFA(dvn, model = "intercept")
  }
  model.fit <- lavaan::cfa(script, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)

  isat.mod <- ISAT(dvn)
  isat.fit <- cfa(isat.mod, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)

  inull.mod <- INULL(dvn)
  inull.fit <- cfa(inull.mod, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)

  ind.fit <- indistFit(indmodel = model.fit, isatmod = isat.fit, inullmod = inull.fit)

  prob.func<- ecdf(permout)
  prob<-prob.func(ind.fit$chi2_adj)

  perm.plot <-ggplot2::ggplot()+
    ggplot2::geom_density(ggplot2::aes(x = permout))+
    ggplot2::geom_vline(xintercept = ind.fit$chi2_adj, linetype = "dashed")+
    ggplot2::labs(x = "Null Chi Square Values")
  perm.plot

  print(prob)
  newList <- list("p" = prob, "plot" = perm.plot)
  return(newList)
}
