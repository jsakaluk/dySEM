#' A Function that fits dyadic CFA models to randomly permuted data frames
#'
#' This function fits  dyadic CFA models to randomly permuted data frames
#' generated via dyRand(), and extracts specified fit indexes from these null cases to
#' form an empirical sampling distribution to be used for testing dyadic invariance
#' for indistinguishable dyads
#' @param dvn input object from dyadVarNames()
#' @param dat input data frame
#' @param n input numeric argument for number of randomly permuted samples to use
#' @param AFIs Optional character vector indicating which alternative fit indices (or chi-squared itself)
#' @param model Character argument indicating type of invariance model to evaluate
#' are to be used to be extracted from the model fit in each permuted sample
#' @return A randomly permuted data frame
#' @seealso \code{\link{dyadVarNames}} \code{\link{dyRand}} which this function relies on
#' @export
#' @examples
#' dat = dplyr::select(DRES, sexsat1.1:sexsat5.2)
#' dvn = dyadVarNames(dat, xvar = "sexsat", sep = ".", distinguish1 = "1", distinguish2 = "2")
#' permout = dyRandPermFit(dvn, dat, n = 10)

dyRandPermFit<-function(dvn, dat, n, AFIs = NULL, model = "configural"){
  permout = numeric(n)
  for(i in 1:n){
    perm = dyRand(dvn, dat)
    if(model == "configural"){
      script = dyadCFA(dvn, model = "configural")
    }else if(model == "loading"){
      script = dyadCFA(dvn, model = "loading")
    }else if(model == "intercept"){
      script = dyadCFA(dvn, model = "intercept")
    }
    fit.mod <- cfa(script, data = perm, std.lv = F, auto.fix.first= F, meanstructure = T)

    isat.mod <- ISAT(dvn)
    isat.fit <- cfa(isat.mod, data = perm, std.lv = F, auto.fix.first= F, meanstructure = T)

    inull.mod <- INULL(dvn)
    inull.fit <- cfa(inull.mod, data = perm, std.lv = F, auto.fix.first= F, meanstructure = T)

    ind.fit <- indistFit(indmodel = fit.mod, isatmod = isat.fit, inullmod = inull.fit)

    permout[i] = ind.fit$chi2_adj
  }
  return(permout)
}
