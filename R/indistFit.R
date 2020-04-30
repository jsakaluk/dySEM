##' A Function that Computes Corrected Fit Indexes According to the ISAT and INULL Models of Olsen & Kenny (2006)
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' measurement models for either a specified X or Y factor.
#' @param indmodel input lavaan model fitted using dyadCFA(model = "indistinguishable")
#' @param isatmod input lavaan model fitted using ISAT()
#' @param inullmod input lavaan model fitted using INULL()
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' indist.script <- dyadCFA(dvn, lvname = "Conflict", model = "indistinguishable")
#' indist.fit <- cfa(indist.script, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)
#' isat.mod <- ISAT(dvn, lvname = "Conflict")
#' isat.fit <- cfa(isat.mod, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)
#' inull.mod <- INULL(dvn, lvname = "Conflict")
#' inull.fit <- cfa(inull.mod, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)
#' indistFit(indmodel = indist.fit, isatmod = isat.fit, inullmod = inull.fit)

indistFit <- function(indmodel, isatmod, inullmod){
  #Extract and transpose indinstinguishable model fit indexes
  mod.fit <- data.frame(lavaan::fitMeasures(indmodel))
  mod.fit <- data.frame(t(mod.fit))
  #Extract and transpose ISAT model fit indexes
  isat.fit <- data.frame(lavaan::fitMeasures(isatmod))
  isat.fit <- data.frame(t(isat.fit))
  #Extract and transpose INILL model fit indexes
  inull.fit <- data.frame(lavaan::fitMeasures(inullmod))
  inull.fit <- data.frame(t(inull.fit))

  #Round off fit indexes for reporting from indinstinguishable model
  chi2_orig <- round(mod.fit$chisq, 4)
  df_orig <- mod.fit$df
  p_orig <- round(mod.fit$pvalue, 4)
  rmsea_orig <- round(mod.fit$rmsea, 4)
  tli_orig <- round(mod.fit$tli, 4)

  #Calculate adjusted fit indexes a la Olsen & Kenny (2006) and round
  chi2_adj <- round((mod.fit$chisq - isat.fit$chisq), 4)#Substract off ISAT X^2
  df_adj <- mod.fit$df - isat.fit$df#Subtract off ISAT df
  p_adj <-round(c(pchisq(chi2_adj,df_adj, lower.tail = FALSE)), 4) #Compute adjusted-p based on ISAT-adjusted X^2 and df
  rmsea_adj <- round((sqrt(((chi2_adj/df_adj)-1)/(lavaan::lavInspect(indmodel, "nobs")))),4)#Compute adjusted RMSEA
  chi2_null_adj <- inull.fit$chisq - isat.fit$chisq#Adjust INULL X^2 for ISAT X^2
  df_null_adj <- inull.fit$df - isat.fit$df#Adjust INULL df for ISAT df
  tli_adj <- ((chi2_null_adj/df_null_adj) - (chi2_adj/df_adj))/((chi2_null_adj/df_null_adj)-1)#Compute ISAT/INULL-adjusted TLI

  #Put original and adjusted indexes into data frame and return
  adj_fit <- data.frame(chi2_orig, df_orig, p_orig, rmsea_orig, tli_orig, chi2_adj, df_adj, p_adj, rmsea_adj, tli_adj)
  return(adj_fit)
}
