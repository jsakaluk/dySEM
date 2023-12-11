##' A Function that Computes Corrected Fit Indexes According to the ISAT and INULL Models of Olsen & Kenny (2006)
#'
#' This function takes the outputted model fit using scriptCFA() with model = "indist",
#' as well as scriptISAT(), and scriptINULL() and computes corrected model fit indexes according
#' to the approach outlined by Olsen & Kenny (2006)
#'
#' @param indmodel input lavaan model object fitted using dyadCFA(model = "indistinguishable")
#' @param isatmod input lavaan model object fitted using ISAT()
#' @param inullmod input lavaan model object fitted using INULL()
#' @return A data frame of the original and corrected chi sq, df, p, rmsea, and tli
#' @family supplemental model calculators
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")
#'
#' sat.indist.script <-  scriptCFA(dvn, lvname = "Sat")
#' sat.indist.mod <- lavaan::cfa(sat.indist.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' sat.isat.script <- scriptISAT(dvn, lvxname = "Sat")
#' sat.isat.mod <- lavaan::cfa(sat.isat.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = FALSE)
#'
#' sat.inull.script <- scriptINULL(dvn, lvxname = "Sat")
#' sat.inull.mod <- lavaan::cfa(sat.inull.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = FALSE)
#'
#' getIndistFit(sat.indist.mod, sat.isat.mod, sat.inull.mod)
#'
getIndistFit <- function(indmodel, isatmod, inullmod){
  #Extract and transpose indistinguishable model fit indexes
  mod.fit <- data.frame(lavaan::fitMeasures(indmodel))
  mod.fit <- data.frame(t(mod.fit))
  #Extract and transpose ISAT model fit indexes
  isat.fit <- data.frame(lavaan::fitMeasures(isatmod))
  isat.fit <- data.frame(t(isat.fit))
  #Extract and transpose INILL model fit indexes
  inull.fit <- data.frame(lavaan::fitMeasures(inullmod))
  inull.fit <- data.frame(t(inull.fit))

  #Round off fit indexes for reporting from indistinguishable model
  chi2_orig <- round(mod.fit$chisq, 4)
  df_orig <- mod.fit$df
  p_orig <- round(mod.fit$pvalue, 4)
  rmsea_orig <- round(mod.fit$rmsea, 4)
  tli_orig <- round(mod.fit$tli, 4)

  #Calculate adjusted fit indexes a la Olsen & Kenny (2006) and round
  chi2_adj <- round((mod.fit$chisq - isat.fit$chisq), 4)#Subtract off ISAT X^2
  df_adj <- mod.fit$df - isat.fit$df#Subtract off ISAT df
  p_adj <-round(c(stats::pchisq(chi2_adj,df_adj, lower.tail = FALSE)), 4) #Compute adjusted-p based on ISAT-adjusted X^2 and df
  rmsea_adj <- round((sqrt(((chi2_adj/df_adj)-1)/(lavaan::lavInspect(indmodel, "nobs")))),4)#Compute adjusted RMSEA
  chi2_null_adj <- inull.fit$chisq - isat.fit$chisq#Adjust INULL X^2 for ISAT X^2
  df_null_adj <- inull.fit$df - isat.fit$df#Adjust INULL df for ISAT df
  tli_adj <- ((chi2_null_adj/df_null_adj) - (chi2_adj/df_adj))/((chi2_null_adj/df_null_adj)-1)#Compute ISAT/INULL-adjusted TLI

  #Put original and adjusted indexes into data frame and return
  adj_fit <- data.frame(chi2_orig, df_orig, p_orig, rmsea_orig, tli_orig, chi2_adj, df_adj, p_adj, rmsea_adj, tli_adj)
  return(adj_fit)
}
