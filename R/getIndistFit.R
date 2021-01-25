##' A Function that Computes Corrected Fit Indexes According to the ISAT and INULL Models of Olsen & Kenny (2006)
#'
#' This function takes the outputted model fit using scriptCFA() with model = "indist",
#' as well as scriptISAT(), and scriptINULL() and computes corrected model fit indexes according
#' to the approach outlined by Olsen & Kenny (2006)
#'
#' @param indmodel input lavaan model fitted using dyadCFA(model = "indistinguishable")
#' @param isatmod input lavaan model fitted using ISAT()
#' @param inullmod input lavaan model fitted using INULL()
#' @return A data frame of the original and corrected chi sq, df, p, rmsea, and tli
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_", x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2")
#' qual.indist.script <-  scriptCFA(dvn, lvname = "Qual", model = "indist")
#' qual.indist.mod <- cfa(qual.indist.script, data = DRES, std.lv = F, auto.fix.first= F, meanstructure = T)
#' qual.isat.script <- scriptISAT(dvn, lvxname = "Qual")
#' qual.isat.mod <- cfa(qual.isat.script, data = DRES, std.lv = F, auto.fix.first= F, meanstructure = T)
#' qual.inull.script <- scriptINULL(dvn, lvxname = "Qual")
#' qual.inull.mod <- cfa(qual.isat.script, data = DRES, std.lv = F, auto.fix.first= F, meanstructure = T)
#' corr.fit <- getIndistFit(qual.indist.mod, qual.isat.mod, qual.inull.mod)
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
  p_adj <-round(c(pchisq(chi2_adj,df_adj, lower.tail = FALSE)), 4) #Compute adjusted-p based on ISAT-adjusted X^2 and df
  rmsea_adj <- round((sqrt(((chi2_adj/df_adj)-1)/(lavaan::lavInspect(indmodel, "nobs")))),4)#Compute adjusted RMSEA
  chi2_null_adj <- inull.fit$chisq - isat.fit$chisq#Adjust INULL X^2 for ISAT X^2
  df_null_adj <- inull.fit$df - isat.fit$df#Adjust INULL df for ISAT df
  tli_adj <- ((chi2_null_adj/df_null_adj) - (chi2_adj/df_adj))/((chi2_null_adj/df_null_adj)-1)#Compute ISAT/INULL-adjusted TLI

  #Put original and adjusted indexes into data frame and return
  adj_fit <- data.frame(chi2_orig, df_orig, p_orig, rmsea_orig, tli_orig, chi2_adj, df_adj, p_adj, rmsea_adj, tli_adj)
  return(adj_fit)
}
