#' A Function That Exports a Table of Dyadic Invariance Model Comparison Indices
#'
#' This function takes the model from fitted dyCFA() scripts and
#' exports a table of expected output.
#'
#' @param config fitted dyadic configural invariance model lavaan object
#' @param load fitted dyadic loading invariance model lavaan object
#' @param int fitted dyadic intercept invariance model lavaan object
#' @param robust logical for whether a robust estimator has been used (default is F)
#' @return Prints word files for the table outputed to the users working directory
#' @seealso \code{\link{dyadCFA}} which this function relies on
#' @export
#' @examples
#' dvn <- dyadVarNames(dat, xvar="X",  sep = ".",
#' distinguish1 = "1", distinguish2 = "2")
#'
#' con.config.script = dyadCFA(dvn, lvname = "Conflict",  model = "configural")
#' con.loading.script = dyadCFA(dvn, lvname = "Conflict",  model = "loading")
#' con.intercept.script = dyadCFA(dvn, lvname = "Conflict",  model = "intercept")
#'
#' con.fit.config <- cfa(con.config.script, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)
#' con.fit.loading <- cfa(con.loading.script, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)
#' con.fit.intercept <- cfa(con.intercept.script, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)
#'
#' dyMIOutput(con.fit.config, con.fit.loading, con.fit.intercept, robust = F)

dyMIOutput <-function(config, load, int, robust = F){
  dirs("output")
  dirs("output/tables")
  #Extract and transpose fit indices from each model
  config.fit <- data.frame(lavaan::fitMeasures(config))
  config.fit.t <- data.frame(t(config.fit))

  loading.fit <- data.frame(lavaan::fitMeasures(load))
  loading.fit.t <- data.frame(t(loading.fit))

  int.fit <- data.frame(lavaan::fitMeasures(int))
  int.fit.t <- data.frame(t(int.fit))

  #Combine transposed fit indices for each model
  fit <- rbind(config.fit.t, loading.fit.t, int.fit.t)

  if(robust ==F){
    #Extract vectors of fit indices of interest
    Model <-c("Configural", "Loading", "Intercept")
    chi2 <-fit$chisq
    df <-fit$df
    AIC <-fit$aic
    BIC <-fit$bic
    CFI <-fit$cfi
    RMSEA <-fit$rmsea
    RMSEA_LL <-fit$rmsea.ci.lower
    RMSEA_UL <-fit$rmsea.ci.upper

    #Assemble table of descriptive indices
    fit.table <-data.frame(Model, chi2, df, AIC, BIC, CFI, RMSEA, RMSEA_LL, RMSEA_UL)

    #Calculate vectors of comparison indices
    chi2_diff <-c(NA, tail(fit.table$chi2, -1) - head(fit.table$chi2, -1))
    df_diff <-c(NA, tail(fit.table$df, -1) - head(fit.table$df, -1))
    p <-c(pchisq(chi2_diff,df_diff, lower.tail = FALSE))
    AIC_diff <-c(NA, tail(fit.table$AIC, -1) - head(fit.table$AIC, -1))
    BIC_diff <-c(NA, tail(fit.table$BIC, -1) - head(fit.table$BIC, -1))
    CFI_diff <-c(NA, tail(fit.table$CFI, -1) - head(fit.table$CFI, -1))
    RMSEA_diff <-c(NA, tail(fit.table$RMSEA, -1) - head(fit.table$RMSEA, -1))

    #Assemble table of comparison indices
    comp.table <-data.frame(Model, chi2_diff, df_diff, p, AIC_diff, BIC_diff, CFI_diff, RMSEA_diff)
  }
  if(robust == T){
    #Extract vectors of fit indices of interest
    Model <-c("Configural", "Loading", "Intercept")
    chi2 <-fit$chisq.scaled
    df <-fit$df.scaled
    AIC <-fit$aic
    BIC <-fit$bic
    CFI <-fit$cfi.robust
    RMSEA <-fit$rmsea.scaled
    RMSEA_LL <-fit$rmsea.ci.lower.scaled
    RMSEA_UL <-fit$rmsea.ci.upper.scaled

    #Assemble table of descriptive indices
    fit.table <-data.frame(Model, chi2, df, AIC, BIC, CFI, RMSEA, RMSEA_LL, RMSEA_UL)

    #Calculate vectors of comparison indices
    chi2_diff <-c(NA, tail(fit.table$chi2, -1) - head(fit.table$chi2, -1))
    df_diff <-c(NA, tail(fit.table$df, -1) - head(fit.table$df, -1))
    p <-c(pchisq(chi2_diff,df_diff, lower.tail = FALSE))
    AIC_diff <-c(NA, tail(fit.table$AIC, -1) - head(fit.table$AIC, -1))
    BIC_diff <-c(NA, tail(fit.table$BIC, -1) - head(fit.table$BIC, -1))
    CFI_diff <-c(NA, tail(fit.table$CFI, -1) - head(fit.table$CFI, -1))
    RMSEA_diff <-c(NA, tail(fit.table$RMSEA, -1) - head(fit.table$RMSEA, -1))

    #Assemble table of comparison indices
    comp.table <-data.frame(Model, chi2_diff, df_diff, p, AIC_diff, BIC_diff, CFI_diff, RMSEA_diff)
  }
  #Round numeric vectors in tables
  fit.table = fit.table %>%
    dplyr::mutate_if(is.numeric, round, digits = 3)

  comp.table = comp.table %>%
    dplyr::mutate_if(is.numeric, round, digits = 3)

  #Replace cells with NAs with "--" in comparison table
  comp.table[is.na(comp.table)] <- "--"

  #Export fit and comparison tables
  fit.tab <- sjPlot::tab_df(fit.table, title = "Fit of Dyadic Invariance Models",
                            file = "./output/tables/Dyadic Invariance Models (Fit).doc")

  comp.tab <- sjPlot::tab_df(comp.table, title = "Comparisons of Dyadic Invariance Models",
                             file = "./output/tables/Dyadic Invariance Models (Comparisons).doc")

  tab.list = list(fit.tab, comp.tab)
  return(tab.list)
}

