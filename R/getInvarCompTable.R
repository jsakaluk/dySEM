#' Compare model fit of nested dyadic invariance models in order from most parsimonious (residual) to least parsimonious (configural)
#'
#' @param mods a list of neted lavaan dyadic invariance models, in the order of residual, intercept, loading, configural
#'
#' @return a data frame of model fit statistics for each model, as well as the difference in fit statistics between each model and the previous model
#' @export
#'
#' @examples
#'
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi",
#' x_stem = "sat.g", x_delim1 = ".", x_delim2="_", distinguish_1="1", distinguish_2="2")
#'
#' sat.residual.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")
#'
#' sat.intercept.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none")
#'
#' sat.loading.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = c("loadings"), constr_dy_struct = "none")
#'
#' sat.config.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = "none", constr_dy_struct = "none")
#'
#' sat.residual.fit <- lavaan::cfa(sat.residual.script, data = commitmentQ,
#' std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' sat.intercept.fit <- lavaan::cfa(sat.intercept.script, data = commitmentQ,
#' std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' sat.loading.fit <- lavaan::cfa(sat.loading.script, data = commitmentQ,
#' std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' sat.config.fit <- lavaan::cfa(sat.config.script, data = commitmentQ,
#' std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' mods <- list(sat.residual.fit, sat.intercept.fit, sat.loading.fit, sat.config.fit)
#'
#' getInvarCompTable(mods)
#'
getInvarCompTable <- function(mods){
  
  lifecycle::deprecate_warn("1.1.0", "dySEM::getInvarCompTable()", details = "Please use `dySEM::outputInvarCompTab()` instead.")
  
  modfit <- t(as.data.frame(lavaan::fitmeasures(mods[[1]])))

  for(i in 2:length(mods)){
    modfit_iter <- t(as.data.frame(lavaan::fitmeasures(mods[[i]])))
    modfit <- rbind(modfit, modfit_iter)
  }

  modfit <- as.data.frame(modfit)
  modfit$mod <- c("residual", "intercept", "loading", "configural")
  modfit <- modfit |>
    dplyr::select(.data$mod, .data$chisq, .data$df, .data$pvalue, .data$aic, .data$bic, .data$rmsea, .data$cfi)

  modcomp <- modfit |>
    #mutate _diff cols of each except p-value, where the column value for a particular row is is the difference of the previous and current row's value for that column
    dplyr::mutate(chisq_diff = c(NA, diff(.data$chisq)),
                  df_diff = c(NA, diff(.data$df)),
                  p_diff = 1-stats::pchisq(abs(.data$chisq_diff), abs(.data$df_diff)),
                  aic_diff = c(NA, diff(.data$aic)),
                  bic_diff = c(NA, diff(.data$bic)),
                  rmsea_diff = c(NA, diff(.data$rmsea)),
                  cfi_diff = c(NA, diff(.data$cfi))) |>
    dplyr::mutate_if(is.numeric, round, 3)

  rownames(modcomp) <- NULL

  return(modcomp)
}



