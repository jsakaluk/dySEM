#' A function that performs a score test for relaxing each invariance equality constraint between partners in a given dyadic SEM model.
#'
#' @param constrainFit fitted lavaan model with dyadic invariance equality constraints
#' @param filterSig logical indicating whether to filter for significant constraints (default is FALSE)
#'
#' @return a data frame with rows of equality constraints (now with readable param labels) and test statistic, df, and p for whether constraint worsens model fit
#' @export
#'
#' @examples
#' dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
#' x_delim2="_", distinguish_1="f", distinguish_2="m")
#'
#' sat.resids.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = c("loadings", "intercepts", "residuals"),
#' constr_dy_struct = "none")
#'
#' sat.resids.mod <- lavaan::cfa(sat.resids.script, data = commitmentM, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' getConstraintTests(sat.resids.mod)


getConstraintTests <- function(constrainFit, filterSig = FALSE){
  #Extract score test for relaxing each equality constraint
  constrainFit.mi <- lavaan::lavTestScore(constrainFit)
  constrainFit.mi.uni <- constrainFit.mi$uni
  constrainFit.mi.uni <- constrainFit.mi.uni |>
    dplyr::rename(constraint = .data$op)

  #Extract parameter table and create more informative parameter description
  constrainFit.PT <- lavaan::parTable(constrainFit)
  constrainFit.PT$param <- paste(constrainFit.PT$lhs, constrainFit.PT$op, constrainFit.PT$rhs, sep = " ")
  constrainFit.PT.reduce <- constrainFit.PT |>
    dplyr::select(.data$plabel, .data$param)

  #Create subset of lhs labeled parameters
  constrainFit.PT.plabel.lhs <- constrainFit.PT.reduce |>
    #use dplyr filter to select for cases where plabel matches the values of lhs or rhs in int.fit.mi.uni
    dplyr::filter(.data$plabel %in% constrainFit.mi.uni$lhs) |>
    dplyr::rename(plabel1 = .data$plabel,
                  param1 = .data$param) |>
    dplyr::mutate(constraint = "==")

  #Create subset of rhs labeled parameters
  constrainFit.PT.plabel.rhs <- constrainFit.PT.reduce |>
    #use dplyr filter to select for cases where plabel matches the values of lhs or rhs in int.fit.mi.uni
    dplyr::filter(.data$plabel %in% constrainFit.mi.uni$rhs) |>
    dplyr::rename(plabel2 = .data$plabel,
                  param2 = .data$param)

  #cbind subsets together
  constrainFit.PT.merge <- cbind(constrainFit.PT.plabel.lhs, constrainFit.PT.plabel.rhs)

  #carry over chi2, df, and p, and round
  constrainFit.PT.merge <- constrainFit.PT.merge |>
    dplyr::mutate(chi2 = constrainFit.mi.uni$X2,
                  df = constrainFit.mi.uni$df,
                  pvalue = constrainFit.mi.uni$p.value,
                  sig = dplyr::case_when(pvalue < .05 & pvalue > .01 ~ "*",
                                         pvalue < .01 & pvalue > .001 ~ "**",
                                         pvalue < .001 ~ "***")) |>
    dplyr::select(-.data$plabel1, -.data$plabel2) |>
    dplyr::mutate_if(is.numeric, round, 3)

  #filter for significance if filterSig is TRUE
  if(filterSig == TRUE){
    constrainFit.PT.merge <- constrainFit.PT.merge |>
      dplyr::filter(.data$pvalue < .05)
  }

  return(constrainFit.PT.merge)
}


