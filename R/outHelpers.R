#' @name outHelpers
#' @rdname outHelpers
#'
#' @title Helpers for creating output files
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param param input character for what kind of parameter is being scripted ("act", "apim_part", "mim_part", "cf")
#' @param lvar input character for whether scripting helpers target latent "X" or :Y" indicator variables in dvn
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param partner input character to indicate parameters for first or second dyad member
#' @param type input character to indicate whether parameters "free" or "equated" in estimation
#' @family helpers
#' @export

#' @rdname outHelpers

makeTable <- function(dvn, fit, model, tabletype){
  if(length(dvn) == 6 & model == "cfa" & tabletype == "measurement"){
    #Extract intercepts
    xints <- xintercepts(dvn, fit)

    #Extract loadings, SEs, Z, p,
    tab <- loadings(dvn, fit)
    tab$Intercept = xints
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    tab <- gt::gt(tab)
    return(tab)
  }
  else if(length(dvn) == 9 & model == "apim" & tabletype == "measurement"){
    #Extract intercepts
    xyints <- xyintercepts(dvn, fit)

    #Extract loadings, SEs, Z, p,
    tab <- loadings(dvn, fit)
    tab$Intercept = xyints
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    tab <- gt::gt(tab)
    return(tab)
  }
  else if(length(dvn) == 9 & model == "apim" & tabletype == "structural"){
    tab = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
      dplyr::filter(.data$op == "~"|.data$op == ":=") %>%
      dplyr::select('Outcome'=.data$lhs, "Predictor"=.data$rhs, "Label" = .data$label, "Slope"=.data$est, "SE"=.data$se,
                    'p-value'=.data$pvalue, '95%CI LL' = .data$ci.lower, '95%CI UL' = .data$ci.upper, "Std. Slope"=.data$std.all)
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    tab <- gt::gt(tab)
    return(tab)
  }
}

#' @rdname outHelpers

makeFigure <- function(fit, type){
  if(type == "raw"){
    semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "est", edge.label.cex = 0.5,
                                 curvePivot = F, intercepts = F,
                                 edge.color = "black", filetype = "png", filename = sprintf("output/figures/%s unstd", stringr::str_remove_all(as.character(fit@call$model), "[.]")
                                 ), weighted = F,
                                 edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
  }
  else if(type == "std"){
    semplot <- semPlot::semPaths(fit, what = "std", whatLabels = "std", edge.label.cex = 0.5,
                                 curvePivot = F, intercepts = F,
                                 edge.color = "black", filetype = "png", filename = sprintf("output/figures/%s std", stringr::str_remove_all(as.character(fit@call$model), "[.]")
                                 ), weighted = F,
                                 edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
  }
  else if(type == "lab"){
    semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "names", edge.label.cex = 0.5,
                                 curvePivot = F, intercepts = T,
                                 edge.color = "black", filetype = "png", filename = sprintf("output/figures/%s lab", stringr::str_remove_all(as.character(fit@call$model), "[.]")
                                 ), weighted = F,
                                 edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
  }
  return(semplot)
}
