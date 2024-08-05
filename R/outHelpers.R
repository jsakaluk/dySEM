#' @name outHelpers
#' @rdname outHelpers
#'
#' @title Helpers for creating output files
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param fit name of fitted lavaan model
#' @param model type of fitted dyadic model (i.e., "apim", "bidyc", "bidys","cfa", "cfm", "mim")
#' @param tabletype kind of parameter estimates requested (i.e. from "measurement" or "structural" model)
#' @param type input character for sempaths to indicate whether parameters "free" or "equated" in estimation
#' @param writeTo A character string specifying a directory path to where the file(s) should be saved.
#' The default is NULL, and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the output file(s).
#' If a `fileName` not provided (i.e., "fileName = NULL"), then defaults will be used
#' The specified name will be automatically appended with the appropriate file extension (i.e., .png for figures).
#' @param gtTab A logical input indicating whether to generate the table(s) in `gt::gt()` table object format (`TRUE`).
#' @family helpers
#' @noRd

makeTable <- function(dvn, fit, model, tabletype, gtTab = TRUE){
  if(length(dvn) == 6 & model == "cfa" & tabletype == "measurement"){
    #Extract intercepts
    xints <- xintercepts(dvn, fit)

    #Extract loadings, SEs, Z, p,
    tab <- loadings(dvn, fit)
    tab$Intercept = xints
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 6 & model == "bidyc" & tabletype == "measurement"){
    #Extract intercepts
    xints <- xbidyIntercepts(dvn, fit)

    #Extract loadings, SEs, Z, p,
    tab <- bidyLoadings(dvn, fit)
    tab$Intercept = xints
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
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

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 9 & model == "apim" & tabletype == "structural"){
    tab = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
      dplyr::filter(.data$op == "~"|.data$op == ":="|.data$op == "~~") %>%
      dplyr::filter(!.data$lhs %in% dvn[["p1xvarnames"]]&
                      !.data$lhs %in% dvn[["p2xvarnames"]]&
                      !.data$lhs %in% dvn[["p1yvarnames"]]&
                      !.data$lhs %in% dvn[["p2yvarnames"]]) %>%
      dplyr::select(.data$lhs, .data$op,.data$rhs, "Label" = .data$label, "Slope"=.data$est, "SE"=.data$se,
                    'p-value'=.data$pvalue, '95%CI LL' = .data$ci.lower, '95%CI UL' = .data$ci.upper, "Std. Slope"=.data$std.all)
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 9 & model == "mim" & tabletype == "measurement"){
    #Extract intercepts
    xyints <- xyintercepts(dvn, fit)

    #Extract loadings, SEs, Z, p,
    tab <- loadings(dvn, fit)
    tab$Intercept = xyints
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 9 & model == "mim" & tabletype == "structural"){
    tab = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
      dplyr::filter(.data$op == "~"|.data$op == ":="|.data$op == "~~") %>%
      dplyr::filter(!.data$lhs %in% dvn[["p1xvarnames"]]&
                      !.data$lhs %in% dvn[["p2xvarnames"]]&
                      !.data$lhs %in% dvn[["p1yvarnames"]]&
                      !.data$lhs %in% dvn[["p2yvarnames"]]) %>%
      dplyr::select(.data$lhs, .data$op,.data$rhs, "Label" = .data$label, "Slope"=.data$est, "SE"=.data$se,
                    'p-value'=.data$pvalue, '95%CI LL' = .data$ci.lower, '95%CI UL' = .data$ci.upper, "Std. Slope"=.data$std.all)
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 9 & model == "cfm" & tabletype == "measurement"){
    #Extract intercepts
    xyints <- xyintercepts(dvn, fit)

    #Extract loadings, SEs, Z, p,
    tab <- loadings(dvn, fit)
    tab$Intercept = xyints
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 9 & model == "cfm" & tabletype == "structural"){
    tab = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
      dplyr::filter(.data$op == "~"|.data$op == ":="|.data$op == "~~") %>%
      dplyr::filter(!.data$lhs %in% dvn[["p1xvarnames"]]&
                      !.data$lhs %in% dvn[["p2xvarnames"]]&
                      !.data$lhs %in% dvn[["p1yvarnames"]]&
                      !.data$lhs %in% dvn[["p2yvarnames"]]) %>%
      dplyr::select(.data$lhs, .data$op,.data$rhs, .data$est, "SE"=.data$se,
                    'p-value'=.data$pvalue, '95%CI LL' = .data$ci.lower, '95%CI UL' = .data$ci.upper, "Std. Est"=.data$std.all)
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 9 & model == "bidys" & tabletype == "measurement"){
    #Extract intercepts
    xyints <- xybidyIntercepts(dvn, fit)

    #Extract loadings, SEs, Z, p,
    tab <- bidyLoadings(dvn, fit)
    tab$Intercept = xyints
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
  else if(length(dvn) == 9 & model == "bidys" & tabletype == "structural"){
    tab = lavaan::parameterEstimates(fit, standardized=TRUE) %>%
      dplyr::filter(.data$op == "~"|.data$op == ":="|.data$op == "~~") %>%
      dplyr::filter(!.data$lhs %in% dvn[["p1xvarnames"]]&
                      !.data$lhs %in% dvn[["p2xvarnames"]]&
                      !.data$lhs %in% dvn[["p1yvarnames"]]&
                      !.data$lhs %in% dvn[["p2yvarnames"]]) %>%
      dplyr::filter(!is.na(.data$z)) %>%
      dplyr::select(.data$lhs, .data$op,.data$rhs, "Label" = .data$label, .data$est, "SE"=.data$se,
                    'p-value'=.data$pvalue, "95%CI LL" = .data$ci.lower, "95%CI UL" = .data$ci.upper, "Std. Est"=.data$std.all)
    tab = tab %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    tab$'p-value'[tab$'p-value' < .001] = "< .001"

    if(gtTab == TRUE){
      tab <- gt::gt(tab)
    }
    if(gtTab == FALSE){
      tab <- tibble::as_tibble(tab)
    }
    return(tab)
  }
}

#' @rdname outHelpers
#' @noRd

makeFigure <- function(fit, type, writeTo, fileName){
  #user provides fileName
  if(!is.null(fileName)){

    if(type == "raw"){
      semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "est", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = F,
                                   edge.color = "black", filetype = "png", filename = sprintf("%s/%s unstd", writeTo, fileName
                                   ), weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
    else if(type == "std"){
      semplot <- semPlot::semPaths(fit, what = "std", whatLabels = "std", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = F,
                                   edge.color = "black", filetype = "png", filename = sprintf("%s/%s std", writeTo, fileName
                                   ), weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
    else if(type == "lab"){
      semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "names", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = T,
                                   edge.color = "black", filetype = "png", filename = sprintf("%s/%s lab", writeTo, fileName
                                   ), weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
  }

  #default
  if(is.null(fileName)){

    if(type == "raw"){
      semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "est", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = F,
                                   edge.color = "black", filetype = "png", filename = sprintf("%s/dySEM_figure unstd", writeTo),
                                   weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
    else if(type == "std"){
      semplot <- semPlot::semPaths(fit, what = "std", whatLabels = "std", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = F,
                                   edge.color = "black", filetype = "png", filename = sprintf("%s/dySEM_figure std", writeTo), weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
    else if(type == "lab"){
      semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "names", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = T,
                                   edge.color = "black", filetype = "png", filename = sprintf("%s/dySEM_figure lab", writeTo), weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
  }

  return(semplot)
}
