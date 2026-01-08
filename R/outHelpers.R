#' @name outHelpers
#' @rdname outHelpers
#'
#' @title Helpers for creating output files
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param fit name of fitted lavaan model
#' @param model type of fitted dyadic model (i.e., "apim", "bidyc", "bidys","cfa", "cfm", "mim")
#' @param tabletype kind of parameter estimates requested (i.e. from "measurement" or "structural" model, or "correlation" table for latent variables)
#' @param type input character for sempaths to indicate whether parameters "free" or "equated" in estimation
#' @template writeTo
#' @template fileName
#' @template gtTab
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
  else if(length(dvn) == 6 & model == "cfa" & tabletype == "correlation"){

    #get lv correlation matrix (rs)
    rs <- lavaan::lavInspect(fit, "cor.lv")

    #extract lv names from the generated lv matrix
    lv_names <- rownames(rs)

    #get p-values
    params <- lavaan::parameterEstimates(fit) |>
      dplyr::filter(
        .data$op == "~~",
        .data$lhs %in% lv_names,
        .data$rhs %in% lv_names,
        .data$lhs != .data$rhs
      )

    #set up significance stars (ss)
    star_strings <- ifelse(is.na(params$pvalue), "",
                           ifelse(params$pvalue < .001, "***",
                                  ifelse(params$pvalue < .01,  "**",
                                         ifelse(params$pvalue < .05,  "*", ""))))

    ss <- matrix(
      "",
      nrow = length(lv_names),
      ncol = length(lv_names),
      dimnames = list(lv_names, lv_names)
      )

    ss[cbind(match(params$lhs, lv_names), match(params$rhs, lv_names))] <- star_strings
    ss[cbind(match(params$rhs, lv_names), match(params$lhs, lv_names))] <- star_strings

    #create table
    tab <- rs |>
      formatC(format = "f", digits = 3) |>
      matrix(
        nrow = nrow(rs),
        ncol = ncol(rs),
        dimnames = dimnames(rs)
      )
    tab[] <- tab |> paste0(ss)

    #finalize table
    tab[upper.tri(tab)] <- "\u2014" #filler for upper triangle
    diag(tab) <- "\u2014" #filler for diagonal
    tab <- tab|>
      as.data.frame()|>
      tibble::rownames_to_column(var = " ") |>
      tibble::as_tibble()

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
      dplyr::select(.data$lhs, .data$op,.data$rhs, "Label" = .data$label, "Estimate"=.data$est, "SE"=.data$se,
                    'p-value'=.data$pvalue, '95%CI LL' = .data$ci.lower, '95%CI UL' = .data$ci.upper, "Std. Estimate"=.data$std.all)
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
                    'p-value'=.data$pvalue, '95%CI LL' = .data$ci.lower, '95%CI UL' = .data$ci.upper, "Std. Estimate"=.data$std.all)
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
                    'p-value'=.data$pvalue, '95%CI LL' = .data$ci.lower, '95%CI UL' = .data$ci.upper, "Std. Estimate"=.data$std.all)
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
                    'p-value'=.data$pvalue, "95%CI LL" = .data$ci.lower, "95%CI UL" = .data$ci.upper, "Std. Estimate"=.data$std.all)
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
  if(!is.null(writeTo) & !is.null(fileName)){

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
  else if(!is.null(writeTo)& is.null(fileName)){

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
  else if(is.null(writeTo) & is.null(fileName)){

    if(type == "raw"){
      semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "est", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = F,
                                   edge.color = "black", weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
    else if(type == "std"){
      semplot <- semPlot::semPaths(fit, what = "std", whatLabels = "std", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = F,
                                   edge.color = "black", weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
    else if(type == "lab"){
      semplot <- semPlot::semPaths(fit, what = "est", whatLabels = "names", edge.label.cex = 0.5,
                                   curvePivot = F, intercepts = T,
                                   edge.color = "black", weighted = F,
                                   edge.label.position = .3, nCharNodes = 0, fixedStyle = c("black", 2))
    }
  }

  return(semplot)
}
