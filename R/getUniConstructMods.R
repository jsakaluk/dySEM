#' A Function That Fits and Examines Competing Dyadic Uni-construct Models 
#'
#' This function takes the outputted object from `scrapeVarCross()`
#'  along with the corresponding dataset and automatically tests competing
#'  uni-construct dyadic models for the latent variable under consideration.
#'  It inspects four possible model variants:
#'  * Unidimensional (via `scriptUni`)
#'  * Correlated Factors (via `scriptCor`)
#'  * Hierarchical (via `scriptHier`)
#'  * Bifactor (via `scriptBifac`)
#'
#' @param dvn Input dvn list from `scrapeVarCross()`.
#' @param dat Input data frame containing the dataset for model estimation.
#' @param indexes Input character vector specifying which index(es) to return.
#'  Default is `c("npar", "df", "chisq", "cfi", "rmsea", "tli", "aic", "bic")`.
#'  **Note**: Must be valid options from `lavaan::fitMeasures()`.
#' @param modelCompare A logical input indicating whether to compare fit statistics 
#'  (i.e., chi-squared, AIC, and BIC) of the fitted models. The default is `TRUE`.
#'  **Note**: A scaled chi-squared difference test will automatically be applied
#'  (via `lavaan::lavTestLRT()`) if a robust estimator variant (e.g., `estimator = "mlr"`) 
#'  is used.
#' @param genTEFI A logical input indicating whether to compute the 
#'  Generalized Total Entropy Fit Index (genTEFI) (see Golino et al., 2024).
#'    Default is `TRUE`.
#' @param ... Additional arguments to be passed to `lavaan::cfa()`, allowing 
#'  users to customize model estimation settings. By default, the models will
#'  be fit with robust maximum-likelihood (MLR) estimation---`estimator = "mlr"`
#'  ---and missing data will handled using full-information maximum likelihood
#'  (FIML)---`missing = "fiml"`.
#'
#' @returns A `list` containing up to three components:
#'  * `Global Fit`: A `tibble` with the desired index(es) for each fitted model (requested via the `indexes` argument).
#'  * `Model Comparisons`: A `tibble` summarizing the differences in fit statistics between the fitted models. (if `modelCompare = TRUE`).
#'  * `genTEFI`: A `tibble` of the genTEFI (if `genTEFI = TRUE`).
#'  
#' @export
#'
#' @examples
#' 
#' dvn <- scrapeVarCross(
#'   commitmentQ,
#'   x_order = "spi",
#'   x_stem = "sat.g",
#'   x_delim1 = ".",
#'   x_delim2="_",
#'   distinguish_1="1",
#'   distinguish_2="2"
#'   )
#'   
#'   getUniConstructMods(
#'   dvn,
#'   commitmentQ
#'   )
#'   
#'   getUniConstructMods(
#'   dvn,
#'   commitmentQ,
#'   indexes = c("npar", "df", "bic"),
#'   modelCompare = FALSE,
#'   genTEFI = TRUE
#'   )
#'   
#'   getUniConstructMods(
#'   dvn,
#'   commitmentQ,
#'   indexes = c("npar", "df", "bic"),
#'   modelCompare = FALSE,
#'   genTEFI = TRUE,
#'   estimator = "ml"
#'   )
#' 
getUniConstructMods <- function(
    dvn,
    dat,
    indexes = c(
      "npar", "df",
      "chisq", "cfi", "rmsea", "tli", "aic", "bic"
      ),
    modelCompare = TRUE,
    genTEFI = TRUE,
    ...
    ){

  lifecycle::deprecate_warn(
    when = "1.4.0", 
    what = "dySEM::getUniConstructMods()", 
    details = "Please use `dySEM::outputUniConstructComp()` instead."
    )
  
  
  if(length(dvn)!=6){
    stop("You must supply a dvn object containing information for only X [i.e., your target LV]")
  }

# Scripting Models -----------------------------------------------------------------

  uni.script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "LV",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )
  
  cor.script <- scriptCor(
    dvn,
    scaleset = "FF",
    lvname = "LV",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )
  
  hier.script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "LV",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )
  
  bifac.script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "LV",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )
  
# Fitting Models ----------------------------------------------------------

  #merging default argumentation with user-specified argumentation (...)
  default_args <- list(
    estimator = "mlr",
    missing = "fiml"
  )
  user_args <- list(...)
  final_args <- utils::modifyList(
    default_args, user_args
  )
  
  #fitting models with merged argumentation
  uni.fit <- do.call(
    lavaan::cfa,
    c(
      list(model = uni.script, data = dat),
      final_args
      )
    )

  cor.fit <- do.call(
    lavaan::cfa,
    c(
      list(model = cor.script, data = dat),
      final_args
    )
  )
  
  hier.fit <- do.call(
    lavaan::cfa,
    c(
      list(model = hier.script, data = dat),
      final_args
    )
  )
  
  bifac.fit <- do.call(
    lavaan::cfa,
    c(
      list(model = bifac.script, data = dat),
      final_args
    )
  )
  
  #uni.fit
  #cor.fit
  #hier.fit
  #bifac.fit

# `indexes` -----------------------------------------

  valid_indexes <- bifac.fit %>% 
    lavaan::fitMeasures() %>% 
    names() %>% 
    tolower()

  if(!any(tolower(indexes) %in% valid_indexes)){
    stop("`indexes` must be a character vector containing valid options from `lavaan::fitMeasures()`")
  }
  
  uni.fit.measures <- lavaan::fitmeasures(
    object = uni.fit,
    fit.measures = indexes
  ) %>% as.list() %>% tibble::as_tibble() %>% dplyr::mutate("Model" = "Unidimensional")
  
  cor.fit.measures <- lavaan::fitmeasures(
    object = cor.fit,
    fit.measures = indexes
  ) %>% as.list() %>% tibble::as_tibble() %>% dplyr::mutate("Model" = "Correlated Factors")
  
  hier.fit.measures <- lavaan::fitmeasures(
    object = hier.fit,
    fit.measures = indexes
  ) %>% as.list() %>% tibble::as_tibble() %>% dplyr::mutate("Model" = "Hierarchical")
  
  bifac.fit.measures <- lavaan::fitmeasures(
    object = bifac.fit,
    fit.measures = indexes
  ) %>% as.list() %>% tibble::as_tibble() %>% dplyr::mutate("Model" = "Bifactor")
  
  fit_indexes <- dplyr::bind_rows(
    bifac.fit.measures,
    hier.fit.measures,
    cor.fit.measures,
    uni.fit.measures
  ) %>% 
    dplyr::relocate("Model")
    
  
  #fit_indexes

# `modelCompare` ---------------------------------------------------------------------

  modcomp_helper <- function(simple_mod, complex_mod, comp_label){
    
    lav_lrt <- lavaan::lavTestLRT(
      simple_mod, complex_mod
    ) %>% 
      tibble::tibble()
    
    tibble::tibble(
      Comparison = comp_label,
      chisq_diff = round(lav_lrt$`Chisq diff`[2], 3),
      df_diff = lav_lrt$`Df diff`[2],
      pvalue = lav_lrt$`Pr(>Chisq)`[2],
      sig = dplyr::case_when(pvalue < .05 & pvalue > .01 ~ "*",
                             pvalue < .01 & pvalue > .001 ~ "**",
                             pvalue < .001 ~ "***"),
      aic_diff = round(lavaan::fitMeasures(simple_mod, "aic") - lavaan::fitMeasures(complex_mod, "aic"), 3) %>% as.double(),
      bic_diff = round(lavaan::fitMeasures(simple_mod, "bic") - lavaan::fitMeasures(complex_mod, "bic"), 3) %>% as.double()
    ) 
  }
  
  comp_df <- dplyr::bind_rows(
    
    modcomp_helper(
      hier.fit, bifac.fit,
      "Hierarchical vs. Bifactor"
    ),
    suppressWarnings(modcomp_helper(
      cor.fit, hier.fit,
      "Correlated factors vs. Hierarchical"
    )),
    modcomp_helper(
      uni.fit, cor.fit,
      "Unidimensional vs. Correlated factors"
    )
  )  
  
  #comp_df

# `genTEFI` -----------------------------------------------------------------

  ega <- EGAnet::EGA(
    dat %>% dplyr::select(c(dvn$p1xvarnames, dvn$p2xvarnames)),
    plot.EGA = FALSE
  )
  
  gen_tefi <- EGAnet::genTEFI(
    ega$correlation,
    structure = list(
      lower_order = c(rep(1, length(dvn$p1xvarnames)), rep(2, length(dvn$p2xvarnames))),
      higher_order = c(rep(1, length(dvn$p1xvarnames)), rep(1, length(dvn$p2xvarnames)))
    )
  ) %>% tibble::tibble()
  
  #gen_tefi

# Packaging ---------------------------------------------------------------

  #fit_indexes
  #comp_df
  #gen_tefi
  
  results <- list(
    "Global Fit" = fit_indexes, 
    "Model Comparisons" = comp_df,
    "genTEFI" = gen_tefi
  )
  
  #only indexes
  if(!is.null(indexes) && modelCompare == FALSE && genTEFI == FALSE){
    
    #results$"Global Fit"
    
    return(results$"Global Fit")
  }
  
  #only lrt
  else if(is.null(indexes) && modelCompare == TRUE && genTEFI == FALSE){
    
    #results$"Model Comparisons"
    
    return(results$"Model Comparisons")
  }
  
  #only genTEFI
  else if(is.null(indexes) && modelCompare == FALSE && genTEFI == TRUE){
    
    results$"genTEFI"
    
    return(results$"genTEFI")
  }
  
  #modelCompare + genTEFI
  else if(is.null(indexes) && modelCompare == TRUE && genTEFI == TRUE){
    
    #results[c("Model Comparisons", "genTEFI")]
    
    return(results[c("Model Comparisons", "genTEFI")])
  }
  
  #indexes + modelCompare
  else if(!is.null(indexes) && modelCompare == TRUE && genTEFI == FALSE){
    
    #results[c("Global Fit", "Model Comparisons")]
    
    return(results[c("Global Fit", "Model Comparisons")])
  }
  
  #indexes + genTEFI
  else if(!is.null(indexes) && modelCompare == FALSE && genTEFI == TRUE){
    
    results[c("Global Fit", "genTEFI")]
    
    return(results[c("Global Fit", "genTEFI")])
  }
  
  #default (all three)
  else{
    
    results
    
    return(results)
  }
  
}

