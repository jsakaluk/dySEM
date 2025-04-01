#' A Function That Fits and Compares Competing Dyadic Uni-construct Models
#' 
#' This function takes the outputted object from `scrapeVarCross()`
#'  along with the corresponding dataset and automatically tests competing
#'  uni-construct dyadic models for the latent variable under consideration.
#'  It inspects four possible model variants:
#'  * Bifactor (scripted via `dySEM::scriptBifac`)
#'  * Hierarchical (scripted via `dySEM::scriptHier`) 
#'  * Correlated Factors (scripted via `dySEM::scriptCor`)
#'  * Unidimensional (scripted via `dySEM::scriptUni`)
#'  
#'
#' @param dvn Input dvn list from `scrapeVarCross()`.
#' @param dat Input data frame containing the dataset for model estimation.
#' @param indexes Input character vector specifying which index(es) to return.
#'  Default is `c("df", "chisq", "cfi", "rmsea", "bic", "GenTEFI")`.
#'  **Note**:
#'  * Valid entries include "GenTEFI"---the Generalized Total Entropy Fit Index 
#'    (see Golino et al., 2024)---and those from `lavaan::fitMeasures()`.
#'  * If "chisq" is entered, chi-squared difference tests are automatically performed via
#'    `lavaan::lavTestLRT()`, and the resulting p-values are added to the output.
#' @param ... Additional arguments to be passed to `lavaan::cfa()`, allowing 
#'  users to customize model estimation settings. By default, the models are
#'  fit with maximum-likelihood estimation (`estimator = "ml"`) and missing
#'  data are handled via listwise deletion (`missing = "listwise"`), as per
#'  `lavaan::cfa()`'s default behaviour.
#' @param gtTab A logical input indicating whether to generate the requested 
#'  index(es) for each fitted model (requested via the `indexes` argument) in 
#'  `gt::gt()` table object format (`TRUE`). Users can also apply the `writeTo` 
#'  argument if they wish to export the `gt::gt()` table object.
#' @param writeTo A character vector string specifying a directory path to where 
#'  the `gt::gt()` table object should be saved. If set to ".", the file will be 
#'  written to the current working directory. The default is `NULL`, and examples 
#'  use a temporary directory created by `tempdir()`. `writeTo` is only relevant 
#'  if `gtTab = TRUE`.
#' @param fileName A character string specifying a desired base name for the output 
#'  `gt::gt()` file. The resulting base name will automatically be appended with a
#'  `.rtf` file extension. `fileName` is only relevant if `gtTab = TRUE` and `writeTo` 
#'  is specified.
#'  
#' @returns A `list` containing up to two components:
#'  * `Indexes`: A `tibble::tibble()` if `gtTab = FALSE` (default), or `gt::gt()` 
#'      object if `gtTab = TRUE`, with the desired index(es) for each fitted model 
#'      (requested via the `indexes` argument).
#'  * `GenTEFI`: A `tibble::tibble()` of the GenTEFI (if "GenTEFI" is included in 
#'      the `indexes` argument).
#' 
#' @details
#' * If "chisq" is included in `indexes`, the specific form of the applied 
#'    chi-squared difference test (e.g., standard vs. robust) is determined 
#'    automatically by `lavaan::lavTestLRT()`, based on the model estimation 
#'    method used.
#' * If `gtTab = TRUE` and `writeTo` is specified, then output will simultaneously 
#'    be saved as a `.rtf` file to the user's specified directory.
#' * If output file is successfully saved, a confirmation message will be printed 
#'    to the console.
#' * If a file with the same name already exists in the user's chosen directory, 
#'    it will be overwritten.
#' 
#' @export
#'
#' @examples
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
#'   outputUniConstructComp(
#'   dvn,
#'   commitmentQ,
#'   missing = "fiml"
#'   )
#'   
#'   outputUniConstructComp(
#'   dvn,
#'   commitmentQ,
#'   indexes = c("df", "bic"),
#'   missing = "fiml"
#'   )
#'   
#'   outputUniConstructComp(
#'   dvn,
#'   commitmentQ,
#'   indexes = c("df", "bic"),
#'   estimator = "ml",
#'   missing = "fiml"
#'   )
#'   
#'   outputUniConstructComp(
#'   dvn,
#'   commitmentQ,
#'   indexes = c("df", "bic"),
#'   missing = "fiml",
#'   gtTab = TRUE,
#'   writeTo = tempdir(),
#'   fileName = "uni-construct-dyad-models"
#'   )
#' 
outputUniConstructComp <- function(
    dvn,  
    dat,
    indexes = c(
      "df", "chisq", "cfi", "rmsea", "bic", "GenTEFI"
    ),
    ...,
    gtTab = FALSE,
    writeTo = NULL,
    fileName = NULL
){
  
  #checking for valid dvn
  if(length(dvn)!=6){
    stop("You must supply a dvn object containing information for only X [i.e., your target LV]")
  }
  
  #checking for valid directory path
  if (gtTab == TRUE && !is.null(writeTo)){
    
    if (!is.character(writeTo)){
      stop("The `writeTo` argument must be a character string. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
    }
    if (!dir.exists(writeTo)){
      stop("The specified directory does not exist. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
    }
    if (!is.null(fileName) && !is.character(fileName)){
      stop("The `fileName` argument must be a character string.")
    }
  }
  
  # Scripting Models --------------------------------------------------------
  
  scripts <- scriptAll(
    dvn,
    form = "uniconstruct",
    modelNames = c(
      "Bifactor",
      "Hierarchical",
      "Correlated Factors",
      "Unidimensional"
    )
  )
  
  #scripts
  
  # Fitting Models ----------------------------------------------------------
  
  mods <- fitAll(
    scripts,
    dat,
    ...
  )
  
  #mods
  
  # Extracting Indexes ---------------------------------------------------------------
  
  all_indexes <- indexExtractor(
    mods,
    dvn,
    dat
  )
  
  fit_indexes <- all_indexes$model_indexes
  gen_tefi <- all_indexes$gen_tefi
  
  
  # Creating Base -----------------------------------------------------------
  
  #verifying requests
  user_requests <- indexes %>% tolower()
  valid_indexes <- mods$"Unidimensional" %>%
    lavaan::fitMeasures() %>%
    names() %>%
    c("GenTEFI") %>%
    tolower()
  
  invalid_indexes <- setdiff(
    user_requests, 
    valid_indexes
  )
  
  if (length(invalid_indexes) > 0) {
    stop(
      "The following `indexes` are not recognized: ",
      paste(invalid_indexes, collapse = ", "), ".\n",
      "Valid options include 'GenTEFI' and those from `lavaan::fitMeasures()`."
    )
  }
  
  #generating table
  fit_indexes <- fit_indexes %>% 
    dplyr::mutate(
      "Type" = dplyr::case_when(
        Model == "Bifactor" ~ "Higher Order",
        Model == "Hierarchical" ~ "Higher Order",
        Model == "Correlated Factors" ~ "Lower Order",
        Model == "Unidimensional" ~ "Lower Order"
      )
    ) %>% 
    dplyr::select("Model", "Type", dplyr::any_of(indexes)) %>%  
    dplyr::mutate(
      dplyr::across(
        .cols = -c("Model", "Type"),
        .fns = ~ . - dplyr::lag(.),
        .names = "{.col}_diff"
      )
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_diff"),
        .fns = ~dplyr::case_when(
          dplyr::row_number() == 3 ~ NA_real_,
          TRUE ~ .
        ) 
      )
    ) 
  
  #fit_indexes
  
  # Packaging ---------------------------------------------
  
  #chi-squared difference test
  if (any(grepl("^chisq(\\.scaled)?$", user_requests))) {
    
    lrt_df <- lavaan::lavTestLRT(
      mods$Bifactor,
      mods$Hierarchical,
      mods$`Correlated Factors`,
      mods$Unidimensional
    ) %>% suppressWarnings() %>% 
      dplyr::rename(
        "pvalue" = "Pr(>Chisq)"
      )
    
    fit_indexes <- fit_indexes %>% 
      tibble::tibble(
        "chisq_diff_test_p" = lrt_df$pvalue
      ) %>% 
      dplyr::relocate("chisq_diff_test_p", .after = dplyr::any_of(c("chisq_diff", "chisq.scaled_diff")))
    
  }
  
  #GenTEFI
  if ("gentefi" %in% user_requests) {
    
    #gen_tefi
    
    fit_indexes <- fit_indexes %>% dplyr::mutate(
      "genTEFI" = dplyr::case_when(
        gen_tefi$Lower.Order.VN < gen_tefi$Higher.Order.VN & Model %in% c("Unidimensional", "Correlated Factors") ~ "Yes",
        gen_tefi$Higher.Order.VN < gen_tefi$Lower.Order.VN & Model %in% c("Hierarchical", "Bifactor") ~ "Yes",
        TRUE ~ "No"
      )
    )%>% 
      dplyr::relocate("genTEFI", .after = "Type")
    
    #print messages to console
    if (gen_tefi$Lower.Order.VN < gen_tefi$Higher.Order.VN) {
      message(
        "According to GenTEFI (%s), 
      lower-order models (e.g., correlated factors) fit the data better than higher-order models (e.g., bifactor or hierarchical),
      as the lower-order TEFI (%s) is smaller than the higher-order TEFI (%s).",
        gen_tefi$`VN.Entropy.Fit` %>% round(3),
        gen_tefi$`Lower.Order.VN` %>% round(3),
        gen_tefi$`Higher.Order.VN`%>% round(3)
      )
    } 
    else if(gen_tefi$Higher.Order.VN < gen_tefi$Lower.Order.VN) {
      message(
        sprintf(
          "According to GenTEFI (%s), 
          higher-order models (e.g., bifactor or hierarchical) fit the data better than lower-order models (e.g., correlated factors),
          as the higher-order TEFI (%s) is smaller than the lower-order TEFI (%s).",
          gen_tefi$`VN.Entropy.Fit` %>% round(3),
          gen_tefi$`Higher.Order.VN` %>% round(3),
          gen_tefi$`Lower.Order.VN` %>% round(3)
        )
      )
    }
    
    if (gtTab == TRUE){
      
      fit_indexes <- fit_indexes %>% 
        gt::gt()
      
      if (is.null(fileName)){
        gt::gtsave(
          fit_indexes,
          filename = "dySEM_table.rtf",
          path = writeTo
        )
        message( #confirmation message
          sprintf(
            "Output stored in: %s/dySEM_table.rtf",
            writeTo)
        )
      }
      else if (!is.null(fileName)){
        gt::gtsave(fit_indexes,
                   filename = sprintf("%s.rtf",
                                      fileName),
                   path = writeTo)
        message( #confirmation message
          sprintf(
            "Output stored in: %s/%s.rtf",
            writeTo, fileName)
        )
      }
      else if (is.null(writeTo)){
        
        fit_indexes <- fit_indexes %>% 
          gt::gt()
        
      }
      
    }
    
    return(
      list(
        "Indexes" = fit_indexes, 
        "GenTEFI Details" = gen_tefi
      )
    )
  }
  
  #return `fit_indexes` as-is if no special conditions are met
  
  if (gtTab == TRUE){
    
    fit_indexes <- fit_indexes %>% 
      gt::gt()
    
    if (is.null(fileName)){
      gt::gtsave(
        fit_indexes,
        filename = "dySEM_table.rtf",
        path = writeTo
      )
      message( #confirmation message
        sprintf(
          "Output stored in: %s/dySEM_table.rtf",
          writeTo)
      )
    }
    else if (!is.null(fileName)){
      gt::gtsave(fit_indexes,
                 filename = sprintf("%s.rtf",
                                    fileName),
                 path = writeTo)
      message( #confirmation message
        sprintf(
          "Output stored in: %s/%s.rtf",
          writeTo, fileName)
      )
    }
    else if (is.null(writeTo)){
      
      fit_indexes <- fit_indexes %>% 
        gt::gt()
      
    }
    
  }
  
  return(fit_indexes)  
  
}

