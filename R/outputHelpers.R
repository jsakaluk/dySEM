#' @name outputHelpers
#' @rdname outputHelpers
#'
#' @title Helper functions for model evaluation and outputting indexes of model fit
#'
#' @param dvn A dvn list from `scrapeVarCross`.
#' @param form A character string specifying the form of the models under consideration (e.g., "uniconstruct" or "biconstruct").
#' @param modelNames A character vector detailing the names of the models to be scripted.
#' @param scripts A list of `lavaan` model scripts.
#' @param dat A data frame containing the dataset for model estimation.
#' @param ... Additional arguments to be passed to `lavaan::cfa()`, allowing
#'  users to customize model estimation settings. By default, the models are
#'  fit with maximum-likelihood estimation (`estimator = "ml"`) and missing
#'  data are handled via listwise deletion (`missing = "listwise"`), as per
#'  `lavaan::cfa()`'s default behaviour.
#'  @param models A list of fitted `lavaan` models.
#'
#' @family helpers
#'
#' @noRd
scriptAll <- function(
    dvn,
    form = "uniconstruct",
    modelNames = c(
      "Bifactor",
      "Hierarchical",
      "Correlated Factors",
      "Unidimensional"
    )) {
  if (form %>% tolower() == "uniconstruct") {
    valid_models <- c(
      "Bifactor",
      "Hierarchical",
      "Correlated Factors",
      "Unidimensional"
    )

    requested_models <- modelNames

    invalid_models <- setdiff(
      requested_models,
      valid_models
    )

    if (length(invalid_models) > 0) {
      stop(
        "Invalid model type(s)/spelling: ", paste(invalid_models, collapse = ", "),
        ". Valid options are: ", paste(valid_models, collapse = ", "), "."
      )
    }


    script_bank <- list(
      "Unidimensional" = scriptUni(
        dvn,
        scaleset = "FF",
        lvname = "LV",
        constr_dy_meas = "none",
        constr_dy_struct = "none"
      ),
      "Correlated Factors" = scriptCor(
        dvn,
        scaleset = "FF",
        lvname = "LV",
        constr_dy_meas = "none",
        constr_dy_struct = "none"
      ),
      "Hierarchical" = scriptHier(
        dvn,
        scaleset = "FF",
        lvname = "LV",
        constr_dy_meas = "none",
        constr_dy_struct = "none"
      ),
      "Bifactor" = scriptBifac(
        dvn,
        scaleset = "FF",
        lvname = "LV",
        constr_dy_meas = "none",
        constr_dy_struct = "none"
      )
    )

    script_bank[requested_models]
  }

  if (form %>% tolower() == "biconstruct") {
    valid_models <- c(
      "APIM",
      "MIM",
      "CFM",
      "BiDy"
    )

    requested_models <- modelNames

    invalid_models <- setdiff(
      requested_models,
      valid_models
    )

    if (length(invalid_models) > 0) {
      stop(
        "Invalid model type(s)/spelling: ", paste(invalid_models, collapse = ", "),
        ". Valid options are: ", paste(valid_models, collapse = ", "), "."
      )
    }

    script_bank <- list(

      # bi-construct models
      "APIM" = scriptAPIM(
        dvn,
        scaleset = "FF",
        "LVX", "LVY",
        constr_dy_x_meas = "none",
        constr_dy_x_struct = "none",
        constr_dy_y_meas = "none",
        constr_dy_y_struct = "none",
        constr_dy_xy_struct = "none"
      ),
      "BiDy" = scriptBiDy(
        dvn,
        type = "SEM",
        # scaleset = "FF",
        "LVX", "LVY",
        constr_dy_x_meas = "none",
        constr_dy_x_struct = "none",
        constr_dy_y_meas = "none",
        constr_dy_y_struct = "none",
        constr_dy_xy_struct = "none"
      ),
      "CFM" = scriptCFM(
        dvn,
        scaleset = "FF",
        "LVX", "LVY",
        constr_dy_x_meas = "none",
        constr_dy_x_struct = "none",
        constr_dy_y_meas = "none",
        constr_dy_y_struct = "none",
        constr_dy_xy_struct = "none"
      ),
      "MIM" = scriptMIM(
        dvn,
        scaleset = "FF",
        "LVX", "LVY",
        constr_dy_x_meas = "none",
        constr_dy_x_struct = "none",
        constr_dy_y_meas = "none",
        constr_dy_y_struct = "none",
        constr_dy_xy_struct = "none"
      )
    )

    script_bank[requested_models]
  }

  return(
    script_bank[requested_models]
  )
}


#' @rdname outputHelpers
#' @noRd
fitAll <- function(
    scripts,
    dat,
    ...) {
  models <- lapply(
    scripts, function(script) {
      lavaan::cfa(
        model = script,
        data = dat,
        ...
      )
    }
  )

  names(models) <- names(scripts)

  return(models)
}


#' @rdname outputHelpers
#' @noRd
indexExtractor <- function(
    models,
    dvn,
    dat) {
  # indexes by `lavaan`
  model_indexes <- lapply(
    models,
    function(model) {
      # Check if model converged before extracting fit measures
      if (!lavaan::lavInspect(model, "converged")) {
        stop("Cannot extract fit measures: model did not converge. Please check your model specification and data.")
      }
      tryCatch(
        {
          model %>%
            lavaan::fitMeasures(
              fit.measures = "all"
            ) %>%
            as.list() %>%
            tibble::as_tibble()
        },
        error = function(e) {
          stop("Failed to extract fit measures from a model. Error: ", e$message)
        }
      )
    }
  )

  model_indexes <- dplyr::bind_rows(
    model_indexes,
    .id = "Model"
  )

  # computing GenTEFI
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

  # packaging
  list(
    model_indexes = model_indexes,
    gen_tefi = gen_tefi
  )
}
