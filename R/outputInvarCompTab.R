#' Compare Model Fit of Nested Dyadic Invariance Models
#'
#' `outputInvarCompTab()` is used to compare the model fit of nested dyadic invariance models
#'  in order from most parsimonious (residual) to least parsimonious (configural)
#'
#' @param mods A list of nested `lavaan` dyadic invariance models, in the order of residual, intercept, loading, configural
#' @param gtTab A logical input indicating whether to generate the output in `gt::gt()` table object format (`TRUE`).
#'  By default (`FALSE`), the output is generated in `tibble::tibble()` format.
#'  Users can also apply the `writeTo` argument if they wish to export the `gt:gt()` table object.
#' @param writeTo A character string specifying a directory path to where the `gt::gt()` table object should be saved.
#'  If set to ".", the file will be written to the current working directory.
#'  The default is `NULL`, and examples use a temporary directory created by `tempdir()`.
#'  `writeTo` is only relevant if `gtTab = TRUE`.
#' @param fileName A character string specifying a desired base name for the output `gt::gt()` file.
#'  If a `fileName` is not provided (i.e., `fileName = NULL`), then a default will be used (i.e., "dySEM_table").
#'  The resulting base name will automatically be appended with a `.rtf` file extension.
#'  `fileName` is only relevant if `gtTab = TRUE` and `writeTo` is specified.
#'
#' @return A `tibble::tibble()` if `gtTab = FALSE` (default), or `gt::gt()` object if `gtTab = TRUE`, of model fit statistics for each model, as well as the difference in fit statistics between each model and the previous model
#'
#' @details
#' * If `gtTab = TRUE` and `writeTo` is specified, then output will simultaneously be saved as a `.rtf` file to the user's specified directory.
#' * If output file is successfully saved, a confirmation message will be printed to the console.
#' * If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#'
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
#' outputInvarCompTab(mods,
#' gtTab = TRUE, writeTo = tempdir(), fileName = "dCFA_Invar")
#'
outputInvarCompTab <- function(mods,
                               gtTab = FALSE,
                               writeTo = NULL,
                               fileName = NULL){

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

  if (gtTab == FALSE){

    modcomp <- modcomp |>
      tibble::as_tibble()

    return(modcomp)
  }

  else if (gtTab == TRUE){ #Create gt table

    #user specifies writeTo
    if (!is.null(writeTo)){

      modcomp <- modcomp |>
        gt::gt()

      if (is.null(fileName)){
        gt::gtsave(modcomp,
                   filename = "dySEM_table.rtf",
                   path = writeTo)
        message( #confirmation message
          sprintf(
            "Output stored in: %s/dySEM_table.rtf",
            writeTo)
        )
      }

      else if (!is.null(fileName)){
        gt::gtsave(modcomp,
                   filename = sprintf("%s.rtf",
                                      fileName),
                   path = writeTo)
        message( #confirmation message
          sprintf(
            "Output stored in: %s/%s.rtf",
            writeTo, fileName)
        )
      }
    }

    #user does not specify writeTo
    else if (is.null(writeTo)){

      modcomp <- modcomp |>
        gt::gt()

    }

    return(modcomp)

  }

}
