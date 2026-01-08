#' Evaluate Invariance Equality Constraints in a specified Dyadic Invariance Model
#'
#' `outputConstraintTab()` is used to perform a score test for relaxing each
#' invariance equality constraint between partners in a given dyadic SEM model.
#'
#' @param constrainFit A fitted `lavaan` model with dyadic invariance equality constraints
#' @param filterSig A logical indicating whether to filter for significant constraints (default is `FALSE`)
#' @template gtTab
#' @template writeTo
#' @template fileName
#'
#' @return A `tibble::tibble()` if `gtTab = FALSE` (default), or `gt::gt()` object if `gtTab = TRUE`, with
#' rows of equality constraints (now with readable `param` labels) and test statistic, *df*, and *p* for whether constraint worsens model fit.
#'
#' @details
#' * If `gtTab = TRUE` and `writeTo` is specified, then output will simultaneously be saved as a `.rtf` file to the user's specified directory.
#' * If output file is successfully saved, a confirmation message will be printed to the console.
#' * If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#'
#' @export
#'
#' @examples
#' dvn <- scrapeVarCross(
#'   dat = commitmentM, x_order = "sip", x_stem = "sat.g",
#'   x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m"
#' )
#'
#' sat.resids.script <- scriptCor(dvn,
#'   lvname = "Sat",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = "none"
#' )
#'
#' sat.resids.mod <- lavaan::cfa(sat.resids.script,
#'   data = commitmentM, std.lv = FALSE,
#'   auto.fix.first = FALSE, meanstructure = TRUE
#' )
#'
#' outputConstraintTab(sat.resids.mod,
#'   filterSig = FALSE,
#'   gtTab = TRUE, writeTo = tempdir(), fileName = "dCFA_Residual"
#' )
#'
outputConstraintTab <- function(constrainFit, filterSig = FALSE,
                                gtTab = FALSE, writeTo = NULL, fileName = NULL) {
  # Input validation
  # Validate constrainFit argument
  if (missing(constrainFit) || is.null(constrainFit)) {
    stop("The `constrainFit` argument is required and cannot be NULL.")
  }
  if (!inherits(constrainFit, "lavaan")) {
    stop("The `constrainFit` argument must be a fitted lavaan model object.")
  }

  # Validate filterSig argument
  if (!is.logical(filterSig)) {
    stop("The `filterSig` argument must be a logical value (TRUE or FALSE).")
  }

  # Validate gtTab argument
  if (!is.logical(gtTab)) {
    stop("The `gtTab` argument must be a logical value (TRUE or FALSE).")
  }

  # checking for valid directory path
  if (gtTab == TRUE && !is.null(writeTo)) {
    if (!is.character(writeTo)) {
      stop("The `writeTo` argument must be a character string. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
    }
    if (!dir.exists(writeTo)) {
      stop("The specified directory does not exist. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
    }
    if (!is.null(fileName) && !is.character(fileName)) {
      stop("The `fileName` argument must be a character string.")
    }
  }


  # Extract score test for relaxing each equality constraint
  constrainFit.mi <- lavaan::lavTestScore(constrainFit)
  constrainFit.mi.uni <- constrainFit.mi$uni
  constrainFit.mi.uni <- constrainFit.mi.uni |>
    dplyr::rename(constraint = .data$op)

  # Extract parameter table and create more informative parameter description
  constrainFit.PT <- lavaan::parTable(constrainFit)
  constrainFit.PT$param <- paste(constrainFit.PT$lhs, constrainFit.PT$op, constrainFit.PT$rhs, sep = " ")
  constrainFit.PT.reduce <- constrainFit.PT |>
    dplyr::select(.data$plabel, .data$param)

  # Create subset of lhs labeled parameters
  constrainFit.PT.plabel.lhs <- constrainFit.PT.reduce |>
    # use dplyr filter to select for cases where plabel matches the values of lhs or rhs in int.fit.mi.uni
    dplyr::filter(.data$plabel %in% constrainFit.mi.uni$lhs) |>
    dplyr::rename(
      plabel1 = .data$plabel,
      param1 = .data$param
    ) |>
    dplyr::mutate(constraint = "==")

  # Create subset of rhs labeled parameters
  constrainFit.PT.plabel.rhs <- constrainFit.PT.reduce |>
    # use dplyr filter to select for cases where plabel matches the values of lhs or rhs in int.fit.mi.uni
    dplyr::filter(.data$plabel %in% constrainFit.mi.uni$rhs) |>
    dplyr::rename(
      plabel2 = .data$plabel,
      param2 = .data$param
    )

  # cbind subsets together
  constrainFit.PT.merge <- cbind(constrainFit.PT.plabel.lhs, constrainFit.PT.plabel.rhs)

  # carry over chi2, df, and p, and round
  constrainFit.PT.merge <- constrainFit.PT.merge |>
    dplyr::mutate(
      chi2 = constrainFit.mi.uni$X2,
      df = constrainFit.mi.uni$df,
      pvalue = constrainFit.mi.uni$p.value,
      sig = dplyr::case_when(
        pvalue < .05 & pvalue > .01 ~ "*",
        pvalue < .01 & pvalue > .001 ~ "**",
        pvalue < .001 ~ "***"
      )
    ) |>
    dplyr::select(-.data$plabel1, -.data$plabel2) |>
    dplyr::mutate_if(is.numeric, round, 3)

  # filter for significance if filterSig is TRUE
  if (filterSig == TRUE) {
    constrainFit.PT.merge <- constrainFit.PT.merge |>
      dplyr::filter(.data$pvalue < .05)
  }

  if (gtTab == FALSE) {
    constrainFit.PT.merge <- constrainFit.PT.merge |>
      tibble::as_tibble()

    return(constrainFit.PT.merge)
  } else if (gtTab == TRUE) { # Create gt table


    # user specifies writeTo
    if (!is.null(writeTo)) {
      constrainFit.PT.merge <- constrainFit.PT.merge |>
        gt::gt()

      if (is.null(fileName)) {
        gt::gtsave(constrainFit.PT.merge,
          filename = "dySEM_table.rtf",
          path = writeTo
        )
        message( # confirmation message
          sprintf(
            "Output stored in: %s/dySEM_table.rtf",
            writeTo
          )
        )
      } else if (!is.null(fileName)) {
        gt::gtsave(constrainFit.PT.merge,
          filename = sprintf(
            "%s.rtf",
            fileName
          ),
          path = writeTo
        )
        message( # confirmation message
          sprintf(
            "Output stored in: %s/%s.rtf",
            writeTo, fileName
          )
        )
      }
    }

    # user does not specify writeTo
    else if (is.null(writeTo)) {
      constrainFit.PT.merge <- constrainFit.PT.merge |>
        gt::gt()
    }

    return(constrainFit.PT.merge)
  }
}
