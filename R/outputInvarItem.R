#' Output Item-Level Invariance Summary Table
#'
#' `outputInvarItem()` generates a summary table showing which items exhibit
#' noninvariance in loadings and/or intercepts based on a partial invariance model script,
#' along with dMACS effect sizes for each item.
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param fit outputted dyadic cfa lavaan object from the partial invariance model
#' @param partialScript character string containing the lavaan syntax for the partial invariance model
#' @param dat data frame of indicators used to fit the model (needed for dMACS calculation)
#' @param nodewidth space between nodes during quadrature approximation for dMACS (default = .01)
#' @param lowerLV lowest latent variable value evaluated for dMACS (default = -5)
#' @param upperLV greatest latent variable value evaluated for dMACS (default = 5)
#' @param gtTab A logical input indicating whether to generate the output in `gt::gt()` table object format (`TRUE`).
#'  By default (`FALSE`), the output is generated in `tibble::tibble()` format.
#'  Users can also apply the `writeTo` argument if they wish to export the `gt:gt()` table object.
#' @param writeTo A character string specifying a directory path to where the `gt::gt()` table object should be saved.
#'  If set to ".", the file will be written to the current working directory.
#'  The default is `NULL`, and examples use a temporary directory created by `tempdir()`.
#'  `writeTo` is only relevant if `gtTab = TRUE`.
#' @param fileName A character string specifying a desired base name for the output `gt::gt()` file.
#'
#' @return A `tibble::tibble()` if `gtTab = FALSE` (default), or `gt::gt()` object if `gtTab = TRUE`, with
#' columns for Item, LoadingNoninvariance, InterceptNoninvariance, and dMACS. For multi-LV structures,
#' an additional LV column is included; in gt tables, this is used as the row group label with Item names
#' as row labels within each group.
#'
#' @details
#' * The function parses the lavaan syntax to determine which loadings and intercepts are constrained
#'   to equality across partners (invariant) versus freed (noninvariant).
#' * LoadingNoninvariance = "Yes" indicates the loading was freed in the partial invariance model.
#' * InterceptNoninvariance = "Yes" indicates the intercept was freed in the partial invariance model.
#' * dMACS values are calculated using the `getDydmacs()` function.
#' * If `gtTab = TRUE` and `writeTo` is specified, then output will simultaneously be saved as a `.rtf` file to the user's specified directory.
#' * If output file is successfully saved, a confirmation message will be printed to the console.
#' * If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#'
#' @family output functions
#' @export
#'
#' @examples
#' \dontrun{
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#'                       x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2")
#'
#' # Create and fit a partial invariance model
#' partial.script <- scriptCor(dvn, lvname = "Sat",
#'                             constr_dy_meas = c("loadings", "intercepts"),
#'                             constr_dy_struct = "none")
#'
#' partial.fit <- lavaan::cfa(partial.script, data = commitmentQ, std.lv = FALSE,
#'                            auto.fix.first = FALSE, meanstructure = TRUE)
#'
#' outputInvarItem(dvn, partial.fit, partial.script, dat = commitmentQ,
#'                 gtTab = TRUE, writeTo = tempdir(), fileName = "item_invar")
#' }
#'
outputInvarItem <- function(dvn, fit, partialScript, dat,
                            nodewidth = 0.01, lowerLV = -5, upperLV = 5,
                            gtTab = FALSE, writeTo = NULL, fileName = NULL) {

  # Input validation
  # Validate dvn argument

  if (missing(dvn) || is.null(dvn)) {
    stop("The `dvn` argument is required and cannot be NULL.")
  }
  if (!is.list(dvn)) {
    stop("The `dvn` argument must be a list from scrapeVarCross().")
  }

  # Validate fit argument
  if (missing(fit) || is.null(fit)) {
    stop("The `fit` argument is required and cannot be NULL.")
  }
  if (!inherits(fit, "lavaan")) {
    stop("The `fit` argument must be a fitted lavaan model object.")
  }

  # Validate partialScript argument
  if (missing(partialScript) || is.null(partialScript)) {
    stop("The `partialScript` argument is required and cannot be NULL.")
  }
  if (!is.character(partialScript)) {
    stop("The `partialScript` argument must be a character string containing lavaan syntax.")
  }

  # Validate dat argument
  if (missing(dat) || is.null(dat)) {
    stop("The `dat` argument is required and cannot be NULL.")
  }
  if (!is.data.frame(dat)) {
    stop("The `dat` argument must be a data.frame.")
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

  # Parse the lavaan syntax to get parameter table
  parsedSyntax <- lavaan::lavaanify(partialScript)

  # Check if dvn has multiple LVs (list structure) or single LV

  is_multi_lv <- is.list(dvn[["p1xvarnames"]]) && !is.data.frame(dvn[["p1xvarnames"]])

  # Get all p1 and p2 variable names, and track LV membership

  if (is_multi_lv) {
    p1_vars <- unlist(dvn[["p1xvarnames"]], use.names = FALSE)
    p2_vars <- unlist(dvn[["p2xvarnames"]], use.names = FALSE)
    lv_names <- names(dvn[["p1xvarnames"]])

    # Create a vector tracking which LV each item belongs to
    lv_membership <- rep(lv_names, times = lengths(dvn[["p1xvarnames"]]))
  } else {
    p1_vars <- dvn[["p1xvarnames"]]
    p2_vars <- dvn[["p2xvarnames"]]
    lv_membership <- NULL
  }

  # Create pairs of corresponding items
  if (length(p1_vars) != length(p2_vars)) {
    stop("Mismatch in number of partner 1 and partner 2 variables.")
  }

  n_items <- length(p1_vars)

  # Extract loadings from parsed syntax
  loadings_syntax <- parsedSyntax[parsedSyntax$op == "=~", ]

  # Extract intercepts from parsed syntax
  intercepts_syntax <- parsedSyntax[parsedSyntax$op == "~1", ]

  # Initialize results vectors
  item_names <- character(n_items)
  loading_noninvar <- character(n_items)
  intercept_noninvar <- character(n_items)

  # For each item pair, check if loadings and intercepts are constrained

  for (i in seq_len(n_items)) {
    p1_var <- p1_vars[i]
    p2_var <- p2_vars[i]

    # Create a "base" item name (removing partner distinguisher)
    # Try to find common part of the variable name
    item_names[i] <- paste0(p1_var, " / ", p2_var)

    # Check loading invariance
    # Get the label for p1 and p2 loadings
    p1_load <- loadings_syntax[loadings_syntax$rhs == p1_var, ]
    p2_load <- loadings_syntax[loadings_syntax$rhs == p2_var, ]

    if (nrow(p1_load) > 0 && nrow(p2_load) > 0) {
      p1_label <- p1_load$label[1]
      p2_label <- p2_load$label[1]

      # If both have the same non-empty label, they are constrained (invariant)
      # If labels differ or are empty, they are freed (noninvariant)
      if (!is.na(p1_label) && !is.na(p2_label) &&
          nchar(p1_label) > 0 && nchar(p2_label) > 0 &&
          p1_label == p2_label) {
        loading_noninvar[i] <- "No"
      } else {
        loading_noninvar[i] <- "Yes"
      }
    } else {
      # If loading not found, mark as NA
      loading_noninvar[i] <- NA_character_
    }

    # Check intercept invariance
    # Get the label for p1 and p2 intercepts
    p1_int <- intercepts_syntax[intercepts_syntax$lhs == p1_var, ]
    p2_int <- intercepts_syntax[intercepts_syntax$lhs == p2_var, ]

    if (nrow(p1_int) > 0 && nrow(p2_int) > 0) {
      p1_int_label <- p1_int$label[1]
      p2_int_label <- p2_int$label[1]

      # If both have the same non-empty label, they are constrained (invariant)
      if (!is.na(p1_int_label) && !is.na(p2_int_label) &&
          nchar(p1_int_label) > 0 && nchar(p2_int_label) > 0 &&
          p1_int_label == p2_int_label) {
        intercept_noninvar[i] <- "No"
      } else {
        intercept_noninvar[i] <- "Yes"
      }
    } else {
      # If intercept not found, mark as NA
      intercept_noninvar[i] <- NA_character_
    }
  }

  # Calculate dMACS values
  dmacs_result <- getDydmacs(dat, dvn, fit,
                             nodewidth = nodewidth,
                             lowerLV = lowerLV,
                             upperLV = upperLV)

  # Extract dMACS values based on structure

  if (is.list(dmacs_result) && !is.matrix(dmacs_result)) {
    # Multiple LVs case - flatten the list
    dmacs_values <- unlist(lapply(dmacs_result, function(x) as.numeric(x)))
  } else {
    # Single LV case
    dmacs_values <- as.numeric(dmacs_result)
  }

  # Create the output data frame
  if (is_multi_lv) {
    result_df <- tibble::tibble(
      LV = lv_membership,
      Item = item_names,
      LoadingNoninvariance = loading_noninvar,
      InterceptNoninvariance = intercept_noninvar,
      dMACS = dmacs_values
    )
  } else {
    result_df <- tibble::tibble(
      Item = item_names,
      LoadingNoninvariance = loading_noninvar,
      InterceptNoninvariance = intercept_noninvar,
      dMACS = dmacs_values
    )
  }

  # Return based on gtTab argument

  if (gtTab == FALSE) {
    return(result_df)
  } else if (gtTab == TRUE) {
    # Create gt table with row grouping for multi-LV structures
    if (is_multi_lv) {
      result_gt <- result_df |>
        gt::gt(groupname_col = "LV", rowname_col = "Item") |>
        gt::tab_header(
          title = "Item-Level Invariance Summary",
          subtitle = "Noninvariance status and dMACS effect sizes"
        ) |>
        gt::cols_label(
          LoadingNoninvariance = "Loading Noninvariance",
          InterceptNoninvariance = "Intercept Noninvariance",
          dMACS = "dMACS"
        ) |>
        gt::fmt_number(
          columns = "dMACS",
          decimals = 4
        ) |>
        gt::tab_stubhead(label = "Item")
    } else {
      result_gt <- result_df |>
        gt::gt(rowname_col = "Item") |>
        gt::tab_header(
          title = "Item-Level Invariance Summary",
          subtitle = "Noninvariance status and dMACS effect sizes"
        ) |>
        gt::cols_label(
          LoadingNoninvariance = "Loading Noninvariance",
          InterceptNoninvariance = "Intercept Noninvariance",
          dMACS = "dMACS"
        ) |>
        gt::fmt_number(
          columns = "dMACS",
          decimals = 4
        ) |>
        gt::tab_stubhead(label = "Item")
    }

    # user specifies writeTo
    if (!is.null(writeTo)) {
      if (is.null(fileName)) {
        gt::gtsave(result_gt,
                   filename = "dySEM_item_invar.rtf",
                   path = writeTo
        )
        message(
          sprintf(
            "Output stored in: %s/dySEM_item_invar.rtf",
            writeTo
          )
        )
      } else {
        gt::gtsave(result_gt,
                   filename = sprintf("%s.rtf", fileName),
                   path = writeTo
        )
        message(
          sprintf(
            "Output stored in: %s/%s.rtf",
            writeTo, fileName
          )
        )
      }
    }

    return(result_gt)
  }
}
