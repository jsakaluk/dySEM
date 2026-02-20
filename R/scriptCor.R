#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Correlated Dyadic Factor Models
#'
#' This function takes the outputted object from `scrapeVarCross()`
#'  and automatically writes, returns, and exports (`.txt`) `lavaan` syntax
#'  for specifying dyadic configural, loading, intercept, and residual invariant
#'  two-factor models.
#'
#' @param dvn Input dvn list from `scrapeVarCross()`
#' @template scaleset
#' @param lvname Input character to (arbitrarily) name the latent variable in `lavaan` syntax
#' @param lvar Input character to specify whether the latent variable represents
#'  "X" or "Y" in the model. Default is `"X"`. This argument controls parameter
#'  labeling (e.g., `lx` vs `ly` for loadings, `tx` vs `ty` for intercepts).
#' @param constr_dy_meas Input character vector detailing which measurement model parameters to constrain across dyad members.
#' @param constr_dy_struct Input character vector detailing which structural model parameters to constrain across dyad members.
#' Default is `c("variances", "means")`(in combination with defaults for `constr_dy_meas`, an indistinguishable correlated dyadic factors model),
#' but user can specify any combination of `"variances"` and `"means"`, or `"none"`.
#' @param constr_group_meas Optional character vector detailing which measurement model parameters to constrain across groups.
#' Valid values: `"loadings"`, `"intercepts"`, `"residuals"`, `"residual.covariances"`, or `"none"`. Default `NULL` = single-group. Requires dvn from `scrapeVarCross(..., group = "varname")`.
#' @param constr_group_struct Optional character vector detailing which structural model parameters to constrain across groups.
#' Valid values: `"variances"`, `"means"`, or `"none"`. Default `NULL` = single-group. Requires dvn from `scrapeVarCross(..., group = "varname")`.
#' @template writeTo
#' @template fileName
#' @param outputType Character string specifying the type of output to return.
#'  Options are `"lavaan script"` (default) to return a character object of
#'  `lavaan` syntax that can be passed immediately to `lavaan` functions, or
#'  `"syntax components"` to return a structured list of model components.
#' @return Character object of `lavaan` script that can be passed immediately to
#'  `lavaan` functions (when `outputType = "lavaan script"`), or a structured list
#'  of model components (when `outputType = "syntax components"`).
#'
#' @details
#' * By default, many `dySEM::` functions (including `scriptCor()`) default to
#'    a fixed-factor method of scale-setting, whereby the latent variance of
#'    a given factor is constrained to 1 for both partners in the configurally invariant
#'    model, and then one of these variances is freely estimated in subsequent
#'    models of the invariance testing sequence.
#'    We have selected this default for two reasons:
#'    (1) the selection of a marker-variable is usually arbitrary,
#'    yet can have a large influence on the estimation and testing of of structural parameters
#'    (see https://stats.stackexchange.com/questions/402133/in-cfa-does-it-matter-which-factor-loading-is-set-to-1/402732#402732);
#'    and (2) the selection of a non-invariant marker-variable
#'    can have disastrous down-stream consequences for the identification of
#'    non-invariant measurement parameters, following a the rejection of an omnibus
#'    invariance constraint set (see Lee, Preacher, & Little, 2011).
#'
#' @seealso \code{\link{scrapeVarCross}} which this function relies on.
#' @family uni-construct script-writing functions
#' @export
#'
#' @examples
#' dvn <- scrapeVarCross(
#'   dat = commitmentQ,
#'   x_order = "spi",
#'   x_stem = "sat.g",
#'   x_delim1 = ".",
#'   x_delim2 = "_",
#'   distinguish_1 = "1",
#'   distinguish_2 = "2"
#' )
#'
#' sat.indist.script <- scriptCor(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat"
#' )
#'
#' sat.lvars.script <- scriptCor(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = "loadings",
#'   constr_dy_struct = "variances"
#' )
#'
#' sat.resids.script <- scriptCor(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dCor_residual"
#' )
#'
#' sat.ints.script <- scriptCor(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = c("loadings", "intercepts"),
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dCor_intercept"
#' )
#'
#' sat.loads.script <- scriptCor(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = c("loadings"),
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dCor_loading"
#' )
#'
#' sat.config.script <- scriptCor(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = "none",
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dCor_configural"
#' )
scriptCor <- function(
  dvn,
  scaleset = "FF",
  lvname = "X",
  lvar = "X",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"),
  constr_group_meas = NULL,
  constr_group_struct = NULL,
  writeTo = NULL,
  fileName = NULL,
  outputType = "lavaan script"
) {
  # Input validation
  # Validate dvn argument
  if (missing(dvn) || is.null(dvn)) {
    stop("The `dvn` argument is required and cannot be NULL.")
  }
  if (!is.list(dvn)) {
    stop("The `dvn` argument must be a list object.")
  }

  # Validate lvname argument
  if (!is.character(lvname)) {
    stop("The `lvname` argument must be a character string.")
  }

  # Validate lvar argument
  if (!lvar %in% c("X", "Y")) {
    stop("lvar must be either 'X' or 'Y'")
  }

  # Validate dvn has required elements and only contains information for the
  # requested latent variable family (X or Y). For historical reasons,
  # scriptCor() expects a dvn with *only* X (or only Y) information, in a
  # 6-element list: p1[var]varnames, p2[var]varnames, [var]indper, dist1,
  # dist2, indnum.
  if (lvar == "X") {
    required_elements <- c("p1xvarnames", "p2xvarnames", "xindper", "dist1", "dist2", "indnum")
  } else { # lvar == "Y"
    required_elements <- c("p1yvarnames", "p2yvarnames", "yindper", "dist1", "dist2", "indnum")
  }
  if (!all(required_elements %in% names(dvn))) {
    stop("You must supply a dvn object containing information for only X")
  }
  if (lvar == "X" && "p1yvarnames" %in% names(dvn)) {
    stop("You must supply a dvn object containing information for only X")
  }
  if (lvar == "Y" && "p1xvarnames" %in% names(dvn)) {
    stop("You must supply a dvn object containing information for only Y")
  }

  # check for valid inputs
  valid_dy_meas <- c("loadings", "intercepts", "residuals", "none")
  invalid_dy_meas <- setdiff(constr_dy_meas, valid_dy_meas)
  if (length(invalid_dy_meas) > 0) {
    stop("constr_dy_meas contains invalid value(s): ", paste(sQuote(invalid_dy_meas), collapse = ", "),
         ". Valid options: ", paste(sQuote(valid_dy_meas), collapse = ", "))
  }

  valid_dy_struct <- c("variances", "means", "none")
  invalid_dy_struct <- setdiff(constr_dy_struct, valid_dy_struct)
  if (length(invalid_dy_struct) > 0) {
    stop("constr_dy_struct contains invalid value(s): ", paste(sQuote(invalid_dy_struct), collapse = ", "),
         ". Valid options: ", paste(sQuote(valid_dy_struct), collapse = ", "))
  }

  if (!scaleset %in% c("FF", "MV")) {
    stop("scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
  }

  # Validate outputType argument
  if (!outputType %in% c("lavaan script", "syntax components")) {
    stop("outputType must be either 'lavaan script' or 'syntax components'")
  }

  # Multi-group: validate constr_group_* and derive group_n / constr flags
  group_n <- NULL
  constr_group_loadings <- FALSE
  constr_group_intercepts <- FALSE
  constr_group_residuals <- FALSE
  constr_group_residual_covariances <- FALSE
  constr_group_variances <- FALSE
  constr_group_means <- FALSE
  if (!is.null(constr_group_meas) || !is.null(constr_group_struct)) {
    valid_group_meas <- c("loadings", "intercepts", "residuals", "residual.covariances", "none")
    if (!is.null(constr_group_meas)) {
      invalid_group_meas <- setdiff(constr_group_meas, valid_group_meas)
      if (length(invalid_group_meas) > 0) {
        stop("constr_group_meas contains invalid value(s): ", paste(sQuote(invalid_group_meas), collapse = ", "),
             ". Valid options: ", paste(sQuote(valid_group_meas), collapse = ", "))
      }
    }
    valid_group_struct <- c("variances", "means", "none")
    if (!is.null(constr_group_struct)) {
      invalid_group_struct <- setdiff(constr_group_struct, valid_group_struct)
      if (length(invalid_group_struct) > 0) {
        stop("constr_group_struct contains invalid value(s): ", paste(sQuote(invalid_group_struct), collapse = ", "),
             ". Valid options: ", paste(sQuote(valid_group_struct), collapse = ", "))
      }
    }
    if (!"group_n" %in% names(dvn)) {
      stop("Multi-group analysis requires dvn from scrapeVarCross(..., group = \"varname\"). Run scrapeVarCross with a group argument.")
    }
    group_n <- dvn$group_n
    if (!is.null(constr_group_meas) && !identical(constr_group_meas, "none")) {
      constr_group_loadings <- "loadings" %in% constr_group_meas
      constr_group_intercepts <- "intercepts" %in% constr_group_meas
      constr_group_residuals <- "residuals" %in% constr_group_meas
      constr_group_residual_covariances <- "residual.covariances" %in% constr_group_meas
    }
    if (!is.null(constr_group_struct) && !identical(constr_group_struct, "none")) {
      constr_group_variances <- "variances" %in% constr_group_struct
      constr_group_means <- "means" %in% constr_group_struct
    }
  }

  # loadings
  if (any(constr_dy_meas == "loadings")) {
    if (scaleset == "FF") {
      xloads1 <- loads(dvn, lvar = lvar, lvname, partner = "1", type = "equated", group_n = group_n, constr_group_loadings = constr_group_loadings)
      xloads2 <- loads(dvn, lvar = lvar, lvname, partner = "2", type = "equated", group_n = group_n, constr_group_loadings = constr_group_loadings)
    } else if (scaleset == "MV") {
      xloads1 <- loads(dvn, lvar = lvar, lvname, partner = "1", type = "equated_mv", group_n = group_n, constr_group_loadings = constr_group_loadings)
      xloads2 <- loads(dvn, lvar = lvar, lvname, partner = "2", type = "equated", group_n = group_n, constr_group_loadings = constr_group_loadings)
    }
  } else {
    if (scaleset == "FF") {
      xloads1 <- loads(dvn, lvar = lvar, lvname, partner = "1", type = "free", group_n = group_n, constr_group_loadings = constr_group_loadings)
      xloads2 <- loads(dvn, lvar = lvar, lvname, partner = "2", type = "free", group_n = group_n, constr_group_loadings = constr_group_loadings)
    } else if (scaleset == "MV") {
      xloads1 <- loads(dvn, lvar = lvar, lvname, partner = "1", type = "fixed", group_n = group_n, constr_group_loadings = constr_group_loadings)
      xloads2 <- loads(dvn, lvar = lvar, lvname, partner = "2", type = "fixed", group_n = group_n, constr_group_loadings = constr_group_loadings)
    }
  }

  # intercepts
  if (any(constr_dy_meas == "intercepts")) {
    if (scaleset == "FF") {
      xints1 <- intercepts(dvn, lvar = lvar, partner = "1", type = "equated", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
      xints2 <- intercepts(dvn, lvar = lvar, partner = "2", type = "equated", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
    } else if (scaleset == "MV") {
      xints1 <- intercepts(dvn, lvar = lvar, partner = "1", type = "equated_mv", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
      xints2 <- intercepts(dvn, lvar = lvar, partner = "2", type = "equated", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
    }
  } else {
    if (scaleset == "FF") {
      xints1 <- intercepts(dvn, lvar = lvar, partner = "1", type = "free", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
      xints2 <- intercepts(dvn, lvar = lvar, partner = "2", type = "free", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
    } else if (scaleset == "MV") {
      xints1 <- intercepts(dvn, lvar = lvar, partner = "1", type = "fixed", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
      xints2 <- intercepts(dvn, lvar = lvar, partner = "2", type = "fixed", group_n = group_n, constr_group_intercepts = constr_group_intercepts)
    }
  }

  # residual variances
  if (any(constr_dy_meas == "residuals")) {
    xres1 <- resids(dvn, lvar = lvar, partner = "1", type = "equated", group_n = group_n, constr_group_residuals = constr_group_residuals)
    xres2 <- resids(dvn, lvar = lvar, partner = "2", type = "equated", group_n = group_n, constr_group_residuals = constr_group_residuals)
  } else {
    xres1 <- resids(dvn, lvar = lvar, partner = "1", type = "free", group_n = group_n, constr_group_residuals = constr_group_residuals)
    xres2 <- resids(dvn, lvar = lvar, partner = "2", type = "free", group_n = group_n, constr_group_residuals = constr_group_residuals)
  }

  # Correlated residuals
  xcoresids <- coresids(dvn, lvar = lvar, type = "free", group_n = group_n, constr_group_residual_covariances = constr_group_residual_covariances)

  # latent variances
  if (any(constr_dy_struct == "variances")) {
    if (scaleset == "FF") {
      xvar1 <- lvars(dvn, lvar = lvar, lvname, partner = "1", type = "equated_ff", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
      xvar2 <- lvars(dvn, lvar = lvar, lvname, partner = "2", type = "equated", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
    } else if (scaleset == "MV") {
      xvar1 <- lvars(dvn, lvar = lvar, lvname, partner = "1", type = "equated", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
      xvar2 <- lvars(dvn, lvar = lvar, lvname, partner = "2", type = "equated", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
    }
  } else if (!any(constr_dy_struct == "variances") & any(constr_dy_meas == "loadings") & scaleset == "FF") {
    xvar1 <- lvars(dvn, lvar = lvar, lvname, partner = "1", type = "fixed", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
    xvar2 <- lvars(dvn, lvar = lvar, lvname, partner = "2", type = "free", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
  } else {
    if (scaleset == "FF") {
      xvar1 <- lvars(dvn, lvar = lvar, lvname, partner = "1", type = "fixed", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
      xvar2 <- lvars(dvn, lvar = lvar, lvname, partner = "2", type = "fixed", group_n = group_n, constr_group_variances = constr_group_variances, constr_group_loadings = constr_group_loadings)
    } else if (scaleset == "MV") {
      xvar1 <- lvars(dvn, lvar = lvar, lvname, partner = "1", type = "free", group_n = group_n, constr_group_variances = constr_group_variances)
      xvar2 <- lvars(dvn, lvar = lvar, lvname, partner = "2", type = "free", group_n = group_n, constr_group_variances = constr_group_variances)
    }
  }

  # latent covariance
  if (any(constr_dy_struct == "orthogonal")) {
    xcovar <- lcovars(dvn, lvname, type = "zero", group_n = group_n, constr_group_variances = constr_group_variances)
  } else {
    xcovar <- lcovars(dvn, lvname, type = "free", group_n = group_n, constr_group_variances = constr_group_variances)
  }

  # latent means
  if (any(constr_dy_struct == "means")) {
    if (scaleset == "FF") {
      xmean1 <- lmeans(dvn, lvar = lvar, lvname, partner = "1", type = "equated_ff", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
      xmean2 <- lmeans(dvn, lvar = lvar, lvname, partner = "2", type = "equated", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
    } else if (scaleset == "MV") {
      xmean1 <- lmeans(dvn, lvar = lvar, lvname, partner = "1", type = "equated", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
      xmean2 <- lmeans(dvn, lvar = lvar, lvname, partner = "2", type = "equated", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
    }
  } else if (!any(constr_dy_struct == "means") & any(constr_dy_meas == "intercepts") & scaleset == "FF") {
    xmean1 <- lmeans(dvn, lvar = lvar, lvname, partner = "1", type = "fixed", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
    xmean2 <- lmeans(dvn, lvar = lvar, lvname, partner = "2", type = "free", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
  } else {
    if (scaleset == "FF") {
      xmean1 <- lmeans(dvn, lvar = lvar, lvname, partner = "1", type = "fixed", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
      xmean2 <- lmeans(dvn, lvar = lvar, lvname, partner = "2", type = "fixed", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
    } else if (scaleset == "MV") {
      xmean1 <- lmeans(dvn, lvar = lvar, lvname, partner = "1", type = "free", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
      xmean2 <- lmeans(dvn, lvar = lvar, lvname, partner = "2", type = "free", group_n = group_n, constr_group_means = constr_group_means, constr_group_intercepts = constr_group_intercepts)
    }
  }

  # Return syntax components if requested
  if (outputType == "syntax components") {
    return(list(
      measurement = list(
        loadings = c(xloads1, xloads2),
        intercepts = c(xints1, xints2),
        residuals = c(xres1, xres2),
        coresids = xcoresids
      ),
      structural = list(
        variances = c(xvar1, xvar2),
        covariances = xcovar,
        means = c(xmean1, xmean2)
      ),
      form = "Cor",
      lvname = lvname,
      partner_types = c("1", "2")
    ))
  }

  # Script Creation Syntax
  script <- sprintf(
    "#Measurement Model\n\n#Loadings\n%s\n%s\n\n#Intercepts\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n#Structural Model\n\n#Latent (Co)Variances\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s",
    xloads1, xloads2,
    xints1, xints2,
    xres1, xres2, xcoresids,
    xvar1, xvar2, xcovar,
    xmean1, xmean2
  )

  # Write script to file if requested

  if (!is.null(writeTo) | !is.null(fileName)) {
    # if there is a path or file name,
    # check for valid input,
    # and if valid, write script


    # checking for valid directory path and fileName
    if (!is.character(writeTo)) {
      stop("The `writeout` argument must be a character string. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!dir.exists(writeTo)) {
      stop("The specified directory does not exist. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!is.character(fileName)) {
      stop("The `fileName` argument must be a character string.")
    }

    # write file
    cat(script, "\n",
      file = sprintf(
        "%s/%s.txt",
        writeTo,
        fileName
      )
    )

    return(script)
  } else if (is.null(writeTo) & is.null(fileName)) {
    # otherwise just return script
    return(script)
  }
}
