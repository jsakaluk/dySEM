#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Boutique Two-Construct Dyadic SEM Models
#'
#' This function takes the outputted object from `scrapeVarCross()` with both
#' X and Y indicators and automatically writes, returns, and exports (`.txt`)
#' `lavaan` syntax for specifying a two-construct model where the measurement
#' structure of X and Y can differ (e.g., X as correlated factors, Y as
#' unidimensional).
#'
#' @param dvn Input dvn list from `scrapeVarCross()` containing information for
#'  both X and Y (i.e., must include both x_stem and y_stem arguments when
#'  calling scrapeVarCross).
#' @param x_scaleset Character string specifying how to set the scale of latent X.
#'  Options are `"FF"` (fixed-factor) or `"MV"` (marker variable). Default is `"FF"`.
#' @param y_scaleset Character string specifying how to set the scale of latent Y.
#'  Options are `"FF"` (fixed-factor) or `"MV"` (marker variable). Default is `"FF"`.
#' @param lvxname Input character to (arbitrarily) name latent variable X in
#'  `lavaan` syntax.
#' @param lvyname Input character to (arbitrarily) name latent variable Y in
#'  `lavaan` syntax.
#' @param x_model Character string specifying the measurement model for X.
#'  Options are `"uni"` (unidimensional), `"cor"` (correlated factors),
#'  `"hier"` (hierarchical), or `"bifactor"`.
#' @param y_model Character string specifying the measurement model for Y.
#'  Same options as `x_model`.
#' @param constr_dy_x_meas Input character vector detailing which measurement
#'  model parameters to constrain across dyad members for latent X.
#' @param constr_dy_x_struct Input character vector detailing which structural
#'  model parameters to constrain across dyad members for latent X.
#' @param constr_dy_y_meas Input character vector detailing which measurement
#'  model parameters to constrain across dyad members for latent Y.
#' @param constr_dy_y_struct Input character vector detailing which structural
#'  model parameters to constrain across dyad members for latent Y.
#' @param constr_dy_xy_struct Character string for the X-to-Y regression paths.
#'  Options are `"free"` (default; all paths freely estimated) or `"zero"`
#'  (all paths fixed to zero).
#' @param includeMeanStruct Logical for whether to include the mean structure
#'  (intercepts and latent means) in the model. Defaults to `FALSE`.
#' @template writeTo
#' @template fileName
#' @return Character object of `lavaan` script that can be passed immediately to
#'  `lavaan` functions.
#' @seealso \code{\link{scrapeVarCross}} which this function relies on;
#'  \code{\link{scriptUni}}, \code{\link{scriptCor}}, \code{\link{scriptHier}},
#'  \code{\link{scriptBifac}} for the underlying scripters;
#'  \code{\link{scriptAPIM}} for a related two-construct function.
#' @family bi-construct script-writing functions
#' @export
#'
#' @examples
#' dvn <- scrapeVarCross(
#'   dat = commitmentQ,
#'   x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
#'   distinguish_1 = "1", distinguish_2 = "2",
#'   y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
#' )
#'
#' # Correlated X, unidimensional Y
#' scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
#'   x_model = "cor", y_model = "uni")
#'
#' # Both correlated
#' scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
#'   x_model = "cor", y_model = "cor",
#'   writeTo = tempdir(), fileName = "twoCross_cor_cor")
scriptTwoCross <- function(
    dvn,
    x_scaleset = "FF",
    y_scaleset = "FF",
    lvxname = "X",
    lvyname = "Y",
    x_model = "cor",
    y_model = "cor",
    constr_dy_x_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_x_struct = c("variances", "means"),
    constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_y_struct = c("variances", "means"),
    constr_dy_xy_struct = "free",
    includeMeanStruct = FALSE,
    writeTo = NULL,
    fileName = NULL
) {
  # Input validation
  if (missing(dvn) || is.null(dvn)) {
    stop("The `dvn` argument is required and cannot be NULL.")
  }
  if (!is.list(dvn)) {
    stop("The `dvn` argument must be a list object.")
  }
  required_dvn <- c("p1xvarnames", "p2xvarnames", "xindper", "dist1", "dist2",
                   "p1yvarnames", "p2yvarnames", "yindper")
  if (!all(required_dvn %in% names(dvn))) {
    stop("You must supply a dvn object containing information for both X and Y.")
  }
  if (!x_model %in% c("uni", "cor", "hier", "bifactor")) {
    stop("x_model must be one of: 'uni', 'cor', 'hier', 'bifactor'")
  }
  if (!y_model %in% c("uni", "cor", "hier", "bifactor")) {
    stop("y_model must be one of: 'uni', 'cor', 'hier', 'bifactor'")
  }
  if (!constr_dy_xy_struct %in% c("free", "zero")) {
    stop("constr_dy_xy_struct must be either 'free' or 'zero'")
  }
  if (!x_scaleset %in% c("FF", "MV")) {
    stop("x_scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
  }
  if (!y_scaleset %in% c("FF", "MV")) {
    stop("y_scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
  }

  # Build dvn subsets for single-construct scripters
  dvn_x <- list(
    p1xvarnames = dvn$p1xvarnames,
    p2xvarnames = dvn$p2xvarnames,
    xindper = dvn$xindper,
    dist1 = dvn$dist1,
    dist2 = dvn$dist2,
    indnum = 2L * as.integer(dvn$xindper)
  )
  dvn_y <- list(
    p1yvarnames = dvn$p1yvarnames,
    p2yvarnames = dvn$p2yvarnames,
    yindper = dvn$yindper,
    dist1 = dvn$dist1,
    dist2 = dvn$dist2,
    indnum = 2L * as.integer(dvn$yindper)
  )

  # Map model to scripter
  scripter_x <- switch(x_model,
    uni = scriptUni,
    cor = scriptCor,
    hier = scriptHier,
    bifactor = scriptBifac,
    stop("Unknown x_model: ", x_model)
  )
  scripter_y <- switch(y_model,
    uni = scriptUni,
    cor = scriptCor,
    hier = scriptHier,
    bifactor = scriptBifac,
    stop("Unknown y_model: ", y_model)
  )

  # Call X scripter (scriptUni requires constr_dy_struct = "none")
  comp_x <- scripter_x(
    dvn = dvn_x,
    scaleset = x_scaleset,
    lvname = lvxname,
    lvar = "X",
    constr_dy_meas = constr_dy_x_meas,
    constr_dy_struct = if (x_model == "uni") "none" else constr_dy_x_struct,
    outputType = "syntax components"
  )

  # Call Y scripter (scriptUni requires constr_dy_struct = "none")
  comp_y <- scripter_y(
    dvn = dvn_y,
    scaleset = y_scaleset,
    lvname = lvyname,
    lvar = "Y",
    constr_dy_meas = constr_dy_y_meas,
    constr_dy_struct = if (y_model == "uni") "none" else constr_dy_y_struct,
    outputType = "syntax components"
  )

  # Extract factor names and generate regressions
  x_factors <- getTwoCrossFactors(comp_x, dvn)
  y_factors <- getTwoCrossFactors(comp_y, dvn)
  regs <- twoCross_regs(x_factors, y_factors, type = constr_dy_xy_struct)

  # Helper to collapse component elements
  paste_meas <- function(m) {
    if (is.null(m)) return("")
    if (is.character(m) && length(m) == 1L) return(m)
    paste(m, collapse = "\n")
  }
  paste_struct <- function(s, exclude_means = FALSE) {
    if (is.null(s)) return("")
    if (exclude_means && "means" %in% names(s)) {
      s <- s[names(s) != "means"]
    }
    parts <- lapply(s, function(x) {
      if (is.character(x) && length(x) == 1L) x else paste(x, collapse = "\n")
    })
    paste(unlist(parts), collapse = "\n")
  }

  # Assemble script sections
  x_loads <- paste_meas(comp_x$measurement$loadings)
  y_loads <- paste_meas(comp_y$measurement$loadings)
  x_res <- paste_meas(comp_x$measurement$residuals)
  y_res <- paste_meas(comp_y$measurement$residuals)
  x_cores <- paste_meas(comp_x$measurement$coresids)
  y_cores <- paste_meas(comp_y$measurement$coresids)
  x_struct <- paste_struct(comp_x$structural, exclude_means = !includeMeanStruct)
  y_struct <- paste_struct(comp_y$structural, exclude_means = !includeMeanStruct)

  if (includeMeanStruct) {
    x_ints <- paste_meas(comp_x$measurement$intercepts)
    y_ints <- paste_meas(comp_y$measurement$intercepts)
    script <- sprintf(
      "#Measurement Model\n\n#Loadings\n%s\n\n%s\n\n#Intercepts\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n%s\n\n#Structural Model\n\n#Latent (Co)Variances\n%s\n\n%s\n\n#Latent Regressions (X -> Y)\n%s",
      x_loads, y_loads,
      x_ints, y_ints,
      x_res, y_res,
      x_cores, y_cores,
      x_struct, y_struct,
      regs
    )
  } else {
    script <- sprintf(
      "#Measurement Model\n\n#Loadings\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n%s\n\n#Structural Model\n\n#Latent (Co)Variances\n%s\n\n%s\n\n#Latent Regressions (X -> Y)\n%s",
      x_loads, y_loads,
      x_res, y_res,
      x_cores, y_cores,
      x_struct, y_struct,
      regs
    )
  }

  # Write script to file if requested
  if (!is.null(writeTo) || !is.null(fileName)) {
    if (!is.character(writeTo)) {
      stop("The `writeTo` argument must be a character string. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!dir.exists(writeTo)) {
      stop("The specified directory does not exist. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!is.character(fileName)) {
      stop("The `fileName` argument must be a character string.")
    }
    cat(script, "\n", file = sprintf("%s/%s.txt", writeTo, fileName))
    return(script)
  }
  return(script)
}
