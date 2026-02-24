#' Write Population lavaan Syntax for Unidimensional Dyadic CFA
#'
#' Produces lavaan syntax with fixed numeric values for data generation in
#' Monte Carlo simulations. Used with \code{lavaan::simulateData()}.
#'
#' @param dvn Input dvn list from \code{build_dvn()} or \code{scrapeVarCross()}
#' @param lvname Character name for the latent variable
#' @param pop_values Named list of population parameter values:
#'   \itemize{
#'     \item \code{loadings}: numeric vector of loadings (length = n_items)
#'     \item \code{residuals}: numeric vector of residual variances (length = n_items)
#'     \item \code{coresids}: numeric vector of residual covariances (length = n_items)
#'     \item \code{intercepts}: optional; numeric vector (default 0 for all)
#'     \item \code{lv_var}: latent variance (default 1)
#'     \item \code{lv_mean}: latent mean (default 0)
#'   }
#' @param lvar Character \code{"X"} or \code{"Y"}. Default \code{"X"}.
#' @return Character string of lavaan syntax
#' @family population-scripting functions
#' @export
#' @examples
#' dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
#'   distinguish_1 = "A", distinguish_2 = "B")
#' scriptPopUni(dvn, "X", list(
#'   loadings = c(.75, .80, .55),
#'   residuals = c(.44, .36, .70),
#'   coresids = c(.10, .12, .08)
#' ))
scriptPopUni <- function(dvn, lvname = "X", pop_values, lvar = "X") {
  n_items <- if (lvar == "X") dvn$xindper else dvn$yindper

  loadings <- pop_values$loadings
  if (is.null(loadings) || length(loadings) != n_items) {
    stop("`pop_values$loadings` must be a vector of length ", n_items, ".")
  }

  residuals <- pop_values$residuals
  if (is.null(residuals) || length(residuals) != n_items) {
    stop("`pop_values$residuals` must be a vector of length ", n_items, ".")
  }

  coresids <- pop_values$coresids
  if (is.null(coresids) || length(coresids) != n_items) {
    stop("`pop_values$coresids` must be a vector of length ", n_items, ".")
  }

  intercepts <- pop_values$intercepts
  if (is.null(intercepts)) intercepts <- rep(0, n_items)
  if (length(intercepts) != n_items) {
    stop("`pop_values$intercepts` must be a vector of length ", n_items, ".")
  }

  lv_var <- pop_values$lv_var
  if (is.null(lv_var)) lv_var <- 1

  lv_mean <- pop_values$lv_mean
  if (is.null(lv_mean)) lv_mean <- 0

  loads_syntax <- popLoadings(dvn, lvname, "g", loadings, lvar = lvar)

  int1 <- popIntercepts(dvn, "1", intercepts, lvar = lvar)
  int2 <- popIntercepts(dvn, "2", intercepts, lvar = lvar)

  res1 <- popResiduals(dvn, "1", residuals, lvar = lvar)
  res2 <- popResiduals(dvn, "2", residuals, lvar = lvar)

  cores_syntax <- popCoresids(dvn, coresids, lvar = lvar)

  lv_label <- paste0(lvname, "Dy")
  lvar_syntax <- paste0(lv_label, " ~~ ", lv_var, "*", lv_label)
  lmean_syntax <- paste0(lv_label, " ~ ", lv_mean, "*1")

  sprintf(
    "# Unidimensional Dyadic CFA - Population Model\n\n# Loadings\n%s\n\n# Intercepts\n%s\n%s\n\n# Residual Variances\n%s\n%s\n\n# Residual Covariances\n%s\n\n# Latent Variance\n%s\n\n# Latent Mean\n%s",
    loads_syntax,
    int1, int2,
    res1, res2,
    cores_syntax,
    lvar_syntax,
    lmean_syntax
  )
}
