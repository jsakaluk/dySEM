#' Write Population lavaan Syntax for Correlated Dyadic CFA
#'
#' Produces lavaan syntax with fixed numeric values for data generation in
#' Monte Carlo simulations. Used with \code{lavaan::simulateData()}.
#'
#' @param dvn Input dvn list from \code{build_dvn()} or \code{scrapeVarCross()}
#' @param lvname Character name for the latent variable
#' @param pop_values Named list of population parameter values:
#'   \itemize{
#'     \item \code{loadings_p1}: numeric vector for partner 1 (length = n_items)
#'     \item \code{loadings_p2}: numeric vector for partner 2 (length = n_items). If NULL, uses loadings_p1.
#'     \item \code{residuals_p1}: numeric vector (length = n_items)
#'     \item \code{residuals_p2}: numeric vector (length = n_items). If NULL, uses residuals_p1.
#'     \item \code{coresids}: numeric vector (length = n_items)
#'     \item \code{intercepts_p1}: optional (default 0)
#'     \item \code{intercepts_p2}: optional (default 0)
#'     \item \code{lv_var}: latent variances; scalar (both=1) or length-2 vector
#'     \item \code{lv_cov}: latent covariance
#'     \item \code{lv_means}: latent means; scalar (both=0) or length-2 vector
#'   }
#' @param lvar Character \code{"X"} or \code{"Y"}. Default \code{"X"}.
#' @return Character string of lavaan syntax
#' @family population-scripting functions
#' @export
#' @examples
#' dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
#'   distinguish_1 = "A", distinguish_2 = "B")
#' scriptPopCor(dvn, "X", list(
#'   loadings_p1 = c(.75, .80, .55),
#'   loadings_p2 = c(.70, .85, .50),
#'   residuals_p1 = c(.44, .36, .70),
#'   residuals_p2 = c(.51, .28, .75),
#'   coresids = c(.10, .12, .08),
#'   lv_cov = 0.3
#' ))
scriptPopCor <- function(dvn, lvname = "X", pop_values, lvar = "X") {
  n_items <- if (lvar == "X") dvn$xindper else dvn$yindper

  loadings_p1 <- pop_values$loadings_p1
  if (is.null(loadings_p1) || length(loadings_p1) != n_items) {
    stop("`pop_values$loadings_p1` must be a vector of length ", n_items, ".")
  }
  loadings_p2 <- pop_values$loadings_p2
  if (is.null(loadings_p2)) loadings_p2 <- loadings_p1
  if (length(loadings_p2) != n_items) {
    stop("`pop_values$loadings_p2` must be a vector of length ", n_items, ".")
  }

  residuals_p1 <- pop_values$residuals_p1
  if (is.null(residuals_p1) || length(residuals_p1) != n_items) {
    stop("`pop_values$residuals_p1` must be a vector of length ", n_items, ".")
  }
  residuals_p2 <- pop_values$residuals_p2
  if (is.null(residuals_p2)) residuals_p2 <- residuals_p1
  if (length(residuals_p2) != n_items) {
    stop("`pop_values$residuals_p2` must be a vector of length ", n_items, ".")
  }

  coresids <- pop_values$coresids
  if (is.null(coresids) || length(coresids) != n_items) {
    stop("`pop_values$coresids` must be a vector of length ", n_items, ".")
  }

  intercepts_p1 <- pop_values$intercepts_p1
  if (is.null(intercepts_p1)) intercepts_p1 <- rep(0, n_items)
  if (length(intercepts_p1) != n_items) {
    stop("`pop_values$intercepts_p1` must be a vector of length ", n_items, ".")
  }
  intercepts_p2 <- pop_values$intercepts_p2
  if (is.null(intercepts_p2)) intercepts_p2 <- rep(0, n_items)
  if (length(intercepts_p2) != n_items) {
    stop("`pop_values$intercepts_p2` must be a vector of length ", n_items, ".")
  }

  lv_var <- pop_values$lv_var
  if (is.null(lv_var)) lv_var <- c(1, 1)
  if (length(lv_var) == 1) lv_var <- rep(lv_var, 2)

  lv_cov <- pop_values$lv_cov
  if (is.null(lv_cov)) lv_cov <- 0

  lv_means <- pop_values$lv_means
  if (is.null(lv_means)) lv_means <- c(0, 0)
  if (length(lv_means) == 1) lv_means <- rep(lv_means, 2)

  loads1 <- popLoadings(dvn, lvname, "1", loadings_p1, lvar = lvar)
  loads2 <- popLoadings(dvn, lvname, "2", loadings_p2, lvar = lvar)

  int1 <- popIntercepts(dvn, "1", intercepts_p1, lvar = lvar)
  int2 <- popIntercepts(dvn, "2", intercepts_p2, lvar = lvar)

  res1 <- popResiduals(dvn, "1", residuals_p1, lvar = lvar)
  res2 <- popResiduals(dvn, "2", residuals_p2, lvar = lvar)

  cores_syntax <- popCoresids(dvn, coresids, lvar = lvar)

  lvar1 <- popLatentVars(dvn, lvname, "1", lv_var[1], lvar = lvar)
  lvar2 <- popLatentVars(dvn, lvname, "2", lv_var[2], lvar = lvar)
  lcov_syntax <- popLatentCovars(dvn, lvname, lv_cov, lvar = lvar)

  lmean1 <- popLatentMeans(dvn, lvname, "1", lv_means[1], lvar = lvar)
  lmean2 <- popLatentMeans(dvn, lvname, "2", lv_means[2], lvar = lvar)

  sprintf(
    "# Correlated Dyadic CFA - Population Model\n\n# Loadings\n%s\n%s\n\n# Intercepts\n%s\n%s\n\n# Residual Variances\n%s\n%s\n\n# Residual Covariances\n%s\n\n# Latent Variances\n%s\n%s\n\n# Latent Covariance\n%s\n\n# Latent Means\n%s\n%s",
    loads1, loads2,
    int1, int2,
    res1, res2,
    cores_syntax,
    lvar1, lvar2,
    lcov_syntax,
    lmean1, lmean2
  )
}
