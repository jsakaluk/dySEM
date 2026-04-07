#' Population lavaan syntax for a latent indistinguishable APIM
#'
#' Writes a fully specified population model with fixed numeric parameters for use
#' with [lavaan::simulateData()]. Measurement blocks follow the same conventions
#' as [scriptPopCor()] for each construct; structural paths use a single actor
#' and partner coefficient on both partners' equations.
#'
#' @param dvn List from [build_dvn()] or [scrapeVarCross()] with X and Y
#'   indicators (`p1yvarnames`, `p2yvarnames`, `yindper` required).
#' @param lvxname,lvyname Base names for latent X and Y (partner suffixes from
#'   `dvn$dist1` / `dvn$dist2` are appended, as in [scriptAPIM()]).
#' @param pop_values Named list; see **Population values** below.
#'
#' @section Population values:
#' **X measurement** (lengths `nx = dvn$xindper`): `x_loadings_p1`, `x_loadings_p2`
#' (optional recycle), `x_residuals_p1`, `x_residuals_p2`, `x_coresids`,
#' optional `x_intercepts_p1` / `x_intercepts_p2`.
#'
#' **Y measurement** (`ny = dvn$yindper`): `y_loadings_p1`, `y_loadings_p2`,
#' `y_residuals_p1`, `y_residuals_p2`, `y_coresids`, optional `y_intercepts_*`.
#'
#' **Latent X:** `x_lv_var` (scalar or length 2, default `c(1,1)`), `x_lv_cov`
#' (default 0), `x_lv_means` (optional, default `c(0,0)`).
#'
#' **Structural:** `actor`, `partner` (numeric), `y_lv_var` (scalar or length 2,
#' default `c(0.5, 0.5)`), `y_lv_cov` (default 0; covariance between latent Y
#' residuals).
#'
#' Vectors of length 1 are recycled to `nx` or `ny` for measurement slots.
#'
#' @return Single string of lavaan model syntax.
#' @family population-scripting functions
#' @export
scriptPopAPIM <- function(dvn, lvxname, lvyname, pop_values) {
  if (missing(dvn) || !is.list(dvn)) {
    stop("`dvn` must be a list from build_dvn() or scrapeVarCross().")
  }
  req <- c("p1xvarnames", "p2xvarnames", "xindper", "dist1", "dist2",
           "p1yvarnames", "p2yvarnames", "yindper")
  miss <- req[!req %in% names(dvn)]
  if (length(miss)) {
    stop("`dvn` must include: ", paste(miss, collapse = ", "))
  }
  if (!is.character(lvxname) || length(lvxname) != 1L) {
    stop("`lvxname` must be a single character string.")
  }
  if (!is.character(lvyname) || length(lvyname) != 1L) {
    stop("`lvyname` must be a single character string.")
  }
  if (!is.list(pop_values)) {
    stop("`pop_values` must be a list.")
  }

  nx <- dvn$xindper
  ny <- dvn$yindper

  recycle_vec <- function(v, n, nm) {
    if (is.null(v)) {
      stop("`", nm, "` is required.")
    }
    v <- unlist(v, use.names = FALSE)
    if (length(v) == 1L) {
      rep(v, n)
    } else if (length(v) == n) {
      v
    } else {
      stop("`", nm, "` must have length 1 or ", n, ".")
    }
  }

  xl1 <- recycle_vec(pop_values$x_loadings_p1, nx, "pop_values$x_loadings_p1")
  xl2 <- pop_values$x_loadings_p2
  if (is.null(xl2)) xl2 <- xl1
  xl2 <- recycle_vec(xl2, nx, "pop_values$x_loadings_p2")
  xr1 <- recycle_vec(pop_values$x_residuals_p1, nx, "pop_values$x_residuals_p1")
  xr2 <- pop_values$x_residuals_p2
  if (is.null(xr2)) xr2 <- xr1
  xr2 <- recycle_vec(xr2, nx, "pop_values$x_residuals_p2")
  xc <- recycle_vec(pop_values$x_coresids, nx, "pop_values$x_coresids")
  xi1 <- pop_values$x_intercepts_p1
  if (is.null(xi1)) xi1 <- rep(0, nx)
  xi1 <- recycle_vec(xi1, nx, "pop_values$x_intercepts_p1")
  xi2 <- pop_values$x_intercepts_p2
  if (is.null(xi2)) xi2 <- xi1
  xi2 <- recycle_vec(xi2, nx, "pop_values$x_intercepts_p2")

  yl1 <- recycle_vec(pop_values$y_loadings_p1, ny, "pop_values$y_loadings_p1")
  yl2 <- pop_values$y_loadings_p2
  if (is.null(yl2)) yl2 <- yl1
  yl2 <- recycle_vec(yl2, ny, "pop_values$y_loadings_p2")
  yr1 <- recycle_vec(pop_values$y_residuals_p1, ny, "pop_values$y_residuals_p1")
  yr2 <- pop_values$y_residuals_p2
  if (is.null(yr2)) yr2 <- yr1
  yr2 <- recycle_vec(yr2, ny, "pop_values$y_residuals_p2")
  yc <- recycle_vec(pop_values$y_coresids, ny, "pop_values$y_coresids")
  yi1 <- pop_values$y_intercepts_p1
  if (is.null(yi1)) yi1 <- rep(0, ny)
  yi1 <- recycle_vec(yi1, ny, "pop_values$y_intercepts_p1")
  yi2 <- pop_values$y_intercepts_p2
  if (is.null(yi2)) yi2 <- yi1
  yi2 <- recycle_vec(yi2, ny, "pop_values$y_intercepts_p2")

  x_lv_var <- pop_values$x_lv_var
  if (is.null(x_lv_var)) x_lv_var <- c(1, 1)
  x_lv_var <- unlist(x_lv_var, use.names = FALSE)
  if (length(x_lv_var) == 1L) x_lv_var <- rep(x_lv_var, 2L)
  if (length(x_lv_var) != 2L) stop("`x_lv_var` must have length 1 or 2.")
  x_lv_cov <- pop_values$x_lv_cov
  if (is.null(x_lv_cov)) x_lv_cov <- 0
  x_lv_cov <- as.numeric(x_lv_cov)[1L]
  x_lv_means <- pop_values$x_lv_means
  if (is.null(x_lv_means)) x_lv_means <- c(0, 0)
  x_lv_means <- unlist(x_lv_means, use.names = FALSE)
  if (length(x_lv_means) == 1L) x_lv_means <- rep(x_lv_means, 2L)
  if (length(x_lv_means) != 2L) stop("`x_lv_means` must have length 1 or 2.")

  actor <- pop_values$actor
  partner <- pop_values$partner
  if (is.null(actor) || is.null(partner)) {
    stop("`pop_values$actor` and `pop_values$partner` are required.")
  }
  actor <- as.numeric(actor)[1L]
  partner <- as.numeric(partner)[1L]

  y_lv_var <- pop_values$y_lv_var
  if (is.null(y_lv_var)) y_lv_var <- c(0.5, 0.5)
  y_lv_var <- unlist(y_lv_var, use.names = FALSE)
  if (length(y_lv_var) == 1L) y_lv_var <- rep(y_lv_var, 2L)
  if (length(y_lv_var) != 2L) stop("`y_lv_var` must have length 1 or 2.")
  y_lv_cov <- pop_values$y_lv_cov
  if (is.null(y_lv_cov)) y_lv_cov <- 0
  y_lv_cov <- as.numeric(y_lv_cov)[1L]

  loads_x1 <- popLoadings(dvn, lvxname, "1", xl1, lvar = "X")
  loads_x2 <- popLoadings(dvn, lvxname, "2", xl2, lvar = "X")
  loads_y1 <- popLoadings(dvn, lvyname, "1", yl1, lvar = "Y")
  loads_y2 <- popLoadings(dvn, lvyname, "2", yl2, lvar = "Y")

  int_x1 <- popIntercepts(dvn, "1", xi1, lvar = "X")
  int_x2 <- popIntercepts(dvn, "2", xi2, lvar = "X")
  int_y1 <- popIntercepts(dvn, "1", yi1, lvar = "Y")
  int_y2 <- popIntercepts(dvn, "2", yi2, lvar = "Y")

  res_x1 <- popResiduals(dvn, "1", xr1, lvar = "X")
  res_x2 <- popResiduals(dvn, "2", xr2, lvar = "X")
  res_y1 <- popResiduals(dvn, "1", yr1, lvar = "Y")
  res_y2 <- popResiduals(dvn, "2", yr2, lvar = "Y")

  core_x <- popCoresids(dvn, xc, lvar = "X")
  core_y <- popCoresids(dvn, yc, lvar = "Y")

  lvx1 <- popLatentVars(dvn, lvxname, "1", x_lv_var[1L], lvar = "X")
  lvx2 <- popLatentVars(dvn, lvxname, "2", x_lv_var[2L], lvar = "X")
  lvx_cov <- popLatentCovars(dvn, lvxname, x_lv_cov, lvar = "X")
  lmx1 <- popLatentMeans(dvn, lvxname, "1", x_lv_means[1L], lvar = "X")
  lmx2 <- popLatentMeans(dvn, lvxname, "2", x_lv_means[2L], lvar = "X")

  d1 <- dvn[["dist1"]]
  d2 <- dvn[["dist2"]]
  reg1 <- sprintf(
    "%s%s ~ %s*%s%s + %s*%s%s",
    lvyname, d1, actor, lvxname, d1, partner, lvxname, d2
  )
  reg2 <- sprintf(
    "%s%s ~ %s*%s%s + %s*%s%s",
    lvyname, d2, actor, lvxname, d2, partner, lvxname, d1
  )

  y1lab <- paste0(lvyname, d1)
  y2lab <- paste0(lvyname, d2)
  y_var1 <- sprintf("%s ~~ %s*%s", y1lab, y_lv_var[1L], y1lab)
  y_var2 <- sprintf("%s ~~ %s*%s", y2lab, y_lv_var[2L], y2lab)
  y_cov <- sprintf("%s ~~ %s*%s", y1lab, y_lv_cov, y2lab)

  sprintf(
    paste0(
      "# Latent APIM - population model (simulateData)\n\n",
      "# X measurement\n%s\n%s\n\n",
      "# Y measurement\n%s\n%s\n\n",
      "# Manifest intercepts X\n%s\n%s\n\n",
      "# Manifest intercepts Y\n%s\n%s\n\n",
      "# Residual variances X\n%s\n%s\n\n",
      "# Residual variances Y\n%s\n%s\n\n",
      "# Matched-pair residual covariances\n%s\n\n%s\n\n",
      "# Latent X variances, covariance, means\n%s\n%s\n%s\n%s\n%s\n\n",
      "# Structural regressions\n%s\n%s\n\n",
      "# Latent Y residual (co)variances\n%s\n%s\n%s"
    ),
    loads_x1, loads_x2,
    loads_y1, loads_y2,
    int_x1, int_x2,
    int_y1, int_y2,
    res_x1, res_x2,
    res_y1, res_y2,
    core_x, core_y,
    lvx1, lvx2, lvx_cov, lmx1, lmx2,
    reg1, reg2,
    y_var1, y_var2, y_cov
  )
}
