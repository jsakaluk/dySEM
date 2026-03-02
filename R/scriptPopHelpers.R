#' Simulation Population Model Helpers
#'
#' Helper functions for writing lavaan syntax with fixed numeric values
#' for population models used in Monte Carlo simulations. All helpers require
#' explicit numeric values (no heuristics).
#'
#' @param dvn Input dvn list from \code{build_dvn()} or \code{scrapeVarCross()}
#' @param lvname Character name for the latent variable in lavaan syntax
#' @param partner Character: \code{"1"} or \code{"2"} for partner-specific, or \code{"g"} for unidimensional (both partners)
#' @param values Numeric vector of parameter values. Length must match number of indicators.
#' @param lvar Character \code{"X"} or \code{"Y"} for latent variable family. Default \code{"X"}.
#' @family helpers


#' @rdname scriptPopHelpers
#' @noRd
popLoadings <- function(dvn, lvname, partner, values, lvar = "X") {
  nvar <- if (lvar == "X") dvn$xindper else dvn$yindper
  if (partner == "g") {
    p1vars <- if (lvar == "X") dvn$p1xvarnames else dvn$p1yvarnames
    p2vars <- if (lvar == "X") dvn$p2xvarnames else dvn$p2yvarnames
    if (length(values) != nvar) {
      stop("Length of `values` (", length(values), ") must equal n_items (", nvar, ").")
    }
    lv_label <- paste0(lvname, "Dy")
    parts <- c(
      paste(paste0(values, "*", p1vars), collapse = " + "),
      paste(paste0(values, "*", p2vars), collapse = " + ")
    )
    paste0(lv_label, " =~ ", paste(parts, collapse = " + "))
  } else {
    varnames <- if (partner == "1") {
      if (lvar == "X") dvn$p1xvarnames else dvn$p1yvarnames
    } else {
      if (lvar == "X") dvn$p2xvarnames else dvn$p2yvarnames
    }
    if (length(values) != length(varnames)) {
      stop("Length of `values` (", length(values), ") must equal number of indicators (", length(varnames), ").")
    }
    dist <- if (partner == "1") dvn$dist1 else dvn$dist2
    lv_label <- paste0(lvname, dist)
    paste0(lv_label, " =~ ", paste(paste0(values, "*", varnames), collapse = " + "))
  }
}

#' @rdname scriptPopHelpers
#' @noRd
popIntercepts <- function(dvn, partner, values, lvar = "X") {
  varnames <- if (partner == "1") {
    if (lvar == "X") dvn$p1xvarnames else dvn$p1yvarnames
  } else {
    if (lvar == "X") dvn$p2xvarnames else dvn$p2yvarnames
  }
  nvar <- if (lvar == "X") dvn$xindper else dvn$yindper
  if (length(values) != nvar) {
    stop("Length of `values` (", length(values), ") must equal n_items (", nvar, ").")
  }
  paste(paste0(varnames, " ~ ", values, "*1"), collapse = "\n")
}

#' @rdname scriptPopHelpers
#' @noRd
popResiduals <- function(dvn, partner, values, lvar = "X") {
  varnames <- if (partner == "1") {
    if (lvar == "X") dvn$p1xvarnames else dvn$p1yvarnames
  } else {
    if (lvar == "X") dvn$p2xvarnames else dvn$p2yvarnames
  }
  nvar <- if (lvar == "X") dvn$xindper else dvn$yindper
  if (length(values) != nvar) {
    stop("Length of `values` (", length(values), ") must equal n_items (", nvar, ").")
  }
  paste(paste0(varnames, " ~~ ", values, "*", varnames), collapse = "\n")
}

#' @rdname scriptPopHelpers
#' @noRd
popCoresids <- function(dvn, values, lvar = "X") {
  p1vars <- if (lvar == "X") dvn$p1xvarnames else dvn$p1yvarnames
  p2vars <- if (lvar == "X") dvn$p2xvarnames else dvn$p2yvarnames
  nvar <- if (lvar == "X") dvn$xindper else dvn$yindper
  if (length(values) != nvar) {
    stop("Length of `values` (", length(values), ") must equal n_items (", nvar, ").")
  }
  paste(paste0(p1vars, " ~~ ", values, "*", p2vars), collapse = "\n")
}

#' @rdname scriptPopHelpers
#' @param value Numeric; single value for latent variance
#' @noRd
popLatentVars <- function(dvn, lvname, partner, value, lvar = "X") {
  dist <- if (partner == "1") dvn$dist1 else dvn$dist2
  lv_label <- paste0(lvname, dist)
  paste0(lv_label, " ~~ ", value, "*", lv_label)
}

#' @rdname scriptPopHelpers
#' @param value Numeric; latent covariance value
#' @noRd
popLatentCovars <- function(dvn, lvname, value, lvar = "X") {
  lv1 <- paste0(lvname, dvn$dist1)
  lv2 <- paste0(lvname, dvn$dist2)
  paste0(lv1, " ~~ ", value, "*", lv2)
}

#' @rdname scriptPopHelpers
#' @param value Numeric; latent mean value
#' @noRd
popLatentMeans <- function(dvn, lvname, partner, value, lvar = "X") {
  dist <- if (partner == "1") dvn$dist1 else dvn$dist2
  lv_label <- paste0(lvname, dist)
  paste0(lv_label, " ~ ", value, "*1")
}
