#' Observed Actor-Partner Interdependence Model (lavaan syntax)
#'
#' Writes lavaan syntax for an APIM with one observed (or composite) X and Y per
#' partner. Structural constraints mirror [scriptAPIM()] (dyadic variances/means,
#' actor/partner paths, optional orthogonal dyadic covariances).
#'
#' @param X1,X2,Y1,Y2 Character names of variables for partners 1 and 2.
#' @param constr_dy_x_struct Character vector: any of `"variances"`, `"means"`,
#'   `"orthogonal"`, and/or `"none"`. Defaults match [scriptAPIM()]. `"means"`
#'   is applied only when `includeMeanStruct` is `TRUE` (same behavior as
#'   [scriptAPIM()]). `"orthogonal"` fixes the dyadic covariance between `X1` and
#'   `X2` to zero.
#' @param constr_dy_y_struct Same allowed values as `constr_dy_x_struct`, for the
#'   Y side. `"variances"` equates residual variances of `Y1` and `Y2`;
#'   `"orthogonal"` fixes the residual covariance between `Y1` and `Y2` to zero.
#' @param constr_dy_xy_struct Character vector: `"actors"`, `"partners"`, `"all"`,
#'   `"actors_zero"`, `"partners_zero"`, and/or `"none"` (same semantics as
#'   [scriptAPIM()]).
#' @param includeMeanStruct Logical; if `TRUE`, mean structure is included and
#'   `"means"` in `constr_dy_x_struct` / `constr_dy_y_struct` equates exogenous
#'   means and regression intercepts, respectively.
#' @param est_k Logical; if `TRUE`, Kenny & Ledermann's (2010) k ratio(s) are
#'   defined in the syntax (see [scriptAPIM()]).
#' @param equate `r lifecycle::badge("deprecated")` Use `constr_dy_xy_struct`
#'   instead. If supplied, maps `none` → `"none"`, `actor` → `c("actors")`,
#'   `partner` → `c("partners")`, `all` → `c("actors", "partners")`.
#' @param k `r lifecycle::badge("deprecated")` Use `est_k` instead.
#' @template writeTo
#' @template fileName
#'
#' @return Character string of lavaan model syntax.
#' @seealso [scriptAPIM()]
#' @family bi-construct script-writing functions
#' @export
#'
#' @examples
#' scriptObsAPIM(
#'   X1 = "SexSatA", Y1 = "RelSatA",
#'   X2 = "SexSatB", Y2 = "RelSatB",
#'   constr_dy_xy_struct = "none",
#'   constr_dy_x_struct = "none",
#'   constr_dy_y_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "obsAPIM_script"
#' )
scriptObsAPIM <- function(X1 = NULL, Y1 = NULL,
                          X2 = NULL, Y2 = NULL,
                          constr_dy_x_struct = c("variances", "means"),
                          constr_dy_y_struct = c("variances", "means"),
                          constr_dy_xy_struct = c("actors", "partners"),
                          includeMeanStruct = FALSE,
                          equate = lifecycle::deprecated(),
                          k = lifecycle::deprecated(),
                          est_k = FALSE,
                          writeTo = NULL,
                          fileName = NULL) {
  if (lifecycle::is_present(equate)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "scriptObsAPIM(equate)",
      with = "scriptObsAPIM(constr_dy_xy_struct)",
      always = TRUE
    )
    equate <- match.arg(equate, choices = c("none", "actor", "partner", "all"))
    constr_dy_xy_struct <- switch(equate,
      none = "none",
      actor = c("actors"),
      partner = c("partners"),
      all = c("actors", "partners")
    )
  }

  if (lifecycle::is_present(k)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "scriptObsAPIM(k)",
      with = "scriptObsAPIM(est_k)",
      always = TRUE
    )
    est_k <- isTRUE(k)
  }

  if (missing(X1) || is.null(X1) || missing(Y1) || is.null(Y1) ||
        missing(X2) || is.null(X2) || missing(Y2) || is.null(Y2)) {
    stop("Arguments `X1`, `Y1`, `X2`, and `Y2` are required.", call. = FALSE)
  }
  if (!all(is.character(c(X1, Y1, X2, Y2)), length(X1) == 1L, length(Y1) == 1L,
           length(X2) == 1L, length(Y2) == 1L)) {
    stop("`X1`, `Y1`, `X2`, and `Y2` must each be a single character string.", call. = FALSE)
  }

  if (!is.logical(includeMeanStruct) || length(includeMeanStruct) != 1L) {
    stop("`includeMeanStruct` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(est_k) || length(est_k) != 1L) {
    stop("`est_k` must be a single logical value.", call. = FALSE)
  }

  if (length(constr_dy_xy_struct) == 0L) {
    stop("`constr_dy_xy_struct` must be non-empty.", call. = FALSE)
  }
  if (length(constr_dy_x_struct) == 0L) {
    stop("`constr_dy_x_struct` must be non-empty.", call. = FALSE)
  }
  if (length(constr_dy_y_struct) == 0L) {
    stop("`constr_dy_y_struct` must be non-empty.", call. = FALSE)
  }

  ok_xy <- c("actors", "partners", "all", "actors_zero", "partners_zero", "none")
  ok_struct <- c("variances", "means", "none", "orthogonal")
  if (!all(constr_dy_xy_struct %in% ok_xy)) {
    stop(
      "`constr_dy_xy_struct` must contain only ",
      paste0("\"", ok_xy, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!all(constr_dy_x_struct %in% ok_struct)) {
    stop(
      "`constr_dy_x_struct` must contain only ",
      paste0("\"", ok_struct, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!all(constr_dy_y_struct %in% ok_struct)) {
    stop(
      "`constr_dy_y_struct` must contain only ",
      paste0("\"", ok_struct, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }

  xy <- constr_dy_xy_struct
  has_actors_eq <- any(xy %in% c("actors", "all"))
  has_partners_eq <- any(xy %in% c("partners", "all"))
  actors_zero <- any(xy == "actors_zero")
  partners_zero <- any(xy == "partners_zero")

  if (actors_zero) {
    a1 <- "0*"
    a2 <- "0*"
  } else if (has_actors_eq) {
    a1 <- "a*"
    a2 <- "a*"
  } else {
    a1 <- "a1*"
    a2 <- "a2*"
  }

  if (partners_zero) {
    p1 <- "0*"
    p2 <- "0*"
  } else if (has_partners_eq) {
    p1 <- "p*"
    p2 <- "p*"
  } else {
    p1 <- "p1*"
    p2 <- "p2*"
  }

  y_int1 <- ""
  y_int2 <- ""
  if (isTRUE(includeMeanStruct)) {
    if (any(constr_dy_y_struct == "means")) {
      y_int1 <- "iy*1 + "
      y_int2 <- "iy*1 + "
    } else {
      y_int1 <- "iy1*1 + "
      y_int2 <- "iy2*1 + "
    }
  }

  reg1 <- paste0(Y1, " ~ ", y_int1, a1, X1, " + ", p1, X2)
  reg2 <- paste0(Y2, " ~ ", y_int2, a2, X2, " + ", p2, X1)

  x_var <- character(0)
  if (any(constr_dy_x_struct == "variances")) {
    x_var <- c(paste0(X1, " ~~ vx*", X1), paste0(X2, " ~~ vx*", X2))
  }

  x_mean <- character(0)
  if (isTRUE(includeMeanStruct)) {
    if (any(constr_dy_x_struct == "means")) {
      x_mean <- c(paste0(X1, " ~ mx*1"), paste0(X2, " ~ mx*1"))
    } else {
      x_mean <- c(paste0(X1, " ~ mx1*1"), paste0(X2, " ~ mx2*1"))
    }
  }

  if (any(constr_dy_x_struct == "orthogonal")) {
    x_dyad <- paste0(X1, " ~~ 0*", X2)
  } else {
    x_dyad <- paste0(X1, " ~~ ", X2)
  }

  y_var <- character(0)
  if (any(constr_dy_y_struct == "variances")) {
    y_var <- c(paste0(Y1, " ~~ vy*", Y1), paste0(Y2, " ~~ vy*", Y2))
  }

  if (any(constr_dy_y_struct == "orthogonal")) {
    y_dyad <- paste0(Y1, " ~~ 0*", Y2)
  } else {
    y_dyad <- paste0(Y1, " ~~ ", Y2)
  }

  k_block <- character(0)
  if (isTRUE(est_k)) {
    if (any(xy == "partners_zero")) {
      stop("You cannot estimate k when constraining partner effects to zero.", call. = FALSE)
    }
    if (any(xy == "actors_zero")) {
      stop("You cannot estimate k when constraining actor effects to zero.", call. = FALSE)
    }
    if (any(xy == "all")) {
      k_block <- "k := p/a"
    } else if (any(xy == "actors") && any(xy == "partners")) {
      k_block <- "k := p/a"
    } else if (any(xy == "actors")) {
      k_block <- c("k1 := p1/a", "k2 := p2/a")
    } else if (any(xy == "partners")) {
      k_block <- c("k1 := p/a1", "k2 := p/a2")
    } else if (any(xy == "none")) {
      k_block <- c("k1 := p1/a1", "k2 := p2/a2")
    }
  }

  parts <- list()
  parts <- c(parts, list(paste(
    "# Actor and partner effects",
    paste(reg1, reg2, sep = "\n"),
    sep = "\n"
  )))

  if (length(x_mean)) {
    parts <- c(parts, list(paste(
      "# Exogenous means",
      paste(x_mean, collapse = "\n"),
      sep = "\n"
    )))
  }

  if (length(x_var)) {
    parts <- c(parts, list(paste(
      "# Exogenous variances",
      paste(x_var, collapse = "\n"),
      sep = "\n"
    )))
  }

  parts <- c(parts, list(paste(
    "# Dyadic X covariance",
    x_dyad,
    sep = "\n"
  )))

  if (length(y_var)) {
    parts <- c(parts, list(paste(
      "# Endogenous residual variances",
      paste(y_var, collapse = "\n"),
      sep = "\n"
    )))
  }

  parts <- c(parts, list(paste(
    "# Residual covariance between Y",
    y_dyad,
    sep = "\n"
  )))

  if (length(k_block)) {
    parts <- c(parts, list(paste(
      "# k parameter(s)",
      paste(k_block, collapse = "\n"),
      sep = "\n"
    )))
  }

  script <- paste(unlist(parts), collapse = "\n\n")

  if (!is.null(writeTo) || !is.null(fileName)) {
    if (is.null(writeTo) || is.null(fileName)) {
      stop(
        "Both `writeTo` and `fileName` must be non-NULL to save the script.",
        call. = FALSE
      )
    }
    if (!is.character(writeTo) || length(writeTo) != 1L) {
      stop("`writeTo` must be a single character string.", call. = FALSE)
    }
    if (!dir.exists(writeTo)) {
      stop("The directory in `writeTo` does not exist.", call. = FALSE)
    }
    if (!is.character(fileName) || length(fileName) != 1L) {
      stop("`fileName` must be a single character string.", call. = FALSE)
    }
    cat(script, "\n", file = file.path(writeTo, paste0(fileName, ".txt")))
  }

  script
}
