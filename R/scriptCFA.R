#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Dyadic Confirmatory Factor Analysis (CFA) Models with Multiple Factors
#'
#' This function takes the outputted object from scrapeVarCross() when the var_list argument has been used,
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' measurement models for either a group of latent variables (e.g., different sub-scales from a self-report measures).
#'
#' By default, many dySEM:: functions (including scriptCFA() default to
#' a fixed-factor method of scale-setting, whereby the latent variance of
#' a given factor is constrained to 1 for both partners in the configurally invariant
#' #model, and then one of these variances is freely estimated in subsequent
#' #models of the invariance testing sequence. We have selected this default
#' for two reasons: (1) the selection of a marker-variable is usually arbitrary,
#' yet can have a large influence on the estimation and testing of of structural
#' parameters (see https://stats.stackexchange.com/questions/402133/in-cfa-does-it-matter-which-factor-loading-is-set-to-1/402732#402732); and (2) the selection of a non-invariant marker-variable
#' can have disastrous down-stream consequences for the identification of
#' non-invariant measurement parameters, following a the rejection of an omnibus
#' #invariance constraint set (see Lee, Preacher, & Little, 2011).
#'
#' @param dvn input dvn list from scrapeVarCross(); scrapeVarCross *must* have been run with the var_list argument
#' @param scaleset input character to specify how to set the scale of the latent variable(s). Default is
#' "FF" (fixed-factor; see Details for rationale), but user can specify "MV" (Marker Variable)
#' @param constr_dy_meas input character vector detailing which measurement model parameters to constrain across dyad members.
#' Default is c("loadings", "intercepts", "residuals")(in combination with defaults for constr_dy_struct, an indistinguishable dyadic CFA),
#' but user can specify any combination of "loadings", "intercepts", and "residuals",
#' or "none" to specify an otherwise unconstrained dyadic configural invariance model
#' @param constr_dy_struct input character vector detailing which structural model parameters to constrain across dyad members.
#' Default is c("variances", "means")(in combination with defaults for constr_dy_meas, an indistinguishable dyadic CFA),
#' but user can specify any combination of "variances" and "means", or "none".
#' @param writeTo A character string specifying a directory path to where a .txt file of the resulting lavaan script should be written.
#' If set to “.”, the .txt file will be written to the current working directory.
#' The default is NULL, and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the .txt output file.
#' The default is NULL. The specified name will be automatically appended with the .txt file extension.
#' If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{scrapeVarCross}} which this function relies on
#' @family multi-construct script-writing functions
#' @export
#' @examples
#'
#' # When different factor use distinct stems:
#' imsList <- list(
#'   lvnames = c("Sat", "Q_Alt", "Invest", "Comm"),
#'   stem = c("sat.g", "qalt.g", "invest.g", "com"),
#'   delim1 = c("", "", "", ""),
#'   delim2 = c("_", "_", "_", "_")
#' )
#'
#' dvnIMS <- scrapeVarCross(imsM,
#'   var_list = imsList,
#'   var_list_order = "sip",
#'   distinguish_1 = "f",
#'   distinguish_2 = "m"
#' )
#'
#' script.ims.config <- scriptCFA(dvnIMS,
#'   scaleset = "FF",
#'   constr_dy_meas = "none",
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_config"
#' )
#'
#' script.ims.load <- scriptCFA(dvnIMS,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings"),
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_load"
#' )
#'
#' script.ims.int <- scriptCFA(dvnIMS,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings", "intercepts"),
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_int"
#' )
#'
#' script.ims.res <- scriptCFA(dvnIMS,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_res"
#' )
#'
#' script.ims.indist <- scriptCFA(dvnIMS,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = c("variances", "means"), writeTo = tempdir(), fileName = "ims_indist"
#' )
#'
#' # When different factor use the same stem and muse be detected through numeric position
#'
#' prqcList <- list(
#'   lvnames = c("Sat", "Comm", "Intim", "Trust", "Pass", "Love"),
#'   stem = c("prqc", "prqc", "prqc", "prqc", "prqc", "prqc"),
#'   delim1 = c(".", ".", ".", ".", ".", "."),
#'   delim2 = c("_", "_", "_", "_", "_", "_"),
#'   min_num = c(1, 4, 7, 10, 13, 16),
#'   max_num = c(3, 6, 9, 12, 15, 18)
#' )
#'
#' dvnPRQC <- scrapeVarCross(prqcQ,
#'   var_list = prqcList,
#'   var_list_order = "spi",
#'   distinguish_1 = "1",
#'   distinguish_2 = "2"
#' )
#'
#' script.prqc.config <- scriptCFA(dvnPRQC,
#'   scaleset = "FF",
#'   constr_dy_meas = "none",
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_config"
#' )
#'
#' script.prqc.load <- scriptCFA(dvnPRQC,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings"),
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_load"
#' )
#'
#' script.prqc.int <- scriptCFA(dvnPRQC,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings", "intercepts"),
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_int"
#' )
#'
#' script.prqc.res <- scriptCFA(dvnPRQC,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_res"
#' )
#'
#' script.prqc.indist <- scriptCFA(dvnPRQC,
#'   scaleset = "FF",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = c("variances", "means"), writeTo = tempdir(), fileName = "prqc_indist"
#' )
scriptCFA <- function(dvn, scaleset = "FF",
                      constr_dy_meas = c("loadings", "intercepts", "residuals"),
                      constr_dy_struct = c("variances", "means"),
                      writeTo = NULL,
                      fileName = NULL) {
  # Input validation
  # Validate dvn argument
  if (missing(dvn) || is.null(dvn)) {
    stop("The `dvn` argument is required and cannot be NULL.")
  }
  if (!is.list(dvn)) {
    stop("The `dvn` argument must be a list object.")
  }

  # Validate scaleset argument
  if (!is.character(scaleset)) {
    stop("The `scaleset` argument must be a character string.")
  }


  # check for valid inputs
  if (!is.list(dvn$p1xvarnames) | !is.list(dvn$p2xvarnames)) {
    stop("You must supply a dvn object from an instance of scrapeVarCross() when the var_list argument has been used (i.e., the dvn should contain indicators for multiple factors per partner. If you are looking to script a correlated factors model (scriptCFA()'s previous functionality), please use scriptCor() instead.")
  }

  if (!any(constr_dy_meas %in% c("loadings", "intercepts", "residuals", "none"))) {
    stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'")
  }

  if (!any(constr_dy_struct %in% c("variances", "means", "none"))) {
    stop("constr_dy_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
  }

  if (!scaleset %in% c("FF", "MV")) {
    stop("scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
  }

  # loadings
  if (any(constr_dy_meas == "loadings")) {
    if (scaleset == "FF") {
      x1loads <- multifac_loads(dvn, partner = "1", type = "equated")
      x2loads <- multifac_loads(dvn, partner = "2", type = "equated")
    } else if (scaleset == "MV") {
      x1loads <- multifac_loads(dvn, partner = "1", type = "fixed")
      x2loads <- multifac_loads(dvn, partner = "2", type = "equated")
    }
  } else {
    if (scaleset == "FF") {
      x1loads <- multifac_loads(dvn, partner = "1", type = "free")
      x2loads <- multifac_loads(dvn, partner = "2", type = "free")
    } else if (scaleset == "MV") {
      x1loads <- multifac_loads(dvn, partner = "1", type = "fixed")
      x2loads <- multifac_loads(dvn, partner = "2", type = "fixed")
    }
  }

  # intercepts
  if (any(constr_dy_meas == "intercepts")) {
    if (scaleset == "FF") {
      xints1 <- multifac_intercepts(dvn, partner = "1", type = "equated")
      xints2 <- multifac_intercepts(dvn, partner = "2", type = "equated")
    } else if (scaleset == "MV") {
      xints1 <- multifac_intercepts(dvn, partner = "1", type = "fixed")
      xints2 <- multifac_intercepts(dvn, partner = "2", type = "equated")
    }
  } else {
    if (scaleset == "FF") {
      xints1 <- multifac_intercepts(dvn, partner = "1", type = "free")
      xints2 <- multifac_intercepts(dvn, partner = "2", type = "free")
    } else if (scaleset == "MV") {
      xints1 <- multifac_intercepts(dvn, partner = "1", type = "fixed")
      xints2 <- multifac_intercepts(dvn, partner = "2", type = "fixed")
    }
  }

  # residual variances
  if (any(constr_dy_meas == "residuals")) {
    xres1 <- multifac_resids(dvn, partner = "1", type = "equated")
    xres2 <- multifac_resids(dvn, partner = "2", type = "equated")
  } else {
    # Residual variances
    xres1 <- multifac_resids(dvn, partner = "1", type = "free")
    xres2 <- multifac_resids(dvn, partner = "2", type = "free")
  }

  # Correlated residuals
  xcoresids <- multifac_coresids(dvn, type = "free")

  # latent variances
  if (any(constr_dy_struct == "variances")) {
    if (scaleset == "FF") {
      xvar1 <- multifac_lvars(dvn, partner = "1", type = "constrain")
      xvar2 <- multifac_lvars(dvn, partner = "2", type = "equate")
    } else if (scaleset == "MV") {
      xvar1 <- multifac_lvars(dvn, partner = "1", type = "equate")
      xvar2 <- multifac_lvars(dvn, partner = "2", type = "equate")
    }
  } else if (!any(constr_dy_struct == "variances") & any(constr_dy_meas == "loadings") & scaleset == "FF") {
    xvar1 <- multifac_lvars(dvn, partner = "1", type = "constrain")
    xvar2 <- multifac_lvars(dvn, partner = "2", type = "free")
  } else {
    if (scaleset == "FF") {
      xvar1 <- multifac_lvars(dvn, partner = "1", type = "constrain")
      xvar2 <- multifac_lvars(dvn, partner = "2", type = "constrain")
    } else if (scaleset == "MV") {
      xvar1 <- multifac_lvars(dvn, partner = "1", type = "free")
      xvar2 <- multifac_lvars(dvn, partner = "2", type = "free")
    }
  }

  # latent covariance
  if (any(constr_dy_struct == "orthogonal")) {
    xcovar <- multifac_lcovars(dvn, type = "zero")
  } else {
    xcovar <- multifac_lcovars(dvn, type = "free")
  }

  # latent means
  if (any(constr_dy_struct == "means")) {
    if (scaleset == "FF") {
      xmean1 <- multifac_lmeans(dvn, partner = "1", type = "fixed")
      xmean2 <- multifac_lmeans(dvn, partner = "2", type = "equated")
    } else if (scaleset == "MV") {
      xmean1 <- multifac_lmeans(dvn, partner = "1", type = "equated")
      xmean2 <- multifac_lmeans(dvn, partner = "2", type = "equated")
    }
  } else if (!any(constr_dy_struct == "means") & any(constr_dy_meas == "intercepts") & scaleset == "FF") {
    xmean1 <- multifac_lmeans(dvn, partner = "1", type = "fixed")
    xmean2 <- multifac_lmeans(dvn, partner = "2", type = "free")
  } else {
    if (scaleset == "FF") {
      xmean1 <- multifac_lmeans(dvn, partner = "1", type = "fixed")
      xmean2 <- multifac_lmeans(dvn, partner = "2", type = "fixed")
    } else if (scaleset == "MV") {
      xmean1 <- multifac_lmeans(dvn, partner = "1", type = "free")
      xmean2 <- multifac_lmeans(dvn, partner = "2", type = "free")
    }
  }

  # Script Creation Syntax
  script <- sprintf(
    "#Measurement Model\n\n#Loadings\n%s\n%s\n\n#Intercepts\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n#Structural Model\n\n#Latent (Co)Variances\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s",
    paste(x1loads, collapse = "\n"),
    paste(x2loads, collapse = "\n"),
    paste(xints1, collapse = "\n"),
    paste(xints2, collapse = "\n"),
    paste(xres1, collapse = "\n"),
    paste(xres2, collapse = "\n"),
    paste(xcoresids, collapse = "\n"),
    paste(xvar1, collapse = "\n"),
    paste(xvar2, collapse = "\n"),
    paste(xcovar, collapse = "\n"),
    paste(xmean1, collapse = "\n"),
    paste(xmean2, collapse = "\n")
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
