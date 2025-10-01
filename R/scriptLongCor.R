#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Longitudinal Dyadic Factor Models
#'
#' This function takes the outputted object from `scrapeVarLong()`
#'  and automatically writes, returns, and exports (`.txt`) `lavaan` syntax
#'  for specifying longitudinal dyadic configural, loading, intercept, and residual invariant
#'  multi-factor models.
#'
#' @param dvn Input dvn list from `scrapeVarLong()`
#' @param scaleset Input character to specify how to set the scale of the latent variable. Default is
#'  `"FF"` (fixed-factor; see Details for rationale), but user can specify `"MV"` (Marker Variable)
#' @param lvname Input character to (arbitrarily) name the latent variable in `lavaan` syntax
#' @param constr_dy_meas Input character vector detailing which measurement model parameters to constrain across dyad members.
#' @param constr_dy_struct Input character vector detailing which structural model parameters to constrain across dyad members.
#' Default is `c("variances", "means")`(in combination with defaults for `constr_dy_meas`, an indistinguishable correlated dyadic factors model),
#' but user can specify any combination of `"variances"` and `"means"`, or `"none"`.
#' @param constr_time_meas Input character vector detailing which measurement model parameters to constrain across time points.
#' @param constr_time_struct Input character vector detailing which structural model parameters to constrain across time points.
#' Default is `c("variances", "means")`, but user can specify any combination of `"variances"` and `"means"`, or `"none"`.
#' @param writeTo A character string specifying a directory path to where a `.txt` file of the resulting `lavaan` script should be written.
#'  If set to `"."`, the `.txt` file will be written to the current working directory.
#'  The default is `NULL`, and examples use a temporary directory created by `tempdir()`.
#' @param fileName A character string specifying a desired base name for the `.txt` output file.
#'  The default is `NULL`. The specified name will be automatically appended with the `.txt` file extension.
#'  If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' @return Character object of `lavaan` script that can be passed immediately to
#'  `lavaan` functions.
#'
#' @details
#' * By default, many `dySEM::` functions (including `scriptLongCor()`) default to
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
#' @seealso \code{\link{scrapeVarLong}} which this function relies on.
#' @family longitudinal dyadic script-writing functions
#' @export
#'
scriptLongCor <- function(
    dvn,
    scaleset = "FF",
    lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"),
    constr_time_meas = c("loadings", "intercepts", "residuals"),
    constr_time_struct = c("variances", "means"),
    writeTo = NULL,
    fileName = NULL) {
    # check for valid inputs
    if (length(dvn) < 10) {
        stop("You must supply a valid dvn object from scrapeVarLong()")
    }

    if (!any(constr_dy_meas %in% c("loadings", "intercepts", "residuals", "none"))) {
        stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'")
    }

    if (!any(constr_dy_struct %in% c("variances", "means", "none"))) {
        stop("constr_dy_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
    }

    if (!any(constr_time_meas %in% c("loadings", "intercepts", "residuals", "none"))) {
        stop("constr_time_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'")
    }

    if (!any(constr_time_struct %in% c("variances", "means", "none"))) {
        stop("constr_time_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
    }

    if (!scaleset %in% c("FF", "MV")) {
        stop("scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
    }

    # loadings
    if (any(constr_dy_meas == "loadings") && any(constr_time_meas == "loadings")) {
        # Both dyadic and longitudinal constraints
        if (scaleset == "FF") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy_long")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy_long")
        } else if (scaleset == "MV") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy_long_mv")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy_long")
        }
    } else if (any(constr_dy_meas == "loadings")) {
        # Only dyadic constraints
        if (scaleset == "FF") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy")
        } else if (scaleset == "MV") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy_mv")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy")
        }
    } else if (any(constr_time_meas == "loadings")) {
        # Only longitudinal constraints
        if (scaleset == "FF") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "equated_long")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "equated_long")
        } else if (scaleset == "MV") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "equated_long_mv")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "equated_long")
        }
    } else {
        # No constraints
        if (scaleset == "FF") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "free")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "free")
        } else if (scaleset == "MV") {
            xloads1 <- longLoads(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
            xloads2 <- longLoads(dvn, lvar = "X", lvname, partner = "2", type = "fixed")
        }
    }

    # intercepts
    if (any(constr_dy_meas == "intercepts") && any(constr_time_meas == "intercepts")) {
        # Both dyadic and longitudinal constraints
        if (scaleset == "FF") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "equated_dy_long")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "equated_dy_long")
        } else if (scaleset == "MV") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "equated_dy_long_mv")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "equated_dy_long")
        }
    } else if (any(constr_dy_meas == "intercepts")) {
        # Only dyadic constraints
        if (scaleset == "FF") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "equated_dy")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "equated_dy")
        } else if (scaleset == "MV") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "equated_dy_mv")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "equated_dy")
        }
    } else if (any(constr_time_meas == "intercepts")) {
        # Only longitudinal constraints
        if (scaleset == "FF") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "equated_long")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "equated_long")
        } else if (scaleset == "MV") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "equated_long_mv")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "equated_long")
        }
    } else {
        # No constraints
        if (scaleset == "FF") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "free")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "free")
        } else if (scaleset == "MV") {
            xints1 <- longIntercepts(dvn, lvar = "X", partner = "1", type = "fixed")
            xints2 <- longIntercepts(dvn, lvar = "X", partner = "2", type = "fixed")
        }
    }

    # residual variances
    if (any(constr_dy_meas == "residuals") && any(constr_time_meas == "residuals")) {
        # Both dyadic and longitudinal constraints
        if (scaleset == "FF") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "equated_dy_long")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "equated_dy_long")
        } else if (scaleset == "MV") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "equated_dy_long")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "equated_dy_long")
        }
    } else if (any(constr_dy_meas == "residuals")) {
        # Only dyadic constraints
        if (scaleset == "FF") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "equated_dy")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "equated_dy")
        } else if (scaleset == "MV") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "equated_dy")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "equated_dy")
        }
    } else if (any(constr_time_meas == "residuals")) {
        # Only longitudinal constraints
        if (scaleset == "FF") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "equated_long")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "equated_long")
        } else if (scaleset == "MV") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "equated_long")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "equated_long")
        }
    } else {
        # No constraints
        if (scaleset == "FF") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "free")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "free")
        } else if (scaleset == "MV") {
            xres1 <- longResids(dvn, lvar = "X", partner = "1", type = "free")
            xres2 <- longResids(dvn, lvar = "X", partner = "2", type = "free")
        }
    }

    # Correlated residuals - only implementing "free" for now
    xcoresids_dyad <- longCoresidsDyad(dvn, lvar = "X", type = "free")
    xcoresids_time <- longCoresidsTime(dvn, lvar = "X", type = "free")

    # latent variances
    if (any(constr_dy_struct == "variances") && any(constr_time_struct == "variances")) {
        # Both dyadic and longitudinal constraints
        if (scaleset == "FF") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy_long_ff")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy_long")
        } else if (scaleset == "MV") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy_long")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy_long")
        }
    } else if (any(constr_dy_struct == "variances")) {
        # Only dyadic constraints
        if (scaleset == "FF") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy_ff")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy")
        } else if (scaleset == "MV") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "equated_dy")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "equated_dy")
        }
    } else if (any(constr_time_struct == "variances")) {
        # Only longitudinal constraints
        if (scaleset == "FF") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "fixed")
        } else if (scaleset == "MV") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "equated_long")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "equated_long")
        }
    } else if (!any(constr_dy_struct == "variances") && !any(constr_time_struct == "variances") && any(constr_dy_meas == "loadings") && scaleset == "FF") {
        # Special case: no structural constraints on variances but loadings are constrained across dyad (FF scaleset)
        xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
        xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "free")
    } else if (!any(constr_dy_struct == "variances") && !any(constr_time_struct == "variances") && any(constr_time_meas == "loadings") && scaleset == "FF") {
        # Special case: no structural constraints on variances but loadings are constrained across time (FF scaleset)
        xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "equated_long_ff")
        xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "equated_long_ff")
    } else {
        # No constraints
        if (scaleset == "FF") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "fixed")
        } else if (scaleset == "MV") {
            xvar1 <- longLvars(dvn, lvar = "X", lvname, partner = "1", type = "free")
            xvar2 <- longLvars(dvn, lvar = "X", lvname, partner = "2", type = "free")
        }
    }

    # latent covariance - only implementing "free" for now
    xcovar_intraclass <- longLcovarsIntraclass(dvn, type = "free")
    xcovar_intraind <- longLcovarsIntraind(dvn, type = "free")
    xcovar_interind <- longLcovarsInterind(dvn, type = "free")

    # latent means
    if (any(constr_dy_struct == "means") && any(constr_time_struct == "means")) {
        # Both dyadic and longitudinal constraints
        if (scaleset == "FF") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "equated_dy_long_ff")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "equated_dy_long")
        } else if (scaleset == "MV") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "equated_dy_long")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "equated_dy_long")
        }
    } else if (any(constr_dy_struct == "means")) {
        # Only dyadic constraints
        if (scaleset == "FF") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "equated_dy_ff")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "equated_dy")
        } else if (scaleset == "MV") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "equated_dy")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "equated_dy")
        }
    } else if (any(constr_time_struct == "means")) {
        # Only longitudinal constraints
        if (scaleset == "FF") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "fixed")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "fixed")
        } else if (scaleset == "MV") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "equated_long")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "equated_long")
        }
    } else if (!any(constr_dy_struct == "means") && !any(constr_time_struct == "means") && any(constr_dy_meas == "intercepts") && scaleset == "FF") {
        # Special case: no structural constraints on means but intercepts are constrained across dyad (FF scaleset)
        xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "fixed")
        xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "free")
    } else if (!any(constr_dy_struct == "means") && !any(constr_time_struct == "means") && any(constr_time_meas == "intercepts") && scaleset == "FF") {
        # Special case: no structural constraints on means but intercepts are constrained across time (FF scaleset)
        xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "equated_long_ff")
        xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "equated_long_ff")
    } else {
        # No constraints
        if (scaleset == "FF") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "fixed")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "fixed")
        } else if (scaleset == "MV") {
            xmean1 <- longLmeans(dvn, lvar = "X", partner = "1", type = "free")
            xmean2 <- longLmeans(dvn, lvar = "X", partner = "2", type = "free")
        }
    }

    # Script Creation Syntax
    # Helper functions return vectors, so we need to combine them
    loadings_section <- paste(c(xloads1, xloads2), collapse = "\n")
    intercepts_section <- paste(c(xints1, xints2), collapse = "\n")
    residuals_section <- paste(c(xres1, xres2), collapse = "\n")
    variances_section <- paste(c(xvar1, xvar2), collapse = "\n")
    covariances_section <- paste(c(xcovar_intraclass, xcovar_intraind, xcovar_interind), collapse = "\n")
    means_section <- paste(c(xmean1, xmean2), collapse = "\n")

    script <- paste("#Longitudinal Correlated Dyadic Factors Model",
        "",
        "#Measurement Model",
        "",
        "#Loadings",
        loadings_section,
        "",
        "#Intercepts",
        intercepts_section,
        "",
        "#Residual Variances",
        residuals_section,
        "",
        "#Residual Covariances",
        paste(xcoresids_dyad, collapse = "\n"),
        paste(xcoresids_time, collapse = "\n"),
        "",
        "#Structural Model",
        "",
        "#Latent Variances",
        variances_section,
        "",
        "#Latent Covariances",
        covariances_section,
        "",
        "#Latent Means",
        means_section,
        sep = "\n"
    )

    # Write script to file if requested
    if (!is.null(writeTo) || !is.null(fileName)) {
        # if there is a path or file name,
        # check for valid input,
        # and if valid, write script

        # checking for valid directory path and fileName
        if (!is.character(writeTo)) {
            stop("The `writeTo` argument must be a character string. \n Use writeTo = '.' to save script in the current working directory, for example.")
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
    } else if (is.null(writeTo) && is.null(fileName)) {
        # otherwise just return script
        return(script)
    }
}
