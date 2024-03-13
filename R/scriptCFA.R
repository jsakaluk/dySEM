#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Dyadic Confirmatory Factor Analysis (CFA) Models
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' measurement models for either a specified X or Y factor.
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
#' @param dvn input dvn list from scrapeVarCross
#' @param scaleset input character to specify how to set the scale of the latent variable(s). Default is
#' "FF" (fixed-factor; see Details for rationale), but user can specify "MV" (Marker Variable)
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param constr_dy_meas input character vector detailing which measurement model parameters to constrain across dyad members.
#' Default is c("loadings", "intercepts", "residuals")(in combination with defaults for constr_dy_struct, an indistinguishable dyadic CFA),
#' but user can specify any combination of "loadings", "intercepts", and "residuals",
#' #or "none" to specify an otherwise unconstrained dyadic configural invariance model
#' @param constr_dy_struct input character vector detailing which structural model parameters to constrain across dyad members.
#' Default is c("variances", "means")(in combination with defaults for constr_dy_meas, an indistinguishable dyadic CFA),
#' but user can specify any combination of "variances" and "means", or "none".
#' @param model Depreceated input character used to specify which level of invariance is
#' modeled ("configural", "loading", "intercept", "residual", or "indist"). Users should rely upon constr_dy_meas and
#' constr_dy_struct instead, for making constraints to the measurement and/or structural portions of the model.
#' @param writeTo A character string specifying a directory path to where a .txt file of the resulting lavaan script should be written.
#' If set to “.”, the .txt file will be written to the current working directory.
#' The default is NULL, and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the .txt output file.
#' The default is NULL. The specified name will be automatically appended with the .txt file extension.
#' If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{scrapeVarCross}} which this function relies on
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")
#'
#' sat.indist.script <- scriptCFA(dvn, lvname = "Sat")
#'
#' sat.lvars.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = "loadings",
#' constr_dy_struct = "variances")
#'
#' sat.resids.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = c("loadings", "intercepts", "residuals"),
#' constr_dy_struct = "none",
#' writeTo = tempdir(),
#' fileName = "dCFA_residual")
#'
#' sat.ints.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = c("loadings", "intercepts"),
#' constr_dy_struct = "none",
#' writeTo = tempdir(),
#' fileName = "dCFA_intercept")
#'
#' sat.loads.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = c("loadings"),
#' constr_dy_struct = "none",
#' writeTo = tempdir(),
#' fileName = "dCFA_loading")
#'
#' sat.config.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = "none",
#' constr_dy_struct = "none",
#' writeTo = tempdir(),
#' fileName = "dCFA_configural")

scriptCFA <- function(dvn, scaleset = "FF", lvname = "X",
                      constr_dy_meas = c("loadings", "intercepts", "residuals"),
                      constr_dy_struct = c("variances", "means"),
                      model = lifecycle::deprecated(),
                      writeTo = NULL,
                      fileName = NULL){

  if (lifecycle::is_present(model)) {
    lifecycle::deprecate_stop("1.0.0", "scriptCFA(model)", "scriptCFA(constr_dy_meas)")
  }

  #check for valid inputs
  if(length(dvn)!=6){
    stop("You must supply a dvn object containing information for only X [i.e., your target LV]")
  }

  if(!any(constr_dy_meas %in% c("loadings", "intercepts", "residuals", "none"))){
    stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'")
  }

  if(!any(constr_dy_struct %in% c("variances", "means", "none"))){
    stop("constr_dy_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
  }

  if(!scaleset %in% c("FF", "MV")){
    stop("scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
  }

  #loadings
  if(any(constr_dy_meas == "loadings")){
    if(scaleset == "FF"){
      xloads1 <- loads(dvn, lvar = "X", lvname, partner="1", type = "equated")
      xloads2 <- loads(dvn, lvar = "X", lvname, partner="2", type = "equated")
    }
    else if(scaleset == "MV"){
      xloads1 <- loads(dvn, lvar = "X", lvname, partner="1", type = "equated_mv")
      xloads2 <- loads(dvn, lvar = "X", lvname, partner="2", type = "equated")
    }
  }else{
    if(scaleset == "FF"){

      xloads1 <- loads(dvn, lvar = "X", lvname, partner="1", type = "free")
      xloads2 <- loads(dvn, lvar = "X", lvname, partner="2", type = "free")
    }
    else if(scaleset == "MV"){
      xloads1 <- loads(dvn, lvar = "X", lvname, partner="1", type = "fixed")
      xloads2 <- loads(dvn, lvar = "X", lvname, partner="2", type = "fixed")
    }
  }

  #intercepts
  if(any(constr_dy_meas == "intercepts")){
    if(scaleset == "FF"){
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
    }else if(scaleset == "MV"){
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated_mv")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
    }
  }else{
    if(scaleset == "FF"){
      xints1 <- intercepts(dvn, lvar = "X", partner="1", type = "free")
      xints2 <- intercepts(dvn, lvar = "X", partner="2", type = "free")
    }
    else if(scaleset == "MV"){
      xints1 <- intercepts(dvn, lvar = "X", partner="1", type = "fixed")
      xints2 <- intercepts(dvn, lvar = "X", partner="2", type = "fixed")
    }
  }

  #residual variances
  if(any(constr_dy_meas == "residuals")){
    xres1 <- resids(dvn, lvar = "X", partner="1", type = "equated")
    xres2 <- resids(dvn, lvar = "X", partner="2", type = "equated")
  }else{
    #Residual variances
    xres1 <- resids(dvn, lvar = "X", partner="1", type = "free")
    xres2 <- resids(dvn, lvar = "X", partner="2", type = "free")
  }

  #Correlated residuals
  xcoresids <- coresids(dvn, lvar = "X", "free")

  #latent variances
  if(any(constr_dy_struct == "variances")){
    if(scaleset == "FF"){
      xvar1 <- lvars(dvn, lvar = "X", lvname, partner = "1", type = "equated_ff")
      xvar2 <- lvars(dvn, lvar = "X", lvname, partner = "2", type = "equated")
    }else if(scaleset == "MV"){
      xvar1 <- lvars(dvn, lvar = "X", lvname, partner = "1", type = "equated")
      xvar2 <- lvars(dvn, lvar = "X", lvname, partner = "2", type = "equated")
    }
  }else if(!any(constr_dy_struct == "variances") & any(constr_dy_meas == "loadings") & scaleset == "FF"){
    xvar1 = lvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
    xvar2 = lvars(dvn, lvar = "X", lvname, partner = "2", type = "free")
  }else{
    if(scaleset == "FF"){
      xvar1 <- lvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
      xvar2 <- lvars(dvn, lvar = "X", lvname, partner = "2", type = "fixed")
    }
    else if(scaleset == "MV"){
      xvar1 <- lvars(dvn, lvar = "X", lvname, partner = "1", type = "free")
      xvar2 <- lvars(dvn, lvar = "X", lvname, partner = "2", type = "free")
    }
  }

  #latent covariance
  if(any(constr_dy_struct == "orthogonal")){
    xcovar <- lcovars(dvn, lvname, type = "zero")
  }else{
    xcovar <- lcovars(dvn, lvname, type = "free")
  }

  #latent means
  if(any(constr_dy_struct == "means")){
    if(scaleset == "FF"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "equated_ff")
      xmean2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "equated")
    }else if(scaleset == "MV"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "equated")
      xmean2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "equated")
    }
  }else if(!any(constr_dy_struct == "means") & any(constr_dy_meas == "intercepts") & scaleset == "FF"){
    xmean1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "fixed")
    xmean2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "free")
  }else{
    if(scaleset == "FF"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "fixed")
      xmean2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "fixed")
    }
    else if(scaleset == "MV"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "free")
      xmean2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "free")
    }
  }

  #Script Creation Syntax
  script <- sprintf("#Measurement Model\n\n#Loadings\n%s\n%s\n\n#Intercepts\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n#Structural Model\n\n#Latent (Co)Variances\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s",
                    xloads1, xloads2,
                    xints1, xints2,
                    xres1, xres2, xcoresids,
                    xvar1, xvar2, xcovar,
                    xmean1, xmean2)

  #Write script to file if requested

  if(!is.null(writeTo) | !is.null(fileName)){
    #if there is a path or file name,
    #check for valid input,
    #and if valid, write script


    # checking for valid directory path and fileName
    if (!is.character(writeTo)){
      stop("The `writeout` argument must be a character string. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!dir.exists(writeTo)){
      stop("The specified directory does not exist. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!is.character(fileName)){
      stop("The `fileName` argument must be a character string.")
    }

    #write file
    cat(script, "\n",
        file = sprintf("%s/%s.txt",
                       writeTo,
                       fileName))

    return(script)

  }else if(is.null(writeTo) & is.null(fileName)){
    #otherwise just return script
    return(script)
  }

}

