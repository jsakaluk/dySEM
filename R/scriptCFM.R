#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Common Fate Models (CFMs)
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying Common Fate Models (CFMs). Users can
#' also invoke configural, loading, and/or intercept invariant
#' measurement models, and particular types of structural comparisons.
#' @param dvn input dvn list from scrapeVarCross
#' @param scaleset input character to specify how to set the scale of the latent variable(s). Default is
#' "FF" (fixed-factor; see Details for rationale), but user can specify "MV" (Marker Variable)
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param constr_dy_x_meas input character vector detailing which measurement model parameters to constrain across dyad members for latent X.
#' Default is c("loadings", "intercepts", "residuals"),
#' but user can specify any combination of "loadings", "intercepts", and "residuals",
#' #or "none" to specify an otherwise unconstrained dyadic configural invariance model
#' @param constr_dy_x_struct input character vector detailing which structural model parameters to constrain across dyad members for latent X.
#' Default is c("variances", "means"),
#' but user can specify any combination of "variances" and "means", or "none".
#' @param constr_dy_y_meas input character vector detailing which measurement model parameters to constrain across dyad members for latent X.
#' Default is c("loadings", "intercepts", "residuals"),
#' but user can specify any combination of "loadings", "intercepts", and "residuals",
#' #or "none" to specify an otherwise unconstrained dyadic configural invariance model
#' @param constr_dy_y_struct input character vector detailing which structural model parameters to constrain across dyad members for latent X.
#' Default is c("variances", "means"),
#' but user can specify any combination of "variances" and "means", or "none".
#' @param constr_dy_xy_struct input character vector detailing which structural model parameters to constrain for modeling the predictive association(s) between
#' partners' latent x and y. Defaults to "none". Options include "p1_zero" or "p2_zero" (to constrain within-person  latent residual covariances between X and Y to zero), or
#' "covar_zero" (to constrain both within-person latent residual correlations to zero), and/or "dyadic_zero" (to constrain the dyadic effect to zero).
#' @param model Deprecated input character used to specify which level of invariance is
#' modeled. Users should rely upon constr_dy_x_meas/constr_dy_y_meas and
#' constr_dy_x_struct/constr_dy_y_struct instead, for making constraints to the measurement and/or structural portions of the model for latent x and y.
#' @param writeTo A character string specifying a directory path to where a .txt file of the resulting lavaan script should be written.
#' If set to “.”, the .txt file will be written to the current working directory.
#' The default is NULL, and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the .txt output file.
#' The default is NULL. The specified name will be automatically appended with the .txt file extension.
#' If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions. Users will receive message if structural comparisons are specified
#' when the recommended level of invariance is not also specified. If user supplies dvn
#' with containing X or Y variables, they are alerted to respecify the dvn object.
#' @seealso \code{\link{scrapeVarCross}} which this function relies on
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2",
#' y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#' cfm.script.indist <-  scriptCFM(dvn, lvxname = "Sat", lvyname = "Com",
#' writeTo = tempdir(),
#' fileName = "CFM_indist")


scriptCFM  <- function(dvn, scaleset = "FF",
                     lvxname, lvyname,
                     constr_dy_x_meas = c("loadings", "intercepts", "residuals"),
                     constr_dy_x_struct = c("variances", "means"),
                     constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
                     constr_dy_y_struct = c("variances", "means"),
                     constr_dy_xy_struct = "none",
                     model = lifecycle::deprecated(),
                     writeTo = NULL,
                     fileName = NULL){

  #stop if model is provided
  if (lifecycle::is_present(model)) {
    lifecycle::deprecate_stop(
      when = "1.0.0",
      what = I('The argument "scriptCFM(model)"'),
      with = I('"scriptCFM(constr_dy_x_meas) and/or scriptCFM(constr_dy_y_meas)"')
    )
  }

  #check for valid inputs
  if(length(dvn)!=9){
    stop("You must supply a dvn object containing information for both X and Y")
  }
  if(!any(constr_dy_x_meas %in% c("loadings", "intercepts", "residuals", "none"))){
    stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'")
  }
  if(!any(constr_dy_x_struct %in% c("variances", "means", "none"))){
    stop("constr_dy_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
  }
  if(!any(constr_dy_y_meas %in% c("loadings", "intercepts", "residuals", "none"))){
    stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'")
  }
  if(!any(constr_dy_y_struct %in% c("variances", "means", "none"))){
    stop("constr_dy_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
  }
  if(!any(constr_dy_xy_struct %in% c("p1_zero", "p2_zero", "cov_zero", "dyadic_zero", "none"))){
    stop("constr_dy_xy_struct must be a character vector containing any combination of 'p1_zero', 'p2_zero', 'cov_zero', 'dyadic_zero', or 'none'")
  }
  if(!scaleset %in% c("FF", "MV")){
    stop("scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
  }

  #loadings for X
  if(any(constr_dy_x_meas == "loadings")){
    if(scaleset == "FF"){
      xloads1 <- loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
      xloads2 <- loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")
    }
    else if(scaleset == "MV"){
      xloads1 <- loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated_mv")
      xloads2 <- loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")
    }
  }else{
    if(scaleset == "FF"){
      xloads1 <- loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
      xloads2 <- loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
    }
    else if(scaleset == "MV"){
      xloads1 <- loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
      xloads2 <- loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")
    }
  }

  #loadings for Y
  if(any(constr_dy_y_meas == "loadings")){
    if(scaleset == "FF"){
      yloads1 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
      yloads2 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
    }
    else if(scaleset == "MV"){
      yloads1 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated_mv")
      yloads2 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
    }
  }else{
    if(scaleset == "FF"){

      yloads1 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
      yloads2 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
    }
    else if(scaleset == "MV"){
      yloads1 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
      yloads2 <- loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
    }
  }

  #Common Fate Loadings
  if(scaleset == "FF"){
    hierloads <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "equated")

  }else if(scaleset == "MV"){
    hierloads <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "fixed")
  }

  #intercepts for X
  if(any(constr_dy_x_meas == "intercepts")){
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
  #intercepts for Y
  if(any(constr_dy_y_meas == "intercepts")){
    if(scaleset == "FF"){
      yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated")
      yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
    }else if(scaleset == "MV"){
      yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated_mv")
      yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
    }
  }else{
    if(scaleset == "FF"){
      yints1 <- intercepts(dvn, lvar = "Y", partner="1", type = "free")
      yints2 <- intercepts(dvn, lvar = "Y", partner="2", type = "free")
    }
    else if(scaleset == "MV"){
      yints1 <- intercepts(dvn, lvar = "Y", partner="1", type = "fixed")
      yints2 <- intercepts(dvn, lvar = "Y", partner="2", type = "fixed")
    }
  }

  #residuals for X
  if(any(constr_dy_x_meas == "residuals")){
    xres1 <- resids(dvn, lvar = "X", partner="1", type = "equated")
    xres2 <- resids(dvn, lvar = "X", partner="2", type = "equated")
  }else{
    xres1 <- resids(dvn, lvar = "X", partner="1", type = "free")
    xres2 <- resids(dvn, lvar = "X", partner="2", type = "free")
  }

  #residuals for Y
  if(any(constr_dy_y_meas == "residuals")){
    yres1 <- resids(dvn, lvar = "Y", partner="1", type = "equated")
    yres2 <- resids(dvn, lvar = "Y", partner="2", type = "equated")
  }else{
    yres1 <- resids(dvn, lvar = "Y", partner="1", type = "free")
    yres2 <- resids(dvn, lvar = "Y", partner="2", type = "free")
  }

  #correlated residuals for X
  xcoresids <- coresids(dvn, lvar = "X", "free")

  #correlated residuals for Y
  ycoresids <- coresids(dvn, lvar = "Y", "free")


  #latent variances for X
  if(any(constr_dy_x_struct == "variances")){
    if(scaleset == "FF"){
      hierxvar <- cfvars(lvname = lvxname, type = "fixed")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "equated")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "equated")
    }else if(scaleset == "MV"){
      hierxvar <- cfvars(lvname = lvxname, type = "free")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "equated")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "equated")
    }
  }else if(!any(constr_dy_x_struct == "variances") & any(constr_dy_x_meas == "loadings") & scaleset == "FF"){
    hierxvar <- cfvars(lvname = lvxname, type = "fixed")
    xvar1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
    xvar2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")
  }else{
    if(scaleset == "FF"){
      hierxvar <- cfvars(lvname = lvxname, type = "fixed")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")
    }else if(scaleset == "MV"){
      hierxvar <- cfvars(lvname = lvxname, type = "free")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")
    }
  }
  #latent variances for Y
  if(any(constr_dy_y_struct == "variances")){
    if(scaleset == "FF"){
      hieryvar <- cfvars(lvname = lvyname, type = "fixed")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "equated")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "equated")
    }else if(scaleset == "MV"){
      hieryvar <- cfvars(lvname = lvyname, type = "free")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "equated")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "equated")
    }
  }else if(!any(constr_dy_y_struct == "variances") & any(constr_dy_y_meas == "loadings") & scaleset == "FF"){
    hieryvar <- cfvars(lvname = lvyname, type = "fixed")
    yvar1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
    yvar2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
  }else{
    if(scaleset == "FF"){
      hieryvar <- cfvars(lvname = lvyname, type = "fixed")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
    }else if(scaleset == "MV"){
      hieryvar <- cfvars(lvname = lvyname, type = "free")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
    }
  }

  #hierarchical means
  if(scaleset == "FF"){
    xmean <- cfmeans(lvname = lvxname, type = "fixed")
    ymean <- cfmeans(lvname = lvyname, type = "fixed")

  }else if(scaleset == "MV"){
    xmean <- cfmeans(lvname = lvxname, type = "free")
    ymean <- cfmeans(lvname = lvyname, type = "free")
  }

  #latent means for X
  if(any(constr_dy_x_struct == "means")){
    if(scaleset == "FF"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated_ff")
      xmean2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")
    }else if(scaleset == "MV"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated_ff")
      xmean2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")
    }
  }else if(!any(constr_dy_x_struct == "means") & any(constr_dy_x_meas == "intercepts") & scaleset == "FF"){
    xmean1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
    xmean2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
  }else{
    if(scaleset == "FF"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
      xmean2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")
    }
    else if(scaleset == "MV"){
      xmean1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
      xmean2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
    }
  }
  #latent means for Y
  if(any(constr_dy_y_struct == "means")){
    if(scaleset == "FF"){
      ymean1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated_ff")
      ymean2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
    }else if(scaleset == "MV"){
      ymean1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated_ff")
      ymean2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
    }
  }else if(!any(constr_dy_y_struct == "means") & any(constr_dy_y_meas == "intercepts") & scaleset == "FF"){
    ymean1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
    ymean2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
  }else{
    if(scaleset == "FF"){
      ymean1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
      ymean2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
    }
    else if(scaleset == "MV"){
      ymean1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
      ymean2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
    }
  }

  #Latent Covariances
  if(any(constr_dy_xy_struct == "p1_zero")){
    p1covar = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
    p2covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
  }else{
    p1covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
    p2covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
  }

  if(any(constr_dy_xy_struct == "p2_zero")){
    p1covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
    p2covar = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
  }else{
    p1covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
    p2covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
  }

  if(any(constr_dy_xy_struct == "cov_zero")){
    p1covar = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
    p2covar = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
  }else{
    p1covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
    p2covar = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
  }

  if(any(constr_dy_xy_struct == "dyadic_zero")){
    dyadic <- paste0(lvyname, " ~ 0*", lvxname)
  }else{
    dyadic <- paste(lvyname, "~", lvxname)
  }

  #write script
  script <- sprintf("#Measurement Model\n\n#Loadings\n%s\n%s\n\n%s\n%s\n\n%s\n\n#Intercepts\n%s\n\n%s\n\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n%s\n\n#Structural Model\n\n#Latent (Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Latent Dyadic Effect\n%s",
                    xloads1, xloads2,
                    yloads1, yloads2,
                    hierloads,
                    xints1, xints2,
                    yints1, yints2,
                    xres1, xres2,
                    yres1, yres2,
                    xcoresids, ycoresids,
                    hierxvar, xvar1, xvar2,
                    hieryvar, yvar1, yvar2,
                    p1covar, p2covar,
                    xmean, xmean1, xmean2,
                    ymean, ymean1, ymean2,
                    dyadic)

  #Write script to file if requested
  if(!is.null(writeTo) | !is.null(fileName) ){
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
  }
  else if(is.null(writeTo) & is.null(fileName)){
    #otherwise just return script
    return(script)
  }

}
