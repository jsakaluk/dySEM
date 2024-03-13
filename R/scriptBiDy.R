#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Bifactor Dyadic (BiDy) models
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' BiDy CFA (BiDy-C) or SEM (BiDy-S) Model. Currently only uses fixed-factor scale-setting
#' @param dvn input dvn list from scrapeVarCross
#' @param type input character to specify whether to script a BiDy-CFA ("CFA", default) or BiDy-SEM ("SEM") model
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param constr_dy_x_meas input character vector detailing which measurement model parameters to constrain across dyad members for latent X.
#' Default is c("loadings", "intercepts", "residuals"),
#' but user can specify any combination of "loadings", "intercepts", and "residuals",
#' #or "none" to specify an otherwise unconstrained dyadic configural invariance model. Users may also specify more
#' boutique patterns of bifactor loading constraints with "loadings_source" or "loadings_mutual".
#' @param constr_dy_x_struct input character vector detailing which structural model parameters to constrain across dyad members for latent X.
#' Default is c("variances", "means"),
#' but user can specify any combination of "variances" and "means", or "none".
#' @param constr_dy_y_meas input character vector detailing which measurement model parameters to constrain across dyad members for latent X.
#' Default is c("loadings", "intercepts", "residuals"),
#' but user can specify any combination of "loadings", "intercepts", and "residuals",
#' #or "none" to specify an otherwise unconstrained dyadic configural invariance model. Users may also specify more
#' boutique patterns of bifactor loading constraints with "loadings_source" or "loadings_mutual".
#' @param constr_dy_y_struct input character vector detailing which structural model parameters to constrain across dyad members for latent X.
#' Default is c("variances", "means"),
#' but user can specify any combination of "variances" and "means", or "none".
#' @param constr_dy_xy_struct input character vector detailing which structural model parameters to constrain for modeling the predictive association(s) between
#' partners' latent x and y. Default is c("actors"), but users can also specify "dyadic_zero" or "none".
#' @param model Deprecated input character used to specify which level of invariance is
#' modeled. Users should rely upon constr_dy_x_meas/constr_dy_y_meas and
#' constr_dy_x_struct/constr_dy_y_struct instead, for making constraints to the measurement and/or structural portions of the model for latent x and y.
#' @param equate Deprecated input character to specify which type of structural parameters
#' are constrained to equivalency between partners. Users should rely upon constr_dy_xy_struct for making
#' constraints to the structural portion of the model for associative relationship between latent x and y.
#' @param writeTo A character string specifying a directory path to where a .txt file of the resulting lavaan script should be written.
#' If set to “.”, the .txt file will be written to the current working directory.
#' The default is NULL, and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the .txt output file.
#' The default is NULL. The specified name will be automatically appended with the .txt file extension.
#' If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' @importFrom rlang .data
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(DRES, x_order = "sip", x_stem = "sexsat",
#' x_delim2=".", distinguish_1="1", distinguish_2="2")
#'
#' sexsat.bidyc.script <- scriptBiDy(dvn, lvxname = "SexSat", type = "CFA",
#' writeTo = tempdir(),
#' fileName = "BiDy_C")
#'
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2",
#' y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#'
#' comsat.bidys.config.script <- scriptBiDy(dvn, lvxname = "Sat",
#' lvyname = "Com", type = "SEM",
#' writeTo = tempdir(),
#' fileName = "BiDy_S")


scriptBiDy <- function(dvn, type = "CFA", lvxname, lvyname,
                      constr_dy_x_meas = c("loadings", "intercepts", "residuals"),
                      constr_dy_x_struct = c("variances", "means"),
                      constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
                      constr_dy_y_struct = c("variances", "means"),
                      constr_dy_xy_struct = c("actors"),
                      model = lifecycle::deprecated(), equate = lifecycle::deprecated(),
                      writeTo = NULL,
                      fileName = NULL){
  if(type == "CFA"){

    #stop if model is provided
    if(lifecycle::is_present(model)){
      lifecycle::deprecate_stop(
        when = "1.0.0",
        what = I('The argument "scriptBiDy(model)"'),
        with = I('"scriptBiDy(constr_dy_x_meas) and/or scriptBiDy(constr_dy_y_meas)"')
      )
    }

    #stop if equate is provided
    if(lifecycle::is_present(equate)){
      lifecycle::deprecate_stop(
        when = "1.0.0",
        what = I('The argument "scriptBiDy(equate)"'),
        with = I('"constr_dy_xy_struct"')
      )
    }

    #check for valid inputs
    if(length(dvn)!=6){
     stop("You must supply a dvn object containing information for only X [i.e., your target LV]")
    }

    if(!any(constr_dy_x_meas %in% c("loadings", "loading_source", "loading_mutual", "intercepts", "residuals", "none"))){
      stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'loading_source', 'loading_releq', 'intercepts', 'residuals', or 'none'")
    }

    if(!any(constr_dy_x_struct %in% c("variances", "means", "none"))){
      stop("constr_dy_x_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
    }

    #loadings for X
    if(any(constr_dy_x_meas == "loadings")){
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "equated")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "equated")
    }else if(any(constr_dy_x_meas == "loading_source")){
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated_source")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "equated_source")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "equated_source")
    }else if(any(constr_dy_x_meas == "loading_mutual")){
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "free")
    }else{
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "free")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "free")
    }

    #intercepts for X
    if(any(constr_dy_x_meas == "intercepts")){
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
    }else{
      xints1 <- intercepts(dvn, lvar = "X", partner="1", type = "free")
      xints2 <- intercepts(dvn, lvar = "X", partner="2", type = "free")
    }

    #residuals for X
    if(any(constr_dy_x_meas == "residuals")){
      xres1 <- resids(dvn, lvar = "X", partner="1", type = "equated")
      xres2 <- resids(dvn, lvar = "X", partner="2", type = "equated")
    }else{
      xres1 <- resids(dvn, lvar = "X", partner="1", type = "free")
      xres2 <- resids(dvn, lvar = "X", partner="2", type = "free")
    }

    #correlated residuals for X
    xcoresids <- coresids(dvn, lvar = "X", "free")

    #latent variances for X
    if(any(constr_dy_x_struct == "variances")){
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "equated_ff")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "equated")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "equated")
    }else if(!any(constr_dy_x_struct == "variances") &
             any(constr_dy_x_meas %in% c("loadings", "loadings_source"))){
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")
    }else if(!any(constr_dy_x_struct == "variances") & any(constr_dy_x_meas == "loadings_mutual")){
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "free")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")
    }else{
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")
    }

    #orthogonal between bifactor and specific factors
    xcovargxx1 <- sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
    xcovargxx2 <- sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
    xcovarx1x2 <- sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

    #latent means for X
    if(any(constr_dy_x_struct == "means")){
      xmeang <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "equated_ff")
      xmean1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "equated")
      xmean2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "equated")
    }else if(!any(constr_dy_x_struct == "means") & any(constr_dy_x_meas == "intercepts")){
      xmeang <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      xmean1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "free")
      xmean2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "free")
    }else{
      xmeang <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      xmean1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      xmean2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")
    }

    #Script Creation Syntax
    script <- sprintf("#Measurement Model\n\n#Loadings\n%s\n%s\n%s\n\n#Intercepts\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                      xloads1, xloads2, xloadsg,
                      xints1, xints2,
                      xres1, xres2, xcoresids,
                      xvar1, xvar2, xvarg, xcovargxx1, xcovargxx2, xcovarx1x2,
                      xmean1, xmean2, xmeang)

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
  else if(type == "SEM"){

    #stop if model is provided
    if (lifecycle::is_present(model)) {
      lifecycle::deprecate_stop(
        when = "1.0.0",
        what = I('The argument "scriptBiDy(model)"'),
        with = I('"scriptBiDy(constr_dy_x_meas) and/or scriptBiDy(constr_dy_y_meas)"')
      )
    }

    #stop if equate is provided
    if (lifecycle::is_present(equate)) {
      lifecycle::deprecate_stop(
        when = "1.0.0",
        what = I('The argument "scriptBiDy(equate)"'),
        with = I('"constr_dy_xy_struct"')
      )
    }

    #check for valid inputs
    if(length(dvn)!=9){
      stop("You must supply a dvn object containing information for both X and Y")
    }
    if(!any(constr_dy_x_meas %in% c("loadings", "loading_source", "loading_mutual", "intercepts", "residuals", "none"))){
      stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'loading_source', 'loading_releq', 'intercepts', 'residuals', or 'none'")
    }
    if(!any(constr_dy_x_struct %in% c("variances", "means", "none"))){
      stop("constr_dy_x_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
    }
    if(!any(constr_dy_y_meas %in% c("loadings", "loading_source", "loading_mutual", "intercepts", "residuals", "none"))){
      stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'loading_source', 'loading_releq', 'intercepts', 'residuals', or 'none'")
    }
    if(!any(constr_dy_y_struct %in% c("variances", "means", "none"))){
      stop("constr_dy_y_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
    }
    if(!any(constr_dy_xy_struct %in% c("actors", "actors_zero", "dyadic_zero", "none"))){
      stop("constr_dy_xy_struct must be a character vector containing any combination of 'actors', 'partners', 'all', 'actors_zero', 'partners_zero' or 'none'")
    }

    #loadings for X
    if(any(constr_dy_x_meas == "loadings")){
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "equated")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "equated")
    }
    else if(any(constr_dy_x_meas == "loading_source")){
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated_source")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "equated_source")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "equated_source")
    }else if(any(constr_dy_x_meas == "loading_mutual")){
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "free")
    }else{
      xloadsg <- loads(dvn, lvar = "X", lvxname, partner = "g", type = "free")
      xloads1 <- loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      xloads2 <- loads(dvn, lvar = "X",lvxname, partner="2", type = "free")
    }

    #loadings for Y
    if(any(constr_dy_y_meas == "loadings")){
      yloadsg <- loads(dvn, lvar = "Y", lvyname, partner = "g", type = "equated")
      yloads1 <- loads(dvn, lvar = "Y",lvyname, partner="1", type = "equated")
      yloads2 <- loads(dvn, lvar = "Y",lvyname, partner="2", type = "equated")
    }
    else if(any(constr_dy_y_meas == "loading_source")){
      yloadsg <- loads(dvn, lvar = "Y", lvyname, partner = "g", type = "equated_source")
      yloads1 <- loads(dvn, lvar = "Y",lvyname, partner="1", type = "equated_source")
      yloads2 <- loads(dvn, lvar = "Y",lvyname, partner="2", type = "equated_source")
    }else if(any(constr_dy_y_meas == "loading_mutual")){
      yloadsg <- loads(dvn, lvar = "Y", lvyname, partner = "g", type = "equated")
      yloads1 <- loads(dvn, lvar = "Y",lvyname, partner="1", type = "free")
      yloads2 <- loads(dvn, lvar = "Y",lvyname, partner="2", type = "free")
    }else{
      yloadsg <- loads(dvn, lvar = "Y", lvyname, partner = "g", type = "free")
      yloads1 <- loads(dvn, lvar = "Y",lvyname, partner="1", type = "free")
      yloads2 <- loads(dvn, lvar = "Y",lvyname, partner="2", type = "free")
    }

    #intercepts for X
    if(any(constr_dy_x_meas == "intercepts")){
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
    }else{
      xints1 <- intercepts(dvn, lvar = "X", partner="1", type = "free")
      xints2 <- intercepts(dvn, lvar = "X", partner="2", type = "free")
    }

    #intercepts for Y
    if(any(constr_dy_y_meas == "intercepts")){
      yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated")
      yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
    }else{
      yints1 <- intercepts(dvn, lvar = "Y", partner="1", type = "free")
      yints2 <- intercepts(dvn, lvar = "Y", partner="2", type = "free")
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

    #correlated residuals for X
    ycoresids <- coresids(dvn, lvar = "Y", "free")

    #latent variances for X
    if(any(constr_dy_x_struct == "variances")){
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "equated_ff")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "equated")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "equated")
    }else if(!any(constr_dy_x_struct == "variances") &
             any(constr_dy_x_meas %in% c("loadings", "loadings_source"))){
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")
    }else if(!any(constr_dy_x_struct == "variances") & any(constr_dy_x_meas == "loadings_mutual")){
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "free")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")
    }else{
      xvarg  <- lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      xvar1 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
      xvar2 <- lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")
    }

    #latent variances for Y
    if(any(constr_dy_y_struct == "variances")){
      yvarg  <- lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "equated_ff")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "equated")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "equated")
    }else if(!any(constr_dy_y_struct == "variances") &
             any(constr_dy_y_meas %in% c("loadings", "loadings_source"))){
      yvarg  <- lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "fixed")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
    }else if(!any(constr_dy_y_struct == "variances") & any(constr_dy_y_meas == "loadings_mutual")){
      yvarg  <- lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "free")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
    }else{
      yvarg  <- lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "fixed")
      yvar1 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
      yvar2 <- lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
    }

    #orthogonal between bifactor and specific factors for X
    xcovargxx1 <- sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
    xcovargxx2 <- sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
    xcovarx1x2 <- sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])


    #orthogonal between bifactor and specific factors for Y
    ycovargxx1 <- sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist1"]])
    ycovargxx2 <- sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist2"]])
    ycovarx1x2 <- sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

    #latent means for X
    if(any(constr_dy_x_struct == "means")){
      xmeang <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "equated_ff")
      xmean1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "equated")
      xmean2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "equated")
    }else if(!any(constr_dy_x_struct == "means") & any(constr_dy_x_meas == "intercepts")){
      xmeang <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      xmean1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "free")
      xmean2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "free")
    }else{
      xmeang <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      xmean1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      xmean2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")
    }

    #latent means for Y
    if(any(constr_dy_y_struct == "means")){
      ymeang <- lmeans(dvn, lvar = "Y", lvyname, partner="g", type = "equated_ff")
      ymean1 <- lmeans(dvn, lvar = "Y", lvyname, partner="1", type = "equated")
      ymean2 <- lmeans(dvn, lvar = "Y", lvyname, partner="2", type = "equated")
    }else if(!any(constr_dy_y_struct == "means") & any(constr_dy_y_meas == "intercepts")){
      ymeang <- lmeans(dvn, lvar = "Y", lvyname, partner="g", type = "fixed")
      ymean1 <- lmeans(dvn, lvar = "Y", lvyname, partner="1", type = "free")
      ymean2 <- lmeans(dvn, lvar = "Y", lvyname, partner="2", type = "free")
    }else{
      ymeang <- lmeans(dvn, lvar = "Y", lvyname, partner="g", type = "fixed")
      ymean1 <- lmeans(dvn, lvar = "Y", lvyname, partner="1", type = "fixed")
      ymean2 <- lmeans(dvn, lvar = "Y", lvyname, partner="2", type = "fixed")
    }

    if(any(constr_dy_xy_struct == "actors")){
      actor1 <- sprintf("%s%s ~ a1*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      actor2 <- sprintf("%s%s ~ a1*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
    }else if(any(constr_dy_xy_struct == "actor_zero")){
      actor1 <- sprintf("%s%s ~ 0*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      actor2 <- sprintf("%s%s ~ 0*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
    }else{
      actor1 <- sprintf("%s%s ~~ a1*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      actor2 <- sprintf("%s%s ~~ a2*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
    }

    if(any(constr_dy_xy_struct == "dyadic_zero")){
      dyadic <- sprintf("%sDy ~ 0*%sDy",lvyname,lvxname)
    }else{
      dyadic <- sprintf("%sDy ~ %sDy",lvyname,lvxname)
    }
    #Script Creation Syntax
    script <- sprintf("#Measurement Model\n\n#Loadings\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Intercepts\n%s\n\n%s\n\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n%s\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Latent Actor Effects\n%s\n%s\n\n#Latent Dyadic Effect\n%s\n\n",
                      xloads1, xloads2, xloadsg,
                      yloads1, yloads2, yloadsg,
                      xints1, xints2,
                      yints1, yints2,
                      xres1, xres2,  yres1, yres2,
                      xcoresids, ycoresids,
                      xvar1, xvar2, xvarg,
                      yvar1, yvar2, yvarg,
                      xcovargxx1, xcovargxx2, xcovarx1x2,
                      ycovargxx1, ycovargxx2, ycovarx1x2,
                      xmean1, xmean2, xmeang,
                      ymean1, ymean2, ymeang,
                      actor1, actor2, dyadic)

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

}
