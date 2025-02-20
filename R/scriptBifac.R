#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Bifactor Dyadic Models
#'
#' This function takes the outputted object from `scrapeVarCross()`
#'  and automatically writes, returns, and exports (`.txt`) `lavaan` syntax
#'  for specifying dyadic configural, loading, intercept, and residual invariant
#'  bifactor models.
#'
#' @param dvn Input dvn list from `scrapeVarCross()`
#' @param scaleset Input character to specify how to set the scale of the latent variable. Default is
#'  `"FF"` (fixed-factor; see Details for rationale), but user can specify `"MV"` (Marker Variable)
#' @param lvname Input character to (arbitrarily) name the latent variable in `lavaan` syntax
#' @param constr_dy_meas Input character vector detailing which measurement model parameters to constrain across dyad members.
#' @param constr_dy_struct Input character vector detailing which structural model parameters to constrain across dyad members.
#' Default is `c("variances", "means")`(in combination with defaults for `constr_dy_meas`, an indistinguishable correlated dyadic factors model),
#' but user can specify any combination of `"variances"` and `"means"`, or `"none"`.
#' @param writeTo A character string specifying a directory path to where a `.txt` file of the resulting `lavaan` script should be written.
#'  If set to `“.”`, the `.txt` file will be written to the current working directory.
#'  The default is `NULL`, and examples use a temporary directory created by `tempdir()`.
#' @param fileName A character string specifying a desired base name for the `.txt` output file.
#'  The default is `NULL`. The specified name will be automatically appended with the `.txt` file extension.
#'  If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' @return Character object of `lavaan` script that can be passed immediately to
#'  `lavaan` functions.
#'
#' @details
#' * By default, many `dySEM::` functions (including `scriptBifac()`) default to
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
#' @seealso \code{\link{scrapeVarCross}} which this function relies on.
#' @family uni-construct script-writing functions
#' @export
#'
#' @examples
#' dvn <- scrapeVarCross(
#'   dat = commitmentQ,
#'   x_order = "spi",
#'   x_stem = "sat.g",
#'   x_delim1 = ".",
#'   x_delim2="_",
#'   distinguish_1="1",
#'   distinguish_2="2"
#'   )
#'
#' sat.indist.script <- scriptBifac(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat"
#'   )
#'
#' sat.lvars.script <- scriptBifac(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = "loadings",
#'   constr_dy_struct = "variances"
#'   )
#'
#' sat.resids.script <- scriptBifac(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dBiFac_residual"
#'   )
#'
#' sat.ints.script <- scriptBifac(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = c("loadings", "intercepts"),
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dBiFac_intercept"
#'   )
#'
#' sat.loads.script <- scriptBifac(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = c("loadings"),
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dBiFac_loading"
#'   )
#'
#' sat.config.script <- scriptBifac(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = "none",
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dBiFac_configural"
#'   )
#'
#' sat.source.script <- scriptBifac(
#'   dvn,
#'   scaleset = "FF",
#'   lvname = "Sat",
#'   constr_dy_meas = "loadings_source",
#'   constr_dy_struct = "none",
#'   writeTo = tempdir(),
#'   fileName = "dBiFac_source"
#'   )
scriptBifac <- function(
    dvn,
    scaleset = "FF",
    lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"),
    writeTo = NULL,
    fileName = NULL
){

  #check for valid inputs
  if(length(dvn)!=6){
    stop("You must supply a dvn object containing information for only X [i.e., your target LV]")
  }

  if(!scaleset %in% c("FF", "MV")){
    stop("scaleset must be either 'FF' (fixed-factor) or 'MV' (marker variable)")
  }

  if(!any(constr_dy_meas %in% c("loadings", "loadings_source", "intercepts", "residuals", "none"))){
    stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'")
  }

  if(!any(constr_dy_struct %in% c("variances", "means", "none"))){
    stop("constr_dy_struct must be a character vector containing any combination of 'variances', 'means', or 'none'")
  }

  if(scaleset == "FF"){


    #measurement model

    #loadings
    if(any(constr_dy_meas == "loadings")){

      xloadsg <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "equated"
      )

      xloads1 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "equated"
      )
      xloads2 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "equated"
      )
    }

    #boutique loading invariance models
    else if(any(constr_dy_meas == "loadings_source")){

      xloadsg <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "equated_source"
      )

      xloads1 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "equated_source"
      )
      xloads2 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "equated_source"
      )
    }

    else{

      xloadsg <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "free"
      )
      xloads1 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "free"
      )
      xloads2 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "free"
      )
    }

    #intercepts
    if(any(constr_dy_meas == "intercepts")){
      xints1 = intercepts(
        dvn,
        lvar = "X",
        partner="1",
        type = "equated"
      )
      xints2 = intercepts(
        dvn,
        lvar = "X",
        partner="2",
        type = "equated"
      )
    }

    else{
      xints1 <- intercepts(
        dvn,
        lvar = "X",
        partner="1",
        type = "free"
      )
      xints2 <- intercepts(
        dvn,
        lvar = "X",
        partner="2",
        type = "free"
      )
    }

    #residuals
    if(any(constr_dy_meas == "residuals")){

      xres1 <- resids(
        dvn,
        lvar = "X",
        partner="1",
        type = "equated"
      )
      xres2 <- resids(
        dvn,
        lvar = "X",
        partner="2",
        type = "equated"
      )

    }

    else{
      xres1 <- resids(
        dvn,
        lvar = "X",
        partner="1",
        type = "free"
      )
      xres2 <- resids(
        dvn,
        lvar = "X",
        partner="2",
        type = "free"
      )
    }

    #correlated residuals
    xcoresids <- coresids(
      dvn,
      lvar = "X",
      "free"
    )


    #structural model

    #latent variances
    if(any(constr_dy_struct == "variances")){
      xvarg  <- lvars(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "equated_ff"
      )
      xvar1 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "1",
        type = "equated"
      )
      xvar2 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "2",
        type = "equated"
      )
    }
    else if(!any(constr_dy_struct == "variances") &
            any(constr_dy_meas %in% c("loadings"))){
      xvarg  <- lvars(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "fixed"
      )
      xvar1 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "1",
        type = "free"
      )
      xvar2 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "2",
        type = "free"
      )
    }
    else{
      xvarg <- lvars(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "fixed"
      )
      xvar1 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "1",
        type = "fixed"
      )
      xvar2 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "2",
        type = "fixed"
      )
    }

    #orthogonal between bifactor and specific factors
    xcovargxx1 <- sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist1"]])
    xcovargxx2 <- sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist2"]])
    xcovarx1x2 <- sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])

    #latent means for X
    if(any(constr_dy_struct == "means")){
      xmeang <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="g",
        type = "equated_ff"
      )
      xmean1 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "equated"
      )
      xmean2 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "equated"
      )
    }
    else if(!any(constr_dy_struct == "means") & any(constr_dy_meas == "intercepts")){
      xmeang <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="g",
        type = "fixed"
      )
      xmean1 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "free"
      )
      xmean2 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "free"
      )
    }
    else{
      xmeang <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="g",
        type = "fixed"
      )
      xmean1 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "fixed"
      )
      xmean2 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "fixed"
      )
    }

    #Script Creation Syntax
    script <- sprintf("#Measurement Model\n\n#Loadings\n%s\n%s\n%s\n\n#Intercepts\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                      xloads1, xloads2, xloadsg,
                      xints1, xints2,
                      xres1, xres2, xcoresids,
                      xvar1, xvar2, xvarg, xcovargxx1, xcovargxx2, xcovarx1x2,
                      xmean1, xmean2, xmeang)

  }

  if(scaleset == "MV"){

    #loadings
    if(any(constr_dy_meas == "loadings") & any(constr_dy_struct == "variances")){
      xloadsg <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "equated"
      )
      xloads1 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "equated"
      )
      xloads2 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "equated"
      )
    }else if(any(constr_dy_meas == "loadings")){

      xloadsg <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "equated_mv"
      )
      xloads1 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "equated"
      )
      xloads2 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "equated"
      )
    }
    else {
      xloadsg <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "fixed"
      )

      xloads1 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "fixed"
      )
      xloads2 <- loads(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "fixed"
      )
    }

    #intercepts
    if(any(constr_dy_meas == "intercepts")){
      xints1 <- intercepts(
        dvn,
        lvar = "X",
        partner = "1",
        type = "equated_mv"
      )
      xints2 <- intercepts(
        dvn,
        lvar = "X",
        partner = "2",
        type = "equated"
      )
    }
    else {
      xints1 <- intercepts(
        dvn,
        lvar = "X",
        partner = "1",
        type = "fixed"
      )
      xints2 <- intercepts(
        dvn,
        lvar = "X",
        partner = "2",
        type = "fixed"
      )
    }

    #residuals
    if(any(constr_dy_meas == "residuals")){
      xres1 <- resids(
        dvn,
        lvar = "X",
        partner = "1",
        type = "equated"
      )
      xres2 <- resids(
        dvn,
        lvar = "X",
        partner = "2",
        type = "equated"
      )
    }
    else {
      xres1 <- resids(
        dvn,
        lvar = "X",
        partner = "1",
        type = "free"
      )
      xres2 <- resids(
        dvn,
        lvar = "X",
        partner = "2",
        type = "free"
      )
    }


    #correlated residuals for X
    xcoresids <- coresids(dvn, lvar = "X", "free")


    #latent variances
    if(any(constr_dy_struct == "variances")){
      xvarg  <- lvars(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "equated"
      )

      xvar1 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "1",
        type = "equated"
      )
      xvar2 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "2",
        type = "equated"
      )
    }

    else{
      xvarg <- lvars(
        dvn,
        lvar = "X",
        lvname,
        partner = "g",
        type = "free"
      )

      xvar1 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "1",
        type = "free"
      )
      xvar2 <- lvars(
        dvn,
        lvar = "X",
        lvname = lvname,
        partner = "2",
        type = "free"
      )
    }

    #orthogonal between bifactor and specific factors
    xcovargxx1 <- sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist1"]])
    xcovargxx2 <- sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist2"]])
    xcovarx1x2 <- sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])


    #latent means
    if(any(constr_dy_struct == "means")){
      xmeang <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="g",
        type = "equated"
      )

      xmean1 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "equated"
      )
      xmean2 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "equated"
      )
    }else if(any(constr_dy_meas == "intercepts")){
      xmeang <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="g",
        type = "free"
      )

      xmean1 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "free"
      )
      xmean2 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "free"
      )
    }
    else{
      xmeang <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="g",
        type = "fixed"
      )

      xmean1 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="1",
        type = "free"
      )
      xmean2 <- lmeans(
        dvn,
        lvar = "X",
        lvname,
        partner="2",
        type = "free"
      )
    }


    #Script Creation Syntax
    script <- sprintf("#Measurement Model\n\n#Loadings\n%s\n%s\n%s\n\n#Intercepts\n%s\n\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                      xloads1, xloads2, xloadsg,
                      xints1, xints2,
                      xres1, xres2, xcoresids,
                      xvar1, xvar2, xvarg, xcovargxx1, xcovargxx2, xcovarx1x2,
                      xmean1, xmean2, xmeang)

  }


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
