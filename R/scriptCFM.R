#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Common Fate Models
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying Common Fate Models (CFMs). Users can
#' also invoke configural, loading, and/or intercept invariant
#' measurement models, and particular types of structural comparisons.
#' @param dvn input dvn list from scrapeVarCross
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param model input character used to specify which level of invariance is
#' modeled. Defaults to "configural"
#' @param scaleset input character to specify how to set the scale of the latent variable(s). Default is
#' "FF" (fixed-factor; see Details for rationale), but user can specify "MV" (Marker Variable)
#' @param writescript input logical (default FALSE) for whether lavaan script should
#' be concatenated and written to current working directory (in subdirectory "scripts")
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions. Users will receive message if structural comparisons are specified
#' when the recommended level of invariance is not also specified. If user supplies dvn
#' with containing X or Y variables, they are alerted to respecify the dvn object.
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_", x_delim2=".", distinguish_1="1", distinguish_2="2",
#' y_order="sip", y_stem="sexsat", y_delim2=".")
#' cfm.script.config  <-  scriptCFM(dvn, lvxname = "Quality", lvyname = "SexSat", model = "configural", scaleset = "FF")
#' cfm.script.load  <-  scriptCFM(dvn, lvxname = "Quality", lvyname = "SexSat", model = "loading", scaleset = "FF")
#' cfm.script.int  <-  scriptCFM(dvn, lvxname = "Quality", lvyname = "SexSat", model = "intercept", scaleset = "FF")
#' cfm.script.ind  <-  scriptCFM(dvn, lvxname = "Quality", lvyname = "SexSat", model = "indist", scaleset = "FF")

scriptCFM = function(dvn, lvxname, lvyname, model = "indist", scaleset = "FF",
                     writescript = FALSE){
  dirs("scripts")
  if(length(dvn)==9){
    if(model == "configural"){
      #Loadings
      if(scaleset == "FF"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }else if(scaleset == "MV"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }

      #Common Fate Loadings
      if(scaleset == "FF"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "equated")

      }else if(scaleset == "MV"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "fixed")
      }

      #Latent Variances
      if(scaleset == "FF"){
        psi_x <- cfvars(lvname = lvxname, type = "fixed")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")

        psi_y <- cfvars(lvname = lvyname, type = "fixed")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
      }else if(scaleset == "MV"){
        psi_x <- cfvars(lvname = lvxname, type = "free")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y <- cfvars(lvname = lvyname, type = "free")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1y1 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[[3]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[[8]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids.y = paste(resids.y, collapse = "\n")

      #Residual variances
      resx1 = resids(dvn, lvar = "X", partner="1", type = "free")
      resx2 = resids(dvn, lvar = "X", partner="2", type = "free")
      resy1 = resids(dvn, lvar = "Y", partner="1", type = "free")
      resy2 = resids(dvn, lvar = "Y", partner="2", type = "free")

      #Intercepts
      if(scaleset == "FF"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "free")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "free")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "free")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "free")
      }else if(scaleset == "MV"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "fixed")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "fixed")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "fixed")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "fixed")
      }

      #Latent Means
      if(scaleset == "FF"){
        alpha_x <- cfmeans(lvname = lvxname, type = "fixed")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")

        alpha_y <- cfmeans(lvname = lvyname, type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }else if(scaleset == "MV"){
        alpha_x <- cfmeans(lvname = lvxname, type = "free")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        alpha_y <- cfmeans(lvname = lvyname, type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Script Creation Syntax

        configural.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                                    eta.x1, eta.x2, eta.c, eta.y1,eta.y2,
                                    psi_x1, psi_x2,psi_x, psi_y1, psi_y2,psi_y, psi_x1y1, psi_x2y2,
                                    resx1, resx2, resy1, resy2, resids.x, resids.y,
                                    xints1, xints2, yints1, yints2,
                                    alpha_x1, alpha_x2,alpha_x, alpha_y1, alpha_y2,alpha_y)

      if(isTRUE(writescript)){
        cat(configural.script,"\n", file = sprintf("./scripts/%s_%s_cfm_configural.txt",lvyname,lvxname))
      }
      return(configural.script)
    }
    else if (model == "loading"){
      #Loadings
      if(scaleset == "FF"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }else if(scaleset == "MV"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated_mv")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated_mv")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Common Fate Loadings
      if(scaleset == "FF"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "equated")

      }else if(scaleset == "MV"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "fixed")
      }

      #Latent Variances
      if(scaleset == "FF"){
        psi_x <- cfvars(lvname = lvxname, type = "fixed")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y <- cfvars(lvname = lvyname, type = "fixed")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }else if(scaleset == "MV"){
        psi_x <- cfvars(lvname = lvxname, type = "free")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y <- cfvars(lvname = lvyname, type = "free")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1y1 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[[3]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[[8]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids.y = paste(resids.y, collapse = "\n")

      #Residual variances
      resx1 = resids(dvn, lvar = "X", partner="1", type = "free")
      resx2 = resids(dvn, lvar = "X", partner="2", type = "free")
      resy1 = resids(dvn, lvar = "Y", partner="1", type = "free")
      resy2 = resids(dvn, lvar = "Y", partner="2", type = "free")

      #Intercepts
      if(scaleset == "FF"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "free")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "free")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "free")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "free")
      }else if(scaleset == "MV"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "fixed")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "fixed")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "fixed")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "fixed")
      }

      #Latent Means
      if(scaleset == "FF"){
        alpha_x <- cfmeans(lvname = lvxname, type = "fixed")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")

        alpha_y <- cfmeans(lvname = lvyname, type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }else if(scaleset == "MV"){
        alpha_x <- cfmeans(lvname = lvxname, type = "free")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        alpha_y <- cfmeans(lvname = lvyname, type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Script Creation Syntax

     loading.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                                  eta.x1, eta.x2,eta.y1,eta.y2,eta.c,
                                  psi_x1, psi_x2,psi_x, psi_y1, psi_y2,psi_y, psi_x1y1, psi_x2y2,
                                  resx1, resx2, resy1, resy2, resids.x, resids.y,
                                  xints1, xints2, yints1, yints2,
                                  alpha_x1, alpha_x2,alpha_x, alpha_y1, alpha_y2,alpha_y)

     if(isTRUE(writescript)){
       cat(loading.script,"\n", file = sprintf("./scripts/%s_%s_apim_loading.txt",lvyname,lvxname))
     }

     return(loading.script)
    }
    else if (model == "intercept"){
      #Loadings
      if(scaleset == "FF"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }else if(scaleset == "MV"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated_mv")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated_mv")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Common Fate Loadings
      if(scaleset == "FF"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "equated")

      }else if(scaleset == "MV"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "fixed")
      }

      #Latent Variances
      if(scaleset == "FF"){
        psi_x <- cfvars(lvname = lvxname, type = "fixed")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y <- cfvars(lvname = lvyname, type = "fixed")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }else if(scaleset == "MV"){
        psi_x <- cfvars(lvname = lvxname, type = "free")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y <- cfvars(lvname = lvyname, type = "free")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1y1 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[[3]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[[8]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids.y = paste(resids.y, collapse = "\n")

      #Residual variances
      resx1 = resids(dvn, lvar = "X", partner="1", type = "free")
      resx2 = resids(dvn, lvar = "X", partner="2", type = "free")
      resy1 = resids(dvn, lvar = "Y", partner="1", type = "free")
      resy2 = resids(dvn, lvar = "Y", partner="2", type = "free")

      #Intercepts
      if(scaleset == "FF"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
      }else if(scaleset == "MV"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated_mv")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated_mv")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
      }

      #Latent Means
      if(scaleset == "FF"){
        alpha_x <- cfmeans(lvname = lvxname, type = "fixed")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")

        alpha_y <- cfmeans(lvname = lvyname, type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }else if(scaleset == "MV"){
        alpha_x <- cfmeans(lvname = lvxname, type = "free")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")

        alpha_y <- cfmeans(lvname = lvyname, type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }

      #Script Creation Syntax

      intercept.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                               eta.x1, eta.x2,eta.y1,eta.y2,eta.c,
                               psi_x1, psi_x2,psi_x, psi_y1, psi_y2,psi_y, psi_x1y1, psi_x2y2,
                               resx1, resx2, resy1, resy2, resids.x, resids.y,
                               xints1, xints2, yints1, yints2,
                               alpha_x1, alpha_x2,alpha_x, alpha_y1, alpha_y2,alpha_y)

      if(isTRUE(writescript)){
        cat(intercept.script,"\n", file = sprintf("./scripts/%s_%s_apim_intercept.txt",lvyname,lvxname))
      }
      return(intercept.script)
    }
    else if (model == "residual"){
      #Loadings
      if(scaleset == "FF"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }else if(scaleset == "MV"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated_mv")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated_mv")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Common Fate Loadings
      if(scaleset == "FF"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "equated")

      }else if(scaleset == "MV"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "fixed")
      }

      #Latent Variances
      if(scaleset == "FF"){
        psi_x <- cfvars(lvname = lvxname, type = "fixed")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y <- cfvars(lvname = lvyname, type = "fixed")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }else if(scaleset == "MV"){
        psi_x <- cfvars(lvname = lvxname, type = "free")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y <- cfvars(lvname = lvyname, type = "free")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1y1 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[[3]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[[8]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids.y = paste(resids.y, collapse = "\n")

      #Residual variances
      resx1 = resids(dvn, lvar = "X", partner="1", type = "equated")
      resx2 = resids(dvn, lvar = "X", partner="2", type = "equated")
      resy1 = resids(dvn, lvar = "Y", partner="1", type = "equated")
      resy2 = resids(dvn, lvar = "Y", partner="2", type = "equated")

      #Intercepts
      if(scaleset == "FF"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
      }else if(scaleset == "MV"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated_mv")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated_mv")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
      }

      #Latent Means
      if(scaleset == "FF"){
        alpha_x <- cfmeans(lvname = lvxname, type = "fixed")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")

        alpha_y <- cfmeans(lvname = lvyname, type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }else if(scaleset == "MV"){
        alpha_x <- cfmeans(lvname = lvxname, type = "free")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")

        alpha_y <- cfmeans(lvname = lvyname, type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }

      #Script Creation Syntax

      residual.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2,eta.c,
                                 psi_x1, psi_x2,psi_x, psi_y1, psi_y2,psi_y, psi_x1y1, psi_x2y2,
                                 resx1, resx2, resy1, resy2, resids.x, resids.y,
                                 xints1, xints2, yints1, yints2,
                                 alpha_x1, alpha_x2,alpha_x, alpha_y1, alpha_y2,alpha_y)

      if(isTRUE(writescript)){
        cat(residual.script,"\n", file = sprintf("./scripts/%s_%s_apim_intercept.txt",lvyname,lvxname))
      }
      return(residual.script)
    }
    else if(model == "indist"){
      #Loadings
      if(scaleset == "FF"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }else if(scaleset == "MV"){
        eta.x1 = loads(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated_mv")
        eta.x2 = loads(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        eta.y1 = loads(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated_mv")
        eta.y2 = loads(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Common Fate Loadings
      if(scaleset == "FF"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "equated")

      }else if(scaleset == "MV"){
        eta.c <- cfloads(dvn, lvxname = lvxname, lvyname = lvyname, type = "fixed")
      }

      #Latent Variances
      if(scaleset == "FF"){
        psi_x <- cfvars(lvname = lvxname, type = "fixed")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")

        psi_y <- cfvars(lvname = lvyname, type = "fixed")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
      }else if(scaleset == "MV"){
        psi_x <- cfvars(lvname = lvxname, type = "free")
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "equated")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "equated")

        psi_y <- cfvars(lvname = lvyname, type = "free")
        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "equated")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "equated")
      }

      #Latent Covariances
      psi_x1y1 = sprintf("%s%s ~~ psyxy*%s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ psyxy*%s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[[3]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[[8]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids.y = paste(resids.y, collapse = "\n")

      #Residual variances
      resx1 = resids(dvn, lvar = "X", partner="1", type = "equated")
      resx2 = resids(dvn, lvar = "X", partner="2", type = "equated")
      resy1 = resids(dvn, lvar = "Y", partner="1", type = "equated")
      resy2 = resids(dvn, lvar = "Y", partner="2", type = "equated")

      #Intercepts
      if(scaleset == "FF"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
      }else if(scaleset == "MV"){
        xints1 = intercepts(dvn, lvar = "X", partner="1", type = "equated_mv")
        xints2 = intercepts(dvn, lvar = "X", partner="2", type = "equated")
        yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "equated_mv")
        yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "equated")
      }

      #Latent Means
      if(scaleset == "FF"){
        alpha_x <- cfmeans(lvname = lvxname, type = "fixed")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")

        alpha_y <- cfmeans(lvname = lvyname, type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }else if(scaleset == "MV"){
        alpha_x <- cfmeans(lvname = lvxname, type = "free")
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")

        alpha_y <- cfmeans(lvname = lvyname, type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Script Creation Syntax

      indist.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                                eta.x1, eta.x2,eta.y1,eta.y2,eta.c,
                                psi_x1, psi_x2,psi_x, psi_y1, psi_y2,psi_y, psi_x1y1, psi_x2y2,
                                resx1, resx2, resy1, resy2, resids.x, resids.y,
                                xints1, xints2, yints1, yints2,
                                alpha_x1, alpha_x2,alpha_x, alpha_y1, alpha_y2,alpha_y)

      if(isTRUE(writescript)){
        cat(indist.script,"\n", file = sprintf("./scripts/%s_%s_apim_indist.txt",lvyname,lvxname))
      }
      return(indist.script)
    }
  }
  else if(length(dvn)==6){
    cat(crayon::red("You must supply a dvn object containing information for both X and Y"))
  }
}
