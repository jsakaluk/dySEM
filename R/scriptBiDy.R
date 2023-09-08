#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Bifactor Dyadic (Bidy) Models
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' BiDy CFA (BiDy-C) or SEM (BiDy-S) Model. Currently only uses fixed-factor scale-setting
#' @param dvn input dvn list from scrapeVarCross
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param type input character used to specify what kind of BiDy model to specify ("C" for BiDy-C, or "S" for BiDy-S)
#' @param model input character used to specify which level of invariance is
#' modeled (for BiDys, options thus far are "loading", "loading_source", and "loading_releq"). Defaults to "configural".
#' @param equate input character to specify which type of structural parameters
#' ("actor" = actor effects)
#' @param writescript input logical (default FALSE) for whether lavaan script should
#' be concatenated and written to current working directory (in subdirectory "scripts")
#' @importFrom rlang .data
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(DRES, x_order = "sip", x_stem = "sexsat",
#' x_delim2=".", distinguish_1="1", distinguish_2="2")
#' sexsat.bidyc.config.script <- scriptBiDy(dvn, lvxname = "SexSat",
#' model = "configural", type = "C")
#' sexsat.bidyc.loadsource.script <- scriptBiDy(dvn, lvxname = "SexSat",
#' model = "loading_source", type = "C")
#' sexsat.bidyc.loadreleq.script <- scriptBiDy(dvn, lvxname = "SexSat",
#' model = "loading_releq", type = "C")
#'
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2",
#' y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#'
#' comsat.bidys.config.script <- scriptBiDy(dvn, lvxname = "Sat",
#' lvyname = "Com", model = "configural", type = "S")


scriptBiDy = function(dvn, lvxname, lvyname,
                      type = "C", model = "configural", equate="none",
                      writescript = FALSE){
  if(type == "C"){
    if(model == "configural"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "free")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "free")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "fixed")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[3]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Residual variances
      res1 = resids(dvn, lvar = "X", partner="1", type = "free")
      res2 = resids(dvn, lvar = "X", partner="2", type = "free")

      #Intercepts
      xints1 = intercepts(dvn, partner="1", type = "free")
      xints2 = intercepts(dvn, partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")

      #Script Creation Syntax
      configural.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                                  eta.x1, eta.gx, eta.x2,
                                  psi_gx, psi_x1, psi_x2,
                                  psi_gxx1, psi_gxx2, psi_x1x2,
                                  resids, res1, res2,
                                  xints1, xints2,
                                  alpha_gx, alpha_x1, alpha_x2)

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(configural.script,"\n", file = sprintf("./scripts/%s_bidyc_configural.txt",lvxname))
      }
      return(configural.script)
    }
    else if (model == "loading"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "equated")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "equated")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "free")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "free")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[3]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Residual variances
      res1 = resids(dvn, lvar = "X", partner="1", type = "free")
      res2 = resids(dvn, lvar = "X", partner="2", type = "free")

      #Intercepts
      xints1 = intercepts(dvn, partner="1", type = "free")
      xints2 = intercepts(dvn, partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")


      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                               eta.x1, eta.gx, eta.x2,
                               psi_gx, psi_x1, psi_x2,
                               psi_gxx1, psi_gxx2, psi_x1x2,
                               resids, res1, res2,
                               xints1, xints2,
                               alpha_gx, alpha_x1, alpha_x2)
      if(isTRUE(writescript)){
        dirs("scripts")
        cat(loading.script,"\n", file = sprintf("./scripts/%s_bidy_loading.txt",lvxname))
      }
      return(loading.script)
    }
    else if (model == "loading_source"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated_source")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "equated_source")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "equated_source")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "fixed")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[3]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Residual variances
      res1 = resids(dvn, lvar = "X", partner="1", type = "free")
      res2 = resids(dvn, lvar = "X", partner="2", type = "free")

      #Intercepts
      xints1 = intercepts(dvn, partner="1", type = "free")
      xints2 = intercepts(dvn, partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")


      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                               eta.x1, eta.gx, eta.x2,
                               psi_gx, psi_x1, psi_x2,
                               psi_gxx1, psi_gxx2, psi_x1x2,
                               resids, res1, res2,
                               xints1, xints2,
                               alpha_gx, alpha_x1, alpha_x2)
      if(isTRUE(writescript)){
        dirs("scripts")
        cat(loading.script,"\n", file = sprintf("./scripts/%s_bidy_loading_src.txt",lvxname))
      }
      return(loading.script)
    }
    else if (model == "loading_releq"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "free")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "fixed")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[3]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Residual variances
      res1 = resids(dvn, lvar = "X", partner="1", type = "free")
      res2 = resids(dvn, lvar = "X", partner="2", type = "free")

      #Intercepts
      xints1 = intercepts(dvn, partner="1", type = "free")
      xints2 = intercepts(dvn, partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")

      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                                  eta.x1, eta.gx, eta.x2,
                                  psi_gx, psi_x1, psi_x2,
                                  psi_gxx1, psi_gxx2, psi_x1x2,
                                  resids, res1, res2,
                                  xints1, xints2,
                                  alpha_gx, alpha_x1, alpha_x2)

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(loading.script,"\n", file = sprintf("./scripts/%s_bidyc_loading_releq.txt",lvxname))
      }
      return(loading.script)
    }
  }
  else if(type == "S"){
    if(model == "configural"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "free")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "free")

      eta.gy = loads(dvn, lvar = "Y", lvyname, partner = "g", type = "free")
      eta.y1 = loads(dvn, lvar = "Y",lvyname, partner="1", type = "free")
      eta.y2 = loads(dvn, lvar = "Y",lvyname, partner="2", type = "free")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "fixed")

      psi_gy = lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "fixed")
      psi_y1 = lvars(dvn, lvar = "Y", lvyname, partner = "1", type = "fixed")
      psi_y2 = lvars(dvn, lvar = "Y", lvyname, partner = "2", type = "fixed")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      psi_gyy1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist1"]])
      psi_gyy2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      psi_gxy1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist1"]])
      psi_gxy2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist2"]])

      psi_gyx1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist1"]])
      psi_gyx2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist2"]])

      psi_x1y2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      psi_y1x2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      if(equate == "actor"){
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }else{
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a2*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }

      psi_gxgy <- sprintf("%sDy ~~ %sDy",lvxname,lvyname)

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
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "free")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "free")
      yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "free")
      yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")

      #Latent Means
      alpha_gy <- lmeans(dvn, lvar = "Y", lvyname, partner="g", type = "fixed")
      alpha_y1 <- lmeans(dvn, lvar = "Y", lvyname, partner="1", type = "fixed")
      alpha_y2 <- lmeans(dvn, lvar = "Y", lvyname, partner="2", type = "fixed")
      #Script Creation Syntax
      configural.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Actor and Dyadic Effects\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                                  eta.x1, eta.gx, eta.x2,
                                  eta.y1, eta.gy, eta.y2,
                                  psi_gx, psi_x1, psi_x2,
                                  psi_gy, psi_y1, psi_y2,
                                  psi_gxx1, psi_gxx2, psi_x1x2,
                                  psi_gyy1, psi_gyy2, psi_y1y2,
                                  psi_gxy1, psi_gxy2, psi_gyx1,
                                  psi_gyx2, psi_x1y2, psi_y1x2,
                                  psi_x1y1, psi_x2y2, psi_gxgy,
                                  resids.x, resids.y,
                                  resx1, resx2, resy1, resy2,
                                  xints1, xints2, yints1, yints2,
                                  alpha_gx, alpha_x1, alpha_x2,
                                  alpha_gy, alpha_y1, alpha_y2)

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(configural.script,"\n", file = sprintf("./scripts/%s%s_bidys_configural.txt",lvxname, lvyname))
      }
      return(configural.script)
    }
    else if(model == "loading"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "equated")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "equated")

      eta.gy = loads(dvn, lvar = "Y", lvyname, partner = "g", type = "equated")
      eta.y1 = loads(dvn, lvar = "Y",lvyname, partner="1", type = "equated")
      eta.y2 = loads(dvn, lvar = "Y",lvyname, partner="2", type = "equated")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "free")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "free")

      psi_gy = lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "fixed")
      psi_y1 = lvars(dvn, lvar = "Y", lvyname, partner = "1", type = "free")
      psi_y2 = lvars(dvn, lvar = "Y", lvyname, partner = "2", type = "free")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      psi_gyy1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist1"]])
      psi_gyy2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      psi_gxy1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist1"]])
      psi_gxy2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist2"]])

      psi_gyx1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist1"]])
      psi_gyx2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist2"]])

      psi_x1y2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      psi_y1x2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      if(equate == "actor"){
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }else{
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a2*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }

      psi_gxgy <- sprintf("%sDy ~~ %sDy",lvxname,lvyname)

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
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "free")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "free")
      yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "free")
      yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")

      #Latent Means
      alpha_gy <- lmeans(dvn, lvar = "Y", lvyname, partner="g", type = "fixed")
      alpha_y1 <- lmeans(dvn, lvar = "Y", lvyname, partner="1", type = "fixed")
      alpha_y2 <- lmeans(dvn, lvar = "Y", lvyname, partner="2", type = "fixed")
      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Actor and Dyadic Effects\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                                  eta.x1, eta.gx, eta.x2,
                                  eta.y1, eta.gy, eta.y2,
                                  psi_gx, psi_x1, psi_x2,
                                  psi_gy, psi_y1, psi_y2,
                                  psi_gxx1, psi_gxx2, psi_x1x2,
                                  psi_gyy1, psi_gyy2, psi_y1y2,
                                  psi_gxy1, psi_gxy2, psi_gyx1,
                                  psi_gyx2, psi_x1y2, psi_y1x2,
                                  psi_x1y1, psi_x2y2, psi_gxgy,
                                  resids.x, resids.y,
                                  resx1, resx2, resy1, resy2,
                                  xints1, xints2, yints1, yints2,
                                  alpha_gx, alpha_x1, alpha_x2,
                                  alpha_gy, alpha_y1, alpha_y2)
      if(isTRUE(writescript)){
        dirs("scripts")
        cat(loading.script,"\n", file = sprintf("./scripts/%s%s_bidys_loading.txt",lvxname, lvyname))
      }
      return(loading.script)
    }
    else if(model == "loading_source"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated_source")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "equated_source")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "equated_source")

      eta.gy = loads(dvn, lvar = "Y", lvyname, partner = "g", type = "equated_source")
      eta.y1 = loads(dvn, lvar = "Y",lvyname, partner="1", type = "equated_source")
      eta.y2 = loads(dvn, lvar = "Y",lvyname, partner="2", type = "equated_source")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "fixed")

      psi_gy = lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "fixed")
      psi_y1 = lvars(dvn, lvar = "Y", lvyname, partner = "1", type = "fixed")
      psi_y2 = lvars(dvn, lvar = "Y", lvyname, partner = "2", type = "fixed")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      psi_gyy1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist1"]])
      psi_gyy2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      psi_gxy1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist1"]])
      psi_gxy2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist2"]])

      psi_gyx1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist1"]])
      psi_gyx2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist2"]])

      psi_x1y2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      psi_y1x2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      if(equate == "actor"){
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }else{
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a2*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }

      psi_gxgy <- sprintf("%sDy ~~ %sDy",lvxname,lvyname)

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
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "free")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "free")
      yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "free")
      yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")

      #Latent Means
      alpha_gy <- lmeans(dvn, lvar = "Y", lvyname, partner="g", type = "fixed")
      alpha_y1 <- lmeans(dvn, lvar = "Y", lvyname, partner="1", type = "fixed")
      alpha_y2 <- lmeans(dvn, lvar = "Y", lvyname, partner="2", type = "fixed")
      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Actor and Dyadic Effects\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                               eta.x1, eta.gx, eta.x2,
                               eta.y1, eta.gy, eta.y2,
                               psi_gx, psi_x1, psi_x2,
                               psi_gy, psi_y1, psi_y2,
                               psi_gxx1, psi_gxx2, psi_x1x2,
                               psi_gyy1, psi_gyy2, psi_y1y2,
                               psi_gxy1, psi_gxy2, psi_gyx1,
                               psi_gyx2, psi_x1y2, psi_y1x2,
                               psi_x1y1, psi_x2y2, psi_gxgy,
                               resids.x, resids.y,
                               resx1, resx2, resy1, resy2,
                               xints1, xints2, yints1, yints2,
                               alpha_gx, alpha_x1, alpha_x2,
                               alpha_gy, alpha_y1, alpha_y2)

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(loading.script,"\n", file = sprintf("./scripts/%s%s_bidys_loading_source.txt",lvxname, lvyname))
      }
      return(loading.script)
    }
    else if(model == "loading_releq"){
      #Loadings
      eta.gx = loads(dvn, lvar = "X", lvxname, partner = "g", type = "equated")
      eta.x1 = loads(dvn, lvar = "X",lvxname, partner="1", type = "free")
      eta.x2 = loads(dvn, lvar = "X",lvxname, partner="2", type = "free")

      eta.gy = loads(dvn, lvar = "Y", lvyname, partner = "g", type = "equated")
      eta.y1 = loads(dvn, lvar = "Y",lvyname, partner="1", type = "free")
      eta.y2 = loads(dvn, lvar = "Y",lvyname, partner="2", type = "free")

      #Latent (co)variances
      psi_gx = lvars(dvn, lvar = "X", lvxname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvxname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvxname, partner = "2", type = "fixed")

      psi_gy = lvars(dvn, lvar = "Y", lvyname, partner = "g", type = "fixed")
      psi_y1 = lvars(dvn, lvar = "Y", lvyname, partner = "1", type = "fixed")
      psi_y2 = lvars(dvn, lvar = "Y", lvyname, partner = "2", type = "fixed")

      psi_gxx1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist1"]])
      psi_gxx2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvxname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      psi_gyy1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist1"]])
      psi_gyy2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvyname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      psi_gxy1 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist1"]])
      psi_gxy2 = sprintf("%sDy ~~ 0*%s%s",lvxname,lvyname, dvn[["dist2"]])

      psi_gyx1 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist1"]])
      psi_gyx2 = sprintf("%sDy ~~ 0*%s%s",lvyname,lvxname, dvn[["dist2"]])

      psi_x1y2 = sprintf("%s%s ~~ 0*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      psi_y1x2 = sprintf("%s%s ~~ 0*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])

      if(equate == "actor"){
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }else{
        psi_x1y1 <- sprintf("%s%s ~~ a1*%s%s",lvxname, dvn[["dist1"]],lvyname, dvn[["dist1"]])
        psi_x2y2 <- sprintf("%s%s ~~ a2*%s%s",lvxname, dvn[["dist2"]],lvyname, dvn[["dist2"]])
      }

      psi_gxgy <- sprintf("%sDy ~~ %sDy",lvxname,lvyname)

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
      xints1 = intercepts(dvn, lvar = "X", partner="1", type = "free")
      xints2 = intercepts(dvn, lvar = "X", partner="2", type = "free")
      yints1 = intercepts(dvn, lvar = "Y", partner="1", type = "free")
      yints2 = intercepts(dvn, lvar = "Y", partner="2", type = "free")

      #Latent Means
      alpha_gx <- lmeans(dvn, lvar = "X", lvxname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvxname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvxname, partner="2", type = "fixed")

      #Latent Means
      alpha_gy <- lmeans(dvn, lvar = "Y", lvyname, partner="g", type = "fixed")
      alpha_y1 <- lmeans(dvn, lvar = "Y", lvyname, partner="1", type = "fixed")
      alpha_y2 <- lmeans(dvn, lvar = "Y", lvyname, partner="2", type = "fixed")
      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Actor and Dyadic Effects\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n%s\n%s",
                                  eta.x1, eta.gx, eta.x2,
                                  eta.y1, eta.gy, eta.y2,
                                  psi_gx, psi_x1, psi_x2,
                                  psi_gy, psi_y1, psi_y2,
                                  psi_gxx1, psi_gxx2, psi_x1x2,
                                  psi_gyy1, psi_gyy2, psi_y1y2,
                                  psi_gxy1, psi_gxy2, psi_gyx1,
                                  psi_gyx2, psi_x1y2, psi_y1x2,
                                  psi_x1y1, psi_x2y2, psi_gxgy,
                                  resids.x, resids.y,
                                  resx1, resx2, resy1, resy2,
                                  xints1, xints2, yints1, yints2,
                                  alpha_gx, alpha_x1, alpha_x2,
                                  alpha_gy, alpha_y1, alpha_y2)
      if(isTRUE(writescript)){
        dirs("scripts")
        cat(loading.script,"\n", file = sprintf("./scripts/%s%s_bidys_loading_releq.txt",lvxname, lvyname))
      }
      return(loading.script)
    }
  }
}
