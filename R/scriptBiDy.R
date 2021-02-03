#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting BiDY  Models
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' BiDy CFA (BiDy-C) or SEM (BiDy-S) Model. Currently only uses fixed-factor scale-setting
#' @param dvn input dvn list from scrapeVarCross
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param type input character used to specify what kind of BiDy model to specify ("C" for BiDy-C, or "S" for BiDy-S)
#' @param model input character used to specify which level of invariance is
#' modeled ("configural", "loading", "intercept", "residual", or "indistinguishable"). Defaults to "configural".
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(DRES, x_order = "sip", x_stem = "sexsat",  x_delim2=".", distinguish_1="1", distinguish_2="2")
#' sexsat.bidy.config.script <- scriptBiDy(dvn, lvname = "SexSat", model = "configural", type = "C")
#' sexsat.bidy.loadsource.script <- scriptBiDy(dvn, lvname = "SexSat", model = "loading_source", type = "C")
#' sexsat.bidy.loadreleq.script <- scriptBiDy(dvn, lvname = "SexSat", model = "loading_releq", type = "C")

scriptBiDy = function(dvn, lvname = "X", type = "C", model = "configural"){
  dirs("scripts")
  if(type == "C"){
    if(model == "configural"){
      #Loadings
      eta.g = loads(dvn, lvar = "X", lvname, partner = "g", type = "free")
      eta.x1 = loads(dvn, lvar = "X",lvname, partner="1", type = "free")
      eta.x2 = loads(dvn, lvar = "X",lvname, partner="2", type = "free")

      #Latent (co)variances
      psi_g = lvars(dvn, lvar = "X", lvname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvname, partner = "2", type = "fixed")

      psi_gx1 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist1"]])
      psi_gx2 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])

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
      alpha_g <- lmeans(dvn, lvar = "X", lvname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "fixed")

      #Script Creation Syntax
      configural.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                                  eta.x1, eta.g, eta.x2,
                                  psi_g, psi_x1, psi_x2,
                                  psi_gx1, psi_gx2, psi_x1x2,
                                  resids, res1, res2,
                                  xints1, xints2,
                                  alpha_g, alpha_x1, alpha_x2)
      cat(configural.script,"\n", file = sprintf("./scripts/%s_bidyc_configural.txt",lvname))
      return(configural.script)
    }
    else if (model == "loading"){
      #Loadings
      eta.g = loads(dvn, lvar = "X", lvname, partner = "g", type = "equated")
      eta.x1 = loads(dvn, lvar = "X",lvname, partner="1", type = "equated")
      eta.x2 = loads(dvn, lvar = "X",lvname, partner="2", type = "equated")

      #Latent (co)variances
      psi_g = lvars(dvn, lvar = "X", lvname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvname, partner = "2", type = "fixed")

      psi_gx1 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist1"]])
      psi_gx2 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])

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
      alpha_g <- lmeans(dvn, lvar = "X", lvname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "fixed")


      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                               eta.x1, eta.g, eta.x2,
                               psi_g, psi_x1, psi_x2,
                               psi_gx1, psi_gx2, psi_x1x2,
                               resids, res1, res2,
                               xints1, xints2,
                               alpha_g, alpha_x1, alpha_x2)
      cat(loading.script,"\n", file = sprintf("./scripts/%s_bidy_loading.txt",lvname))
      return(loading.script)
    }
    else if (model == "loading_source"){
      #Loadings
      eta.g = loads(dvn, lvar = "X", lvname, partner = "g", type = "equated_source")
      eta.x1 = loads(dvn, lvar = "X",lvname, partner="1", type = "equated_source")
      eta.x2 = loads(dvn, lvar = "X",lvname, partner="2", type = "equated_source")

      #Latent (co)variances
      psi_g = lvars(dvn, lvar = "X", lvname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvname, partner = "2", type = "fixed")

      psi_gx1 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist1"]])
      psi_gx2 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])

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
      alpha_g <- lmeans(dvn, lvar = "X", lvname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "fixed")


      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                               eta.x1, eta.g, eta.x2,
                               psi_g, psi_x1, psi_x2,
                               psi_gx1, psi_gx2, psi_x1x2,
                               resids, res1, res2,
                               xints1, xints2,
                               alpha_g, alpha_x1, alpha_x2)
      cat(loading.script,"\n", file = sprintf("./scripts/%s_bidy_loading_src.txt",lvname))
      return(loading.script)
    }
    else if (model == "loading_releq"){
      #Loadings
      eta.g = loads(dvn, lvar = "X", lvname, partner = "g", type = "equated")
      eta.x1 = loads(dvn, lvar = "X",lvname, partner="1", type = "free")
      eta.x2 = loads(dvn, lvar = "X",lvname, partner="2", type = "free")

      #Latent (co)variances
      psi_g = lvars(dvn, lvar = "X", lvname, partner = "g", type = "fixed")
      psi_x1 = lvars(dvn, lvar = "X", lvname, partner = "1", type = "fixed")
      psi_x2 = lvars(dvn, lvar = "X", lvname, partner = "2", type = "fixed")

      psi_gx1 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist1"]])
      psi_gx2 = sprintf("%sDy ~~ 0*%s%s",lvname,lvname, dvn[["dist2"]])
      psi_x1x2 = sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])

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
      alpha_g <- lmeans(dvn, lvar = "X", lvname, partner="g", type = "fixed")
      alpha_x1 <- lmeans(dvn, lvar = "X", lvname, partner="1", type = "fixed")
      alpha_x2 <- lmeans(dvn, lvar = "X", lvname, partner="2", type = "fixed")

      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                                  eta.x1, eta.g, eta.x2,
                                  psi_g, psi_x1, psi_x2,
                                  psi_gx1, psi_gx2, psi_x1x2,
                                  resids, res1, res2,
                                  xints1, xints2,
                                  alpha_g, alpha_x1, alpha_x2)
      cat(loading.script,"\n", file = sprintf("./scripts/%s_bidyc_loading_releq.txt",lvname))
      return(loading.script)
    }
    else if (model == "intercept"){

      #Script Creation Syntax
      intercept.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                                 eta.g, eta.x1, eta.x2,
                                 psi_g, psi_x1, psi_x2,
                                 psi_gx1, psi_gx2, psi_x1x2,
                                 resids, res1, res2,
                                 xints1, xints2,
                                 alpha_g, alpha_x1, alpha_x2)
      cat(intercept.script,"\n", file = sprintf("./scripts/%s_bidi_intercept.txt",lvname))
      return(intercept.script)
    }
    else if (model == "residual"){


      #Script Creation Syntax
      residual.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                                eta.g, eta.x1, eta.x2,
                                psi_g, psi_x1, psi_x2,
                                psi_gx1, psi_gx2, psi_x1x2,
                                resids, res1, res2,
                                xints1, xints2,
                                alpha_g, alpha_x1, alpha_x2)
      cat(residual.script,"\n", file = sprintf("./scripts/%s_bidi_residual.txt",lvname))
      return(residual.script)
    }
    else if (model == "indistinguishable"){

      #Script Creation Syntax
      indist.script = sprintf("#Loadings\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s",
                              eta.g, eta.x1, eta.x2,
                              psi_g, psi_x1, psi_x2,
                              psi_gx1, psi_gx2, psi_x1x2,
                              resids, res1, res2,
                              xints1, xints2,
                              alpha_g, alpha_x1, alpha_x2)
      cat(indist.script,"\n", file = sprintf("./scripts/%s_bidi_indistinguishable.txt",lvname))
      return(indist.script)
    }
  }
  else if(type == "S"){

  }
}
