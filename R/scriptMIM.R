#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Mutual influence Model
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying Mutual Influence Models (MIMs). Users can
#' also invoke configural, loading, and/or intercept invariant
#' measurement models, and particular types of structural comparisons.
#' @param dvn input dvn list from scrapeVarCross
#' @param scaleset input character to specify how to set the scale of the latent variable(s). Default is
#' "FF" (fixed-factor; see Details for rationale), but user can specify "MV" (Marker Variable)
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param model input character used to specify which level of invariance is
#' modeled. Defaults to "configural"
#' @param equate input character to specify which type of structural parameters
#' ("actor" = actor effects, "partner" = partner effects,
#' "all_effects" = actor and partner effects, x_means, y_means, or all_means)
#' are constrained to equivalency between partners. Default is "none". Actor or partner effect constraints
#' require at least a loading-invariant model to be specified, otherwise a warning is returned; mean constraints
#' require an intercept-invariant model to be specified, otherwise a warning is returned.
#' @param k input logical for whether Kenny & Ledermann's (2010) k parameter should be
#' calculated to characterize the dyadic pattern in the mim. Defaults FALSE, and requires at least
#' a loading-invariant model to be specified, otherwise a warning is returned.
#' @param writescript input logical (default FALSE) for whether lavaan script should
#' be concatenated and written to current working directory (in subdirectory "scripts")
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions. Users will receive message if structural comparisons are specified
#' when the recommended level of invariance is not also specified. If user supplies dvn
#' with containing X or Y variables, they are alerted to respecify the dvn object.
#' @seealso \code{\link{scrapeVarCross}} which this function relies on
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_",
#' x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2",
#'                     y_order="sip", y_stem="sexsat", y_delim2=".", y_item_num="\\d+")
#' mim.script.config = scriptMIM(dvn, lvxname = "Quality",
#' lvyname = "SexSat", model = "configural", scaleset = "MV")
#' mim.script.load = scriptMIM(dvn, lvxname = "Quality",
#' lvyname = "SexSat", model = "loading", scaleset = "MV")
#' mim.script.int = scriptMIM(dvn, lvxname = "Quality",
#' lvyname = "SexSat", model = "intercept", scaleset = "MV")
#' mim.script.load.actor = scriptMIM(dvn, lvxname = "Quality",
#' lvyname = "SexSat", model = "loading", equate = "actor")

scriptMIM = function(dvn, scaleset = "FF",
                     lvxname, lvyname,
                     model = "configural",
                     equate="none", k = FALSE,
                     writescript = FALSE){
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

      #Latent Variances
      if(scaleset == "FF"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
      }else if(scaleset == "MV"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      
      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[["xindper"]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")
      
      resids.y = list()
      for (i in 1:dvn[["yindper"]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
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
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }else if(scaleset == "MV"){
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }

      #Actor effects
      if(equate=="none"|equate=="partner"|equate=="means"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "free")
      }
      else if(equate=="actor"|equate=="all_effects"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "equated")
      }
      #Partner effects
      if(equate=="none"|equate=="actor"|equate=="means"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "free")
      }
      else if(equate=="partner"|equate=="all_effects"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "equated")
      }

      #parameter k
      if(k == TRUE & equate=="none"){
        k1 = paste("k1 := p1/a1")
        k2 = paste("k2 := p2/a2")
      }else if(k == TRUE & equate=="actor"){
        k1 = paste("k1 := p1/a")
        k2 = paste("k2 := p2/a")
      }else if(k == TRUE & equate=="partner"){
        k1 = paste("k1 := p/a1")
        k2 = paste("k2 := p/a2")
      }else if(k == TRUE & equate=="all_effects"){
        k1 = paste("k := p/a")
      }

      #Script Creation Syntax (contingent on whether k ==T and pattern of a/p constraints)
      if(k == FALSE){
        configural.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s",
                                    eta.x1, eta.x2,eta.y1,eta.y2,
                                    psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                    resx1, resx2, resy1, resy2, resids.x, resids.y,
                                    xints1, xints2, yints1, yints2,
                                    alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                    actors, partners
                                    )
      }else if(k == TRUE & equate=="all_effects"){
        configural.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s",
                                    eta.x1, eta.x2,eta.y1,eta.y2,
                                    psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                    resx1, resx2, resy1, resy2, resids.x, resids.y,
                                    xints1, xints2, yints1, yints2,
                                    alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                    actors, partners,
                                    k1
        )
      }else{
        configural.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s\n%s",
                                    eta.x1, eta.x2,eta.y1,eta.y2,
                                    psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                    resx1, resx2, resy1, resy2, resids.x, resids.y,
                                    xints1, xints2, yints1, yints2,
                                    alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                    actors, partners,
                                    k1, k2
        )
      }

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(configural.script,"\n", file = sprintf("./scripts/%s_%s_mim_configural.txt",lvyname,lvxname))
      }

      if(equate=="actor"|equate=="partner"|equate=="all_effects"|k == TRUE){
        warning("Caution: comparisons of actor/partner effects, and/or computation of k may be invalid when loadings are not invariant")
      }
      else if(equate=="x_means"|equate=="y_means"|equate=="all_means"){
        warning("Caution: comparisons of means may be invalid when loadings and intercepts are not invariant")
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

      #Latent Variances
      if(scaleset == "FF"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }else if(scaleset == "MV"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      
      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[["xindper"]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")
      
      resids.y = list()
      for (i in 1:dvn[["yindper"]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
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
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }else if(scaleset == "MV"){
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }

      #Actor effects
      if(equate=="none"|equate=="partner"|equate=="means"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "free")
      }
      else if(equate=="actor"|equate=="all_effects"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "equated")
      }
      #Partner effects
      if(equate=="none"|equate=="actor"|equate=="means"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "free")
      }
      else if(equate=="partner"|equate=="all_effects"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "equated")
      }

      #parameter k
      if(k == TRUE & equate=="none"){
        k1 = paste("k1 := p1/a1")
        k2 = paste("k2 := p2/a2")
      }else if(k == TRUE & equate=="actor"){
        k1 = paste("k1 := p1/a")
        k2 = paste("k2 := p2/a")
      }else if(k == TRUE & equate=="partner"){
        k1 = paste("k1 := p/a1")
        k2 = paste("k2 := p/a2")
      }else if(k == TRUE & equate=="all_effects"){
        k1 = paste("k := p/a")
      }

      #Script Creation Syntax (contingent on whether k ==T and pattern of a/p constraints)
      if(k == FALSE){
        loading.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s",
                                    eta.x1, eta.x2,eta.y1,eta.y2,
                                    psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                    resx1, resx2, resy1, resy2, resids.x, resids.y,
                                    xints1, xints2, yints1, yints2,
                                    alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                    actors, partners
        )
      }else if(k == TRUE & equate=="all_effects"){
        loading.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s",
                                    eta.x1, eta.x2,eta.y1,eta.y2,
                                    psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                    resx1, resx2, resy1, resy2, resids.x, resids.y,
                                    xints1, xints2, yints1, yints2,
                                    alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                    actors, partners,
                                    k1
        )
      }else{
        loading.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s\n%s",
                                    eta.x1, eta.x2,eta.y1,eta.y2,
                                    psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                    resx1, resx2, resy1, resy2, resids.x, resids.y,
                                    xints1, xints2, yints1, yints2,
                                    alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                    actors, partners,
                                    k1, k2
        )
      }
      if(isTRUE(writescript)){
        dirs("scripts")
        cat(loading.script,"\n", file = sprintf("./scripts/%s_%s_mim_loading.txt",lvyname,lvxname))
      }

      if(equate=="x_means"|equate=="y_means"|equate=="all_means"){
        warning("Caution: comparisons of means may be invalid when loadings and intercepts are not invariant")
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

      #Latent Variances
      if(scaleset == "FF"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }else if(scaleset == "MV"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[["xindper"]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[["yindper"]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
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
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }else if(scaleset == "MV"){
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }

      #Actor effects
      if(equate=="none"|equate=="partner"|equate=="means"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "free")
      }
      else if(equate=="actor"|equate=="all_effects"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "equated")
      }
      #Partner effects
      if(equate=="none"|equate=="actor"|equate=="means"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "free")
      }
      else if(equate=="partner"|equate=="all_effects"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "equated")
      }

      #parameter k
      if(k == TRUE & equate=="none"){
        k1 = paste("k1 := p1/a1")
        k2 = paste("k2 := p2/a2")
      }else if(k == TRUE & equate=="actor"){
        k1 = paste("k1 := p1/a")
        k2 = paste("k2 := p2/a")
      }else if(k == TRUE & equate=="partner"){
        k1 = paste("k1 := p/a1")
        k2 = paste("k2 := p/a2")
      }else if(k == TRUE & equate=="all_effects"){
        k1 = paste("k := p/a")
      }

      #Script Creation Syntax (contingent on whether k ==T and pattern of a/p constraints)
      if(k == FALSE){
        intercept.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2,
                                 psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                 resx1, resx2, resy1, resy2, resids.x, resids.y,
                                 xints1, xints2, yints1, yints2,
                                 alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                 actors, partners
        )
      }else if(k == TRUE & equate=="all_effects"){
        intercept.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2,
                                 psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                 resx1, resx2, resy1, resy2, resids.x, resids.y,
                                 xints1, xints2, yints1, yints2,
                                 alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                 actors, partners,
                                 k1
        )
      }else{
        intercept.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2,
                                 psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                 resx1, resx2, resy1, resy2, resids.x, resids.y,
                                 xints1, xints2, yints1, yints2,
                                 alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                 actors, partners,
                                 k1, k2
        )
      }

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(intercept.script,"\n", file = sprintf("./scripts/%s_%s_mim_intercept.txt",lvyname,lvxname))
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

      #Latent Variances
      if(scaleset == "FF"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }else if(scaleset == "MV"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "free")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "free")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "free")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "free")
      }

      #Latent Covariances
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[["xindper"]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[["yindper"]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
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
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }else if(scaleset == "MV"){
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }

      #Actor effects
      if(equate=="none"|equate=="partner"|equate=="means"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "free")
      }
      else if(equate=="actor"|equate=="all_effects"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "equated")
      }
      #Partner effects
      if(equate=="none"|equate=="actor"|equate=="means"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "free")
      }
      else if(equate=="partner"|equate=="all_effects"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "equated")
      }

      #parameter k
      if(k == TRUE & equate=="none"){
        k1 = paste("k1 := p1/a1")
        k2 = paste("k2 := p2/a2")
      }else if(k == TRUE & equate=="actor"){
        k1 = paste("k1 := p1/a")
        k2 = paste("k2 := p2/a")
      }else if(k == TRUE & equate=="partner"){
        k1 = paste("k1 := p/a1")
        k2 = paste("k2 := p/a2")
      }else if(k == TRUE & equate=="all_effects"){
        k1 = paste("k := p/a")
      }

      #Script Creation Syntax (contingent on whether k ==T and pattern of a/p constraints)
      if(k == FALSE){
        residual.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s",
                                   eta.x1, eta.x2,eta.y1,eta.y2,
                                   psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                   resx1, resx2, resy1, resy2, resids.x, resids.y,
                                   xints1, xints2, yints1, yints2,
                                   alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                   actors, partners
        )
      }else if(k == TRUE & equate=="all_effects"){
        residual.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s",
                                   eta.x1, eta.x2,eta.y1,eta.y2,
                                   psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                   resx1, resx2, resy1, resy2, resids.x, resids.y,
                                   xints1, xints2, yints1, yints2,
                                   alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                   actors, partners,
                                   k1
        )
      }else{
        residual.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s\n%s",
                                   eta.x1, eta.x2,eta.y1,eta.y2,
                                   psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                   resx1, resx2, resy1, resy2, resids.x, resids.y,
                                   xints1, xints2, yints1, yints2,
                                   alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                   actors, partners,
                                   k1, k2
        )
      }

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(residual.script,"\n", file = sprintf("./scripts/%s_%s_mim_intercept.txt",lvyname,lvxname))
      }

      return(residual.script)
    }
    else if (model == "lvariance"){
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

      #Latent Variances
      if(scaleset == "FF"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
      }else if(scaleset == "MV"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "equated")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "equated")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "equated")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "equated")
      }

      #Latent Covariances
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[["xindper"]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[["yindper"]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
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
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }else if(scaleset == "MV"){
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "free")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "free")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "free")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "free")
      }

      #Actor effects
      if(equate=="none"|equate=="partner"|equate=="means"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "free")
      }
      else if(equate=="actor"|equate=="all_effects"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "equated")
      }
      #Partner effects
      if(equate=="none"|equate=="actor"|equate=="means"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "free")
      }
      else if(equate=="partner"|equate=="all_effects"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "equated")
      }

      #parameter k
      if(k == TRUE & equate=="none"){
        k1 = paste("k1 := p1/a1")
        k2 = paste("k2 := p2/a2")
      }else if(k == TRUE & equate=="actor"){
        k1 = paste("k1 := p1/a")
        k2 = paste("k2 := p2/a")
      }else if(k == TRUE & equate=="partner"){
        k1 = paste("k1 := p/a1")
        k2 = paste("k2 := p/a2")
      }else if(k == TRUE & equate=="all_effects"){
        k1 = paste("k := p/a")
      }

      #Script Creation Syntax (contingent on whether k ==T and pattern of a/p constraints)
      if(k == FALSE){
        lvariance.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2,
                                 psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                 resx1, resx2, resy1, resy2, resids.x, resids.y,
                                 xints1, xints2, yints1, yints2,
                                 alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                 actors, partners
        )
      }else if(k == TRUE & equate=="all_effects"){
        lvariance.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2,
                                 psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                 resx1, resx2, resy1, resy2, resids.x, resids.y,
                                 xints1, xints2, yints1, yints2,
                                 alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                 actors, partners,
                                 k1
        )
      }else{
        lvariance.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2,
                                 psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                 resx1, resx2, resy1, resy2, resids.x, resids.y,
                                 xints1, xints2, yints1, yints2,
                                 alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                 actors, partners,
                                 k1, k2
        )
      }

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(lvariance.script,"\n", file = sprintf("./scripts/%s_%s_mim_loading.txt",lvyname,lvxname))
      }

      if(equate=="x_means"|equate=="y_means"|equate=="all_means"){
        warning("Caution: comparisons of means may be invalid when loadings and intercepts are not invariant")
      }
      return(lvariance.script)
    }
    else if(model == "lmean"){
    #Check on equate option; might be able to get away with if statements in other models
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

      #Latent Variances
      if(scaleset == "FF"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "fixed")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "fixed")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "fixed")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "fixed")
      }else if(scaleset == "MV"){
        psi_x1 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "1", type = "equated")
        psi_x2 = lvars(dvn, lvar = "X", lvname = lvxname, partner = "2", type = "equated")

        psi_y1 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "1", type = "equated")
        psi_y2 = lvars(dvn, lvar = "Y", lvname = lvyname, partner = "2", type = "equated")
      }

      #Latent Covariances
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])

      #Correlated residuals
      resids.x = list()
      for (i in 1:dvn[["xindper"]]) {
        resids.x[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
      }
      resids.x = paste(resids.x, collapse = "\n")

      resids.y = list()
      for (i in 1:dvn[["yindper"]]) {
        resids.y[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
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
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "fixed")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "fixed")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "fixed")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "fixed")
      }else if(scaleset == "MV"){
        alpha_x1 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="1", type = "equated")
        alpha_x2 <- lmeans(dvn, lvar = "X", lvname = lvxname, partner="2", type = "equated")
        alpha_y1 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="1", type = "equated")
        alpha_y2 <- lmeans(dvn, lvar = "Y", lvname = lvyname, partner="2", type = "equated")
      }

      #Actor effects
      if(equate=="none"|equate=="partner"|equate=="means"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "free")
      }
      else if(equate=="actor"|equate=="all_effects"){
        actors <- lregs(dvn, param = "act", lvxname, lvyname, type = "equated")
      }
      #Partner effects
      if(equate=="none"|equate=="actor"|equate=="means"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "free")
      }
      else if(equate=="partner"|equate=="all_effects"){
        partners <- lregs(dvn, param = "mim_part", lvxname, lvyname, type = "equated")
      }

      #parameter k
      if(k == TRUE & equate=="none"){
        k1 = paste("k1 := p1/a1")
        k2 = paste("k2 := p2/a2")
      }else if(k == TRUE & equate=="actor"){
        k1 = paste("k1 := p1/a")
        k2 = paste("k2 := p2/a")
      }else if(k == TRUE & equate=="partner"){
        k1 = paste("k1 := p/a1")
        k2 = paste("k2 := p/a2")
      }else if(k == TRUE & equate=="all_effects"){
        k1 = paste("k := p/a")
      }

      #Script Creation Syntax (contingent on whether k ==T and pattern of a/p constraints)
      if(k == FALSE){
       indist.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s",
                                  eta.x1, eta.x2,eta.y1,eta.y2,
                                  psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                  resx1, resx2, resy1, resy2, resids.x, resids.y,
                                  xints1, xints2, yints1, yints2,
                                  alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                  actors, partners
        )
      }else if(k == TRUE & equate=="all_effects"){
        indist.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s",
                                  eta.x1, eta.x2,eta.y1,eta.y2,
                                  psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                  resx1, resx2, resy1, resy2, resids.x, resids.y,
                                  xints1, xints2, yints1, yints2,
                                  alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                  actors, partners,
                                  k1
        )
      }else{
        indist.script = sprintf("#Loadings\n%s\n%s\n%s\n%s\n\n#Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Residual (Co)variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s\n\n#Latent Means\n%s\n%s\n%s\n%s\n\n#Actor and Partner Effects\n%s\n%s\n\n#k Parameter\n%s\n%s",
                                  eta.x1, eta.x2,eta.y1,eta.y2,
                                  psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2,
                                  resx1, resx2, resy1, resy2, resids.x, resids.y,
                                  xints1, xints2, yints1, yints2,
                                  alpha_x1, alpha_x2, alpha_y1, alpha_y2,
                                  actors, partners,
                                  k1, k2
        )
      }

      if(isTRUE(writescript)){
        dirs("scripts")
        cat(indist.script,"\n", file = sprintf("./scripts/%s_%s_mim_intercept.txt",lvyname,lvxname))
      }

      return(indist.script)
    }
  }
  else if(length(dvn)==6){
    stop("You must supply a dvn object containing information for both X and Y")
  }
}
