#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Actor-Partner Interdependence Models
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying Actor-Partner Interdependence Models (APIMs). Users can
#' also invoke configural, loading, and/or intercept invariant
#' measurement models, and particular types of structural comparisons.
#' @param dvn input object from dyadVarNames()
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param model input character used to specify which level of invariance is
#' modeled. Defaults to "configural"
#' @param compare input character to specify which type of structural parameters
#' ("actor" = actor effects, "partner" = partner effects,
#' "all_effects" = actor and partner effects, x_means, y_means, or all_means)
#' are constrained to equivalencey between partners. Default is "none".
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions. Users will receive message if structural comparisons are specified
#' when the recommended level of invariance is not also specified.
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", yvar="Y", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' apim.script.config = apimSEM(dvn, lvxname = "Conflict",
#' lvyname = "Satisfaction", model = "configural")
#' apim.script.load = apimSEM(dvn, lvxname = "Conflict",
#' lvyname = "Satisfaction", model = "loading")
#' apim.script.int = apimSEM(dvn, lvxname = "Conflict",
#' lvyname = "Satisfaction", model = "intercept")
#' apim.script.load.actor = apimSEM(dvn, lvxname = "Conflict",
#' lvyname = "Satisfaction", model = "loading", compare = "actor")

apimSEM = function(dvn, lvxname, lvyname, model = "configural", compare="none"){
    if(model == "configural"){
      #Loadings
      eta_x1 = sprintf("%s%s =~ NA*",lvxname, dvn[[4]])
      eta.x1 = gsub(" ", "",paste(eta_x1,paste(dvn[[1]], collapse = "+")), fixed = T)
      eta_x2 = sprintf("%s%s =~ NA*",lvxname, dvn[[5]])
      eta.x2 = gsub(" ", "",paste(eta_x2,paste(dvn[[2]], collapse = "+")), fixed = T)

      eta_y1 = sprintf("%s%s =~ NA*",lvyname, dvn[[4]])
      eta.y1 = gsub(" ", "",paste(eta_y1,paste(dvn[[6]], collapse = "+")), fixed = T)
      eta_y2 = sprintf("%s%s =~ NA*",lvyname, dvn[[5]])
      eta.y2 = gsub(" ", "",paste(eta_y2,paste(dvn[[7]], collapse = "+")), fixed = T)

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvxname, dvn[[5]])

      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[[4]],lvyname, dvn[[5]])

      #Actor effects
      if(compare=="none"|compare=="partner"|compare=="means"){
        beta_y1x1 = sprintf("%s%s ~ a1*%s%s",lvyname, dvn[[4]],lvxname, dvn[[4]])
        beta_y2x2 = sprintf("%s%s ~ a2*%s%s",lvyname, dvn[[5]],lvxname, dvn[[5]])
      }
      else if(compare=="actor"|compare=="all_effects"){
        beta_y1x1 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[[4]],lvxname, dvn[[4]])
        beta_y2x2 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[[5]],lvxname, dvn[[5]])
      }
      #Partner effects
      if(compare=="none"|compare=="actor"|compare=="means"){
        beta_y1x2 = sprintf("%s%s ~ p1*%s%s",lvyname, dvn[[4]],lvxname, dvn[[5]])
        beta_y2x1 = sprintf("%s%s ~ p2*%s%s",lvyname, dvn[[5]],lvxname, dvn[[4]])
      }
      else if(compare=="partner"|compare=="all_effects"){
        beta_y1x2 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[[4]],lvxname, dvn[[5]])
        beta_y2x1 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[[5]],lvxname, dvn[[4]])
      }

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

      #Intercepts
      xints1 = list()
      xints2 = list()
      for (i in 1:dvn[[3]]) {
        xints1[[i]]=sprintf("%s ~ 1", dvn[[1]][i])
        xints2[[i]]=sprintf("%s ~ 1", dvn[[2]][i])
      }
      xints1 = paste(xints1, collapse = "\n")
      xints2 = paste(xints2, collapse = "\n")

      yints1 = list()
      yints2 = list()
      for (i in 1:dvn[[8]]) {
        yints1[[i]]=sprintf("%s ~ 1", dvn[[6]][i])
        yints2[[i]]=sprintf("%s ~ 1", dvn[[7]][i])
      }
      yints1 = paste(yints1, collapse = "\n")
      yints2 = paste(yints2, collapse = "\n")

      #Script Creation Syntax
      configural.script = sprintf("#Loadings\n%s\n%s\n\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Actor Effects\n%s\n%s\n\n#Partner Effects\n%s\n%s\n\n#Residuals\n%s\n\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s",
                                  eta.x1, eta.x2,eta.y1,eta.y2, psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2, beta_y1x1, beta_y2x2, beta_y1x2, beta_y2x1, resids.x, resids.y, xints1, xints2, yints1, yints2)
      cat(configural.script,"\n", file = sprintf("%s_%s_apim_configural.txt",lvyname,lvxname))
      if(compare=="actor"|compare=="partner"|compare=="all_effects"){
        message("Caution: comparisons of actor/partner effects may be invalid when loadings are not invariant ")
      }
      else if(compare=="x_means"|compare=="y_means"|compare=="all_means"){
        message("Caution: comparisons of means may be invalid when loadings and intercepts are not invariant ")
      }
      return(configural.script)
    }
    else if (model == "loading"){
      #Loadings
      eta_x1 = sprintf("%s%s =~ NA*%s+",lvxname, dvn[[4]], dvn[[1]][1])
      eta.x1 = list()
      for (i in 1:dvn[[3]]) {
        eta.x1[[i]]=sprintf("l%s*%s",i, dvn[[1]][i])
      }
      eta.x1 = gsub(" ", "",paste(eta_x1,paste(eta.x1, collapse = "+")), fixed = T)

      eta_x2 = sprintf("%s%s =~ NA*%s+",lvxname, dvn[[5]], dvn[[2]][1])
      eta.x2 = list()
      for (i in 1:dvn[[3]]) {
        eta.x2[[i]]=sprintf("l%s*%s",i, dvn[[2]][i])
      }
      eta.x2 = gsub(" ", "",paste(eta_x2,paste(eta.x2, collapse = "+")), fixed = T)

      eta_y1 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[4]], dvn[[6]][1])
      eta.y1 = list()
      for (i in 1:dvn[[8]]) {
        eta.y1[[i]]=sprintf("l%s*%s",i+dvn[[3]], dvn[[6]][i])
      }
      eta.y1 = gsub(" ", "",paste(eta_y1,paste(eta.y1, collapse = "+")), fixed = T)

      eta_y2 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[5]], dvn[[7]][1])
      eta.y2 = list()
      for (i in 1:dvn[[8]]) {
        eta.y2[[i]]=sprintf("l%s*%s",i+dvn[[3]], dvn[[7]][i])
      }
      eta.y2 = gsub(" ", "",paste(eta_y2,paste(eta.y2, collapse = "+")), fixed = T)

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvxname, dvn[[5]])

      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ NA*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[[4]],lvyname, dvn[[5]])

      #Actor effects
      if(compare=="none"|compare=="partner"|compare=="means"){
        beta_y1x1 = sprintf("%s%s ~ a1*%s%s",lvyname, dvn[[4]],lvxname, dvn[[4]])
        beta_y2x2 = sprintf("%s%s ~ a2*%s%s",lvyname, dvn[[5]],lvxname, dvn[[5]])
      }
      else if(compare=="actor"|compare=="all_effects"){
        beta_y1x1 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[[4]],lvxname, dvn[[4]])
        beta_y2x2 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[[5]],lvxname, dvn[[5]])
      }
      #Partner effects
      if(compare=="none"|compare=="actor"|compare=="means"){
        beta_y1x2 = sprintf("%s%s ~ p1*%s%s",lvyname, dvn[[4]],lvxname, dvn[[5]])
        beta_y2x1 = sprintf("%s%s ~ p2*%s%s",lvyname, dvn[[5]],lvxname, dvn[[4]])
      }
      else if(compare=="partner"|compare=="all_effects"){
        beta_y1x2 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[[4]],lvxname, dvn[[5]])
        beta_y2x1 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[[5]],lvxname, dvn[[4]])
      }

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
      #Intercepts
      xints1 = list()
      xints2 = list()
      for (i in 1:dvn[[3]]) {
        xints1[[i]]=sprintf("%s ~ 1", dvn[[1]][i])
        xints2[[i]]=sprintf("%s ~ 1", dvn[[2]][i])
      }
      xints1 = paste(xints1, collapse = "\n")
      xints2 = paste(xints2, collapse = "\n")

      yints1 = list()
      yints2 = list()
      for (i in 1:dvn[[8]]) {
        yints1[[i]]=sprintf("%s ~ 1", dvn[[6]][i])
        yints2[[i]]=sprintf("%s ~ 1", dvn[[7]][i])
      }
      yints1 = paste(yints1, collapse = "\n")
      yints2 = paste(yints2, collapse = "\n")

      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Actor Effects\n%s\n%s\n\n#Partner Effects\n%s\n%s\n\n#Residuals\n%s\n\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s",
                               eta.x1, eta.x2,eta.y1,eta.y2, psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2, beta_y1x1, beta_y2x2, beta_y1x2, beta_y2x1, resids.x, resids.y, xints1, xints2, yints1, yints2)
      cat(loading.script,"\n", file = sprintf("%s_%s_apim_loading.txt",lvyname,lvxname))

      if(compare=="x_means"|compare=="y_means"|compare=="all_means"){
        message("Caution: comparisons of means may be invalid when loadings and intercepts are not invariant ")
      }

      return(loading.script)
    }
    else if (model == "intercept"){
      #Loadings
      eta_x1 = sprintf("%s%s =~ NA*%s+",lvxname, dvn[[4]], dvn[[1]][1])
      eta.x1 = list()
      for (i in 1:dvn[[3]]) {
        eta.x1[[i]]=sprintf("l%s*%s",i, dvn[[1]][i])
      }
      eta.x1 = gsub(" ", "",paste(eta_x1,paste(eta.x1, collapse = "+")), fixed = T)

      eta_x2 = sprintf("%s%s =~ NA*%s+",lvxname, dvn[[5]], dvn[[2]][1])
      eta.x2 = list()
      for (i in 1:dvn[[3]]) {
        eta.x2[[i]]=sprintf("l%s*%s",i, dvn[[2]][i])
      }
      eta.x2 = gsub(" ", "",paste(eta_x2,paste(eta.x2, collapse = "+")), fixed = T)

      eta_y1 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[4]], dvn[[6]][1])
      eta.y1 = list()
      for (i in 1:dvn[[8]]) {
        eta.y1[[i]]=sprintf("l%s*%s",i+dvn[[3]], dvn[[6]][i])
      }
      eta.y1 = gsub(" ", "",paste(eta_y1,paste(eta.y1, collapse = "+")), fixed = T)

      eta_y2 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[5]], dvn[[7]][1])
      eta.y2 = list()
      for (i in 1:dvn[[8]]) {
        eta.y2[[i]]=sprintf("l%s*%s",i+dvn[[3]], dvn[[7]][i])
      }
      eta.y2 = gsub(" ", "",paste(eta_y2,paste(eta.y2, collapse = "+")), fixed = T)

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvxname, dvn[[5]])

      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ NA*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[[4]],lvyname, dvn[[5]])
      #Actor effects
      if(compare=="none"|compare=="partner"|compare=="means"){
        beta_y1x1 = sprintf("%s%s ~ a1*%s%s",lvyname, dvn[[4]],lvxname, dvn[[4]])
        beta_y2x2 = sprintf("%s%s ~ a2*%s%s",lvyname, dvn[[5]],lvxname, dvn[[5]])
      }
      else if(compare=="actor"|compare=="all_effects"){
        beta_y1x1 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[[4]],lvxname, dvn[[4]])
        beta_y2x2 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[[5]],lvxname, dvn[[5]])
      }
      #Partner effects
      if(compare=="none"|compare=="actor"|compare=="means"){
        beta_y1x2 = sprintf("%s%s ~ p1*%s%s",lvyname, dvn[[4]],lvxname, dvn[[5]])
        beta_y2x1 = sprintf("%s%s ~ p2*%s%s",lvyname, dvn[[5]],lvxname, dvn[[4]])
      }
      else if(compare=="partner"|compare=="all_effects"){
        beta_y1x2 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[[4]],lvxname, dvn[[5]])
        beta_y2x1 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[[5]],lvxname, dvn[[4]])
      }

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

      #Intercepts
      xints1 = list()
      xints2 = list()
      for (i in 1:dvn[[3]]) {
        xints1[[i]]=sprintf("%s ~ t%s*1", dvn[[1]][i], i)
        xints2[[i]]=sprintf("%s ~ t%s*1", dvn[[2]][i], i)
      }
      xints1 = paste(xints1, collapse = "\n")
      xints2 = paste(xints2, collapse = "\n")

      yints1 = list()
      yints2 = list()
      for (i in 1:dvn[[8]]) {
        yints1[[i]]=sprintf("%s ~ t%s*1", dvn[[6]][i], i+dvn[[3]])
        yints2[[i]]=sprintf("%s ~ t%s*1", dvn[[7]][i], i+dvn[[3]])
      }
      yints1 = paste(yints1, collapse = "\n")
      yints2 = paste(yints2, collapse = "\n")
      #Script Creation Syntax
      intercept.script = sprintf("#Loadings\n%s\n%s\n\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#Actor Effects\n%s\n%s\n\n#Partner Effects\n%s\n%s\n\n#Residuals\n%s\n\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s",
                                 eta.x1, eta.x2,eta.y1,eta.y2, psi_x1, psi_x2, psi_x1x2, psi_y1, psi_y2, psi_y1y2, beta_y1x1, beta_y2x2, beta_y1x2, beta_y2x1, resids.x, resids.y, xints1, xints2, yints1, yints2)
      cat(intercept.script,"\n", file = sprintf("%s_%s_apim_intercept.txt",lvyname,lvxname))
      return(intercept.script)
    }
}
