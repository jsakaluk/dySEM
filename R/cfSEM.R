#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Common Fate Models
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying Common Fate Models (CFMs). Users can
#' also invoke configural, loading, and/or intercept invariant
#' measurement models, and particular types of structural comparisons.
#' @param dvn input object from dyadVarNames()
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param model input character used to specify which level of invariance is
#' modeled. Defaults to "configural"
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions. Users will receive message if structural comparisons are specified
#' when the recommended level of invariance is not also specified. If user supplies dvn
#' with containing X or Y variables, they are alerted to respecify the dvn object.
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", yvar="Y", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' cfm.script.config = cfSEM(dvn, lvxname = "Conflict", lvyname = "Satisfaction", model = "configural")

cfSEM = function(dvn, lvxname, lvyname, model = "configural"){
  dirs("scripts")
  if(length(dvn)==9){
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

      eta.cx = sprintf("%s =~ lx*%s%s + lx*%s%s", lvxname, lvxname, dvn[[4]], lvxname, dvn[[5]])
      eta.cy = sprintf("%s =~ ly*%s%s + ly*%s%s", lvyname, lvyname, dvn[[4]], lvyname, dvn[[5]])

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])

      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])

      psi_x1y1 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      psi_x = sprintf("%s ~~ 1*%s", lvxname, lvxname)
      psi_y = sprintf("%s ~~ 1*%s", lvyname, lvyname)

      #Common Fate effects
      beta_yx = sprintf("%s ~ %s",lvyname, lvxname)

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
      configural.script = sprintf("#Loadings\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n\n%s\n%s\n\n%s\n%s\n\n#Common Fate Effects\n%s\n\n#Residuals\n%s\n\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s",
                                  eta.x1, eta.x2, eta.cx, eta.y1,eta.y2, eta.cy, psi_x1, psi_x2, psi_y1, psi_y2, psi_x1y1, psi_x2y2, beta_yx, resids.x, resids.y, xints1, xints2, yints1, yints2)
      cat(configural.script,"\n", file = sprintf("./scripts/%s_%s_cfm_configural.txt",lvyname,lvxname))
      cat(crayon::yellow("Caution: Common fate models should probably be fit with loading invariance "))

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

      eta.cx = sprintf("%s =~ lx*%s%s + lx*%s%s", lvxname, lvxname, dvn[[4]], lvxname, dvn[[5]])
      eta.cy = sprintf("%s =~ ly*%s%s + ly*%s%s", lvyname, lvyname, dvn[[4]], lvyname, dvn[[5]])

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])

      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ NA*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])

      psi_x1y1 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      psi_x = sprintf("%s ~~ 1*%s", lvxname, lvxname)
      psi_y = sprintf("%s ~~ 1*%s", lvyname, lvyname)

      #Common fate effects
      beta_yx = sprintf("%s ~ %s",lvyname, lvxname)

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
      loading.script = sprintf("#Loadings\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n\n%s\n%s\n\n%s\n%s\n\n#Common Fate Effects\n%s\n\n#Residuals\n%s\n\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s",
                               eta.x1, eta.x2, eta.cx, eta.y1,eta.y2, eta.cy, psi_x1, psi_x2, psi_y1, psi_y2, psi_x1y1, psi_x2y2, beta_yx, resids.x, resids.y, xints1, xints2, yints1, yints2)
      cat(loading.script,"\n", file = sprintf("./scripts/%s_%s_cfm_loading.txt",lvyname,lvxname))

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

      eta.cx = sprintf("%s =~ lx*%s%s + lx*%s%s", lvxname, lvxname, dvn[[4]], lvxname, dvn[[5]])
      eta.cy = sprintf("%s =~ ly*%s%s + ly*%s%s", lvyname, lvyname, dvn[[4]], lvyname, dvn[[5]])

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])

      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ NA*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])

      psi_x1y1 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvyname, dvn[[4]])
      psi_x2y2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[5]],lvyname, dvn[[5]])

      psi_x = sprintf("%s ~~ 1*%s", lvxname, lvxname)
      psi_y = sprintf("%s ~~ 1*%s", lvyname, lvyname)

      #Common fate effects
      beta_yx = sprintf("%s ~ %s",lvyname, lvxname)

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
      intercept.script = sprintf("#Loadings\n%s\n%s\n%s\n\n%s\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n\n%s\n%s\n\n%s\n%s\n\n#Common Fate Effects\n%s\n\n#Residuals\n%s\n\n%s\n\n#Intercepts\n%s\n%s\n%s\n%s",
                                 eta.x1, eta.x2, eta.cx, eta.y1,eta.y2, eta.cy, psi_x1, psi_x2, psi_y1, psi_y2, psi_x1y1, psi_x2y2, beta_yx, resids.x, resids.y, xints1, xints2, yints1, yints2)
      cat(intercept.script,"\n", file = sprintf("./scripts/%s_%s_cfm_intercept.txt",lvyname,lvxname))
      return(intercept.script)
    }
  }
  else if(length(dvn)==6){
    cat(crayon::red("You must supply a dvn object containing information for both X and Y"))
  }
}
