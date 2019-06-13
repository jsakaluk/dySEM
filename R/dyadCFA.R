#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Dyadic Measurement and Invariance Models
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' measurement models for either a specified X or Y factor.
#' @param dvn input object from dyadVarNames()
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param lv input character to indicate which LV is modeled (default is "X")
#' @param model input character used to specify which level of invariance is
#' modeled. Defaults to "configural"
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", yvar="Y", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' con.config.script = dyadCFA(dvn, lvxname = "Conflict", lv="X", model = "configural")
#' con.loading.script = dyadCFA(dvn, lvxname = "Conflict", lv="X", model = "loading")
#' con.intercept.script = dyadCFA(dvn, lvxname = "Conflict", lv="X", model = "intercept")
dyadCFA = function(dvn, lvxname, lvyname, lv="X", model = "configural"){
  if(lv=="X"){
    if(model == "configural"){
      #Loadings
      eta_x1 = sprintf("%s%s =~ NA*",lvxname, dvn[[4]])
      eta.x1 = gsub(" ", "",paste(eta_x1,paste(dvn[[1]], collapse = "+")), fixed = T)
      eta_x2 = sprintf("%s%s =~ NA*",lvxname, dvn[[5]])
      eta.x2 = gsub(" ", "",paste(eta_x2,paste(dvn[[2]], collapse = "+")), fixed = T)

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvxname, dvn[[5]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[3]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Intercepts
      xints1 = list()
      xints2 = list()
      for (i in 1:dvn[[3]]) {
        xints1[[i]]=sprintf("%s ~ 1", dvn[[1]][i])
        xints2[[i]]=sprintf("%s ~ 1", dvn[[2]][i])
      }
      xints1 = paste(xints1, collapse = "\n")
      xints2 = paste(xints2, collapse = "\n")

      #Script Creation Syntax
      configural.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, xints1, xints2)
      cat(configural.script,"\n", file = sprintf("%s_dyadic_configural.txt",lvxname))
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

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvxname, dvn[[5]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[3]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Intercepts
      xints1 = list()
      xints2 = list()
      for (i in 1:dvn[[3]]) {
        xints1[[i]]=sprintf("%s ~ 1", dvn[[1]][i])
        xints2[[i]]=sprintf("%s ~ 1", dvn[[2]][i])
      }
      xints1 = paste(xints1, collapse = "\n")
      xints2 = paste(xints2, collapse = "\n")

      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, xints1, xints2)
      cat(loading.script,"\n", file = sprintf("%s_dyadic_loading.txt",lvxname))
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

      #Latent (co)variances
      psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvxname, dvn[[4]],lvxname, dvn[[4]])
      psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvxname, dvn[[5]],lvxname, dvn[[5]])
      psi_x1x2 = sprintf("%s%s ~~ %s%s",lvxname, dvn[[4]],lvxname, dvn[[5]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[3]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Intercepts
      xints1 = list()
      xints2 = list()
      for (i in 1:dvn[[3]]) {
        xints1[[i]]=sprintf("%s ~ t%s*1", dvn[[1]][i], i)
        xints2[[i]]=sprintf("%s ~ t%s*1", dvn[[2]][i], i)
      }
      xints1 = paste(xints1, collapse = "\n")
      xints2 = paste(xints2, collapse = "\n")

      #Script Creation Syntax
      intercept.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, xints1, xints2)
      cat(intercept.script,"\n", file = sprintf("%s_dyadic_intercept.txt",lvxname))
      return(intercept.script)
    }
  }
  else if(lv=="Y"){
    if(model == "configural"){
      #Loadings
      eta_y1 = sprintf("%s%s =~ NA*",lvyname, dvn[[4]])
      eta.y1 = gsub(" ", "",paste(eta_y1,paste(dvn[[6]], collapse = "+")), fixed = T)
      eta_y2 = sprintf("%s%s =~ NA*",lvyname, dvn[[5]])
      eta.y2 = gsub(" ", "",paste(eta_y2,paste(dvn[[7]], collapse = "+")), fixed = T)

      #Latent (co)variances
      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[[4]],lvyname, dvn[[5]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[8]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Intercepts
      yints1 = list()
      yints2 = list()
      for (i in 1:dvn[[8]]) {
        yints1[[i]]=sprintf("%s ~ 1", dvn[[6]][i])
        yints2[[i]]=sprintf("%s ~ 1", dvn[[7]][i])
      }
      yints1 = paste(yints1, collapse = "\n")
      yints2 = paste(yints2, collapse = "\n")

      #Script Creation Syntax
      configural.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.y1, eta.y2, psi_y1, psi_y2, psi_y1y2, resids, yints1, yints2)
      cat(configural.script,"\n", file = sprintf("%s_dyadic_configural.txt",lvyname))
      return(configural.script)
    }
    else if (model == "loading"){
      #Loadings
      eta_y1 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[4]], dvn[[6]][1])
      eta.y1 = list()
      for (i in 1:dvn[[8]]) {
        eta.y1[[i]]=sprintf("l%s*%s",i, dvn[[6]][i])
      }
      eta.y1 = gsub(" ", "",paste(eta_y1,paste(eta.y1, collapse = "+")), fixed = T)

      eta_y2 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[5]], dvn[[7]][1])
      eta.y2 = list()
      for (i in 1:dvn[[8]]) {
        eta.y2[[i]]=sprintf("l%s*%s",i, dvn[[7]][i])
      }
      eta.y2 = gsub(" ", "",paste(eta_y2,paste(eta.y2, collapse = "+")), fixed = T)

      #Latent (co)variances
      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ NA*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[[4]],lvyname, dvn[[5]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[8]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Intercepts
      yints1 = list()
      yints2 = list()
      for (i in 1:dvn[[8]]) {
        yints1[[i]]=sprintf("%s ~ 1", dvn[[6]][i])
        yints2[[i]]=sprintf("%s ~ 1", dvn[[7]][i])
      }
      yints1 = paste(yints1, collapse = "\n")
      yints2 = paste(yints2, collapse = "\n")

      #Script Creation Syntax
      loading.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.y1, eta.y2, psi_y1, psi_y2, psi_y1y2, resids, yints1, yints2)
      cat(loading.script,"\n", file = sprintf("%s_dyadic_loading.txt",lvyname))
      return(loading.script)
    }
    else if (model == "intercept"){
      #Loadings
      eta_y1 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[4]], dvn[[6]][1])
      eta.y1 = list()
      for (i in 1:dvn[[8]]) {
        eta.y1[[i]]=sprintf("l%s*%s",i, dvn[[6]][i])
      }
      eta.y1 = gsub(" ", "",paste(eta_y1,paste(eta.y1, collapse = "+")), fixed = T)

      eta_y2 = sprintf("%s%s =~ NA*%s+",lvyname, dvn[[5]], dvn[[7]][1])
      eta.y2 = list()
      for (i in 1:dvn[[8]]) {
        eta.y2[[i]]=sprintf("l%s*%s",i, dvn[[7]][i])
      }
      eta.y2 = gsub(" ", "",paste(eta_y2,paste(eta.y2, collapse = "+")), fixed = T)

      #Latent (co)variances
      psi_y1 = sprintf("%s%s ~~ 1*%s%s",lvyname, dvn[[4]],lvyname, dvn[[4]])
      psi_y2 = sprintf("%s%s ~~ NA*%s%s",lvyname, dvn[[5]],lvyname, dvn[[5]])
      psi_y1y2 = sprintf("%s%s ~~ %s%s",lvyname, dvn[[4]],lvyname, dvn[[5]])

      #Correlated residuals
      resids = list()
      for (i in 1:dvn[[8]]) {
        resids[[i]]=sprintf("%s ~~ %s",dvn[[6]][i], dvn[[7]][i])
      }
      resids = paste(resids, collapse = "\n")

      #Intercepts
      yints1 = list()
      yints2 = list()
      for (i in 1:dvn[[8]]) {
        yints1[[i]]=sprintf("%s ~ t%s*1", dvn[[6]][i], i)
        yints2[[i]]=sprintf("%s ~ t%s*1", dvn[[7]][i], i)
      }
      yints1 = paste(yints1, collapse = "\n")
      yints2 = paste(yints2, collapse = "\n")

      #Script Creation Syntax
      intercept.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.y1, eta.y2, psi_y1, psi_y2, psi_y1y2, resids, yints1, yints2)
      cat(intercept.script,"\n", file = sprintf("%s_dyadic_intercept.txt",lvyname))
      return(intercept.script)
    }
  }
}
