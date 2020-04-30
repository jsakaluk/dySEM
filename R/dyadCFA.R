#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Latent Dyadic Measurement and Invariance Models
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying dyadic configural, loading, and intercept invariant
#' measurement models for either a specified X or Y factor.
#' @param dvn input object from dyadVarNames()
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param lvnum optional input character to indicate which LV is modeled ("one" or "two").
#' Only necessary if dvn contains both X and Y information and user wants CFA for Y
#' @param model input character used to specify which level of invariance is
#' modeled ("configural", "loading", "intercept", "residual", or "indistinguishable"). Defaults to "configural".
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' con.config.script = dyadCFA(dvn, lvname = "Conflict", model = "configural")
#' con.loading.script = dyadCFA(dvn, lvname = "Conflict",  model = "loading")
#' con.intercept.script = dyadCFA(dvn, lvname = "Conflict",  model = "intercept")
dyadCFA = function(dvn, lvname, model = "configural"){
  dirs("scripts")
  if(model == "configural"){
    #Loadings
    eta.x1 = loadings(dvn, lvname, partner="1", type = "free")
    eta.x2 = loadings(dvn, lvname, partner="2", type = "free")

    #Latent (co)variances
    psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvname, dvn[[4]],lvname, dvn[[4]])
    psi_x2 = sprintf("%s%s ~~ 1*%s%s",lvname, dvn[[5]],lvname, dvn[[5]])
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "free")
    xints2 = intercepts(dvn, partner="2", type = "free")

    #Script Creation Syntax
    configural.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, xints1, xints2)
    cat(configural.script,"\n", file = sprintf("./scripts/%s_dyadic_configural.txt",lvname))
    return(configural.script)
  }
  else if (model == "loading"){
    #Loadings
    eta.x1 = loadings(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loadings(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvname, dvn[[4]],lvname, dvn[[4]])
    psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvname, dvn[[5]],lvname, dvn[[5]])
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "free")
    xints2 = intercepts(dvn, partner="2", type = "free")

    #Script Creation Syntax
    loading.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, xints1, xints2)
    cat(loading.script,"\n", file = sprintf("./scripts/%s_dyadic_loading.txt",lvname))
    return(loading.script)
  }
  else if (model == "intercept"){
    #Loadings
    eta.x1 = loadings(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loadings(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvname, dvn[[4]],lvname, dvn[[4]])
    psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvname, dvn[[5]],lvname, dvn[[5]])
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "equated")
    xints2 = intercepts(dvn, partner="2", type = "equated")

    #Script Creation Syntax
    intercept.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, xints1, xints2)
    cat(intercept.script,"\n", file = sprintf("./scripts/%s_dyadic_intercept.txt",lvname))
    return(intercept.script)
  }
  else if (model == "residual"){
    #Loadings
    eta.x1 = loadings(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loadings(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = sprintf("%s%s ~~ 1*%s%s",lvname, dvn[[4]],lvname, dvn[[4]])
    psi_x2 = sprintf("%s%s ~~ NA*%s%s",lvname, dvn[[5]],lvname, dvn[[5]])
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    res1 = resids(dvn, partner="1", type = "equated")
    res2 = resids(dvn, partner="2", type = "equated")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "equated")
    xints2 = intercepts(dvn, partner="2", type = "equated")

    #Script Creation Syntax
    residual.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2)
    cat(residual.script,"\n", file = sprintf("./scripts/%s_dyadic_residual.txt",lvname))
    return(residual.script)
  }
  else if (model == "indistinguishable"){
    #Loadings
    eta.x1 = loadings(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loadings(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = sprintf("%s%s ~~ psi1*%s%s",lvname, dvn[[4]],lvname, dvn[[4]])
    psi_x2 = sprintf("%s%s ~~ psi1*%s%s",lvname, dvn[[5]],lvname, dvn[[5]])
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    res1 = resids(dvn, partner="1", type = "equated")
    res2 = resids(dvn, partner="2", type = "equated")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "equated")
    xints2 = intercepts(dvn, partner="2", type = "equated")

    #Script Creation Syntax
    indist.script = sprintf("#Loadings\n%s\n%s\n\n#(Co)Variances\n%s\n%s\n%s\n\n#Residuals\n%s\n%s\n%s\n\n#Intercepts\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2)
    cat(indist.script,"\n", file = sprintf("./scripts/%s_dyadic_indistinguishable.txt",lvname))
    return(indist.script)
  }
}
