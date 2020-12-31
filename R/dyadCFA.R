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
dyadCFA = function(dvn, lvname = "X", model = "configural"){
  dirs("scripts")
  if(model == "configural"){
    #Loadings
    eta.x1 = loads(dvn, lvname, partner="1", type = "free")
    eta.x2 = loads(dvn, lvname, partner="2", type = "free")

    #Latent (co)variances
    psi_x1 = lvars(dvn, lvname, partner = "1", type = "fixed")
    psi_x2 = lvars(dvn, lvname, partner = "2", type = "fixed")
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Residual variances
    res1 = resids(dvn, partner="1", type = "free")
    res2 = resids(dvn, partner="2", type = "free")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "free")
    xints2 = intercepts(dvn, partner="2", type = "free")

    #Latent Means
    alpha_x1 <- lmeans(dvn, lvname, partner="1", type = "fixed")
    alpha_x2 <- lmeans(dvn, lvname, partner="2", type = "fixed")

    #Script Creation Syntax
    configural.script = sprintf("#Loadings\n%s\n%s\n\n#Latent Variances\n%s\n%s\n\n#Latent Covariance\n%s\n\n#Residual Covariances\n%s\n\n#Residual Variances\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2, alpha_x1, alpha_x2)
    cat(configural.script,"\n", file = sprintf("./scripts/%s_dyadic_configural.txt",lvname))
    return(configural.script)
  }
  else if (model == "loading"){
    #Loadings
    eta.x1 = loads(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loads(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = lvars(dvn, lvname, partner = "1", type = "fixed")
    psi_x2 = lvars(dvn, lvname, partner = "2", type = "free")
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Residual variances
    res1 = resids(dvn, partner="1", type = "free")
    res2 = resids(dvn, partner="2", type = "free")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "free")
    xints2 = intercepts(dvn, partner="2", type = "free")

    #Latent Means
    alpha_x1 <- lmeans(dvn, lvname, partner="1", type = "fixed")
    alpha_x2 <- lmeans(dvn, lvname, partner="2", type = "fixed")

    #Script Creation Syntax
    loading.script = sprintf("#Loadings\n%s\n%s\n\n#Latent Variances\n%s\n%s\n\n#Latent Covariance\n%s\n\n#Residual Covariances\n%s\n\n#Residual Variances\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2, alpha_x1, alpha_x2)
    cat(loading.script,"\n", file = sprintf("./scripts/%s_dyadic_loading.txt",lvname))
    return(loading.script)
  }
  else if (model == "intercept"){
    #Loadings
    eta.x1 = loads(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loads(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = lvars(dvn, lvname, partner = "1", type = "fixed")
    psi_x2 = lvars(dvn, lvname, partner = "2", type = "free")
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Residual variances
    res1 = resids(dvn, partner="1", type = "free")
    res2 = resids(dvn, partner="2", type = "free")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "equated")
    xints2 = intercepts(dvn, partner="2", type = "equated")

    alpha_x1 <- lmeans(dvn, lvname, partner="1", type = "fixed")
    alpha_x2 <- lmeans(dvn, lvname, partner="2", type = "free")

    #Script Creation Syntax
    intercept.script = sprintf("#Loadings\n%s\n%s\n\n#Latent Variances\n%s\n%s\n\n#Latent Covariance\n%s\n\n#Residual Covariances\n%s\n\n#Residual Variances\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2, alpha_x1, alpha_x2)
    cat(intercept.script,"\n", file = sprintf("./scripts/%s_dyadic_intercept.txt",lvname))
    return(intercept.script)
  }
  else if (model == "residual"){
    #Loadings
    eta.x1 = loads(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loads(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = lvars(dvn, lvname, partner = "1", type = "fixed")
    psi_x2 = lvars(dvn, lvname, partner = "2", type = "free")
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Residual variances
    res1 = resids(dvn, partner="1", type = "equated")
    res2 = resids(dvn, partner="2", type = "equated")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "equated")
    xints2 = intercepts(dvn, partner="2", type = "equated")

    alpha_x1 <- lmeans(dvn, lvname, partner="1", type = "fixed")
    alpha_x2 <- lmeans(dvn, lvname, partner="2", type = "free")

    #Script Creation Syntax
    residual.script = sprintf("#Loadings\n%s\n%s\n\n#Latent Variances\n%s\n%s\n\n#Latent Covariance\n%s\n\n#Residual Covariances\n%s\n\n#Residual Variances\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2, alpha_x1, alpha_x2)
    cat(residual.script,"\n", file = sprintf("./scripts/%s_dyadic_residual.txt",lvname))
    return(residual.script)
  }
  else if (model == "lvariance"){
    #Loadings
    eta.x1 = loads(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loads(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = lvars(dvn, lvname, partner = "1", type = "equated")
    psi_x2 = lvars(dvn, lvname, partner = "2", type = "equated")
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Residual variances
    res1 = resids(dvn, partner="1", type = "free")
    res2 = resids(dvn, partner="2", type = "free")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "free")
    xints2 = intercepts(dvn, partner="2", type = "free")

    #Latent Means
    alpha_x1 <- lmeans(dvn, lvname, partner="1", type = "fixed")
    alpha_x2 <- lmeans(dvn, lvname, partner="2", type = "fixed")

    #Script Creation Syntax
    lvariance.script = sprintf("#Loadings\n%s\n%s\n\n#Latent Variances\n%s\n%s\n\n#Latent Covariance\n%s\n\n#Residual Covariances\n%s\n\n#Residual Variances\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2, alpha_x1, alpha_x2)
    cat(lvariance.script,"\n", file = sprintf("./scripts/%s_dyadic_lvariance.txt",lvname))
    return(lvariance.script)
  }
  else if (model == "lmean"){
    #Loadings
    eta.x1 = loads(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loads(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = lvars(dvn, lvname, partner = "1", type = "fixed")
    psi_x2 = lvars(dvn, lvname, partner = "2", type = "free")
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Residual variances
    res1 = resids(dvn, partner="1", type = "free")
    res2 = resids(dvn, partner="2", type = "free")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "equated")
    xints2 = intercepts(dvn, partner="2", type = "equated")

    alpha_x1 <- lmeans(dvn, lvname, partner="1", type = "equated")
    alpha_x2 <- lmeans(dvn, lvname, partner="2", type = "equated")

    #Script Creation Syntax
    lmean.script = sprintf("#Loadings\n%s\n%s\n\n#Latent Variances\n%s\n%s\n\n#Latent Covariance\n%s\n\n#Residual Covariances\n%s\n\n#Residual Variances\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2, alpha_x1, alpha_x2)
    cat(lmean.script,"\n", file = sprintf("./scripts/%s_dyadic_lmean.txt",lvname))
    return(lmean.script)
  }
  else if (model == "indistinguishable"){
    #Loadings
    eta.x1 = loads(dvn, lvname, partner="1", type = "equated")
    eta.x2 = loads(dvn, lvname, partner="2", type = "equated")

    #Latent (co)variances
    psi_x1 = lvars(dvn, lvname, partner = "1", type = "equated")
    psi_x2 = lvars(dvn, lvname, partner = "2", type = "equated")
    psi_x1x2 = sprintf("%s%s ~~ %s%s",lvname, dvn[[4]],lvname, dvn[[5]])

    #Correlated residuals
    resids = list()
    for (i in 1:dvn[[3]]) {
      resids[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[2]][i])
    }
    resids = paste(resids, collapse = "\n")

    #Residual variances
    res1 = resids(dvn, partner="1", type = "equated")
    res2 = resids(dvn, partner="2", type = "equated")

    #Intercepts
    xints1 = intercepts(dvn, partner="1", type = "equated")
    xints2 = intercepts(dvn, partner="2", type = "equated")

    alpha_x1 <- lmeans(dvn, lvname, partner="1", type = "equated")
    alpha_x2 <- lmeans(dvn, lvname, partner="2", type = "equated")

    #Script Creation Syntax
    indist.script = sprintf("#Loadings\n%s\n%s\n\n#Latent Variances\n%s\n%s\n\n#Latent Covariance\n%s\n\n#Residual Covariances\n%s\n\n#Residual Variances\n%s\n%s\n\n#Intercepts\n%s\n%s\n\n#Latent Means\n%s\n%s", eta.x1, eta.x2, psi_x1, psi_x2, psi_x1x2, resids, res1, res2, xints1, xints2, alpha_x1, alpha_x2)
    cat(indist.script,"\n", file = sprintf("./scripts/%s_dyadic_indistinguishable.txt",lvname))
    return(indist.script)
  }
}
