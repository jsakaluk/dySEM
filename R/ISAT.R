#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting the I-SAT model for indistinguishable dyads
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for the I-SAT model described in Olsen & Kenny (2006)
#' @param dvn input object from dyadVarNames()
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' isat.mod = ISAT(dvn, lvname = "X")
ISAT = function(dvn, lvname){
  dirs("scripts")

  #Means
  xints1 = list()
  xints2 = list()
  for (i in 1:dvn[[3]]) {
    xints1[[i]]=sprintf("%s ~ t%s*1", dvn[[1]][i], i)
    xints2[[i]]=sprintf("%s ~ t%s*1", dvn[[2]][i], i)
  }
  xints1 = paste(xints1, collapse = "\n")
  xints2 = paste(xints2, collapse = "\n")

  #Variances
  vars1 = list()
  vars2 = list()
  for (i in 1:dvn[[3]]) {
    vars1[[i]]=sprintf("%s ~~ v%s*%s",dvn[[1]][i],i, dvn[[1]][i])
    vars2[[i]]=sprintf("%s ~~ v%s*%s",dvn[[2]][i],i, dvn[[2]][i])
  }
  vars1 = paste(vars1, collapse = "\n")
  vars2 = paste(vars2, collapse = "\n")

  #Covariances
  #Intrapersonal: p1
  intra1.list = rep(list(NA),dvn[[3]]-1)
  intra1.list[[1]]= dvn[[1]]

  for (i in 2:dvn[[3]] - 1) {
    intra1.list[[i]] =c(dplyr::lead(dvn[[1]], n = i-1))
  }

  for (i in 1:length(intra1.list)) {
    intra1.list[[i]] = intra1.list[[i]][-1]
  }

  intra1.list <- lapply(intra1.list, function(x) x[!is.na(x)])

  for (i in 1:length(intra1.list)){
    for (j in 1:length(intra1.list[[i]])){
      intra1.list[[i]][j] = sprintf("ita%s%s*%s",i,j,intra1.list[[i]][j])
    }
  }

  for (i in 1:length(intra1.list)) {
    intra1.list[[i]] = paste(intra1.list[[i]], collapse = "+")
  }

  intra1covs = list()
  for (i in 1:length(intra1.list)) {
    intra1covs[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], intra1.list[[i]])
  }

  intra1covs = paste(intra1covs, collapse = "\n")

  #Intrapersonal: p2
  intra2.list = rep(list(NA),dvn[[3]]-1)
  intra2.list[[1]]= dvn[[2]]

  for (i in 2:dvn[[3]] - 1) {
    intra2.list[[i]] =c(dplyr::lead(dvn[[2]], n = i-1))
  }

  for (i in 1:length(intra2.list)) {
    intra2.list[[i]] = intra2.list[[i]][-1]
  }

  intra2.list <- lapply(intra2.list, function(x) x[!is.na(x)])

  for (i in 1:length(intra2.list)){
    for (j in 1:length(intra2.list[[i]])){
      intra2.list[[i]][j] = sprintf("ita%s%s*%s",i,j,intra2.list[[i]][j])
    }
  }

  for (i in 1:length(intra2.list)) {
    intra2.list[[i]] = paste(intra2.list[[i]], collapse = "+")
  }

  intra2covs = list()
  for (i in 1:length(intra2.list)) {
    intra2covs[[i]]=sprintf("%s ~~ %s",dvn[[2]][i], intra2.list[[i]])
  }
  intra2covs = paste(intra2covs, collapse = "\n")
  #Interpersonal: p1
  inter1.list = rep(list(NA),dvn[[3]]-1)
  inter1.list[[1]]= dvn[[2]]

  for (i in 2:dvn[[3]] - 1) {
    inter1.list[[i]] =c(dplyr::lead(dvn[[2]], n = i-1))
  }

  for (i in 1:length(inter1.list)) {
    inter1.list[[i]] = inter1.list[[i]][-1]
  }

  inter1.list <- lapply(inter1.list, function(x) x[!is.na(x)])

  for (i in 1:length(inter1.list)){
    for (j in 1:length(inter1.list[[i]])){
      inter1.list[[i]][j] = sprintf("itr%s%s*%s",i,j,inter1.list[[i]][j])
    }
  }

  for (i in 1:length(inter1.list)) {
    inter1.list[[i]] = paste(inter1.list[[i]], collapse = "+")
  }

  inter1covs = list()
  for (i in 1:length(inter1.list)) {
    inter1covs[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], inter1.list[[i]])
  }
  inter1covs = paste(inter1covs, collapse = "\n")
  #Interpersonal: p2
  inter2.list = rep(list(NA),dvn[[3]]-1)
  inter2.list[[1]]= dvn[[1]]

  for (i in 2:dvn[[3]] - 1) {
    inter2.list[[i]] =c(dplyr::lead(dvn[[1]], n = i-1))
  }

  for (i in 1:length(inter2.list)) {
    inter2.list[[i]] = inter2.list[[i]][-1]
  }

  inter2.list <- lapply(inter2.list, function(x) x[!is.na(x)])

  for (i in 1:length(inter2.list)){
    for (j in 1:length(inter2.list[[i]])){
      inter2.list[[i]][j] = sprintf("itr%s%s*%s",i,j,inter2.list[[i]][j])
    }
  }

  for (i in 1:length(inter2.list)) {
    inter2.list[[i]] = paste(inter2.list[[i]], collapse = "+")
  }

  inter2covs = list()
  for (i in 1:length(inter2.list)) {
    inter2covs[[i]]=sprintf("%s ~~ %s",dvn[[2]][i], inter2.list[[i]])
  }
  inter2covs = paste(inter2covs, collapse = "\n")

  #Free Interpersonal of Same Variable
  free.cor = list()
  for (i in 1:dvn[[3]]) {
    free.cor[[i]]=sprintf("%s ~~ NA*%s",dvn[[1]][i], dvn[[2]][i])
  }
  free.cor = paste(free.cor, collapse = "\n")

  #Script Creation Syntax
  ISAT.script = sprintf("#Constrained Means\n%s\n%s\n\n#Constrained Variances\n%s\n%s\n\n#Constrained Intrapersonal Covariances\n%s\n\n\n%s\n\n#Constrained Interpersonal Covariancesn\n%s\n\n%s\n\n#Estimate Same-Indicator Covariances\n%s", xints1, xints2, vars1, vars2, intra1covs, intra2covs, inter1covs, inter2covs, free.cor)
  cat(ISAT.script,"\n", file = sprintf("./scripts/%s_ISAT.txt",lvname))
  return(ISAT.script)
}
