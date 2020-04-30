#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting the I-NULL model for indistinguishable dyads
#'
#' This function takes the outputted object from dyadVarNames()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for the I-NULL model described in Olsen & Kenny (2006)
#' @param dvn input object from dyadVarNames()
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' isat.mod = ISAT(dvn, lvname = "X")
INULL = function(dvn, lvname){
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

  #Constrain Covariances
  varname = c(dvn[[1]], dvn[[2]])
  covar.list = c()
  for (i in 1:dvn[[6]]) {
    covar.list[[i]]=c(varname[i+1:dvn[[6]]])
  }
  covar.list <- covar.list[-c(dvn[[6]])]
  covar.list <- lapply(covar.list, function(x) x[!is.na(x)])

  store = c()
  for (i in 1:length(covar.list)) {
    store[[i]] = paste("0*",covar.list[[i]],collapse = "+")
  }

  store = stringr::str_replace_all(store, fixed(" "), "")

  #store = gsub("", "", store, fixed = TRUE)

  covars = c()
  for (i in 1:length(covar.list)) {
    covars[[i]]=sprintf("%s ~~ %s", varname[[i]], store[[i]])
  }

  covars = paste(covars, collapse = "\n")
  #covars = gsub(" ", "", covars, fixed = T)
  #covars = paste(covars, collapse = "\n")
  #covars = stringi::stri_paste_list(covars, sep = "", collapse = NULL)
  #covars = c(covars, recursive = T)

  #Variances (hacky-way to undo fixing to zero 5 variances)
  #vars1 = list()
  #vars2 = list()
  #for (i in 1:dvn[[3]]) {
    #vars1[[i]]=sprintf("%s ~~ v%s*%s",dvn[[1]][i],i, dvn[[1]][i])
    #vars2[[i]]=sprintf("%s ~~ v%s*%s",dvn[[2]][i],i, dvn[[2]][i])
  #}
  #vars1 = paste(vars1, collapse = "\n")
  #vars2 = paste(vars2, collapse = "\n")

  #Variances
  vars1 = list()
  vars2 = list()
  for (i in 1:dvn[[3]]) {
    vars1[[i]]=sprintf("%s ~~ v%s*%s",dvn[[1]][i],i, dvn[[1]][i])
    vars2[[i]]=sprintf("%s ~~ v%s*%s",dvn[[2]][i],i, dvn[[2]][i])
  }
  vars1 = paste(vars1, collapse = "\n")
  vars2 = paste(vars2, collapse = "\n")

  #Script Creation Syntax
  INULL.script = sprintf("#Equated Means\n%s\n%s\n\n#No Covariances\n%s\n\n#Equated Variances\n%s\n%s", xints1, xints2, covars, vars1, vars2)
  cat(INULL.script,"\n", file = sprintf("./scripts/%s_INULL.txt",lvname))
  return(INULL.script)
}
