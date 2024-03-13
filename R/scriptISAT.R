#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting the I-SAT model for indistinguishable dyads
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for the I-SAT model described in Olsen & Kenny (2006)
#' @param dvn input dvn list from scrapeVarCross
#' @param lvxname input character to (arbitrarily) name X LV in lavaan syntax
#' @param lvyname (optional) input character to (arbitrarily) name X LV in lavaan syntax
#' @param writeTo A character string specifying a directory path to where a .txt file of the resulting lavaan script should be written.
#' If set to “.”, the .txt file will be written to the current working directory.
#' The default is NULL, and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the .txt output file.
#' The default is NULL. The specified name will be automatically appended with the .txt file extension.
#' If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{scrapeVarCross}} which this function relies on
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_",
#' x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2")
#'
#' qual.isat.script <- scriptISAT(dvn, lvxname = "Qual",
#' writeTo = tempdir(),
#' fileName = "I-SAT_script")
scriptISAT = function(dvn, lvxname = "X", lvyname = NULL,
                      writeTo = NULL,
                      fileName = NULL){

  #Intercepts equated between partners
  xints1 = intercepts(dvn, lvar = "X", partner = "1", type  = "equated")
  xints2 = intercepts(dvn, lvar = "X", partner = "2", type  = "equated")

  #If y-variable, equate their intercepts too
  if(length(dvn) == 9){
    yints1 = intercepts(dvn, lvar = "Y", partner = "1", type  = "equated")
    yints2 = intercepts(dvn, lvar = "Y", partner = "2", type  = "equated")
  }

  #Variances equated between partners
  xvars1 <- resids(dvn, lvar = "X", partner = "1", type = "equated")
  xvars2 <- resids(dvn, lvar = "X", partner = "2", type = "equated")

  #If y-variable, equate their variances too
  if(length(dvn) == 9){
    yvars1 <- resids(dvn, lvar = "Y", partner = "1", type = "equated")
    yvars2 <- resids(dvn, lvar = "Y", partner = "2", type = "equated")
  }

  #Covariances
  #Intrapersonal: p1
  xintra1.list = rep(list(NA),dvn[["xindper"]]-1)
  xintra1.list[[1]]= dvn[["p1xvarnames"]]

  for (i in 2:dvn[["xindper"]] - 1) {
    xintra1.list[[i]] =c(dplyr::lead(dvn[["p1xvarnames"]], n = i-1))
  }

  for (i in 1:length(xintra1.list)) {
    xintra1.list[[i]] = xintra1.list[[i]][-1]
  }

  xintra1.list <- lapply(xintra1.list, function(x) x[!is.na(x)])

  for (i in 1:length(xintra1.list)){
    for (j in 1:length(xintra1.list[[i]])){
      xintra1.list[[i]][j] = sprintf("itax%s%s*%s",i,j,xintra1.list[[i]][j])
    }
  }

  for (i in 1:length(xintra1.list)) {
    xintra1.list[[i]] = paste(xintra1.list[[i]], collapse = "+")
  }

  xintra1covs = list()
  for (i in 1:length(xintra1.list)) {
    xintra1covs[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], xintra1.list[[i]])
  }

  xintra1covs = paste(xintra1covs, collapse = "\n")

  #Intrapersonal: p2
  xintra2.list = rep(list(NA),dvn[["xindper"]]-1)
  xintra2.list[[1]]= dvn[["p2xvarnames"]]

  for (i in 2:dvn[["xindper"]] - 1) {
    xintra2.list[[i]] =c(dplyr::lead(dvn[["p2xvarnames"]], n = i-1))
  }

  for (i in 1:length(xintra2.list)) {
    xintra2.list[[i]] = xintra2.list[[i]][-1]
  }

  xintra2.list <- lapply(xintra2.list, function(x) x[!is.na(x)])

  for (i in 1:length(xintra2.list)){
    for (j in 1:length(xintra2.list[[i]])){
      xintra2.list[[i]][j] = sprintf("itax%s%s*%s",i,j,xintra2.list[[i]][j])
    }
  }

  for (i in 1:length(xintra2.list)) {
    xintra2.list[[i]] = paste(xintra2.list[[i]], collapse = "+")
  }

  xintra2covs = list()
  for (i in 1:length(xintra2.list)) {
    xintra2covs[[i]]=sprintf("%s ~~ %s",dvn[["p2xvarnames"]][i], xintra2.list[[i]])
  }
  xintra2covs = paste(xintra2covs, collapse = "\n")

  if(length(dvn) == 9){
    #Covariances
    #Intrapersonal: p1
    yintra1.list = rep(list(NA),dvn[["yindper"]]-1)
    yintra1.list[[1]]= dvn[["p1yvarnames"]]

    for (i in 2:dvn[["yindper"]] - 1) {
      yintra1.list[[i]] =c(dplyr::lead(dvn[["p1yvarnames"]], n = i-1))
    }

    for (i in 1:length(yintra1.list)) {
      yintra1.list[[i]] = yintra1.list[[i]][-1]
    }

    yintra1.list <- lapply(yintra1.list, function(x) x[!is.na(x)])

    for (i in 1:length(yintra1.list)){
      for (j in 1:length(yintra1.list[[i]])){
        yintra1.list[[i]][j] = sprintf("itay%s%s*%s",i,j,yintra1.list[[i]][j])
      }
    }

    for (i in 1:length(yintra1.list)) {
      yintra1.list[[i]] = paste(yintra1.list[[i]], collapse = "+")
    }

    yintra1covs = list()
    for (i in 1:length(yintra1.list)) {
      yintra1covs[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], yintra1.list[[i]])
    }

    yintra1covs = paste(yintra1covs, collapse = "\n")

    #Intrapersonal: p2
    yintra2.list = rep(list(NA),dvn[["yindper"]]-1)
    yintra2.list[[1]]= dvn[["p2yvarnames"]]

    for (i in 2:dvn[["yindper"]] - 1) {
      yintra2.list[[i]] =c(dplyr::lead(dvn[["p2yvarnames"]], n = i-1))
    }

    for (i in 1:length(yintra2.list)) {
      yintra2.list[[i]] = yintra2.list[[i]][-1]
    }

    yintra2.list <- lapply(yintra2.list, function(x) x[!is.na(x)])

    for (i in 1:length(yintra2.list)){
      for (j in 1:length(yintra2.list[[i]])){
        yintra2.list[[i]][j] = sprintf("itay%s%s*%s",i,j,yintra2.list[[i]][j])
      }
    }

    for (i in 1:length(yintra2.list)) {
      yintra2.list[[i]] = paste(yintra2.list[[i]], collapse = "+")
    }

    yintra2covs = list()
    for (i in 1:length(yintra2.list)) {
      yintra2covs[[i]]=sprintf("%s ~~ %s",dvn[["p2yvarnames"]][i], yintra2.list[[i]])
    }
    yintra2covs = paste(yintra2covs, collapse = "\n")
  }

  #Interpersonal: p1
  xinter1.list = rep(list(NA),dvn[["xindper"]]-1)
  xinter1.list[[1]]= dvn[["p2xvarnames"]]

  for (i in 2:dvn[["xindper"]] - 1) {
    xinter1.list[[i]] =c(dplyr::lead(dvn[["p2xvarnames"]], n = i-1))
  }

  for (i in 1:length(xinter1.list)) {
    xinter1.list[[i]] = xinter1.list[[i]][-1]
  }

  xinter1.list <- lapply(xinter1.list, function(x) x[!is.na(x)])

  for (i in 1:length(xinter1.list)){
    for (j in 1:length(xinter1.list[[i]])){
      xinter1.list[[i]][j] = sprintf("itrx%s%s*%s",i,j,xinter1.list[[i]][j])
    }
  }

  for (i in 1:length(xinter1.list)) {
    xinter1.list[[i]] = paste(xinter1.list[[i]], collapse = "+")
  }

  xinter1covs = list()
  for (i in 1:length(xinter1.list)) {
    xinter1covs[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], xinter1.list[[i]])
  }
  xinter1covs = paste(xinter1covs, collapse = "\n")

  #Interpersonal: p2
  xinter2.list = rep(list(NA),dvn[["xindper"]]-1)
  xinter2.list[[1]]= dvn[["p1xvarnames"]]

  for (i in 2:dvn[["xindper"]] - 1) {
    xinter2.list[[i]] =c(dplyr::lead(dvn[["p1xvarnames"]], n = i-1))
  }

  for (i in 1:length(xinter2.list)) {
    xinter2.list[[i]] = xinter2.list[[i]][-1]
  }

  xinter2.list <- lapply(xinter2.list, function(x) x[!is.na(x)])

  for (i in 1:length(xinter2.list)){
    for (j in 1:length(xinter2.list[[i]])){
      xinter2.list[[i]][j] = sprintf("itrx%s%s*%s",i,j,xinter2.list[[i]][j])
    }
  }

  for (i in 1:length(xinter2.list)) {
    xinter2.list[[i]] = paste(xinter2.list[[i]], collapse = "+")
  }

  xinter2covs = list()
  for (i in 1:length(xinter2.list)) {
    xinter2covs[[i]]=sprintf("%s ~~ %s",dvn[["p2xvarnames"]][i], xinter2.list[[i]])
  }
  xinter2covs = paste(xinter2covs, collapse = "\n")

  if(length(dvn) == 9){
    #Interpersonal: p1
    yinter1.list = rep(list(NA),dvn[["yindper"]]-1)
    yinter1.list[[1]]= dvn[["p2yvarnames"]]

    for (i in 2:dvn[["yindper"]] - 1) {
      yinter1.list[[i]] =c(dplyr::lead(dvn[["p2yvarnames"]], n = i-1))
    }

    for (i in 1:length(yinter1.list)) {
      yinter1.list[[i]] = yinter1.list[[i]][-1]
    }

    yinter1.list <- lapply(yinter1.list, function(x) x[!is.na(x)])

    for (i in 1:length(yinter1.list)){
      for (j in 1:length(yinter1.list[[i]])){
        yinter1.list[[i]][j] = sprintf("itry%s%s*%s",i,j,yinter1.list[[i]][j])
      }
    }

    for (i in 1:length(yinter1.list)) {
      yinter1.list[[i]] = paste(yinter1.list[[i]], collapse = "+")
    }

    yinter1covs = list()
    for (i in 1:length(yinter1.list)) {
      yinter1covs[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], yinter1.list[[i]])
    }
    yinter1covs = paste(yinter1covs, collapse = "\n")

    #Interpersonal: p2
    yinter2.list = rep(list(NA),dvn[["yindper"]]-1)
    yinter2.list[[1]]= dvn[["p1yvarnames"]]

    for (i in 2:dvn[["yindper"]] - 1) {
      yinter2.list[[i]] =c(dplyr::lead(dvn[["p1yvarnames"]], n = i-1))
    }

    for (i in 1:length(yinter2.list)) {
      yinter2.list[[i]] = yinter2.list[[i]][-1]
    }

    yinter2.list <- lapply(yinter2.list, function(x) x[!is.na(x)])

    for (i in 1:length(yinter2.list)){
      for (j in 1:length(yinter2.list[[i]])){
        yinter2.list[[i]][j] = sprintf("itry%s%s*%s",i,j,yinter2.list[[i]][j])
      }
    }

    for (i in 1:length(yinter2.list)) {
      yinter2.list[[i]] = paste(yinter2.list[[i]], collapse = "+")
    }

    yinter2covs = list()
    for (i in 1:length(yinter2.list)) {
      yinter2covs[[i]]=sprintf("%s ~~ %s",dvn[["p2yvarnames"]][i], yinter2.list[[i]])
    }
    yinter2covs = paste(yinter2covs, collapse = "\n")
  }

  #Free Interpersonal of Same Variable
  xfree.cor = list()
  for (i in 1:dvn[["xindper"]]) {
    xfree.cor[[i]]=sprintf("%s ~~ NA*%s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
  }
  xfree.cor = paste(xfree.cor, collapse = "\n")

  if(length(dvn) == 9){
    yfree.cor = list()
    for (i in 1:dvn[["yindper"]]) {
      yfree.cor[[i]]=sprintf("%s ~~ NA*%s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
    }
    yfree.cor = paste(yfree.cor, collapse = "\n")
  }

  #Script Creation Syntax
  if(length(dvn) == 6){
    script = sprintf("#Constrained Means\n%s\n%s\n\n#Constrained Variances\n%s\n%s\n\n#Constrained Intrapersonal Covariances\n%s\n\n\n%s\n\n#Constrained Interpersonal Covariancesn\n%s\n\n%s\n\n#Estimate Same-Indicator Covariances\n%s",
                          xints1, xints2,
                          xvars1, xvars2,
                          xintra1covs, xintra2covs,
                          xinter1covs, xinter2covs,
                          xfree.cor)

  }else if(length(dvn) == 9){
    script = sprintf("#Constrained Means\n%s\n%s\n\n%s\n%s\n\n#Constrained Variances\n%s\n%s\n\n%s\n%s\n\n#Constrained Intrapersonal Covariances\n%s\n\n\n%s\n\n%s\n\n\n%s\n\n#Constrained Interpersonal Covariancesn\n%s\n\n%s\n\n%s\n\n%s\n\n#Estimate Same-Indicator Covariances\n%s\n\n%s",
                          xints1, xints2, yints1, yints2,
                          xvars1, xvars2, yvars1, yvars2,
                          xintra1covs, xintra2covs, yintra1covs, yintra2covs,
                          xinter1covs, xinter2covs, yinter1covs, yinter2covs,
                          xfree.cor, yfree.cor)

  }

  #Write script to file if requested
  if(!is.null(writeTo) | !is.null(fileName) ){
    #if there is a path or file name,
    #check for valid input,
    #and if valid, write script

    # checking for valid directory path and fileName
    if (!is.character(writeTo)){
      stop("The `writeout` argument must be a character string. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!dir.exists(writeTo)){
      stop("The specified directory does not exist. \n Use writeTo = '.' to save script in the current working directory, for example.")
    }
    if (!is.character(fileName)){
      stop("The `fileName` argument must be a character string.")
    }

    #write file
    cat(script, "\n",
        file = sprintf("%s/%s.txt",
                       writeTo,
                       fileName))

    return(script)
  }
  else if(is.null(writeTo) & is.null(fileName)){
    #otherwise just return script
    return(script)
  }
}
