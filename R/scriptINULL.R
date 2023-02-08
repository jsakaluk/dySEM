#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting the I-NULL model for indistinguishable dyads
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for the I-NULL model described in Olsen & Kenny (2006)
#' @param dvn input dvn list from scrapeVarCross
#' @param lvxname input character to (arbitrarily) name X LV in lavaan syntax
#' @param lvyname (optional) input character to (arbitrarily) name Y LV in lavaan syntax
#' @param writescript input logical (default FALSE) for whether lavaan script should
#' be concatenated and written to current working directory (in subdirectory "scripts")
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions
#' @seealso \code{\link{scrapeVarCross}} which this function relies on
#' @family script-writing functions
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_",
#' x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2")
#' qual.inull.script <- scriptINULL(dvn, lvxname = "Qual")
scriptINULL = function(dvn, lvxname = "X", lvyname = NULL, writescript =FALSE){

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

  #Constrain Covariances to 0
  if(length(dvn) == 6){
    varname = c(dvn[["p1xvarnames"]], dvn[["p2xvarnames"]])
    covar.list = c()
    for (i in 1:dvn[["indnum"]]) {
      covar.list[[i]]=c(varname[i+1:dvn[["indnum"]]])
    }
    covar.list <- covar.list[-c(dvn[["indnum"]])]
    covar.list <- lapply(covar.list, function(x) x[!is.na(x)])

    store = c()
    for (i in 1:length(covar.list)) {
      store[[i]] = paste("0*",covar.list[[i]],collapse = "+")
    }

    store = stringr::str_replace_all(store, stringr::fixed(" "), "")

    covars = c()
    for (i in 1:length(covar.list)) {
      covars[[i]]=sprintf("%s ~~ %s", varname[[i]], store[[i]])
    }

    covars = paste(covars, collapse = "\n")
  }else if(length(dvn) == 9){
    varname = c(dvn[["p1xvarnames"]], dvn[["p2xvarnames"]],
                dvn[["p1yvarnames"]], dvn[["p2yvarnames"]] )
    covar.list = c()
    for (i in 1:dvn[["indnum"]]) {
      covar.list[[i]]=c(varname[i+1:dvn[["indnum"]]])
    }
    covar.list <- covar.list[-c(dvn[["indnum"]])]
    covar.list <- lapply(covar.list, function(x) x[!is.na(x)])

    store = c()
    for (i in 1:length(covar.list)) {
      store[[i]] = paste("0*",covar.list[[i]],collapse = "+")
    }

    store = stringr::str_replace_all(store, stringr::fixed(" "), "")

    covars = c()
    for (i in 1:length(covar.list)) {
      covars[[i]]=sprintf("%s ~~ %s", varname[[i]], store[[i]])
    }

    covars = paste(covars, collapse = "\n")
  }

  #Script Creation Syntax
  if(length(dvn) == 6){
    INULL.script = sprintf("#Equated Means\n%s\n%s\n\n#Equated Variances\n%s\n%s\n\n#No Covariances\n%s",
                           xints1, xints2,
                           xvars1, xvars2,
                           covars)

    if(isTRUE(writescript)){
      dirs("scripts")
      cat(INULL.script,"\n", file = sprintf("./scripts/%s_INULL.txt",lvxname))
    }

  }else if(length(dvn) == 9){
    INULL.script = sprintf("#Equated Means\n%s\n%s\n%s\n%s\n\n#Equated Variances\n%s\n%s\n%s\n%s\n\n#No Covariances\n%s",
                           xints1, xints2, yints1, yints2,
                           xvars1, xvars2, yvars1, yvars2,
                           covars)

    if(isTRUE(writescript)){
      dirs("scripts")
      cat(INULL.script,"\n", file = sprintf("./scripts/%s_%s_INULL.txt",lvxname, lvyname))
    }
  }

  return(INULL.script)
}
