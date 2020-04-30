#' @name cfa_funs
#' @rdname cfa_funs
#'
#' @title Functions for dyadCFA() for scripting free or fixed families of parameters
#'
#' @param dvn input object from dyadVarNames()
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param partner input character to indicate parameters for first or second dyad member
#' @param type input character to indicate whether parameters "free" or "equated" in estimation
#'
#' @seealso \code{\link{dyadCFA}} which this function supplies
#' @export

#' @rdname cfa_funs
loadings <- function(dvn, lvname, partner="1", type = "free"){

  if(partner == "1" & type == "free"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[[4]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[[1]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[[4]], dvn[[1]][1])
    eta.x = list()
    for (i in 1:dvn[[3]]) {
      eta.x[[i]]=sprintf("l%s*%s",i, dvn[[1]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "free"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[[5]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[[2]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[[5]], dvn[[2]][1])
    eta.x = list()
    for (i in 1:dvn[[3]]) {
      eta.x[[i]]=sprintf("l%s*%s",i, dvn[[2]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }
}

#' @rdname cfa_funs
intercepts <- function(dvn, partner="1", type = "free"){
  if(partner == "1" & type == "free"){
    xints = list()
    for (i in 1:dvn[[3]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[[1]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated"){
    xints = list()
    for (i in 1:dvn[[3]]) {
      xints[[i]]=sprintf("%s ~ t%s*1", dvn[[1]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "free"){
    xints = list()
    for (i in 1:dvn[[3]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[[2]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated"){
    xints = list()
    for (i in 1:dvn[[3]]) {
      xints[[i]]=sprintf("%s ~ t%s*1", dvn[[2]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }
}

#' @rdname cfa_funs
resids <- function(dvn, partner="1", type = "free"){
  if(partner == "1" & type == "free"){
    res = list()
    for (i in 1:dvn[[3]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[[1]][i], dvn[[1]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "equated"){
    res = list()
    for (i in 1:dvn[[3]]) {
      res[[i]]=sprintf("%s ~~ th%s*%s",dvn[[1]][i],i, dvn[[1]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "free"){
    res = list()
    for (i in 1:dvn[[3]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[[2]][i], dvn[[2]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "equated"){
    res = list()
    for (i in 1:dvn[[3]]) {
      res[[i]]=sprintf("%s ~~ th%s*%s",dvn[[2]][i],i, dvn[[2]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }
}

