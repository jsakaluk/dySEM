#' @name cfa_funs
#' @rdname cfa_funs
#'
#' @title Functions for dyadCFA() for scripting free or fixed families of parameters
#'
#' @param dvn input object from dyadVarNames()
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param partner input character to indicate parameters for first or second dyad member
#' @param type input character to indicate whether parameters "fixed", "free", "equated", or "equated_mv" in estimation
#'
#' @seealso \code{\link{dyadCFA}} which this function supplies
#' @export

#' @rdname cfa_funs
loads <- function(dvn, lvname, partner="1", type = "free"){

  if(partner == "1" & type == "fixed"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "free"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["indper"]]) {
      eta.x[[i]]=sprintf("l%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_mv"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["indper"]]) {
      eta.x[[i]]=sprintf("l%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "fixed"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "free"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["indper"]]) {
      eta.x[[i]]=sprintf("l%s*%s",i, dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_mv"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["indper"]]) {
      eta.x[[i]]=sprintf("l%s*%s",i, dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "free"){
    eta_x = sprintf("%sDy =~ NA*",lvname)
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+"), "+",paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["indper"]]) {
      eta.x1[[i]]=sprintf("l%s*%s",(dvn[["indper"]]*2+i), dvn[["p1xvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["indper"]]) {
      eta.x2[[i]]=sprintf("l%s*%s",(dvn[["indper"]]*2+i), dvn[["p2xvarnames"]][i])
    }

    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }
}

#' @rdname cfa_funs
intercepts <- function(dvn, partner="1", type = "free"){
  if(partner == "1" & type == "fixed"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p1xvarnames"]][1])
    for (i in 2:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "free"){
    xints = list()
    for (i in 1:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated"){
    xints = list()
    for (i in 1:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ t%s*1", dvn[["p1xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated_mv"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + t1*1", dvn[["p1xvarnames"]][1])

    for (i in 2:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ t%s*1", dvn[["p1xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "fixed"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p2xvarnames"]][1])
    for (i in 2:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "free"){
    xints = list()
    for (i in 1:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated"){
    xints = list()
    for (i in 1:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ t%s*1", dvn[["p2xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated_mv"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + t1*1", dvn[["p2xvarnames"]][1])

    for (i in 2:dvn[["indper"]]) {
      xints[[i]]=sprintf("%s ~ t%s*1", dvn[["p2xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }
}

#' @rdname cfa_funs
resids <- function(dvn, partner="1", type = "free"){
  if(partner == "1" & type == "free"){
    res = list()
    for (i in 1:dvn[["indper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p1xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "equated"){
    res = list()
    for (i in 1:dvn[["indper"]]) {
      res[[i]]=sprintf("%s ~~ th%s*%s",dvn[["p1xvarnames"]][i],i, dvn[["p1xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "free"){
    res = list()
    for (i in 1:dvn[["indper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p2xvarnames"]][i], dvn[["p2xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "equated"){
    res = list()
    for (i in 1:dvn[["indper"]]) {
      res[[i]]=sprintf("%s ~~ th%s*%s",dvn[["p2xvarnames"]][i],i, dvn[["p2xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }
}

#' @rdname cfa_funs
lvars <- function(dvn, lvname, partner = "1", type = "free"){
  if(partner == "1" & type == "fixed"){
    lvar <- sprintf("%s%s ~~ 1*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    return(lvar)
  }else if(partner == "1" & type == "free"){
    lvar <- sprintf("%s%s ~~ NA*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    return(lvar)
  }else if(partner == "1" & type == "equated"){
    lvar <- sprintf("%s%s ~~ psi*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    return(lvar)
  }else if (partner == "2" & type == "fixed"){
    lvar <- sprintf("%s%s ~~ 1*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    return(lvar)
  }else if(partner == "2" & type == "free"){
    lvar <- sprintf("%s%s ~~ NA*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    return(lvar)
  }else if(partner == "2" & type == "equated"){
    lvar <- sprintf("%s%s ~~ psi*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    return(lvar)
  }

}

#' @rdname cfa_funs
lmeans <- function(dvn, lvname, partner = "1", type = "free"){
  if(partner == "1" & type == "fixed"){
    alpha <- sprintf("%s%s ~ 0*1",lvname, dvn[["dist1"]])
    return(alpha)
  }else if(partner == "1" & type == "free"){
    alpha <- sprintf("%s%s ~ NA*1",lvname, dvn[["dist1"]])
    return(alpha)
  }else if(partner == "1" & type == "equated"){
    alpha <- sprintf("%s%s ~ alpha*1",lvname, dvn[["dist1"]])
    return(alpha)
  }else if (partner == "2" & type == "fixed"){
    alpha <- sprintf("%s%s ~ 0*1",lvname, dvn[["dist2"]])
    return(alpha)
  }else if(partner == "2" & type == "free"){
    alpha <- sprintf("%s%s ~ NA*1",lvname, dvn[["dist2"]])
    return(alpha)
  }else if(partner == "2" & type == "equated"){
    alpha <- sprintf("%s%s ~ alpha*1",lvname, dvn[["dist2"]])
    return(alpha)
  }
}
