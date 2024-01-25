#' @name scriptHelpers
#' @rdname scriptHelpers
#'
#' @title Helper-functions for scripting free, fixed, and equated families of parameters
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param lvar input character for whether scripting helpers target latent "X" or :Y" indicator variables in dvn
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param partner input character to indicate parameters for first or second dyad member
#' @param type input character to indicate whether parameters "fixed", "free", "equated", or "equated_mv" in estimation
#' @family helpers

#' @noRd
loads <- function(dvn, lvar = "X", lvname, partner="1", type = "free"){

  if(partner == "1" & type == "fixed" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "fixed" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "free" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "free" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_mv" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_mv" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist1"]], dvn[["p1yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_source" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_source" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "fixed" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "fixed" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "free" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "free" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_mv" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_mv" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist2"]], dvn[["p2yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_source" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lx%s*%s",(dvn[["xindper"]]+i), dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_source" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("ly%s*%s",(dvn[["yindper"]]+i), dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "free" & lvar == "X"){
    eta_x = sprintf("%sDy =~ NA*",lvname)
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+"), "+",paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "free" & lvar == "Y"){
    eta_x = sprintf("%sDy =~ NA*",lvname)
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1yvarnames"]], collapse = "+"), "+",paste(dvn[["p2yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated" & lvar == "X"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lxg%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x2[[i]]=sprintf("lxg%s*%s",i, dvn[["p2xvarnames"]][i])
    }

    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated" & lvar == "Y"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("lyg%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x2[[i]]=sprintf("lgy%s*%s",i, dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated_source" & lvar == "X"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x2[[i]]=sprintf("lx%s*%s",(dvn[["xindper"]]+i), dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated_source" & lvar == "Y"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x2[[i]]=sprintf("ly%s*%s",(dvn[["yindper"]]+i), dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }
}

#' @rdname scriptHelpers
#' @noRd
intercepts <- function(dvn, lvar = "X", partner="1", type = "free"){
  if(partner == "1" & type == "fixed" & lvar == "X"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p1xvarnames"]][1])
    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "fixed" & lvar == "Y"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p1yvarnames"]][1])
    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "free" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "free" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p1xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p1yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated_mv" & lvar == "X"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + tx1*1", dvn[["p1xvarnames"]][1])

    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p1xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated_mv" & lvar == "Y"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + ty1*1", dvn[["p1yvarnames"]][1])

    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p1yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "fixed" & lvar == "X"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p2xvarnames"]][1])
    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "fixed" & lvar == "Y"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p2yvarnames"]][1])
    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "free" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "free" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p2xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p2yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated_mv" & lvar == "X"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + tx1*1", dvn[["p2xvarnames"]][1])

    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p2xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated_mv" & lvar == "Y"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + ty1*1", dvn[["p2yvarnames"]][1])

    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p2yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }
}

#' @rdname scriptHelpers
#' @noRd
resids <- function(dvn, lvar = "X", partner="1", type = "free"){
  if(partner == "1" & type == "free" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p1xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "free" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p1yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "equated" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ thx%s*%s",dvn[["p1xvarnames"]][i],i, dvn[["p1xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "equated" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ thy%s*%s",dvn[["p1yvarnames"]][i],i, dvn[["p1yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "free" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p2xvarnames"]][i], dvn[["p2xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "free" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p2yvarnames"]][i], dvn[["p2yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "equated" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ thx%s*%s",dvn[["p2xvarnames"]][i],i, dvn[["p2xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "equated" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ thy%s*%s",dvn[["p2yvarnames"]][i],i, dvn[["p2yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }
}

#' @rdname scriptHelpers
#' @noRd
coresids <- function(dvn, lvar = "X", type = "free"){
  if(lvar == "X" & type == "free"){
    coresids <- list()
    for (i in 1:dvn[["xindper"]]) {
      coresids[[i]] <- sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
    }
    coresids <- paste(coresids, collapse = "\n")
  }else if(lvar == "Y" & type == "free"){
    coresids <- list()
    for (i in 1:dvn[["yindper"]]) {
      coresids[[i]] <- sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
    }
    coresids <- paste(coresids, collapse = "\n")
  }
  return(coresids)
}

#' @rdname scriptHelpers
#' @noRd
lvars <- function(dvn, lvar = "X", lvname, partner = "1", type = "free"){
  if(partner == "1" & type == "fixed"){
    lvar <- sprintf("%s%s ~~ 1*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    return(lvar)
  }else if(partner == "1" & type == "free"){
    lvar <- sprintf("%s%s ~~ NA*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    return(lvar)
  }else if(partner == "1" & type == "equated"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ psix*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ psiy*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }
    return(lvar)
  }else if(partner == "1" & type == "equated_ff"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psix*%s%s", lvname, dvn[["dist1"]],lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psiy*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }
    return(lvar)
  }else if (partner == "2" & type == "fixed"){
    lvar <- sprintf("%s%s ~~ 1*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    return(lvar)
  }else if(partner == "2" & type == "free"){
    lvar <- sprintf("%s%s ~~ NA*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    return(lvar)
  }else if(partner == "2" & type == "equated"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ psix*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ psiy*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }
    return(lvar)
  }else if(partner == "2" & type == "equated_ff"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psix*%s%s", lvname, dvn[["dist2"]],lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psiy*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }
    return(lvar)
  }else if(partner == "g" & type == "fixed"){
    lvar <- sprintf("%sDy ~~ 1*%sDy",lvname,lvname)
    return(lvar)
  }else if(partner == "g" & type == "free"){
    lvar <- sprintf("%sDy ~~ NA*%sDy",lvname,lvname)
    return(lvar)
  }else if(partner == "g" & type == "equated_ff"){
    if(lvar == "X"){
      lvar <- sprintf("%sDy ~~ NA*%sDy + psix*%sDy",lvname, lvname, lvname)
    }else if(lvar == "Y"){
      lvar <- sprintf("%sDy ~~ NA*%sDy + psiy*%sDy",lvname, lvname, lvname)
    }
    return(lvar)
  }
}

#' @rdname scriptHelpers
#' @noRd
lcovars <- function(dvn, lvname, type = "free"){
  if(type == "free"){
    lcovar <- sprintf("%s%s ~~ %s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])
  }else if(type == "zero"){
    lcovar <- sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])
  }
  return(lcovar)
}


#' @rdname scriptHelpers
#' @noRd
lmeans <- function(dvn, lvar = "X", lvname, partner = "1", type = "free"){
  if(partner == "1" & type == "fixed"){
    alpha <- sprintf("%s%s ~ 0*1",lvname, dvn[["dist1"]])
    return(alpha)
  }else if(partner == "1" & type == "free"){
    alpha <- sprintf("%s%s ~ NA*1",lvname, dvn[["dist1"]])
    return(alpha)
  }else if(partner == "1" & type == "equated"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ alphax*1",lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ alphay*1",lvname, dvn[["dist1"]])
    }
    return(alpha)
  }else if(partner == "1" & type == "equated_ff"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ 0*1 + alphax*1",lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ 0*1 + alphay*1",lvname, dvn[["dist1"]])
    }
    return(alpha)
  }else if (partner == "2" & type == "fixed"){
    alpha <- sprintf("%s%s ~ 0*1",lvname, dvn[["dist2"]])
    return(alpha)
  }else if(partner == "2" & type == "free"){
    alpha <- sprintf("%s%s ~ NA*1",lvname, dvn[["dist2"]])
    return(alpha)
  }else if(partner == "2" & type == "equated"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ alphax*1",lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ alphay*1",lvname, dvn[["dist2"]])
    }
    return(alpha)
  }else if(partner == "2" & type == "equated_ff"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ 0*1 + alphax*1",lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ 0*1 + alphay*1",lvname, dvn[["dist2"]])
    }
    return(alpha)
  }else if(partner == "g" & type == "free"){
    alpha <- sprintf("%sDy ~ NA*1",lvname)
    return(alpha)
  }else if(partner == "g" & type == "fixed"){
    alpha <- sprintf("%sDy ~ 0*1",lvname)
    return(alpha)
  }else if(partner == "g" & type == "equated_ff"){
    if(lvar == "X"){
      alpha <- sprintf("%sDy ~ 0*1 + alphax*1",lvname)
    }else if(lvar == "Y"){
      alpha <- sprintf("%sDy ~ 0*1 + alphay*1",lvname)
    }
    return(alpha)
  }
}
