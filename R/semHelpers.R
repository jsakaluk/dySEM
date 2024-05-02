#' @name semHelpers
#' @rdname semHelpers
#'
#' @title Functions for scripting structural parameters
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param param input character for what kind of parameter is being scripted ("act", "apim_part", "mim_part", "cf")
#' @param lvar input character for whether scripting helpers target latent "X" or :Y" indicator variables in dvn
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param lvname input character for name of a given lv
#' @param type input character to indicate whether parameters "free" or "equated" in estimation
#' @family helpers

#' @noRd
lregs <- function(dvn, param, lvxname, lvyname, type = "free"){
  if(param == "act"){
    if(type == "free"){
      beta_y1x1 = sprintf("%s%s ~ a1*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      beta_y2x2 = sprintf("%s%s ~ a2*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
      betas <- paste(beta_y1x1, beta_y2x2, sep = "\n")

    }else if(type == "equated"){
      beta_y1x1 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      beta_y2x2 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
      betas <- paste(beta_y1x1, beta_y2x2, sep = "\n")

    }else if(type == "zero"){
      beta_y1x1 = sprintf("%s%s ~ 0*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      beta_y2x2 = sprintf("%s%s ~ 0*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
      betas <- paste(beta_y1x1, beta_y2x2, sep = "\n")
    }
    return(betas)

  }
  else if(param == "apim_part"){
    if(type == "free"){
      beta_y1x2 = sprintf("%s%s ~ p1*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      beta_y2x1 = sprintf("%s%s ~ p2*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist1"]])

      betas <- paste(beta_y1x2, beta_y2x1, sep = "\n")
    }else if(type == "equated"){
      beta_y1x2 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      beta_y2x1 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist1"]])

      betas <- paste(beta_y1x2, beta_y2x1, sep = "\n")
    }else if(type == "zero"){
      beta_y1x2 = sprintf("%s%s ~ 0*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      beta_y2x1 = sprintf("%s%s ~ 0*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist1"]])

      betas <- paste(beta_y1x2, beta_y2x1, sep = "\n")
    }
    return(betas)

  }
  else if(param == "mim_part"){
    if(type == "free"){
      beta_y1y2 = sprintf("%s%s ~ p1*%s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      beta_y2y1 = sprintf("%s%s ~ p2*%s%s",lvyname, dvn[["dist2"]],lvyname, dvn[["dist1"]])

      betas <- paste(beta_y1y2, beta_y2y1, sep = "\n")
      return(betas)
    }else if(type == "equated"){
      beta_y1y2 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[["dist1"]],lvyname, dvn[["dist2"]])
      beta_y2y1 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[["dist2"]],lvyname, dvn[["dist1"]])

      betas <- paste(beta_y1y2, beta_y2y1, sep = "\n")
      return(betas)
    }
  }
  else if(param == "cf"){
    beta_yx <- sprintf("%s ~ %s", lvyname, lvxname)

    betas <- paste(beta_yx)
    return(betas)
  }

}

#' @rdname semHelpers
#' @noRd
cfloads <- function(dvn, lvxname, lvyname, type = "equated"){
  if(type == "equated"){
    eta.cx <-  sprintf("%s =~ NA*%s%s + cfx*%s%s + cfx*%s%s", lvxname, lvxname, dvn[["dist1"]], lvxname, dvn[["dist1"]], lvxname, dvn[["dist2"]])
    eta.cy <- sprintf("%s =~ NA*%s%s + cfy*%s%s + cfy*%s%s", lvyname, lvyname, dvn[["dist1"]], lvyname, dvn[["dist1"]], lvyname, dvn[["dist2"]])
    cfloads <- paste(eta.cx, eta.cy, sep = "\n")
    return(cfloads)
  }else if(type == "fixed"){
    eta.cx <-  sprintf("%s =~ 1*%s%s + 1*%s%s", lvxname, lvxname, dvn[["dist1"]], lvxname, dvn[["dist2"]])
    eta.cy <- sprintf("%s =~ 1*%s%s + 1*%s%s", lvyname, lvyname, dvn[["dist1"]], lvyname, dvn[["dist2"]])
    cfloads <- paste(eta.cx, eta.cy, sep = "\n")
    return(cfloads)
  }
}

#' @rdname semHelpers
#' @noRd
cfvars <- function(lvname, type){
  if(type == "fixed"){
    lvar <- sprintf("%s ~~ 1*%s",lvname, lvname)
    return(lvar)
  }else if(type == "free"){
    lvar <- sprintf("%s ~~ NA*%s",lvname, lvname)
    return(lvar)
  }
}

#' @rdname semHelpers
#' @noRd
cfmeans <- function(lvname, type, lvar = "X"){
  if(type == "fixed"){
    lmean <- sprintf("%s ~ 0*1",lvname)
    return(lmean)
  }else if(type == "free"){
    lmean <- sprintf("%s ~ NA*1",lvname)
    return(lmean)
  }
}
