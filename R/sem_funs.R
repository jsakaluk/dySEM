#' @name sem_funs
#' @rdname sem_funs
#'
#' @title Functions for scripting structural parameters
#'
#' @param dvn input object from dyadVarNames()
#' @param param input character for what kind of parameter is being scripted ("act", "apim_part", "mim_part", "cf")
#' @param lvar input character for whether scripting helpers target latent "X" or :Y" indicator variables in dvn
#' @param lvxname input character to (arbitrarily) name LV X in lavaan syntax
#' @param lvyname input character to (arbitrarily) name LV Y in lavaan syntax
#' @param partner input character to indicate parameters for first or second dyad member
#' @param type input character to indicate whether parameters "free" or "equated" in estimation
#'
#' @seealso \code{\link{dyadCFA}} which this function supplies
#' @export

#' @rdname sem_funs
lregs <- function(dvn, param, lvxname, lvyname, type = "free"){
  if(param == "act"){
    if(type == "free"){
      beta_y1x1 = sprintf("%s%s ~ a1*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      beta_y2x2 = sprintf("%s%s ~ a2*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
      betas <- paste(beta_y1x1, beta_y2x2, sep = "\n")

      return(betas)
    }else if(type == "equated"){
      beta_y1x1 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist1"]])
      beta_y2x2 = sprintf("%s%s ~ a*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist2"]])
      betas <- paste(beta_y1x1, beta_y2x2, sep = "\n")

      return(betas)
    }
  }else if(param == "apim_part"){
    if(type == "free"){
      beta_y1x2 = sprintf("%s%s ~ p1*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      beta_y2x1 = sprintf("%s%s ~ p2*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist1"]])

      betas <- paste(beta_y1x2, beta_y2x1, sep = "\n")
      return(betas)
    }else if(type == "equated"){
      beta_y1x2 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[["dist1"]],lvxname, dvn[["dist2"]])
      beta_y2x1 = sprintf("%s%s ~ p*%s%s",lvyname, dvn[["dist2"]],lvxname, dvn[["dist1"]])

      betas <- paste(beta_y1x2, beta_y2x1, sep = "\n")
      return(betas)
    }
  }else if(param == "mim_part"){
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
  }else if(param == "cf"){
    beta_yx <- sprintf("%s ~ %s", lvyname, lvxname)

    betas <- paste(beta_yx)
    return(betas)
  }
}

#' @rdname sem_funs
ktest <- function(){

}

