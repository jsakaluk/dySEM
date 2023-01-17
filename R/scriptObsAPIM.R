#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Observed Actor-Partner Interdependence Models
#'
#' @param X1 character of vector name containing X variable/composite for partner 1
#' @param Y1 character of vector name containing Y variable/composite for partner 1
#' @param X2 character of vector name containing X variable/composite for partner 2
#' @param Y2 character of vector name containing Y variable/composite for partner 2
#' @param equate character of what parameter(s) to constrain ("actor", "partner", "all"); default is "none" (all freely estimated)
#' @param k input logical for whether Kenny & Ledermann's (2010) k parameter should be
#' calculated to characterize the dyadic pattern in the APIM. Default to FALSE
#' @param writescript input logical (default FALSE) for whether lavaan script should
#' be concatenated and written to current working directory (in subdirectory "scripts")
#'
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions.
#' @export
#'
#' @examples
#'
scriptObsAPIM <- function(X1 = NULL, Y1 = NULL,
                          X2 = NULL, Y2 = NULL,
                          equate = "none", k = FALSE,
                          writescript = FALSE){

  if(equate == "none"){
    reg1 <- paste0(Y1, " ~ a1*", X1, " + p1*", X2)
    reg2 <- paste0(Y2, " ~ a2*", X2, " + p2*", X1)
    icc <- paste0(X1, " ~~ ", X2)
    ricc <- paste0(Y1, " ~~ ", Y2)

    if(isTRUE(k)){
      k1 <- paste("k1 := p1/a1")
      k2 <- paste("k2 := p2/a2")
      ks <- paste0(k1, "\n", k2)

      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, ks)
    }else if(!isTRUE(k)){
      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
                            reg1, reg2, icc, ricc)
    }

  }
  else if(equate == "actor"){
    reg1 <- paste0(Y1, " ~ a*", X1, " + p1*", X2)
    reg2 <- paste0(Y2, " ~ a*", X2, " + p2*", X1)
    icc <- paste0(X1, " ~~ ", X2)
    ricc <- paste0(Y1, " ~~ ", Y2)

    if(isTRUE(k)){
      k1 <- paste("k1 := p1/a")
      k2 <- paste("k2 := p2/a")
      ks <- paste0(k1, "\n", k2)

      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, ks)
    }else if(!isTRUE(k)){
      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
                            reg1, reg2, icc, ricc)
    }
  }
  else if(equate == "partner"){
    reg1 <- paste0(Y1, " ~ a1*", X1, " + p*", X2)
    reg2 <- paste0(Y2, " ~ a2*", X2, " + p*", X1)
    icc <- paste0(X1, " ~~ ", X2)
    ricc <- paste0(Y1, " ~~ ", Y2)

    if(isTRUE(k)){
      k1 <- paste("k1 := p/a1")
      k2 <- paste("k2 := p/a2")
      ks <- paste0(k1, "\n", k2)

      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, ks)
    }else if(!isTRUE(k)){
      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
                            reg1, reg2, icc, ricc)
    }
  }
  else if(equate == "all"){
    reg1 <- paste0(Y1, " ~ a*", X1, " + p*", X2)
    reg2 <- paste0(Y2, " ~ a*", X2, " + p*", X1)
    icc <- paste0(X1, " ~~ ", X2)
    ricc <- paste0(Y1, " ~~ ", Y2)

    if(isTRUE(k)){
      k <- paste("k := p/a")

      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, k)
    }else if(!isTRUE(k)){
      apimScript <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
                            reg1, reg2, icc, ricc)
    }
  }

  if(isTRUE(writescript)){
    cat(apimScript,"\n", file = sprintf("./scripts/observed_apim_equate_%s_k_%s.txt",equate, k))
  }

  return(apimScript)
}
