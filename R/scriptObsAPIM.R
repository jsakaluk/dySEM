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
#' @param writeTo A character string specifying a directory path to where a .txt file of the resulting lavaan script should be written.
#' If set to “.”, the .txt file will be written to the current working directory.
#' The default is NULL, and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the .txt output file.
#' The default is NULL. The specified name will be automatically appended with the .txt file extension.
#' If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#'
#' @return character object of lavaan script that can be passed immediately to
#' lavaan functions.
#' @export
#'
#' @examples
#'
#' obsAPIMScript <- scriptObsAPIM (X1 = "SexSatA", Y1 = "RelSatA",
#' X2 = "SexSatB", Y2 = "RelSatB",
#' equate = "none",
#' writeTo = tempdir(),
#' fileName = "obsAPIM_script")
#'
scriptObsAPIM <- function(X1 = NULL, Y1 = NULL,
                          X2 = NULL, Y2 = NULL,
                          equate = "none", k = FALSE,
                          writeTo = NULL,
                          fileName = NULL){

  if(equate == "none"){
    reg1 <- paste0(Y1, " ~ a1*", X1, " + p1*", X2)
    reg2 <- paste0(Y2, " ~ a2*", X2, " + p2*", X1)
    icc <- paste0(X1, " ~~ ", X2)
    ricc <- paste0(Y1, " ~~ ", Y2)

    if(isTRUE(k)){
      k1 <- paste("k1 := p1/a1")
      k2 <- paste("k2 := p2/a2")
      ks <- paste0(k1, "\n", k2)

      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, ks)
    }else if(!isTRUE(k)){
      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
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

      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, ks)
    }else if(!isTRUE(k)){
      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
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

      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, ks)
    }else if(!isTRUE(k)){
      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
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

      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s\n\n# k parameter(s)\n%s",
                            reg1, reg2, icc, ricc, k)
    }else if(!isTRUE(k)){
      script <- sprintf("#Actor and Partner Effects\n%s\n%s\n\n#ICC and Residual ICC\n%s\n%s",
                            reg1, reg2, icc, ricc)
    }
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
