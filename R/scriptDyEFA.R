#' A Function That Writes, Saves, and Exports Syntax for
#' Fitting Dyadic Exploratory Factor Analysis (DEFA) Models
#'
#' This function takes the outputted object from scrapeVarCross()
#' and automatically writes, returns, and exports (.txt) lavaan() syntax
#' for specifying a dyadic EFA model of a given number of exploratory factors. 
#' 
#' @param dvn input dvn list from scrapeVarCross
#' @param nFactor numeric argument for number of exploratory factors to extract. Defaults to 1. Note 
#' that higher values may cause estimation problems as solution becomes over-factored and/or in the presence
#' of insufficient data. 
#' @param constr_dy_meas input character vector detailing which measurement model parameters to constrain across dyad members.
#' Default is "none" but user can specify "loadings" and/or "residuals", to fit an exploratory model with loadings and/or residuals constrained across partners
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
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")
#'
#' sat.defa1.script <- scriptDyEFA(dvn, nFactor = 1, 
#' writeTo = tempdir(), fileName = "DEFA_1fac")


scriptDyEFA <- function(dvn, nFactor = 1, constr_dy_meas = "none", writeTo = NULL, fileName = NULL){
  #check for valid inputs
  if(length(dvn)!=6){
    stop("You must supply a dvn object containing information for only X [i.e., your target LV]")
  }
  if(!any(constr_dy_meas %in% c("loadings", "residuals", "none"))){
    stop("constr_dy_meas must be a character vector containing any combination of 'loadings', 'residuals', or 'none'")
  }
  
  if(any(constr_dy_meas == "loadings")){
    #loading invariance not yet incorporated into dyadic EFA models
  }else{
    #loadings, conditional on nFactor value
    if(nFactor == 1){
      #one factor case
      eta_x <- "efa('efa')*f1 =~" 
      inds1 = paste(dvn[["p1xvarnames"]], collapse = "+")
      inds2 = paste(dvn[["p2xvarnames"]], collapse = "+")
      
      xloads <- paste(eta_x, inds1, "+", inds2)
      
    }else{
      factors <- list()
      
      for(i in 1:(nFactor-1)){
        factors[[i]] <- paste0('efa("efa")*f', i, " + \n")
      }
      
      finalFactor <-  paste0('efa("efa")*f', nFactor, " =~")
      
      xloads <- paste0(paste0(factors, collapse = ""), finalFactor)
      inds1 <- paste(dvn[["p1xvarnames"]], collapse = " + ")
      inds2 <- paste(dvn[["p2xvarnames"]], collapse = " + ")
      
      xloads <- paste(xloads, inds1, "+", inds2)
    }
  }
  
  #residual variances
  if(any(constr_dy_meas == "residuals")){
    xres1 <- resids(dvn, lvar = "X", partner="1", type = "equated")
    xres2 <- resids(dvn, lvar = "X", partner="2", type = "equated")
  }else{
    #Residual variances
    xres1 <- resids(dvn, lvar = "X", partner="1", type = "free")
    xres2 <- resids(dvn, lvar = "X", partner="2", type = "free")
  }
  
  #residual covariances
  xcoresids <- coresids(dvn, lvar = "X", "free")

  #Script Creation Syntax
  script <- sprintf("#Measurement Model\n\n#Loadings\n%s\n\n#Residual Variances\n%s\n\n%s\n\n#Residual Covariances\n%s",
                    xloads,
                    xres1, xres2, xcoresids)

  #Write script to file if requested

  if(!is.null(writeTo) | !is.null(fileName)){
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

  }else if(is.null(writeTo) & is.null(fileName)){
    #otherwise just return script
    return(script)
  }
}