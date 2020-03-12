#' A Function that randomizes the position (1st or 2nd) of dyad members' observations within a row of a data frame
#'
#' This function takes the variables in a given dvn, corresponding to a data frame
#' and creates a randomly permuted version of the data frame wherein each dyad has
#' the column positions for each member within a row randomly determined to come first or last.
#' This is a helper function for the randomization permutation testing function of
#' dyadic invariance for ostensibly indistinguishable dyads.
#'
#' @param dvn input object from dyadVarNames()
#' @param dat input data frame
#' @return A randomly permuted data frame
#' @seealso \code{\link{dyadVarNames}} which this function relies on
#' @export
#' @examples
#' dat = dplyr::select(DRES, sexsat1.1:sexsat5.2)
#' dvn = dyadVarNames(dat, xvar = "sexsat", sep = ".", distinguish1 = "1", distinguish2 = "2")
#' perm = dyRand(dvn, dat)

dyRand<-function(dvn, dat){
  #Extract vectors of dyad member 1 and 2 variable names from dvn
  varnames1 = paste(dvn[[1]])
  varnames2 = paste(dvn[[2]])

  #Select subsets of columns for each dyad member, separately
  dat.1 = dat %>%
    dplyr::select(matches(varnames1))
  dat.2 = dat %>%
    dplyr::select(matches(varnames2))

  rand = rbinom(nrow(dat),1,.5)

  dat.perm = data.frame(matrix(nrow = nrow(dat), ncol = ncol(dat)))

  for(i in 1:nrow(dat)){
    if(rand[i] ==1){
      ins.row = cbind(dat.1[i,], dat.2[i,])
      dat.perm[i,] = ins.row
    }else if(rand[i]==0){
      ins.row = cbind(dat.2[i,], dat.1[i,])
      dat.perm[i,] = ins.row
    }
  }
  names(dat.perm) = names(dat)

  return(dat.perm)
}
