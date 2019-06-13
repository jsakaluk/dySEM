#' A Function That Computes a Version of the d_MACS Effect Size Measure
#' of Noninvariance (see Nye & Drasgow, 2011; Nye et al., 2018).
#'
#' This function takes a model written with dyadCFA() and fitted with lavaan()
#' and returns d_MACS values for each indicator variable. Function is a renamed
#' version of my  dMACS_long function (https://github.com/jsakaluk/dMACS_long),
#' which was adapted from Johannes Karl's (https://johanneskarl.netlify.com) between-groups
#' dMACS function (https://github.com/Jo-Karl/dMACS) with his assistance.
#' @param fit.cfa output from a dyadCFA() configural invariance model fitted with
#' lavaan()
#' @return numeric vector of dMACS values for each indicator of a given LV
#' @seealso \code{\link{dyadCFA}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", yvar="Y", sep = ".",distinguish1 = "1", distinguish2 = "2")
#' con.config.script = dyadCFA(dvn, lvxname = "Conflict", lv="X", model = "configural")
#' con.fit.config <- lavaan::cfa(con.config.script, data = dat, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#' dydMACS(con.fit.config)
dydMACS<- function(fit.cfa){

  # nitems ------------------------------------------------------------------
  nitems <- lavaan::lavInspect(fit.cfa, what = "rsquare") %>%
    names(.) %>%
    length(.)/2

  # Scale min and max -------------------------------------------------------
  cfa_minmax <- function(fit.cfa) {
    dt <- lavaan::inspect(fit.cfa, what = "data")
    latentMin <- min(dt[[1]]) - 1
    latentMax <- max(dt[[1]]) + 1
    out <-cbind(as.numeric(latentMin),as.numeric(latentMax))
    return(out)
  }
  # Loadings item----------------------------------------------------------------

  reference_load <- lavaan::inspect(fit.cfa, what = "est") %>%
    .$lambda %>%
    .[,1]  %>%
    .[.!=0]
  focal_load <- lavaan::inspect(fit.cfa, what = "est") %>%
    .$lambda %>%
    .[,2] %>%
    .[.!=0]

  # Intercept item----------------------------------------------------------------

  reference_intrcp <- lavaan::inspect(fit.cfa, what = "est") %>%
    .$nu%>%
    .[1:nitems]
  focal_intrcp <- lavaan::inspect(fit.cfa, what = "est") %>%
    .$nu%>%
    .[(nitems+1):(nitems*2)]

  # Pooled standard deviation -----------------------------------------------

  pool.sd<- function(fit.cfa){
    cfa.se <- lavaan::lavInspect(fit.cfa, what = "se")
    cfa.n <- lavaan::lavInspect(fit.cfa, what = "nobs")
    test <- lavaan::lavInspect(fit.cfa, what = "rsquare") %>%
      names(.) %>%
      length(.)/2

    grp1 <- cfa.se$nu[1:test] * sqrt(cfa.n)
    grp2 <- cfa.se$nu[(test+1):(test*2)] * sqrt(cfa.n)
    numerator <- ((cfa.n - 1) * grp1 + (cfa.n - 1) * grp2)
    denominator <- (cfa.n - 1) + (cfa.n - 1)
    pooled.sd <- numerator / denominator
    l <- tibble::tibble(item = paste("item", 1:test),pooled.sd)

    #result <- matrix(unlist(l), nrow = nitems, byrow = TRUE)
    return(l)
  }

  pld_sd <- pool.sd(fit.cfa)

  # latent variance ---------------------------------------------------------
  fcl_lt_vrnc <- lavaan::inspect(fit.cfa, what = "est") %>%
    .$psi
  fcl_lt_vrnc <- fcl_lt_vrnc[2,2]
  ## create functions to calculate the mean predicted response

  l <- list()
  rowlab <- c()
  for(i in c(1:nitems)){
    ## focal group predicted value
    focal.fn <- function(x){
      mpr <- focal_intrcp[i] + focal_load[i] * x
      return(mpr)
    }
    ## reference group predicted value
    reference.fn <- function(x){
      mpr <- reference_intrcp[i] + reference_load[i] * x
      return(mpr)
    }

    ## part under sqrt (function to integrate)
    diff.fn <- function(x, i = i){
      d <- ((reference.fn(x) - focal.fn(x)) ^ 2)* dnorm(x, mean = 0, sd = sqrt(fcl_lt_vrnc))
      return(d)
    }

    ## final equation (and round to 3dp)
    dMACS <- round((1/pld_sd[i,2]) * sqrt(integrate(diff.fn,
                                                    lower = cfa_minmax(fit.cfa)[,1],
                                                    upper = cfa_minmax(fit.cfa)[,2])$value),3)

    l[[length(l) + 1]] <- dMACS
    rowlab[[length(rowlab) + 1]] <- paste("Item",i)
  }
  m <- matrix(unlist(l), nrow = nitems, dimnames = list(rowlab,"dMAC"))
  return(m)
}
