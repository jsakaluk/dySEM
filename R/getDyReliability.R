#' A Function Calculates Omega Total Coefficients from a Dyadic CFA
#'
#' This function takes the  model from fitted scriptCFA() scripts and returns omega total coefficients
#' for each dyad member, adapted following Formula 2 in McNeish (2018).
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param fit outputted dyadic cfa lavaan object based on the default (i.e., "configural") dyadCFA() function
#' @return a tibble/data frame with calculated omega total coefficients for dyad Member 1 and Member 2
#' @export
#' @family supplemental model calculators
#' @examples
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")
#' sat.indist.script <-  scriptCFA(dvn, lvname = "Sat")
#' sat.indist.mod <- lavaan::cfa(sat.indist.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#' getDyReliability(dvn, sat.indist.mod)

getDyReliability <- function(dvn, fit){

  #For P1
  numer1 <- sum(xlamda1(dvn, fit))^2
  denom1 <- (sum(xlamda1(dvn, fit))^2) + sum(xtheta1(dvn, fit))
  omega1 <- numer1/denom1
  #For P2
  numer2 <- sum(xlamda2(dvn, fit))^2
  denom2 <- (sum(xlamda2(dvn, fit))^2) + sum(xtheta2(dvn, fit))
  omega2 <- numer2/denom2

  #Bind omega estimates into tibble
  rels <- cbind(omega1, omega2)
  rel.tib <- tibble::as_tibble(rels)

  #Name omegas based on distinguishing characters in dvn
  name1 <- sprintf("omega.%s", dvn[["dist1"]])
  name2 <- sprintf("omega.%s", dvn[["dist2"]])
  names(rel.tib) <- c(name1, name2)

  return(rel.tib)
}
