#' A Function Calculates Omega Total Coefficients from a Dyadic CFA
#'
#' This function takes the  model from fitted scriptCFA() scripts and returns omega total coefficients
#' for each dyad member following Formula 2 in McNeish (2018).
#'
#' @param dvn input object from dyadVarNames()
#' @param fit input object from fitted lavaan model based on the default (i.e., "configural") dyadCFA() function
#' @return Calculated omega total coefficients for dyad Member 1 and Member 2
#' @export
#' @family supplemental model calculators
#' @examples
#' dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_", x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2")
#' qual.config.script <-  scriptCFA(dvn, lvname = "Qual", model = "configural")
#' qual.config.mod <- lavaan::cfa(qual.config.script, data = DRES, std.lv = F, auto.fix.first= F, meanstructure = T)
#' getDyReliability(dvn, qual.config.mod)

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
  name1 <- sprintf("omega.%s", dvn[[4]])
  name2 <- sprintf("omega.%s", dvn[[5]])
  names(rel.tib) <- c(name1, name2)

  return(rel.tib)
}
