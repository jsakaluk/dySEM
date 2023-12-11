#' Calculates dmacs difference in expected indicator scores for between dyad members
#'
#' @param dat data frame of indicators
#' @param dvn input dvn list from scrapeVarCross
#' @param fit outputted dyadic cfa lavaan object; should be from a partial-invariance model
#' @param nodewidth space between nodes during quadrature approximation (default = .01)
#' @param lowerLV lowest latent variable value evaluated (default = -5)
#' @param upperLV greatest latent variable value evaluated (default = 5)
#' @family supplemental model calculators
#' @return vector of d_macs values
#' @export
#'
#' @examples
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")

#' sat.config.script <- scriptCFA(dvn, lvname = "Sat",
#' constr_dy_meas = "none",
#' constr_dy_struct = "none")
#'
#' sat.config.mod <- lavaan::cfa(sat.config.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)

#' getDydmacs(commitmentQ, dvn, sat.config.mod)
#'
getDydmacs <- function(dat, dvn, fit, nodewidth = 0.01, lowerLV = -5, upperLV = 5){

  #Get indicator names --> # of indicators
  indnames <- lavaan::parameterestimates(fit) %>% #names of indicators
    dplyr::filter(.data$op == "=~") %>%
    dplyr::select(.data$rhs) %>%
    unique()

  p <- length(unique(indnames$rhs))/2

  indnames1 <- indnames$rhs[1:p]
  indnames2 <- indnames$rhs[(p+1):(p*2)]

  lvnames <- lavaan::lavNames(fit, type = "lv")

  #Group info
  gnum <- 2
  dyn <- lavaan::lavInspect(fit, what = "nobs")

  #Use es_helpers to snatch necessary values from cfa model
  load1 <- grouploads(fit, dvn, source = "1")
  load2 <- grouploads(fit, dvn, source = "2")
  loading1 <- load1$est
  loading2 <- load2$est

  int1 <- groupints(fit, dvn, source = "1")
  int2 <- groupints(fit, dvn, source = "2")
  intercept1 <- int1$est
  intercept2 <- int2$est

  stdev1 <- groupindsds(dat, dvn, source = 1)
  stdev2 <- groupindsds(dat, dvn, source = 2)

  fmean2 <- grouplvmean(fit, source = "2")

  fsd2 <- grouplvsd(fit, source = "2")

  N1 = dyn
  N2 = dyn

  # Define evaluation of latent variable
  LV = seq(lowerLV,upperLV,nodewidth)

  # Create empty matrices for future arrays, matrices, etc.
  DiffExpScore = matrix(NA,length(LV),p)
  pdfLV2 = matrix(NA,length(LV),1)
  dmacsNumerator = matrix(NA,length(LV),p)
  stdevpoolND = matrix(NA,p,1)
  dmacs = matrix(NA,p,1)

  # Calculate dmacs and signed dmacs
  for(j in 1:p){
    for(k in 1:length(LV)){
      # Calculate difference in expected indicator scores between groups 1 and 2
      DiffExpScore[k,j] <- (intercept1[j]-intercept2[j]) + (loading1[j]-loading2[j])*LV[k]
      # probability density function for sample estimate of group 2 latent variable distribution
      pdfLV2[k] = stats::dnorm(LV[k], mean=fmean2, sd=fsd2)

      # Multiply by latent variable distribution to calculate individual data point in numerator
      dmacsNumerator[k,j] = DiffExpScore[k,j]*DiffExpScore[k,j]*pdfLV2[k]*nodewidth
    }
    # Calculate pooled SD based on Nye & Drasgow formula
    stdevpoolND[j] = ((N1-1)*stdev1[j] + (N2-1)*stdev2[j])/(N1+N2-2)
    # Sum across range of latent variable using quadrature to calculate numerator & divide by denominator
    dmacs[j] <- sqrt(sum(dmacsNumerator[,j]))/stdevpoolND[j]
  }
  colnames(dmacs) = "dMACS"

  return(dmacs=round(dmacs,4))

}
