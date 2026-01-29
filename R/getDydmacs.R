#' Calculates dmacs difference in expected indicator scores for between dyad members
#'
#' @param dat data frame of indicators
#' @param dvn input dvn list from scrapeVarCross
#' @param fit outputted dyadic cfa lavaan object; should be from a partial-invariance model
#' @param nodewidth space between nodes during quadrature approximation (default = .01)
#' @param lowerLV lowest latent variable value evaluated (default = -5)
#' @param upperLV greatest latent variable value evaluated (default = 5)
#' @family supplemental model calculators
#' @return For single LV models: a matrix with one column "dMACS" containing d_macs values.
#'   For multiple LV models: a named list of matrices, where each element corresponds to one LV
#'   and contains a matrix with one column "dMACS" containing d_macs values for that LV.
#' @export
#'
#' @examples
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")

#' sat.config.script <- scriptCor(dvn, lvname = "Sat",
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
  int1 <- groupints(fit, dvn, source = "1")
  int2 <- groupints(fit, dvn, source = "2")
  stdev1 <- groupindsds(dat, dvn, source = 1)
  stdev2 <- groupindsds(dat, dvn, source = 2)

  # Check if we have multiple LVs (list structure) or single LV (data frame/vector structure)
  if (is.list(load1) && !is.data.frame(load1)) {
    # Multiple LVs case: iterate over each LV
    dmacs_list <- list()
    
    # Get all LV names from the load1 list
    lv_names <- names(load1)
    
    for (lv_name in lv_names) {
      # Extract vectors for this LV
      loading1 <- load1[[lv_name]]$est
      loading2 <- load2[[lv_name]]$est
      intercept1 <- int1[[lv_name]]$est
      intercept2 <- int2[[lv_name]]$est
      stdev1_lv <- stdev1[[lv_name]]
      stdev2_lv <- stdev2[[lv_name]]
      
      # Get number of indicators for this LV
      p_lv <- length(loading1)
      
      # Find the corresponding latent variable in the model for partner 2 of this LV
      # Get the first indicator for this LV from partner 2 and find which LV loads on it
      if (length(dvn[["p2xvarnames"]][[lv_name]]) > 0) {
        p2_first_ind <- dvn[["p2xvarnames"]][[lv_name]][1]
        # Find which LV loads on this indicator
        p2_lv_name <- lavaan::parameterEstimates(fit) %>%
          dplyr::filter(.data$op == "=~" & .data$rhs == p2_first_ind) %>%
          dplyr::pull(.data$lhs) %>%
          unique()
        
        if (length(p2_lv_name) > 0) {
          # Get latent mean and SD for partner 2's LV
          fmean2 <- lavaan::parameterEstimates(fit) %>%
            dplyr::filter(.data$op == "~1" & .data$lhs == p2_lv_name[1]) %>%
            dplyr::pull(.data$est)
          
          if (length(fmean2) == 0) fmean2 <- 0
          
          fsd2 <- lavaan::parameterEstimates(fit) %>%
            dplyr::filter(.data$op == "~~" & .data$lhs == p2_lv_name[1] & .data$rhs == p2_lv_name[1]) %>%
            dplyr::pull(.data$est)
          
          if (length(fsd2) > 0) {
            fsd2 <- sqrt(fsd2)
          } else {
            fsd2 <- 1
          }
        } else {
          # Fallback values if we can't determine the LV
          fmean2 <- 0
          fsd2 <- 1
        }
      } else {
        # Fallback values if we can't determine
        fmean2 <- 0
        fsd2 <- 1
      }
      
      N1 <- dyn
      N2 <- dyn
      
      # Define evaluation of latent variable
      LV <- seq(lowerLV, upperLV, nodewidth)
      
      # Create empty matrices for future arrays, matrices, etc.
      DiffExpScore <- matrix(NA, length(LV), p_lv)
      pdfLV2 <- matrix(NA, length(LV), 1)
      dmacsNumerator <- matrix(NA, length(LV), p_lv)
      stdevpoolND <- matrix(NA, p_lv, 1)
      dmacs <- matrix(NA, p_lv, 1)
      
      # Calculate dmacs and signed dmacs
      for (j in 1:p_lv) {
        for (k in 1:length(LV)) {
          # Calculate difference in expected indicator scores between groups 1 and 2
          DiffExpScore[k, j] <- (intercept1[j] - intercept2[j]) + (loading1[j] - loading2[j]) * LV[k]
          # probability density function for sample estimate of group 2 latent variable distribution
          pdfLV2[k] <- stats::dnorm(LV[k], mean = fmean2, sd = fsd2)
          
          # Multiply by latent variable distribution to calculate individual data point in numerator
          dmacsNumerator[k, j] <- DiffExpScore[k, j] * DiffExpScore[k, j] * pdfLV2[k] * nodewidth
        }
        # Calculate pooled SD based on Nye & Drasgow formula
        stdevpoolND[j] <- ((N1 - 1) * stdev1_lv[j] + (N2 - 1) * stdev2_lv[j]) / (N1 + N2 - 2)
        # Sum across range of latent variable using quadrature to calculate numerator & divide by denominator
        dmacs[j] <- sqrt(sum(dmacsNumerator[, j])) / stdevpoolND[j]
      }
      colnames(dmacs) <- "dMACS"
      
      dmacs_list[[lv_name]] <- round(dmacs, 4)
    }
    
    return(dmacs_list)
    
  } else {
    # Single LV case: use original logic
    loading1 <- load1$est
    loading2 <- load2$est
    intercept1 <- int1$est
    intercept2 <- int2$est
    
    fmean2 <- grouplvmean(fit, source = "2")
    fsd2 <- grouplvsd(fit, source = "2")
    
    N1 <- dyn
    N2 <- dyn
    
    # Define evaluation of latent variable
    LV <- seq(lowerLV, upperLV, nodewidth)
    
    # Create empty matrices for future arrays, matrices, etc.
    DiffExpScore <- matrix(NA, length(LV), p)
    pdfLV2 <- matrix(NA, length(LV), 1)
    dmacsNumerator <- matrix(NA, length(LV), p)
    stdevpoolND <- matrix(NA, p, 1)
    dmacs <- matrix(NA, p, 1)
    
    # Calculate dmacs and signed dmacs
    for (j in 1:p) {
      for (k in 1:length(LV)) {
        # Calculate difference in expected indicator scores between groups 1 and 2
        DiffExpScore[k, j] <- (intercept1[j] - intercept2[j]) + (loading1[j] - loading2[j]) * LV[k]
        # probability density function for sample estimate of group 2 latent variable distribution
        pdfLV2[k] <- stats::dnorm(LV[k], mean = fmean2, sd = fsd2)
        
        # Multiply by latent variable distribution to calculate individual data point in numerator
        dmacsNumerator[k, j] <- DiffExpScore[k, j] * DiffExpScore[k, j] * pdfLV2[k] * nodewidth
      }
      # Calculate pooled SD based on Nye & Drasgow formula
      stdevpoolND[j] <- ((N1 - 1) * stdev1[j] + (N2 - 1) * stdev2[j]) / (N1 + N2 - 2)
      # Sum across range of latent variable using quadrature to calculate numerator & divide by denominator
      dmacs[j] <- sqrt(sum(dmacsNumerator[, j])) / stdevpoolND[j]
    }
    colnames(dmacs) <- "dMACS"
    
    return(dmacs = round(dmacs, 4))
  }
}
