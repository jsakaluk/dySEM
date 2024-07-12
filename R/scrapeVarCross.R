#' A Variable Name-Scraping and Indexing Function for cross-sectional data
#'
#' This function scrapes the names of indicator variables in a
#' wide-format data set used for dyadic analyses of two latent variables (LV; X and Y),
#' and indexes which indicators correspond to which partner, for which LV.
#' It is used primarily to guide the syntax-writing of the other dySEM functions.
#' @param dat input data frame of indicators of a particular LV
#' @param x_order input character for order of (S)tem, (P)artner number, and (I)tem number
#' when creating variable names. Defaults to "spi" (Qualtrics-friendly)
#' @param x_stem input character stem of indicator variables for LV X
#' @param x_delim1 optional character to separate stem from partner number (spi) or item number (sip)
#' @param x_delim2 optional character to separate stem/partner number (spi) or stem/item number (sip) from
#' from final element of variable name
#' @param x_item_num defaults to scrape all items that match the stem with any digits that follow.
#' Will be updated to allow particular range of values, to make more sub-scale friendly.
#' @param distinguish_1 input character used as the identifier for the first partner
#' @param distinguish_2 input character used as the identifier for the first partner
#' @param y_order optional character for order of (S)tem, (P)artner number, and (I)tem number
#' when creating variable names. Defaults to "spi" (Qualtrics-friendly). This and other Y-arguments
#' only necessary if there is a latent Y variable to model
#' @param y_stem optional input character stem of indicator variables for LV X
#' @param y_delim1 optional character to separate stem from partner number (spi) or item number (sip)
#' @param y_delim2 optional character to separate stem/partner number (spi) or stem/item number (sip) from
#' from final element of variable name
#' @param y_item_num defaults to scrape all items that match the stem with any digits that follow.
#' Will be updated to allow particular range of values, to make more sub-scale friendly.
#' @param covs_order optional character for order of (S)tem, (P)artner number, and (I)tem number for any
#' covariate(s). Defaults to NULL. This and other covariate arguments only necessary if there are covariates to be scripted in your model(s).
#' @param covs_stem optional input character stem(s) of indicator variables for covariate(s). Can accept a single stem (e.g., "anx"), or a vector of stems (e.g., c("anx", "dep")).
#' Defaults to NULL.
#' @param covs_delim1 optional character to separate stem from partner number (spi) or item number (sip) for covariate(s). Defaults to NULL.
#' @param covs_delim2 optional character to separate stem/partner number (spi) or stem/item number (sip) from
#' @return a list, referred in short-hand as a "dvn" (dyad variable names list) containing variable names for p1, p2, # of items per LV,
#'characters distinguishing partners, and total number of indicators
#' @family variable-scraping functions
#' @export
#' @examples
#' dvnx <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")
#' dvnxy <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2",
#' y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")

scrapeVarCross <- function(dat, x_order = "spi", x_stem, x_delim1=NULL, x_delim2=NULL, x_item_num="\\d+", distinguish_1="1", distinguish_2="2",
                y_order=NULL, y_stem=NULL, y_delim1=NULL, y_delim2=NULL, y_item_num="\\d+",
              covs_order = NULL, covs_stem = NULL, covs_delim1 = NULL, covs_delim2 = NULL){
  if(is.null(y_order)){
    if(x_order == "sip"){
      x1vars <- sipExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1)
      x2vars <- sipExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2)
    }else if(x_order == "spi"){
      x1vars <- spiExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1)
      x2vars <- spiExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2)
    }
    num_x1_var <- length(x1vars)
    num_x2_var <- length(x2vars)
    dist_1 <- distinguish_1
    dist_2 <- distinguish_2
    tot_var <- num_x1_var + num_x2_var
    if(num_x1_var == num_x2_var){
      varlist <- list(p1xvarnames = x1vars,
                      p2xvarnames = x2vars,
                      xindper = num_x1_var,
                      dist1 =dist_1,
                      dist2 = dist_2,
                      indnum = tot_var)
      return(varlist)
    }else if(num_x1_var != num_x2_var){
      stop("scrapeVarCross() cannot detect a similar number of ", x_stem, " items for P1 and P2")
    }
  }else if(!is.null(y_order)){
    if(x_order == "sip"){
      x1vars <- sipExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1)
      x2vars <- sipExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2)

    }else if(x_order == "spi"){
      x1vars <- spiExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1)
      x2vars <- spiExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2)
    }
    if(y_order == "sip"){
      y1vars <- sipExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_1)
      y2vars <- sipExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_2)

    }else if(y_order == "spi"){
      y1vars <- spiExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_1)
      y2vars <- spiExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_2)
    }
    num_x1_var <- length(x1vars)
    num_x2_var <- length(x2vars)
    dist_1 <- distinguish_1
    dist_2 <- distinguish_2
    num_y1_var <- length(y1vars)
    num_y2_var <- length(y2vars)
    tot_var <- num_x1_var + num_x2_var + num_y1_var + num_y2_var

    if(num_x1_var == num_x2_var && num_y1_var == num_y2_var){
      varlist <- list(p1xvarnames = x1vars,
                      p2xvarnames = x2vars,
                      xindper = num_x1_var,
                      dist1 = dist_1,
                      dist2 = dist_2,
                      p1yvarnames = y1vars,
                      p2yvarnames = y2vars,
                      yindper =num_y1_var,
                      indnum = tot_var)
      return(varlist)
    }else if(num_x1_var != num_x2_var || num_y1_var != num_y2_var){
      stop("dvn() cannot detect a similar number of ", x_stem, "or", y_stem, " items for P1 and P2")
    }
  }
}
