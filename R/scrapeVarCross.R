#' A Variable Name-Scraping and Indexing Function for cross-sectional data
#'
#' This function scrapes the names of indicator variables in a
#' wide-format data set used for dyadic analyses of two latent variables (LV; X and Y),
#' and indexes which indicators correspond to which partner, for which LV.
#' It is used primarily to guide the syntax-writing of the other dySEM functions.
#' @param dat input data frame of indicators of a particular LV
#' @param x_order input character for order of (S)tem, (P)artner number, and (Item) number
#' when creating variable names. Defaults to "spi" (qualtrics-friendly)
#' @param x_stem input character stem of indicator variables for LV X
#' @param x_delim1 optional character to separate stem from partner number (spi) or item number (sip)
#' @param x_delim2 optional character to separate stem/partner number (spi) or stem/item number (sip) from
#' from final element of variable name
#' @param x_item_num defaults to scrape all items that match the stem with any digits that follow.
#' Will be updated to allow particular range of values, to make more sub-scale friendly.
#' @param distinguish1 input character used as the identifier for the first partner
#' @param distinguish2 input character used as the identifier for the first partner
#' @param y_order optional character for order of (S)tem, (P)artner number, and (Item) number
#' when creating variable names. Defaults to "spi" (qualtrics-friendly). This and other Y-arguments
#' only necessary if there is a latent Y variable to model
#' @param y_stem optional input character stem of indicator variables for LV X
#' @param y_delim1 optional character to separate stem from partner number (spi) or item number (sip)
#' @param y_delim2 optional character to separate stem/partner number (spi) or stem/item number (sip) from
#' from final element of variable name
#' @param y_item_num defaults to scrape all items that match the stem with any digits that follow.
#' Will be updated to allow particular range of values, to make more sub-scale friendly.
#' @return list containing variable names for p1, p2, # of items per LV,
#'characters distinguishing partners, and total number of indicators
#' @export
#' @examples
#' dvn <- scrapeVarCross(dat = dat.pkg, x_order = "sip", x_stem = "X", x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2",
#' y_order="sip", y_stem="Y", y_delim2=".", y_item_num="\\d+")

scrapeVarCross <- function(dat, x_order = "spi", x_stem, x_delim1=NULL, x_delim2=NULL, x_item_num="\\d+", distinguish_1="1", distinguish_2="2",
                y_order=NULL, y_stem=NULL, y_delim1=NULL, y_delim2=NULL, y_item_num="\\d+"){
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
      varlist <- list(x1vars, x2vars, num_x1_var, dist_1, dist_2, tot_var)
      return(varlist)
    }else if(num_x1_var != num_x2_var){
      stop("dvn() cannot detect a similar number of ", x_stem, " items for P1 and P2")
    }
  }else if(!is.null(y_order)){
    if(x_order == "sip"){
      x1vars <- sipExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1)
      x2vars <- sipExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2)

    }else if(x_order == "spi"){
      x1vars <- spiExtractor(dat.qual, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1)
      x2vars <- spiExtractor(dat.qual, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2)
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
      varlist <- list(x1vars, x2vars, num_x1_var, dist_1, dist_2, y1vars, y2vars, num_y1_var, tot_var)
      return(varlist)
    }else if(num_x1_var != num_x2_var || num_y1_var != num_y2_var){
      stop("dvn() cannot detect a similar number of ", x_stem, "or", y_stem, " items for P1 and P2")
    }
  }
}