#' A Variable Name-Scraping and Indexing Function for longitudinal data
#'
#' This function scrapes the names of indicator variables in a
#' wide-format data set used for dyadic analyses of two latent variables (LV; X and Y),
#' and indexes which indicators correspond to which partner, for which LV.
#' It is used primarily to guide the syntax-writing of the other dySEM functions.
#' @param dat input data frame of indicators of a particular LV
#' @param x_order input character for order of (S)tem, (P)artner number, (Item) number
#' and (T)ime (if applicable) when creating variable names. Defaults to "spit" (qualtrics-friendly)
#' @param x_stem input character stem of indicator variables for LV X
#' @param time optional input numeric to specify number of longitudinal waves of names (defaults to null)
#' @param x_delim1 optional character to separate first name element (e.g., stem) from the second (e.g., item)
#' @param x_delim2 optional character to separate second name element (e.g., item) from the third (e.g., partner)
#' @param x_delim3 optional character to separate third name element (e.g., partner) from the fourth (e.g., time)
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
#' @family variable-scraping functions
#' @export
#' @examples
#' dvn <- scrapeVarLong(dat = dat.pkg, x_order = "sip", x_stem = "X", x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2",
#' y_order="sip", y_stem="Y", y_delim2=".", y_item_num="\\d+")

scrapeVarLong <- function(dat, x_order = "spi", time = NULL, x_stem, x_delim1=NULL, x_delim2=NULL, x_delim3=NULL, x_item_num="\\d+", distinguish_1="1", distinguish_2="2",
                 y_order=NULL, y_stem=NULL, y_delim1=NULL, y_delim2=NULL, y_delim3=NULL, y_item_num="\\d+"){
  if(is.null(y_order)){
    if(x_order == "sipt"){
      x1vars <- list()
      for(i in 1:time){
        xnames<- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
        x1vars[[i]] <- xnames
      }

      x2vars <- list()
      for(i in 1:time){
        xnames<- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
        x2vars[[i]] <- xnames
      }
    }else if(x_order == "spit"){
      x1vars <- list()
      for(i in 1:time){
        xnames<- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
        x1vars[[i]] <- xnames
      }

      x2vars <- list()
      for(i in 1:time){
        xnames<- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
        x2vars[[i]] <- xnames
      }
    }
    num_x1_var <- length(x1vars)
    num_x2_var <- length(x2vars)
    dist_1 <- distinguish_1
    dist_2 <- distinguish_2
    tot_var <- num_x1_var + num_x2_var

    #Check if number of indicators per wave is the same
    if(is.list(x1vars)){
      ind_num <- logical(num_x1_var)
      for(i in 1:num_x1_var){
        ind_num[i] <- all.equal(length(x1vars[[i]]), length(x2vars[[i]]))
      }
      #Do we get all TRUEs? If so, passes check.
      ind_num_check <- sum(ind_num) == num_x1_var
    }

    #Extract more complex item info if a longitudinal list is extracted
    if(is.list(x1vars)){
      p1num <- numeric(num_x1_var)
      p2num <- numeric(num_x1_var)
      for(i in 1:num_x1_var){
        p1num[i] <- length(x1vars[[i]])
        p2num[i] <- length(x2vars[[i]])
      }
      p1num_total <- sum(p1num)
      p2num_total <- sum(p2num)
      num_total <- p1num_total+p2num_total
      item_wave_num <- length(x1vars[[1]])
    }

    if(is.null(time) & num_x1_var == num_x2_var){
      varlist <- list(x1vars, x2vars, num_x1_var, dist_1, dist_2, tot_var)
      return(varlist)
    }else if(num_x1_var != num_x2_var){
      stop("dvn() cannot detect a similar number of ", x_stem, " items for P1 and P2")
    }else if(!is.null(time) & num_x1_var != num_x2_var & ind_num_check==TRUE){
      stop("dvn() cannot detect a similar number of waves of indicators for P1 and P2")
    }else if(!is.null(time) & num_x1_var == num_x2_var & ind_num_check==FALSE){
      stop("dvn() cannot detect a similar number of indicators per wave of assessment for P1 and P2")
    }else if(!is.null(time) & num_x1_var != num_x2_var & ind_num_check==FALSE){
      stop("dvn() cannot detect a similar number of waves of indicators, or a similar number of indicators per wave of assessment for P1 and P2")
    }else if(!is.null(time) & num_x1_var == num_x2_var & ind_num_check==TRUE){
      varlist <- list(p1xvarnames = x1vars,
                      p2xvarnames = x2vars,
                      p1waves = num_x1_var,
                      p2waves = num_x2_var,
                      dist1 = dist_1,
                      dist2 = dist_2,
                      p1p2waves = tot_var,
                      p1xindnum = p1num_total,
                      p2xindnum = p2num_total,
                      xindnum = num_total,
                      xindperwave = item_wave_num)
      return(varlist)
    }
  }else if(!is.null(y_order)){
    if(x_order == "sipt"){
      x1vars <- list()
      for(i in 1:time){
        xnames<- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
        x1vars[[i]] <- xnames
      }

      x2vars <- list()
      for(i in 1:time){
        xnames<- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
        x2vars[[i]] <- xnames
      }
    }else if(x_order == "spit"){
      x1vars <- list()
      for(i in 1:time){
        xnames<- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
        x1vars[[i]] <- xnames
      }

      x2vars <- list()
      for(i in 1:time){
        xnames<- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
        x2vars[[i]] <- xnames
      }
    }
    if(y_order == "sipt"){
      y1vars <- list()
      for(i in 1:time){
        ynames<- siptExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_1, y_delim3, as.character(i))
        y1vars[[i]] <- ynames
      }

      y2vars <- list()
      for(i in 1:time){
        ynames<- siptExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_2, y_delim3, as.character(i))
        y2vars[[i]] <- ynames
      }
    }else if(y_order == "spit"){
      y1vars <- list()
      for(i in 1:time){
        ynames<- spitExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_1, y_delim3, as.character(i))
        y1vars[[i]] <- ynames
      }

      y2vars <- list()
      for(i in 1:time){
        ynames<- spitExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_2, y_delim3, as.character(i))
        y2vars[[i]] <- ynames
      }
    }
    num_x1_var <- length(x1vars)
    num_x2_var <- length(x2vars)
    tot_xvar <- num_x1_var + num_x2_var

    num_y1_var <- length(y1vars)
    num_y2_var <- length(y2vars)
    tot_yvar <- num_y1_var + num_y2_var

    tot_var <- tot_xvar + tot_yvar

    dist_1 <- distinguish_1
    dist_2 <- distinguish_2
    #Check if number of indicators per wave is the same
    if(is.list(x1vars)){
      xind_num <- logical(num_x1_var)
      for(i in 1:num_x1_var){
        xind_num[i] <- all.equal(length(x1vars[[i]]), length(x2vars[[i]]))
      }
      #Do we get all TRUEs? If so, passes check.
      xind_num_check <- sum(xind_num) ==num_x1_var
    }

    #Check if number of indicators per wave is the same
    if(is.list(y1vars)){
      yind_num <- logical(num_y1_var)
      for(i in 1:num_y1_var){
        yind_num[i] <- all.equal(length(y1vars[[i]]), length(y2vars[[i]]))
      }
      #Do we get all TRUEs? If so, passes check.
      yind_num_check <- sum(yind_num) ==num_y1_var
    }

    #Extract more complex item info if a longitudinal list is extracted
    if(is.list(x1vars)){
      p1xnum <- numeric(num_x1_var)
      p2xnum <- numeric(num_x1_var)
      for(i in 1:num_x1_var){
        p1xnum[i] <- length(x1vars[[i]])
        p2xnum[i] <- length(x2vars[[i]])
      }
      p1xnum_total <- sum(p1xnum)
      p2xnum_total <- sum(p2xnum)
      xnum_total <- p1xnum_total+p2xnum_total
      xitem_wave_num <- length(x1vars[[1]])
    }

    if(is.list(y1vars)){
      p1ynum <- numeric(num_y1_var)
      p2ynum <- numeric(num_y1_var)
      for(i in 1:num_y1_var){
        p1ynum[i] <- length(y1vars[[i]])
        p2ynum[i] <- length(y2vars[[i]])
      }
      p1ynum_total <- sum(p1ynum)
      p2ynum_total <- sum(p2ynum)
      ynum_total <- p1ynum_total+p2ynum_total
      yitem_wave_num <- length(y1vars[[1]])
    }

    if(is.null(time) & num_y1_var == num_y2_var){
      varlist <- list(y1vars, y2vars, num_y1_var, dist_1, dist_2, tot_var)
      return(varlist)
    }else if(num_y1_var != num_y2_var){
      stop("dvn() cannot detect a similar number of ", y_stem, " items for P1 and P2")
    }else if(!is.null(time) & num_y1_var != num_y2_var & yind_num_check==TRUE){
      stop("dvn() cannot detect a similar number of waves of indicators for P1 and P2")
    }else if(!is.null(time) & num_y1_var == num_y2_var & yind_num_check==FALSE){
      stop("dvn() cannot detect a similar number of indicators per wave of assessment for P1 and P2")
    }else if(!is.null(time) & num_y1_var != num_y2_var & yind_num_check==FALSE){
      stop("dvn() cannot detect a similar number of waves of indicators, or a similar number of indicators per wave of assessment for P1 and P2")
    }else if(!is.null(time) & num_y1_var == num_y2_var & yind_num_check==TRUE){
      varlist <- list(p1xvarnames = x1vars,
                      p2xvarnames = x2vars,
                      p1xwaves = num_x1_var,
                      p2xwaves = num_x2_var,
                      dist1 = dist_1,
                      dist2 = dist_2,
                      p1p2xwaves = tot_xvar,
                      p1xindnum = p1xnum_total,
                      p2xindnum = p2xnum_total,
                      xindnum = xnum_total,
                      xindperwave = xitem_wave_num,
                      p1yvarnames = y1vars,
                      p2yvarnames = y2vars,
                      p1ywaves = num_y1_var,
                      p2ywaves = num_y2_var,
                      p1p2ywaves = tot_yvar,
                      p1yindnum = p1ynum_total,
                      p2yindnum = p2ynum_total,
                      yindnum = ynum_total,
                      yindperwave = yitem_wave_num,
                      p1varnum = p1xnum_total+p1ynum_total,
                      p2varnum = p2xnum_total+p2ynum_total,
                      p1p2varnum = p1xnum_total+p1ynum_total+p2xnum_total+p2ynum_total)
      return(varlist)
    }
  }else if(num_x1_var != num_x2_var || num_y1_var != num_y2_var){
    stop("dvn() cannot detect a similar number of ", x_stem, "or", y_stem, " items for P1 and P2")
  }
}

