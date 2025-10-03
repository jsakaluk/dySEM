#' @name scrapeVarLong
#' @rdname scrapeVarLong
#'
#' @title Scrape variable names for longitudinal dyadic data
#'
#' @param dat input data frame of indicators of a particular LV
#' @param x_order optional character for order of (S)tem, (I)tem number, (P)artner number, and (T)ime for X variables.
#' Defaults to "spi". Can be "sipt" or "spit".
#' @param time optional numeric indicating the number of waves of data. Defaults to NULL.
#' @param x_stem input character stem of indicator variables for LV X
#' @param x_delim1 optional character to separate stem from item number (sipt) or partner number (spit)
#' @param x_delim2 optional character to separate item number from partner number (sipt) or partner number from item number (spit)
#' @param x_delim3 optional character to separate partner number from time (sipt) or item number from time (spit)
#' @param x_item_num defaults to scrape all items that match the stem with any digits that follow.
#' Will be updated to allow particular range of values, to make more sub-scale friendly.
#' @param distinguish_1 optional character used as the identifier for partner 1. Defaults to "1".
#' @param distinguish_2 optional character used as the identifier for partner 2. Defaults to "2".
#' @param y_order optional character for order of (S)tem, (I)tem number, (P)artner number, and (T)ime for Y variables.
#' Defaults to NULL. Can be "sipt" or "spit". This and other Y arguments are
#' only necessary if there is a latent Y variable to model
#' @param y_stem optional input character stem of indicator variables for LV Y
#' @param y_delim1 optional character to separate stem from item number (sipt) or partner number (spit)
#' @param y_delim2 optional character to separate item number from partner number (sipt) or partner number from item number (spit)
#' @param y_delim3 optional character to separate partner number from time (sipt) or item number from time (spit)
#' @param y_item_num defaults to scrape all items that match the stem with any digits that follow.
#' Will be updated to allow particular range of values, to make more sub-scale friendly.
#' @param verbose logical indicating whether to print a summary of scraped variables to the console. Defaults to TRUE.
#' @return a list, referred in short-hand as a "dvn" (dyad variable names list) containing variable names for p1, p2, # of items per LV per wave,
#' characters distinguishing partners, and total number of indicators across all waves
#' @family variable-scraping functions
#' @export
#' @examples
#' # Example with sipt order (stem-item-partner-time)
#' dvn_long <- scrapeVarLong(
#'     dat = longData, x_order = "sipt", time = 2,
#'     x_stem = "sat", x_delim1 = ".", x_delim2 = "_", x_delim3 = "_",
#'     distinguish_1 = "w", distinguish_2 = "m"
#' )
#'
#' # Example with spit order (stem-partner-item-time)
#' dvn_long <- scrapeVarLong(
#'     dat = longData, x_order = "spit", time = 2,
#'     x_stem = "sat", x_delim1 = ".", x_delim2 = "_", x_delim3 = "_",
#'     distinguish_1 = "w", distinguish_2 = "m"
#' )
#'
#' # Example with both X and Y variables
#' dvn_long <- scrapeVarLong(
#'     dat = longData, x_order = "sipt", time = 2,
#'     x_stem = "sat", x_delim1 = ".", x_delim2 = "_", x_delim3 = "_",
#'     y_order = "sipt", y_stem = "com", y_delim1 = ".", y_delim2 = "_", y_delim3 = "_",
#'     distinguish_1 = "w", distinguish_2 = "m"
#' )
scrapeVarLong <- function(dat, x_order = "spi", time = NULL, x_stem, x_delim1 = NULL, x_delim2 = NULL, x_delim3 = NULL, x_item_num = "\\d+", distinguish_1 = "1", distinguish_2 = "2",
                          y_order = NULL, y_stem = NULL, y_delim1 = NULL, y_delim2 = NULL, y_delim3 = NULL, y_item_num = "\\d+", verbose = TRUE) {
    if (is.null(y_order)) {
        if (x_order == "sipt") {
            x1vars <- list()
            for (i in 1:time) {
                xnames <- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
                x1vars[[i]] <- xnames
            }

            x2vars <- list()
            for (i in 1:time) {
                xnames <- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
                x2vars[[i]] <- xnames
            }
        } else if (x_order == "spit") {
            x1vars <- list()
            for (i in 1:time) {
                xnames <- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
                x1vars[[i]] <- xnames
            }

            x2vars <- list()
            for (i in 1:time) {
                xnames <- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
                x2vars[[i]] <- xnames
            }
        }
        num_x1_var <- length(x1vars)
        num_x2_var <- length(x2vars)
        dist_1 <- distinguish_1
        dist_2 <- distinguish_2
        tot_var <- num_x1_var + num_x2_var

        # Check if number of indicators per wave is the same
        if (is.list(x1vars)) {
            ind_num <- logical(num_x1_var)
            for (i in 1:num_x1_var) {
                ind_num[i] <- all.equal(length(x1vars[[i]]), length(x2vars[[i]]))
            }
            # Do we get all TRUEs? If so, passes check.
            ind_num_check <- sum(ind_num) == num_x1_var
        }

        # Extract more complex item info if a longitudinal list is extracted
        if (is.list(x1vars)) {
            p1num <- numeric(num_x1_var)
            p2num <- numeric(num_x1_var)
            for (i in 1:num_x1_var) {
                p1num[i] <- length(x1vars[[i]])
                p2num[i] <- length(x2vars[[i]])
            }
            p1num_total <- sum(p1num)
            p2num_total <- sum(p2num)
            num_total <- p1num_total + p2num_total
            item_wave_num <- length(x1vars[[1]])
        }

        if (is.null(time) & num_x1_var == num_x2_var) {
            varlist <- list(x1vars, x2vars, num_x1_var, dist_1, dist_2, tot_var)
            if (verbose) print_scrape_long(varlist, x_stem = x_stem, distinguish_1 = distinguish_1, distinguish_2 = distinguish_2)
            return(varlist)
        } else if (num_x1_var != num_x2_var) {
            stop("scrapeVarLong() cannot detect a similar number of ", x_stem, " items for P1 and P2")
        } else if (!is.null(time) & num_x1_var != num_x2_var & ind_num_check == TRUE) {
            stop("scrapeVarLong() cannot detect a similar number of waves of indicators for P1 and P2")
        } else if (!is.null(time) & num_x1_var == num_x2_var & ind_num_check == FALSE) {
            stop("scrapeVarLong() cannot detect a similar number of indicators per wave of assessment for P1 and P2")
        } else if (!is.null(time) & num_x1_var != num_x2_var & ind_num_check == FALSE) {
            stop("scrapeVarLong() cannot detect a similar number of waves of indicators, or a similar number of indicators per wave of assessment for P1 and P2")
        } else if (!is.null(time) & num_x1_var == num_x2_var & ind_num_check == TRUE) {
            varlist <- list(
                p1xvarnames = x1vars,
                p2xvarnames = x2vars,
                p1waves = num_x1_var,
                p2waves = num_x2_var,
                dist1 = dist_1,
                dist2 = dist_2,
                p1p2waves = tot_var,
                p1xindnum = p1num_total,
                p2xindnum = p2num_total,
                xindnum = num_total,
                xindperwave = item_wave_num
            )
            if (verbose) print_scrape_long(varlist, x_stem = x_stem, distinguish_1 = distinguish_1, distinguish_2 = distinguish_2)
            return(varlist)
        }
    } else if (!is.null(y_order)) {
        if (x_order == "sipt") {
            x1vars <- list()
            for (i in 1:time) {
                xnames <- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
                x1vars[[i]] <- xnames
            }

            x2vars <- list()
            for (i in 1:time) {
                xnames <- siptExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
                x2vars[[i]] <- xnames
            }
        } else if (x_order == "spit") {
            x1vars <- list()
            for (i in 1:time) {
                xnames <- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_1, x_delim3, as.character(i))
                x1vars[[i]] <- xnames
            }

            x2vars <- list()
            for (i in 1:time) {
                xnames <- spitExtractor(dat, x_stem, x_delim1, x_item_num, x_delim2, distinguish_2, x_delim3, as.character(i))
                x2vars[[i]] <- xnames
            }
        }
        if (y_order == "sipt") {
            y1vars <- list()
            for (i in 1:time) {
                ynames <- siptExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_1, y_delim3, as.character(i))
                y1vars[[i]] <- ynames
            }

            y2vars <- list()
            for (i in 1:time) {
                ynames <- siptExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_2, y_delim3, as.character(i))
                y2vars[[i]] <- ynames
            }
        } else if (y_order == "spit") {
            y1vars <- list()
            for (i in 1:time) {
                ynames <- spitExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_1, y_delim3, as.character(i))
                y1vars[[i]] <- ynames
            }

            y2vars <- list()
            for (i in 1:time) {
                ynames <- spitExtractor(dat, y_stem, y_delim1, y_item_num, y_delim2, distinguish_2, y_delim3, as.character(i))
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
        # Check if number of indicators per wave is the same
        if (is.list(x1vars)) {
            xind_num <- logical(num_x1_var)
            for (i in 1:num_x1_var) {
                xind_num[i] <- all.equal(length(x1vars[[i]]), length(x2vars[[i]]))
            }
            # Do we get all TRUEs? If so, passes check.
            xind_num_check <- sum(xind_num) == num_x1_var
        }

        # Check if number of indicators per wave is the same
        if (is.list(y1vars)) {
            yind_num <- logical(num_y1_var)
            for (i in 1:num_y1_var) {
                yind_num[i] <- all.equal(length(y1vars[[i]]), length(y2vars[[i]]))
            }
            # Do we get all TRUEs? If so, passes check.
            yind_num_check <- sum(yind_num) == num_y1_var
        }

        # Extract more complex item info if a longitudinal list is extracted
        if (is.list(x1vars)) {
            p1xnum <- numeric(num_x1_var)
            p2xnum <- numeric(num_x1_var)
            for (i in 1:num_x1_var) {
                p1xnum[i] <- length(x1vars[[i]])
                p2xnum[i] <- length(x2vars[[i]])
            }
            p1xnum_total <- sum(p1xnum)
            p2xnum_total <- sum(p2xnum)
            xnum_total <- p1xnum_total + p2xnum_total
            xitem_wave_num <- length(x1vars[[1]])
        }

        if (is.list(y1vars)) {
            p1ynum <- numeric(num_y1_var)
            p2ynum <- numeric(num_y1_var)
            for (i in 1:num_y1_var) {
                p1ynum[i] <- length(y1vars[[i]])
                p2ynum[i] <- length(y2vars[[i]])
            }
            p1ynum_total <- sum(p1ynum)
            p2ynum_total <- sum(p2ynum)
            ynum_total <- p1ynum_total + p2ynum_total
            yitem_wave_num <- length(y1vars[[1]])
        }

        if (is.null(time) & num_y1_var == num_y2_var) {
            varlist <- list(y1vars, y2vars, num_y1_var, dist_1, dist_2, tot_var)
            if (verbose) print_scrape_long(varlist, x_stem = y_stem, distinguish_1 = distinguish_1, distinguish_2 = distinguish_2)
            return(varlist)
        } else if (num_y1_var != num_y2_var) {
            stop("scrapeVarLong() cannot detect a similar number of ", y_stem, " items for P1 and P2")
        } else if (!is.null(time) & num_y1_var != num_y2_var & yind_num_check == TRUE) {
            stop("scrapeVarLong() cannot detect a similar number of waves of indicators for P1 and P2")
        } else if (!is.null(time) & num_y1_var == num_y2_var & yind_num_check == FALSE) {
            stop("scrapeVarLong() cannot detect a similar number of indicators per wave of assessment for P1 and P2")
        } else if (!is.null(time) & num_y1_var != num_y2_var & yind_num_check == FALSE) {
            stop("scrapeVarLong() cannot detect a similar number of waves of indicators, or a similar number of indicators per wave of assessment for P1 and P2")
        } else if (!is.null(time) & num_x1_var == num_x2_var & xind_num_check == TRUE & yind_num_check == TRUE) {
            varlist <- list(
                p1xvarnames = x1vars,
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
                p1varnum = p1xnum_total + p1ynum_total,
                p2varnum = p2xnum_total + p2ynum_total,
                p1p2varnum = p1xnum_total + p1ynum_total + p2xnum_total + p2ynum_total
            )
            if (verbose) print_scrape_long(varlist, x_stem = x_stem, y_stem = y_stem, distinguish_1 = distinguish_1, distinguish_2 = distinguish_2)
            return(varlist)
        }
    }
}
