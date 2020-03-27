#' A function for transforming (P)airwise datasets in(to) (D)yad datasets (UPDATE EXAMPLE)
#'
#' This function transforms pairwise-styled datasets into dyad T
#' @param dat input data frame of indicators of a particular LV
#' @return Dyad-structured data frame
#' @export
#' @examples
#' dvn <- dvn2(dat = dat.pkg, x_order = "sip", x_stem = "X", x_delim2=".", x_item_num="\\d", distinguish_1="1", distinguish_2="2",
#' y_order="sip", y_stem="Y", y_delim2=".", y_item_num="\\d")

transPtoD <- function(dat){

  dat.PtoD <- dat %>%
    dplyr::filter(row_number() %% 2 == 1)

  names(dat.PtoD) <- names(dat)

  return(dat.PtoD)
}
