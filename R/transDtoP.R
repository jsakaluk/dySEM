#' A function for transforming (D)yad datasets in(to) (P)airwise datasets (UPDATE EXAMPLE)
#'
#' This function transforms dyad-styled datasets into pairwise. The function
#' presumes that variables/columns for a given partner are blocked together in
#' a dataframe (e.g., P1 variables followed by P2)
#' @param dat input data frame of indicators of a particular LV
#' @param firstvar1 input character for first variable/column to extract for P1
#' @param lastvar1 input character for last variable/column to extract for P1
#' @param firstvar2 input character for first variable/column to extract for P2
#' @param lastvar2 input character for last variable/column to extract for P2
#' @return Pairwise-structured data frame
#' @export
#' @examples
#' dvn <- dvn2(dat = dat.pkg, x_order = "sip", x_stem = "X", x_delim2=".", x_item_num="\\d", distinguish_1="1", distinguish_2="2",
#' y_order="sip", y_stem="Y", y_delim2=".", y_item_num="\\d")

transDtoP <- function(dat, firstvar1, lastvar1, firstvar2, lastvar2){
  dat.1 = dat %>%
    dplyr::select(tidyselect::matches(firstvar1):tidyselect::matches(lastvar1))

  dat.2 = dat %>%
    dplyr::select(tidyselect::matches(firstvar2):tidyselect::matches(lastvar2))

  dat.DtoP = data.frame(matrix(nrow = 2*nrow(dat), ncol = ncol(dat)))

  i = 1
  j = 1
  k = nrow(dat.DtoP)

  while(k > 0){
    ins.row = cbind(dat.1[i,], dat.2[i,])
    dat.DtoP[j,] = ins.row

    i = i + 1
    j = j + 2
    k = k - 2
  }

  i = 1
  j = 2
  k = nrow(dat.DtoP)

  while(k > 0){
    ins.row = cbind(dat.2[i,], dat.1[i,])
    dat.DtoP[j,] = ins.row

    i = i + 1
    j = j + 2
    k = k - 2
  }

  names(dat.DtoP) <- names(dat)

  return(dat.DtoP)
}
