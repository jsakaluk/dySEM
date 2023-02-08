#' @name esHelpers
#' @rdname esHelpers
#'
#' @title Helper-functions for noninvariance effect size functions
#'
#' @param dat data frame of indicators
#' @param dvn input dvn list from scrapeVarCross
#' @param fit outputted dyadic cfa lavaan object
#' @param source character for whether parameter of interest should be extracted for group "1", "2", or from the "nogroup" model
#' @family helpers
#'
#' @export

grouploads <- function(fit, dvn, source){
  if(source == "1"){
    loads <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "=~" & .data$rhs %in% {{dvn}}[[1]]) %>%
      dplyr::select(.data$est)
  }else if(source == "2"){
    loads <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "=~" & .data$rhs %in% {{dvn}}[[2]]) %>%
      dplyr::select(.data$est)
  }
  return(loads)
}

#' @rdname esHelpers
groupints <- function(fit, dvn,source){
  if(source == "1"){
    ints <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "~1" & .data$lhs %in% {{dvn}}[[1]]) %>%
      dplyr::select(.data$est)
  }else if(source == "2"){
    ints <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "~1" & .data$lhs %in% {{dvn}}[[2]]) %>%
      dplyr::select(.data$est)
  }
  return(ints)
}

#' @rdname esHelpers
groupindsds <- function(dat, dvn, source){

  if(source == 1){
    sds <- dat %>% dplyr::select(paste({{dvn}}[[1]])) %>%
      dplyr::summarise_all(stats::sd, na.rm = TRUE) %>%
      as.numeric()
  }else if(source == 2){
    sds <- dat %>% dplyr::select(paste({{dvn}}[[2]])) %>%
      dplyr::summarise_all(stats::sd, na.rm = TRUE) %>%
      as.numeric()
  }

  return(sds)
}

#' @rdname esHelpers
grouplvmean <- function(fit, source){
  if(source == "1"){
    lvmean <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "~1" & .data$lhs == lavaan::lavNames(fit, type = "lv")[[1]]) %>%
      dplyr::select(.data$est) %>%
      as.numeric()
  }else if(source == "2"){
    lvmean <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "~1" & .data$lhs == lavaan::lavNames(fit, type = "lv")[[2]]) %>%
      dplyr::select(.data$est)%>%
      as.numeric()
  }
  return(lvmean)
}

#' @rdname esHelpers
grouplvsd <- function(fit, source){
  if(source == "1"){
    lvsd <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "~~"& .data$lhs == lavaan::lavNames(fit, type = "lv")[[1]] & .data$rhs == lavaan::lavNames(fit, type = "lv")[[1]]) %>%
      dplyr::select(.data$est) %>%
      sqrt() %>%
      as.numeric()
  }else if(source == "2"){
    lvsd <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(.data$op == "~~"& .data$lhs == lavaan::lavNames(fit, type = "lv")[[2]] & .data$rhs == lavaan::lavNames(fit, type = "lv")[[2]]) %>%
      dplyr::select(.data$est) %>%
      sqrt() %>%
      as.numeric()
  }
  return(lvsd)
}

