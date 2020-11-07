#' @name es_helpers
#' @rdname es_helpers
#'
#' @title Helper functions for noninvariance effect size functions
#'
#' @param dat data frame of indicators
#' @param dvn dvn for the dyadic cfa measurement model
#' @param fit outputted from dyadic cfa lavaan object
#' @param source character for whether parameter of interest should be extracted for group "1", "2", or from the "nogroup" model
#'
#' @export

#' @rdname es_helpers
grouploads <- function(fit, dvn, source){
  if(source == "1"){
    loads <- dplyr::filter(lavaan::parameterEstimates(fit), op == "=~" & rhs %in% dvn[[1]]) %>%
      dplyr::select(., est)
  }else if(source == "2"){
    loads <- dplyr::filter(lavaan::parameterEstimates(fit), op == "=~" & rhs %in% dvn[[2]]) %>%
      dplyr::select(., est)
  }
  return(loads)
}

#' @rdname es_helpers
groupints <- function(fit, dvn,source){
  if(source == "1"){
    ints <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(., op == "~1" & lhs %in% dvn[[1]]) %>%
      dplyr::select(., est)
  }else if(source == "2"){
    ints <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(., op == "~1" & lhs %in% dvn[[2]]) %>%
      dplyr::select(., est)
  }
  return(ints)
}

#' @rdname es_helpers
groupindsds <- function(dat, dvn, source){

  if(source == 1){
    sds <- dat %>% dplyr::select(., paste(dvn[[1]])) %>%
      dplyr::summarise_all(., sd, na.rm = TRUE) %>%
      as.numeric(.)
  }else if(source == 2){
    sds <- dat %>% dplyr::select(., paste(dvn[[2]])) %>%
      dplyr::summarise_all(., sd, na.rm = TRUE) %>%
      as.numeric(.)
  }

  return(sds)
}

#' @rdname es_helpers
grouplvmean <- function(fit, source){
  if(source == "1"){
    lvmean <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~1" & lhs == lavaan::lavNames(fit, type = "lv")[[1]]) %>%
      dplyr::select(., est) %>%
      as.numeric(.)
  }else if(source == "2"){
    lvmean <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~1" & lhs == lavaan::lavNames(fit, type = "lv")[[2]]) %>%
      dplyr::select(., est)%>%
      as.numeric(.)
  }
  return(lvmean)
}

#' @rdname es_helpers
grouplvsd <- function(fit, source){
  if(source == "1"){
    lvsd <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~~"& lhs == lavaan::lavNames(fit, type = "lv")[[1]] & rhs == lavaan::lavNames(fit, type = "lv")[[1]]) %>%
      dplyr::select(., est) %>%
      sqrt(.) %>%
      as.numeric(.)
  }else if(source == "2"){
    lvsd <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~~"& lhs == lavaan::lavNames(fit, type = "lv")[[2]] & rhs == lavaan::lavNames(fit, type = "lv")[[2]]) %>%
      dplyr::select(., est) %>%
      sqrt(.) %>%
      as.numeric(.)
  }
  return(lvsd)
}

