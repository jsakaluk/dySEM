#' A function for checking if subdirectories exist (and creating them if not)
#'
#' This function checks if subdirectories exist in the current working
#' directory, and creates them if not. Used in functions generating scripts,
#' figures, and tables
#'
#' @param type input character defining name of subdirectory corresponding to
#' type of outputted documents
#' @export

dirs <- function(type){
  if(!dir.exists(file.path(sprintf("./%s", type)))){
    dir.create(file.path(sprintf("./%s", type)))
  }
}
