#' @param writeTo A character string specifying a directory path to where the
#'  output file(s) should be saved. If set to `"."`, the file(s) will be written
#'  to the current working directory. The default is `NULL`, and examples use a
#'  temporary directory created by `tempdir()`. When dealing with tabular output,
#'  `writeTo` is only relevant if `gtTab = TRUE`.
