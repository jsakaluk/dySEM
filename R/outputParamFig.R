#' A function That Exports SEM diagrams based on dySEM models
#'
#' This function takes the model from fitted `dySEM` scripts and exports `.png` path diagram figures of expected output.
#'
#' @param fit Input object from fitted `lavaan` model.
#' @param figtype A character input of what type of model is desired:
#'  * "unstandardized" for unstandardized path coefficients.
#'  * "standardized" for standardized path coefficients.
#'  * "labels" for labelled parameters.
#' @param writeTo A character string specifying a directory path to where the output file of the path diagram should be saved.
#' If set to “.”, the file will be written to the current working directory.
#' The default is `NULL` (which will throw an error), and examples use a temporary directory created by `tempdir()`.
#' @param fileName A character string specifying a desired base name for the output file.
#' If a `fileName` not provided (i.e., fileName = `NULL`), then a default will be used depending on the specified `figtype`
#' (e.g., "dySEM_figure unstd", "dySEM_figure std", or "dySEM_figure lab").
#' The specified name will automatically be appended with the `.png` file extension.
#' 
#' @details
#' * The function uses `semPlot::semPaths()` to create a `qgraph` object of the desired SEM path diagram.
#' * After execution, a `semPlot::semPaths()` message will be printed to the console confirming the directory path of the saved output file.
#' * If a file with the same name already exists in the user's chosen directory, it will be overwritten.
#' 
#' @return A `qgraph` object of the desired SEM path diagram, which is simultaneously exported as a `.png` file to the specified directory.
#'
#' @export
#'
#' @examples
#' dvnx <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")
#'
#' sat.config.script <- scriptCFA(dvnx, lvname = "Sat", constr_dy_meas = "none",
#' constr_dy_struct = "none")
#'
#' sat.config.mod <- lavaan::cfa(sat.config.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' outputParamFig(sat.config.mod, figtype = "standardized",
#' writeTo = tempdir(), fileName = "dCFA_configural")
#'  
#' dvnxy <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2",
#' y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#'
#' apim.indist.script <-  scriptAPIM(dvnxy, lvxname = "Sat", lvyname = "Com", est_k = TRUE)
#'
#' apim.indist.mod <- lavaan::cfa(apim.indist.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#' 
#' outputParamFig(apim.indist.mod, figtype = "standardized",
#' writeTo = tempdir(), fileName = "APIM_indist")

outputParamFig <- function(fit,
                           figtype = NULL,
                           writeTo = NULL,
                           fileName = NULL
                           ){
  
  # checking for valid directory path
  if (is.null(writeTo)){
    stop("Must specify a directory to which the file should be saved. \n Use writeTo = '.' to save output file(s) in the current working directory.")
  }
  if (!is.character(writeTo)){
    stop("The `writeTo` argument must be a character string. \n Use writeTo = '.' to save output file(s) in the current working directory.")
  }
  if (!dir.exists(writeTo)){
    stop("The specified directory does not exist. \n Use writeTo = '.' to save output file(s) in the current working directory.")
  }
  if (!is.null(fileName) && !is.character(fileName)){
    stop("The `fileName` argument must be a character string.")}
  
  #Make path diagram
  if(figtype == "unstandardized"){
    semplot <- makeFigure(fit, type = "raw", writeTo, fileName)
  }
  else if(figtype == "standardized"){
    semplot <- makeFigure(fit, type = "std", writeTo, fileName)
  }
  else if(figtype == "labels"){
    semplot <- makeFigure(fit, type = "lab", writeTo, fileName)
  }

}
