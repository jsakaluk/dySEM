#' A Function That Exports Tables and/or SEM Digrams based on dySEM models
#'
#' This function takes the  model from fitted dySEM() scripts and
#' exports table(s) and/or a path diagram figure of expected output.
#'
#' @param dvn input object from dyadVarNames()
#' @param model character input specying type of model to output (e.g., "cfa", "apim", "cfm")
#' @param fit input object from fitted lavaan model
#' @param table logical input of whether table output is desired. Default is TRUE
#' @param tabletype character input of what type of table(s) is(are) desired.
#'options are "measurement" (i.e,, slopes, intercepts, and (optionally) dMACS),
#' "structural" (i.e., latent slopes, such as actor/partner effects, k parameters), or "both"
#' (i.e., both measurement and structural tables)
#' @param figure logical input of whether figure output is desired. Default is TRUE
#' @param figtype character input of what type of figure is desired.
#' @return Ignore console (prints unneccessary semPlot::semPaths details). More importantly,
#' prints word files for the table(s) and/or figure, outputed to the users working directory
#' @seealso \code{\link{dyadVarNames}} \code{\link{apimSEM}} \code{\link{dydMACS}} which this function relies on
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", yvar="Y", sep = ".",
#' distinguish1 = "1", distinguish2 = "2")
#' apim.script.load = apimSEM(dvn, lvxname = "Conflict",
#' lvyname = "Satisfaction", model = "loading")
#' apim.fit.load <- lavaan::cfa(apim.script.load, data = dat, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#' dyOutput(dvn, model = "cfa", fit, figure = FALSE, dydMACS.x = sexsat.dmacs)

outputModel  <-  function(dvn, model = NULL, fit,
                    table = TRUE, tabletype = NULL,
                    figure = TRUE, figtype = NULL){
  dirs("output")
  if(model=="apim"| model == "mim"){
    if(table==TRUE & figure == FALSE){
      dirs("output/tables")
      if(tabletype== "both"){
        #Make measurement parameter table
         meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")
         gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

        #Extract structural parameters
         struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")
         gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")
        gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")
        gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
      }
    }
    else if(table==FALSE & figure == TRUE){
      dirs("output/figures")
      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw")
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std")
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "raw")
      }
    }
    else if(table==TRUE & figure == TRUE){
      dirs("output/tables")
      dirs("output/figures")
      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")
        gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")
        gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")
        gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")
        gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
      }
      #Make path diagram
      dirs("output/figures")
      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw")
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std")
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "raw")
      }
    }
  }
  else if(model=="cfa"){
    if(table==TRUE & figure == FALSE){
      dirs("output/tables")

      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")
      gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
    }
    else if(table==FALSE & figure == TRUE){
      dirs("output/figures")
      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw")
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std")
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "raw")
      }
    }
    else if(table==TRUE & figure == TRUE){
      dirs("output/tables")
      dirs("output/figures")

      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")
      gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw")
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std")
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "raw")
      }
    }
  }
}

