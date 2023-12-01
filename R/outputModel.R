#' A Function That Exports Tables and/or SEM Diagrams based on dySEM models
#'
#' This function takes the  model from fitted dySEM() scripts and
#' exports table(s) and/or a path diagram figure of expected output.
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param model character input specifying type of model to output (e.g., "cfa", "apim", "cfm")
#' @param fit input object from fitted lavaan model
#' @param table logical input of whether table output is desired. Default is TRUE
#' @param tabletype character input of what type of table(s) is(are) desired.
#'options are "measurement" (i.e,, loadings, intercepts,),
#' "structural" (i.e., latent slopes, such as actor/partner effects, k parameters), or "both"
#' (i.e., both measurement and structural tables)
#' @param figure logical input of whether figure output is desired. Default is TRUE
#' @param figtype character input of what type of figure is desired
#' @param writeout logical (default is FALSE) for whether outputted table and/or figure
#' should be written to an output subdirectory in current working directory
#' @return Ignore console (prints unnecessary semPlot::semPaths details). More importantly,
#' prints word files for the table(s) and/or figure, outputted to the users working directory
#' @export
#' @examples
#' dvnx <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2")
#' sat.config.script <-  scriptCFA(dvnx, lvname = "Sat", model = "configural")
#' sat.config.mod <- lavaan::cfa(sat.config.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#' outputModel(dvnx, model = "cfa", fit = sat.config.mod, table = TRUE,
#' tabletype = "measurement", figure = "TRUE", figtype = "standardized")

#' dvnxy <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
#' x_delim1 = ".", x_delim2="_", distinguish_1="1", distinguish_2="2",
#' y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#' apim.script.config <-  scriptAPIM(dvnxy, lvxname = "Sat", lvyname = "Com", model = "configural")
#' apim.config.fit <- lavaan::cfa(apim.script.config, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#' outputModel(dvnxy, model = "apim", fit = apim.config.fit, table = TRUE,
#' tabletype = "measurement", figure = "TRUE", figtype = "standardized")

outputModel  <-  function(dvn, model = NULL, fit,
                    table = TRUE, tabletype = NULL,
                    figure = TRUE, figtype = NULL,
                    writeout = FALSE){
  if(model=="cfa"){
    if(table==TRUE & figure == FALSE){



      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/tables")
        gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
      }
    }
    else if(table==FALSE & figure == TRUE){

      if(isTRUE(writeout)){
        dirs("output")
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
    else if(table==TRUE & figure == TRUE){

      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/tables")
        dirs("output/figures")

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
  else if(model=="bidyc"){
    if(table==TRUE & figure == FALSE){

      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement")

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/tables")
        gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

      }

    }
    else if(table==FALSE & figure == TRUE){

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/figures")

        #Make path diagram
        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw")
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std")
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab")
        }
      }


    }
    else if(table==TRUE & figure == TRUE){

      meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement")

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/tables")
        dirs("output/figures")

        gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw")
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std")
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab")
        }
      }

    }
  }
  else if(model=="apim"| model == "mim"){
    if(table==TRUE & figure == FALSE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

        }
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")
        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")
        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
    }
    else if(table==FALSE & figure == TRUE){

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/figures")
        #Make path diagram
        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw")
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std")
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab")
        }
      }

    }
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }

      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }

      #Make path diagram

      if(figtype == "unstandardized"){
        if(isTRUE(writeout)){
          dirs("output/figures")
          semplot <- makeFigure(fit, type = "raw")
        }
      }
      else if(figtype == "standardized"){
          if(isTRUE(writeout)){
            dirs("output/figures")
            semplot <- makeFigure(fit, type = "std")
          }

      }
      else if(figtype == "labels"){
        if(isTRUE(writeout)){
          dirs("output/figures")
          semplot <- makeFigure(fit, type = "lab")
        }
      }
    }
  }
  else if(model == "cfm"){
    if(table==TRUE & figure == FALSE){
      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
          }
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
    }
    else if(table==FALSE & figure == TRUE){

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/figures")
        #Make path diagram
        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw")
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std")
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab")
        }
      }
    }
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")
        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          dirs("output/figures")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))

        }
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          dirs("output/figures")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          dirs("output/figures")
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }

      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw")
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std")
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab")
      }
    }
  }
  else if(model == "bidys"){
    if(table==TRUE & figure == FALSE){
      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }

      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
    }
    else if(table==FALSE & figure == TRUE){

      if(isTRUE(writeout)){
        dirs("output")
        dirs("output/figures")
        #Make path diagram
        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw")
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std")
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab")
        }
      }
    }
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          dirs("output/figures")
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          dirs("output/figures")
          gt::gtsave(meas.tab, filename = sprintf("./output/tables/%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        if(isTRUE(writeout)){
          dirs("output")
          dirs("output/tables")
          dirs("output/figures")
          gt::gtsave(struct.tab, filename = sprintf("./output/tables/%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")))
        }
      }

      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw")
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std")
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab")
      }
    }
  }
}

