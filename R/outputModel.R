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
#' @param writeTo A character string specifying a directory path to where the file(s) should be saved.
#' If set to “.”, the file(s) will be written to the current working directory.
#' The default is NULL (which will throw an error), and examples use a temporary directory created by tempdir().
#' @param fileName A character string specifying a desired base name for the output file(s).
#' If a `fileName` not provided (i.e., default fileName = NULL), then defaults will be used
#' (e.g., "dySEM_table"/"dySEM_table_Measurement"/"dySEM_table_Structural for tables; "dySEM_figure" for figures).
#' The specified name will be automatically appended with the appropriate file extension (i.e., .rtf for tables; .png for figures).
#' @details
#' If a file with the same name already exists in the user's chosen directory,
#'  it will be overwritten.
#' @return Ignore console (prints unnecessary semPlot::semPaths details). More importantly,
#' prints word files for the table(s) and/or figure, outputted to the users working directory
#' @export
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
#' outputModel(dvnx, model = "cfa", fit = sat.config.mod, table = TRUE,
#' tabletype = "measurement", figure = "TRUE", figtype = "standardized",
#' writeTo = tempdir(), fileName = "dCFA_configural")

#' dvnxy <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
#' x_delim2="_", distinguish_1="1", distinguish_2="2",
#' y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#'
#' apim.indist.script <-  scriptAPIM(dvnxy, lvxname = "Sat", lvyname = "Com", est_k = TRUE)
#'
#' apim.indist.mod <- lavaan::cfa(apim.indist.script, data = commitmentQ, std.lv = FALSE,
#' auto.fix.first= FALSE, meanstructure = TRUE)
#'
#' outputModel(dvnxy, model = "apim", fit = apim.indist.mod, table = TRUE,
#' tabletype = "measurement", figure = "TRUE", figtype = "standardized",
#' writeTo = tempdir(), fileName = "APIM_indist")

outputModel  <-  function(dvn, model = NULL, fit,
                    table = TRUE, tabletype = NULL,
                    figure = TRUE, figtype = NULL,
                    writeTo = NULL,
                    fileName = NULL){

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

  if(model=="cfa"){
    if(table==TRUE & figure == FALSE){

      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")

      #user provides fileName
      if(!is.null(fileName)){
        gt::gtsave(meas.tab,
                   filename = sprintf("%s.rtf",
                                      fileName),
                   path = writeTo)
      }

      #default
      if(is.null(fileName)){
        gt::gtsave(meas.tab,
                   filename = "dySEM_table.rtf",
                   path = writeTo)
      }

    }
    else if(table==FALSE & figure == TRUE){


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
    else if(table==TRUE & figure == TRUE){

      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")


      #user provides fileName
      if(!is.null(fileName)){
        gt::gtsave(meas.tab,
                   filename = sprintf("%s.rtf",
                                      fileName),
                   path = writeTo)
      }

      #default
      if(is.null(fileName)){

        gt::gtsave(meas.tab,
                   filename = "dySEM_table.rtf",
                   path = writeTo)
      }


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
  }
  else if(model=="bidyc"){
    if(table==TRUE & figure == FALSE){

      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement")

      #user provides fileName
      if(!is.null(fileName)){
        gt::gtsave(meas.tab,
                   filename = sprintf("%s.rtf",
                                      fileName),
                   path = writeTo)
      }

      #default
      if(is.null(fileName)){
        gt::gtsave(meas.tab,
                   filename = "dySEM_table.rtf",
                   path = writeTo)
      }
    }
    else if(table==FALSE & figure == TRUE){

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
    else if(table==TRUE & figure == TRUE){

      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement")

      #user provides fileName
      if(!is.null(fileName)){
        gt::gtsave(meas.tab,
                   filename = sprintf("%s.rtf",
                                      fileName),
                   path = writeTo)
      }

      #default
      if(is.null(fileName)){
        gt::gtsave(meas.tab,
                   filename = "dySEM_table.rtf",
                   path = writeTo)
      }


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
  }
  else if(model=="apim"| model == "mim"){
    if(table==TRUE & figure == FALSE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s_Measurement.rtf",
                                        fileName),
                     path = writeTo)

          gt::gtsave(struct.tab,
                     filename = sprintf("%s_Structural.rtf",
                                        fileName),
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table_Measurement.rtf",
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = "dySEM_table_Structural.rtf",
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.
        }
      }

      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }
      }

      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }

      }
    }

    else if(table==FALSE & figure == TRUE){

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

    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){

          gt::gtsave(meas.tab,
                     filename = sprintf("%s_Measurement.rtf",
                                        fileName),
                     path = writeTo)

          gt::gtsave(struct.tab,
                     filename = sprintf("%s_Structural.rtf",
                                        fileName),
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent the files from overwriting each other.
        }

        #default
        if(is.null(fileName)){

          gt::gtsave(meas.tab,
                     filename = "dySEM_table_Measurement.rtf",
                     path = writeTo)

          gt::gtsave(struct.tab,
                     filename = "dySEM_table_Structural.rtf",
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.
        }
      }

      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)

        }
      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")


        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }
      }

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
  }
  else if(model == "cfm"){
    if(table==TRUE & figure == FALSE){
      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s_Measurement.rtf",
                                        fileName),
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = sprintf("%s_Structural.rtf",
                                        fileName),
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.

        }

        #default
        if(is.null(fileName)){

          gt::gtsave(meas.tab,
                     filename = "dySEM_table_Measurement.rtf",
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = "dySEM_table_Structural.rtf",
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.

        }

      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }

      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }


      }
    }
    else if(table==FALSE & figure == TRUE){
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
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")


        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s_Measurement.rtf",
                                        fileName),
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = sprintf("%s_Structural.rtf",
                                        fileName),
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.

        }

        #default
        if(is.null(fileName)){

          gt::gtsave(meas.tab,
                     filename = "dySEM_table_Measurement.rtf",
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = "dySEM_table_Structural.rtf",
                     path = writeTo)
          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.

        }

      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }


      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)

        }

      }

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
  }
  else if(model == "bidys"){
    if(table==TRUE & figure == FALSE){
      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s_Measurement.rtf",
                                        fileName),
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = sprintf("%s_Structural.rtf",
                                        fileName),
                     path = writeTo)

          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.
        }

        #default
        if(is.null(fileName)){

          gt::gtsave(meas.tab,
                     filename = "dySEM_table_Measurement.rtf",
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = "dySEM_table_Structural.rtf",
                     path = writeTo)

          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.
        }


      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }

      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }
      }
    }
    else if(table==FALSE & figure == TRUE){


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
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s_Measurement.rtf",
                                        fileName),
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = sprintf("%s_Structural.rtf",
                                        fileName),
                     path = writeTo)

          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table_Measurement.rtf",
                     path = writeTo)
          gt::gtsave(struct.tab,
                     filename = "dySEM_table_Structural.rtf",
                     path = writeTo)

          #"_Measurement"/"_Structural" is appended to prevent files from overwriting each other.
        }




      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }


      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

        #user provides fileName
        if(!is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
        }

        #default
        if(is.null(fileName)){
          gt::gtsave(struct.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
        }
      }

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
  }
}

