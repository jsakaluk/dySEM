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
#' @param writeout A character string specifying a directory path to where the output file(s) should be written. 
#' If set to “.”, the output files will be written to the current working directory. 
#' The default is NA, which will throw an error. 
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
#' outputModel(dvnx, model = "cfa", fit = sat.config.mod, table = TRUE,
#' tabletype = "measurement", figure = "TRUE", figtype = "standardized", writeout = tempdir())

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
#' tabletype = "measurement", figure = "TRUE", figtype = "standardized", writeout = tempdir())

outputModel  <-  function(dvn, model = NULL, fit,
                    table = TRUE, tabletype = NULL,
                    figure = TRUE, figtype = NULL,
                    writeout = NA){
  
  # checking for valid directory path
  if (is.na(writeout)){
    stop("Must specify a directory to which the file should be saved. \n Use writeout = '.' to save output file(s) in the current working directory.")
  }
  if (!is.character(writeout)){
    stop("The `writeout` argument must be a character string. \n Use writeout = '.' to save output file(s) in the current working directory.")
    }
  if (!dir.exists(writeout)){ 
    stop("The specified directory does not exist. \n Use writeout = '.' to save output file(s) in the current working directory.")
    }
  
  if(model=="cfa"){
    if(table==TRUE & figure == FALSE){



      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")

      gt::gtsave(meas.tab,
                 filename = sprintf("%s_Measurement.rtf", stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                 path = writeout)
    }
    
    else if(table==FALSE & figure == TRUE){


      
        #Make path diagram
        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw", writeout)
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std", writeout)
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab", writeout)
        }
        
    }
    else if(table==TRUE & figure == TRUE){

      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement")

      gt::gtsave(meas.tab,
                 filename = sprintf("%s_Measurement.rtf", stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                 path = writeout)
      
      #Make measurement parameter table
        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw", writeout)
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std", writeout)
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab", writeout)
        }
      
    }
  }
  else if(model=="bidyc"){
    if(table==TRUE & figure == FALSE){

      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement")

      gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf", stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
    }
    
    else if(table==FALSE & figure == TRUE){

        #Make path diagram
        if(figtype == "unstandardized"){
          semplot <- makeFigure(fit, type = "raw", writeout)
        }
        else if(figtype == "standardized"){
          semplot <- makeFigure(fit, type = "std", writeout)
        }
        else if(figtype == "labels"){
          semplot <- makeFigure(fit, type = "lab", writeout)
        }
      


    }
    else if(table==TRUE & figure == TRUE){
      
      #Make measurement parameter table
      meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement")

      gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                 path = writeout)

      
      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw", writeout)
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std", writeout)
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab", writeout)
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
        
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
      }
      
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")
       
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
      }
      
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")
        
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
      }
    }
    
    else if(table==FALSE & figure == TRUE){

      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw", writeout)
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std", writeout)
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab", writeout)
      }
    }
    
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
      }
      
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement")

        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
      }
      
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural")

        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
      }

      #Make path diagram

      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw", writeout)
      }
      
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std", writeout)
      }
      
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab", writeout)
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

        
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
    }
    else if(table==FALSE & figure == TRUE){
      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw", writeout)
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std", writeout)
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab", writeout)
      }
      
    }
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")
        
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
        
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement")

        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural")

        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }

      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw", writeout)
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std", writeout)
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab", writeout)
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

      
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        

      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
      else if(tabletype == "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

       
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
    }
    else if(table==FALSE & figure == TRUE){


      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw", writeout)
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std", writeout)
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab", writeout)
      }
      
    }
    else if(table==TRUE & figure == TRUE){

      if(tabletype== "both"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")

   
        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
      else if(tabletype== "measurement"){
        #Make measurement parameter table
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement")

  
        gt::gtsave(meas.tab, filename = sprintf("%s_Measurement.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }
      else if(tabletype== "structural"){
        #Extract structural parameters
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural")


        gt::gtsave(struct.tab, filename = sprintf("%s_structural.rtf",stringr::str_remove_all(as.character(fit@call$model), "[.]")),
                   path = writeout)
        
      }

      #Make path diagram
      if(figtype == "unstandardized"){
        semplot <- makeFigure(fit, type = "raw", writeout)
      }
      else if(figtype == "standardized"){
        semplot <- makeFigure(fit, type = "std", writeout)
      }
      else if(figtype == "labels"){
        semplot <- makeFigure(fit, type = "lab", writeout)
      }
    }
  }
}

