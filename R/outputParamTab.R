#' A Function That Exports Tables based on dySEM models
#'
#' This function takes the model from fitted `dySEM` scripts and
#' creates tables of expected output and/or exports them as `.rtf`s.
#'
#' @param dvn Input dvn list from `scrapeVarCross()`
#' @param model A character input specifying type of model to output:
#'  "cfa", "bidyc", "apim", "mim", "cfm", or "bidys".
#' @param fit input object from fitted `lavaan` model.
#' @param tabletype A character input of what type of table(s) is(are) desired.
#'  Options are "measurement" (i.e., loadings, intercepts, etc.),
#'  "structural" (i.e., latent slopes, such as actor/partner effects, k parameters),
#'  or "both" (i.e., both measurement and structural tables).
#' @param gtTab A logical input indicating whether to generate the table(s) in `gt::gt()` table object format (`TRUE`).
#'  By default (`FALSE`), the table(s) are generated in `tibble::tibble()` format.
#'  Users can also apply the `writeTo` argument if they wish to export the `gt:gt()` table object(s).
#' @param writeTo A character string specifying a directory path to where the `gt::gt()` table object(s) should be saved.
#'  If set to ".", the file(s) will be written to the current working directory.
#'  The default is `NULL`, and examples use a temporary directory created by `tempdir()`.
#'  `writeTo` is only relevant if `gtTab = TRUE`.
#' @param fileName A character string specifying a desired base name for the output `gt::gt()` file(s).
#'  If a `fileName` is not provided (i.e., `fileName = NULL`), then defaults will be used
#'  (e.g., "dySEM_table", "dySEM_table_measurement", or "dySEM_table_structural) based on the `tabletype` argument.
#'  The resulting base name will automatically be appended with a `.rtf` file extension.
#'  `fileName` is only relevant if `gtTab = TRUE` and `writeTo` is specified.
#'
#' @return A `tibble::tibble()` if `gtTab = FALSE` (default), or `gt::gt()` object if `gtTab = TRUE`, of specified model parameter estimates and corresponding statistical tests.
#'
#' @details
#' * If `gtTab = TRUE` and `writeTo` is specified, then output will simultaneously be saved as a `.rtf` file to the user's specified directory.
#' * If output file(s) is(are) successfully saved, a confirmation message will be printed to the console.
#' * If a file with the same name already exists in the user's chosen directory, it will be overwritten.
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
#' outputParamTab(dvnx, model = "cfa", sat.config.mod, tabletype = "measurement",
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
#' outputParamTab(dvnxy, model = "cfa", sat.config.mod, tabletype = "measurement",
#' writeTo = tempdir(), fileName = "APIM_indist")

outputParamTab <- function(dvn,
                           model = NULL,
                           fit,
                           tabletype = NULL,
                           gtTab = FALSE,
                           writeTo = NULL,
                           fileName = NULL
                           ){

  #checking for valid directory path
  if (gtTab == TRUE && !is.null(writeTo)){

    if (!is.character(writeTo)){
      stop("The `writeTo` argument must be a character string. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
    }
    if (!dir.exists(writeTo)){
      stop("The specified directory does not exist. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
    }
    if (!is.null(fileName) && !is.character(fileName)){
      stop("The `fileName` argument must be a character string.")
    }

  }

  #cfa
  if (model == "cfa"){

    if (gtTab == FALSE){
      meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement", gtTab = FALSE)
      return(meas.tab)
    }

    else if (gtTab == TRUE){

      #user specifies writeTo
      if (!is.null(writeTo)){
        meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement", gtTab = TRUE)

        if (is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
          message( #confirmation message
            sprintf(
              "Output stored in: %s/dySEM_table.rtf",
              writeTo)
            )
          }

        else if (!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
          message( #confirmation message
            sprintf(
              "Output stored in: %s/%s.rtf",
              writeTo, fileName)
            )
          }
      }

      #user does not specify writeTo
      else if (is.null(writeTo)){
        meas.tab <- makeTable(dvn, fit, model = "cfa", tabletype = "measurement", gtTab = TRUE)
      }

      return(meas.tab)
    }

  }

  #bidy-c
  else if(model == "bidyc"){

    if (gtTab == FALSE){
      meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement", gtTab = FALSE)
      return(meas.tab)
    }

    else if (gtTab == TRUE){

      #user specifies writeTo
      if (!is.null(writeTo)){
        meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement", gtTab = TRUE)

        if (is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = "dySEM_table.rtf",
                     path = writeTo)
          message( #confirmation message
            sprintf(
              "Output stored in: %s/dySEM_table.rtf",
              writeTo)
          )
        }

        else if (!is.null(fileName)){
          gt::gtsave(meas.tab,
                     filename = sprintf("%s.rtf",
                                        fileName),
                     path = writeTo)
          message( #confirmation message
            sprintf(
              "Output stored in: %s/%s.rtf",
              writeTo, fileName)
          )
        }
      }

      #user does not specify writeTo
      else if (is.null(writeTo)){
        meas.tab <- makeTable(dvn, fit, model = "bidyc", tabletype = "measurement", gtTab = TRUE)
      }

      return(meas.tab)
    }

  }

  #apim/mim
  else if(model == "apim"){

    if(tabletype == "both"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement", gtTab = FALSE)
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural", gtTab = FALSE)
        return(list(measurement = meas.tab, structural = struct.tab))
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = "dySEM_table_measurement.rtf",
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = "dySEM_table_structural.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/dySEM_table_measurement.rtf \n %s/dySEM_table_structural.rtf",
                writeTo, writeTo)
            )
          }

          else if (!is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = sprintf("%s_measurement.rtf",
                                          fileName),
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = sprintf("%s_structural.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/%s_measurement.rtf \n %s/%s_structural.rtf",
                writeTo, fileName, writeTo, fileName)
            )
          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural", gtTab = TRUE)
        }

        return(list(measurement = meas.tab, structural = struct.tab))
      }

      }

    else if(tabletype == "measurement"){

        #gtTab = FALSE
        if (gtTab == FALSE){
          meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement", gtTab = FALSE)
          return(meas.tab)
        }

        #gtTab = TRUE
        else if(gtTab == TRUE){

          #user specifies writeTo
          if (!is.null(writeTo)){
            meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement", gtTab = TRUE)

            if (is.null(fileName)){
              gt::gtsave(meas.tab,
                         filename = "dySEM_table.rtf",
                         path = writeTo)
              message( #confirmation message
                sprintf(
                  "Output stored in: %s/dySEM_table.rtf",
                  writeTo)
              )
            }

            else if (!is.null(fileName)){
              gt::gtsave(meas.tab,
                         filename = sprintf("%s.rtf",
                                            fileName),
                         path = writeTo)
              message( #confirmation message
                sprintf(
                  "Output stored in: %s/%s.rtf",
                  writeTo, fileName)
              )
            }
          }

          #user does not specify writeTo
          else if (is.null(writeTo)){
            meas.tab <- makeTable(dvn, fit, model = "apim", tabletype = "measurement", gtTab = TRUE)
          }

          return(meas.tab)

        }

      }

    else if (tabletype == "structural"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural", gtTab = FALSE)
        return(struct.tab)
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = "dySEM_table.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/dySEM_table.rtf",
                writeTo)
            )
          }

          if (!is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = sprintf("%s.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/%s.rtf",
                writeTo, fileName)
            )

          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "apim", tabletype = "structural", gtTab = TRUE)
        }

        return(struct.tab)

      }

    }

  }

  #mim
  else if(model == "mim"){

    if(tabletype == "both"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        meas.tab <- makeTable(dvn, fit, model = "mim", tabletype = "measurement", gtTab = FALSE)
        struct.tab <- makeTable(dvn, fit, model = "mim", tabletype = "structural", gtTab = FALSE)
        return(list(measurement = meas.tab, structural = struct.tab))
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "mim", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "mim", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = "dySEM_table_measurement.rtf",
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = "dySEM_table_structural.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/dySEM_table_measurement.rtf \n %s/dySEM_table_structural.rtf",
                writeTo, writeTo)
            )
          }

          else if (!is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = sprintf("%s_measurement.rtf",
                                          fileName),
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = sprintf("%s_structural.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/%s_measurement.rtf \n %s/%s_structural.rtf",
                writeTo, fileName, writeTo, fileName)
            )
          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "mim", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "mim", tabletype = "structural", gtTab = TRUE)
        }

        return(list(measurement = meas.tab, structural = struct.tab))
      }

    }

    else if(tabletype == "measurement"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        meas.tab <- makeTable(dvn, fit, model = "mim", tabletype = "measurement", gtTab = FALSE)
        return(meas.tab)
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "mim", tabletype = "measurement", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = "dySEM_table.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/dySEM_table.rtf",
                writeTo)
            )
          }

          else if (!is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = sprintf("%s.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/%s.rtf",
                writeTo, fileName)
            )
          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "mim", tabletype = "measurement", gtTab = TRUE)
        }

        return(meas.tab)

      }

    }

    else if (tabletype == "structural"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        struct.tab <- makeTable(dvn, fit, model = "mim", tabletype = "structural", gtTab = FALSE)
        return(struct.tab)
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "mim", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = "dySEM_table.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/dySEM_table.rtf",
                writeTo)
            )
          }

          if (!is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = sprintf("%s.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/%s.rtf",
                writeTo, fileName)
            )

          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "mim", tabletype = "structural", gtTab = TRUE)
        }

        return(struct.tab)

      }

    }

  }

  #cfm
  else if(model == "cfm"){


    if(tabletype == "both"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement", gtTab = FALSE)
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural", gtTab = FALSE)
        return(list(measurement = meas.tab, structural = struct.tab))
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = "dySEM_table_measurement.rtf",
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = "dySEM_table_structural.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/dySEM_table_measurement.rtf \n %s/dySEM_table_structural.rtf",
                writeTo, writeTo)
            )
          }

          else if (!is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = sprintf("%s_measurement.rtf",
                                          fileName),
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = sprintf("%s_structural.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/%s_measurement.rtf \n %s/%s_structural.rtf",
                writeTo, fileName, writeTo, fileName)
            )
          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural", gtTab = TRUE)
        }

        return(list(measurement = meas.tab, structural = struct.tab))
      }

    }

    else if(tabletype == "measurement"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement", gtTab = FALSE)
        return(meas.tab)
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = "dySEM_table.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/dySEM_table.rtf",
                writeTo)
            )
          }

          else if (!is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = sprintf("%s.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/%s.rtf",
                writeTo, fileName)
            )
          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "measurement", gtTab = TRUE)
        }

        return(meas.tab)

      }

    }

    else if (tabletype == "structural"){
      #gtTab = FALSE
      if (gtTab == FALSE){
        struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural", gtTab = FALSE)
        return(struct.tab)
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = "dySEM_table.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/dySEM_table.rtf",
                writeTo)
            )
          }

          if (!is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = sprintf("%s.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/%s.rtf",
                writeTo, fileName)
            )

          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "cfm", tabletype = "structural", gtTab = TRUE)
        }

        return(struct.tab)

      }

    }

  }

  #bidy-s
  else if(model == "bidys"){

    if(tabletype == "both"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement", gtTab = FALSE)
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural", gtTab = FALSE)
        return(list(measurement = meas.tab, structural = struct.tab))
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = "dySEM_table_measurement.rtf",
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = "dySEM_table_structural.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/dySEM_table_measurement.rtf \n %s/dySEM_table_structural.rtf",
                writeTo, writeTo)
            )
          }

          else if (!is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = sprintf("%s_measurement.rtf",
                                          fileName),
                       path = writeTo)
            gt::gtsave(struct.tab,
                       filename = sprintf("%s_structural.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: \n %s/%s_measurement.rtf \n %s/%s_structural.rtf",
                writeTo, fileName, writeTo, fileName)
            )
          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement", gtTab = TRUE)
          struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural", gtTab = TRUE)
        }

        return(list(measurement = meas.tab, structural = struct.tab))
      }

    }

    else if(tabletype == "measurement"){

      #gtTab = FALSE
      if (gtTab == FALSE){
        meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement", gtTab = FALSE)
        return(meas.tab)
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = "dySEM_table.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/dySEM_table.rtf",
                writeTo)
            )
          }

          else if (!is.null(fileName)){
            gt::gtsave(meas.tab,
                       filename = sprintf("%s.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/%s.rtf",
                writeTo, fileName)
            )
          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          meas.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "measurement", gtTab = TRUE)
        }

        return(meas.tab)

      }

    }

    else if (tabletype == "structural"){
      #gtTab = FALSE
      if (gtTab == FALSE){
        struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural", gtTab = FALSE)
        return(struct.tab)
      }

      #gtTab = TRUE
      else if(gtTab == TRUE){

        #user specifies writeTo
        if (!is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural", gtTab = TRUE)

          if (is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = "dySEM_table.rtf",
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/dySEM_table.rtf",
                writeTo)
            )
          }

          if (!is.null(fileName)){
            gt::gtsave(struct.tab,
                       filename = sprintf("%s.rtf",
                                          fileName),
                       path = writeTo)
            message( #confirmation message
              sprintf(
                "Output stored in: %s/%s.rtf",
                writeTo, fileName)
            )

          }
        }

        #user does not specify writeTo
        else if (is.null(writeTo)){
          struct.tab <- makeTable(dvn, fit, model = "bidys", tabletype = "structural", gtTab = TRUE)
        }

        return(struct.tab)

      }

    }

  }

}
