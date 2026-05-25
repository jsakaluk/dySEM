.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return(invisible(NULL))
  }
  txt <- cli::ansi_strip(paste(utils::capture.output(
    {
      cli::cli_rule(left = cli::style_bold("dySEM"))
      cli::cli_alert_info(paste0(
        "Welcome to {.strong ",
        pkgname,
        "!} Citations in publications and GitHub stars help justify ",
        "maintenance - thank you! Run {.code citation('dySEM')} for BibTeX and ",
        "related methodological references."
      ))
      cli::cli_bullets(c(
        "i" = paste(
          "Primary:",
          "{.strong Sakaluk} & {.strong Camanto} (2026). {.emph Journal of Open Source Software},",
          "11(121), 10115. {.url https://doi.org/10.21105/joss.10115}"
        ),
        "*" = paste(
          "{.strong lavaan}:",
          "please cite ({.code citation('lavaan')}); dySEM relies on it for model fitting."
        ),
        "*" = paste(
          "{.strong semPlot}:",
          "please cite ({.code citation('semPlot')}) when using {.fn outputParamFig}."
        )
      ))
      cli::cli_rule()
    },
    type = "message"
  ), collapse = "\n"))
  packageStartupMessage(paste0(txt, "\n"))
}
