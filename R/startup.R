.onAttach <- function(libname, pkgname) {
  message <- c("\n Welcome to dySEM!",
               "\n \n Please support your academic developers by citing dySEM; this helps us justify to our home institutions the time we spend on expanding and improving dySEM:",
               "\n \n Sakaluk, J. K., Fisher, A. N., & Kilshaw, R. K. (2021)\n
               Personal Relationships, 28(1), 190-226. \n
               https://doi.org/10.1111/pere.12341",
               "\n \n dySEM is also heavily dependent on the lavaan package (please cite it too):",
               "\n \n Rosseel, Y. (2012). lavaan: An R package for structural equation modeling\n
               Journal of Statistical Software, 48, 1-36. \n
               https://doi.org/10.18637/jss.v048.i02",
               "\n \n And should you use dySEM's outputParamFig() function, please be sure to",
               "\n \n also cite the semPlot package (upon which it depends):",
               "\n \n Epskamp, S. (2015). semPlot: Unified visualizations of structural equation\n
               models. Structural Equation Modeling: A Multidisciplinary Journal, 22(3), 474-483.\n
               https://doi.org/10.1080/10705511.2014.937847")
  packageStartupMessage(message)
}
