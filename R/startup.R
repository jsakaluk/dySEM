.onAttach <- function(libname, pkgname) {
  message <- c("\n Welcome to dySEM!",
               "\n \n Happy coding!")
  packageStartupMessage(message)
}