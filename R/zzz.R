.onAttach <- function(libname, pkgname) {
    if(interactive())
        packageStartupMessage("See ?SimFunctions to get started with SimDesign")
}
