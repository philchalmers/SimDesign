.onAttach <- function(libname, pkgname) {
    if(interactive())
        packageStartupMessage("See ?SimFunctions to get started with SimDesign")
}

.onLoad <- function(libname, pkgname) {
    # Initialize RNG to ensure .Random.seed exists
    if(!exists(".Random.seed", envir = .GlobalEnv))
        runif(1)
}
