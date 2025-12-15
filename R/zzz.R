.onAttach <- function(libname, pkgname) {
    # Initialize RNG to ensure .Random.seed exists
    if(!exists(".Random.seed", envir = .GlobalEnv))
        runif(1)

    if(interactive())
        packageStartupMessage("See ?SimFunctions to get started with SimDesign")
}
