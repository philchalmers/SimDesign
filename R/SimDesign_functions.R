#' Skeleton functions for simulations
#'
#' This function prints skeleton versions of the required function for simulations, complete with
#' the correct inputs and class of outputs.
#'
#' @param main logical; include a skeleton of the \code{main} function?
#'
#' @aliases SimDesign_functions
#'
#' @export SimDesign_functions
#'
#' @examples
#' \dontrun{
#'
#' # This is the default main function
#' SimDesign_functions()
#'
#' }
SimDesign_functions <- function(main = FALSE){
    cat('\nsim <- function(condition) {')
    cat('\n    ... \n    return(list(dat=data.frame(), parameters=list()))\n}')
    cat('\n\n')
    cat('compute <- function(simlist, condition) {')
    cat('\n    ... \n    return(numeric())    # alternatively, return(list())\n}')
    cat('\n\n')
    cat('collect <- function(results, parameters, condition) {')
    cat('\n    ... \n    return(numeric())\n}')

    if(main){
        cat('main <- ', paste0(deparse(main), '\n'))
    }
    cat('\n\n')
}
