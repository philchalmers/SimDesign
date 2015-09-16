#' Skeleton functions for simulations
#'
#' This function prints skeleton versions of the required functions and workflow required
#' to run simulations, complete with the correct inputs and class of outputs. Use this at the start
#' when defining your simulation.
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
#' SimDesign_functions()
#'
#' }
SimDesign_functions <- function(main = FALSE){
    cat('\n#-------------------------------------------------------------------')
    cat('\n### Define essential simulation functions. It may be helpful to place these')
    cat('\n### functions in a seperate R file and source() them in')
    cat('\n\nGenerate <- function(condition) {')
    cat('\n    ... \n    return(list(dat=data.frame(), parameters=list()))\n}')
    cat('\n\n')
    cat('Analyse <- function(simlist, condition) {')
    cat('\n    ... \n    return(numeric())    # alternatively, return(list())\n}')
    cat('\n\n')
    cat('Summerise <- function(results, parameters, condition) {')
    cat('\n    ... \n    return(numeric())\n}')

    if(main){
        cat('main <- ', paste0(deparse(main), '\n'))
    }
    cat('\n#-------------------------------------------------------------------')

    cat('\n\n### Define design conditions and number of replications')
    cat('\n# Design <- expand.grid(condition1, condition2, ...)')
    cat('\n# replications <- 1000')

    cat('\n\n### Run the simulation')
    cat('\nresults <- runSimulation(Design, replications, ')
    cat('\n    generate=Generate, analyse=Analyse, summerise=Summerise, edit=\'none\')')
    cat('\n\n')
}
