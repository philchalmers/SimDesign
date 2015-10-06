#' Skeleton functions for simulations
#'
#' This function prints skeleton versions of the required functions and work-flow required
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
#'
#' SimDesign_functions()
#'
SimDesign_functions <- function(main = FALSE){
    cat('\n#-------------------------------------------------------------------')
    cat('\n### Define essential simulation functions. It may be helpful to place these')
    cat('\n### functions in a seperate R file and source() them in')
    cat('\n\nGenerate <- function(condition, fixed_design_elements = NULL) {')
    cat('\n    # Define data generation code ...\n')
    cat('\n    # Return a vector, matrix, data.frame, or list')
    cat('\n    return(data.frame())\n}')
    cat('\n\n')
    cat('Analyse <- function(condition, dat, fixed_design_elements = NULL, parameters = NULL) {')
    cat('\n    # Run statistical analyses of interest ... \n\n    # Return a vector or list\n    return(numeric())\n}')
    cat('\n\n')
    cat('Summarise <- function(condition, results, fixed_design_elements = NULL, parameters_list = NULL) {')
    cat('\n    # Summarise the simulation results ...\n\n    # Return a vector\n    return(numeric())\n}')

    if(main){
        cat('main <- ', paste0(deparse(main), '\n'))
    }
    cat('\n#-------------------------------------------------------------------')

    cat('\n\n### Define design conditions and number of replications')
    cat('\n# Design <- expand.grid(condition1, condition2, ...)')
    cat('\n# replications <- 1000')

    cat('\n\n### Run the simulation')
    cat('\nresults <- runSimulation(Design, replications, ')
    cat('\n    generate=Generate, analyse=Analyse, summarise=Summarise, edit=\'none\')')
    cat('\n\n')
}
