#' Skeleton functions for simulations
#'
#' This function prints skeleton versions of the required functions and work-flow required
#' to run simulations, complete with the correct inputs and class of outputs. Use this at the start
#' when defining your simulation.
#'
#' @param filename a character vector indicating whether the output should be saved to two respective files
#'   containing the simulation design and the functional components, respectively. Using this option
#'   is generally the recommended approach when designing simulations
#'
#' @aliases SimDesign_functions
#'
#' @export SimDesign_functions
#'
#' @examples
#'
#' SimDesign_functions()
#'
#'\dontrun{
#'
#' # write output files
#' SimDesign_functions('mysim')
#' }
#'
SimDesign_functions <- function(filename = NULL){
    if(!is.null(filename)) sink(paste0(filename, '-functions.R'))
    cat('#-------------------------------------------------------------------')
    cat('\n### Define essential simulation functions')
    if(is.null(filename)) cat('\nlibrary(SimDesign)')
    cat('\n\nGenerate <- function(condition, fixed_design_elements = NULL) {')
    cat('\n    # Define data generation code ...\n')
    cat('\n    # Return a vector, matrix, data.frame, or list')
    cat('\n    return(data.frame())\n}')
    cat('\n\n')
    cat('Analyse <- function(condition, dat, fixed_design_elements = NULL, parameters = NULL) {')
    cat('\n    # Run statistical analyses of interest ... \n\n    # Return a named vector or list\n    return(numeric())\n}')
    cat('\n\n')
    cat('Summarise <- function(condition, results, fixed_design_elements = NULL, parameters_list = NULL) {')
    cat('\n    # Summarise the simulation results ...\n\n    # Return a vector\n    return(numeric())\n}\n')

    if(!is.null(filename)) sink()
    if(!is.null(filename)){
        sink(paste0(filename, '.R'))
        cat('#-------------------------------------------------------------------')
        cat('\nlibrary(SimDesign)')
        cat('\n\n### Source in essential functions')
        cat('\n# setwd(\"', getwd(), '\")', sep='')
        cat('\nsource(\"', paste0(filename, '-functions.R\"'), ')', sep='')
        cat('\n')
    }
    cat('\n### Define design conditions and number of replications')
    cat('\nDesign <- expand.grid(condition1, condition2, ...)')
    cat('\nreplications <- 1000')
    cat('\n\n### Run the simulation')
    cat('\nresults <- runSimulation(Design, replications, ')
    cat('\n    generate=Generate, analyse=Analyse, summarise=Summarise, edit=\'none\')')
    cat('\n\n')
    if(!is.null(filename)) sink()
    invisible()
}
