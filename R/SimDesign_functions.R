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
#' @param comments logical; include helpful comments?
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
SimDesign_functions <- function(filename = NULL, comments = TRUE){
    if(!is.null(filename)) sink(paste0(filename, '-functions.R'))
    cat('#-------------------------------------------------------------------')
    if(comments) cat('\n### Define essential simulation functions')
    cat('\n\nGenerate <- function(condition, fixed_design_elements = NULL) {')
    if(comments) cat('\n    # Define data generation code ...\n')
    if(comments) cat('\n    # Return a vector, matrix, data.frame, or list')
    cat('\n    dat <- data.frame()')
    cat('\n    dat\n}')
    cat('\n\n')
    cat('Analyse <- function(condition, dat, fixed_design_elements = NULL, parameters = NULL) {')
    if(comments) cat('\n    # Run statistical analyses of interest ... \n')
    if(comments) cat('\n    # Return a named vector or list')
    cat('\n    ret <- c(stat1=NaN, stat2=NaN)\n    ret\n}')
    cat('\n\n')
    cat('Summarise <- function(condition, results, fixed_design_elements = NULL, parameters_list = NULL) {')
    if(comments) cat('\n    # Summarise the simulation results ...\n')
    if(comments) cat('\n    # Return a named vector of results')
    cat('\n    ret <- c(bias=NaN, RMSE=NaN)\n    ret\n}\n\n')

    if(!is.null(filename)) sink()
    if(!is.null(filename)){
        sink(paste0(filename, '.R'))
        cat('#-------------------------------------------------------------------\n')
        cat('\nlibrary(SimDesign)\n')
        if(comments) cat('\n### Source in essential functions')
        cat('\n# setwd(\"', getwd(), '\")', sep='')
        cat('\nsource(\"', paste0(filename, '-functions.R\"'), ')', sep='')
        cat('\n')
    } else {
        cat('#-------------------------------------------------------------------\n')
        cat('\nlibrary(SimDesign)\n')
    }
    if(comments) cat('\n### Define design conditions and number of replications')
    cat('\nDesign <- expand.grid(condition1, condition2)')
    cat('\nReplications <- 1000\n')
    if(comments) cat('\n### Run the simulation')
    cat('\nresults <- runSimulation(design=Design, replications=Replications, ')
    cat('\n    generate=Generate, analyse=Analyse, summarise=Summarise, edit=\'none\')')
    cat('\n\n')
    if(!is.null(filename)) sink()
    invisible()
}
