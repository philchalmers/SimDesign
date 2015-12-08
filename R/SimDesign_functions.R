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
#' @param singlefile logical; when \code{filename} is included, put output in one files? When FALSE the
#'   output is saved to two seperate files containing the functions and design defintiions
#'
#' @aliases SimDesign_functions
#'
#' @export SimDesign_functions
#'
#' @examples
#'
#' SimDesign_functions()
#' SimDesign_functions(comments=FALSE) #without comments
#'
#'\dontrun{
#'
#' # write output to two files
#' SimDesign_functions('mysim')
#'
#' # write output files to a single file without comments
#' SimDesign_functions('mysim', comments = FALSE, singlefile = TRUE)
#' }
#'
SimDesign_functions <- function(filename = NULL, comments = TRUE, singlefile = FALSE){
    LINE <- function()
        cat('#-------------------------------------------------------------------\n')
    HEAD <- function(){
        LINE()
        cat('\nlibrary(SimDesign)\n')
        if(comments)
            cat('\n### Define design conditions')
        cat('\nDesign <- expand.grid(condition1 = NA,
                      condition2 = NA)\n\n')
        if(!is.null(filename) && !singlefile){
            if(comments) cat('### Source in essential functions\n')
            cat('# setwd(\"', getwd(), '\")', sep='')
            cat('\nsource(\"', paste0(filename, '-functions.R\"'), ')\n\n', sep='')
        }
    }

    FUNCTIONS <- function(){
        LINE()
        if(comments) cat('\n### Define essential simulation functions\n')
        cat('\nGenerate <- function(condition, fixed_objects = NULL) {')
        if(comments) cat('\n    # Define data generation code ...\n')
        if(comments) cat('\n    # Return a vector, matrix, data.frame, or list')
        cat('\n    dat <- data.frame()')
        cat('\n    dat\n}')
        cat('\n\n')
        cat('Analyse <- function(condition, dat, fixed_objects = NULL, parameters = NULL) {')
        if(comments) cat('\n    # Run statistical analyses of interest ... \n')
        if(comments) cat('\n    # Return a named vector or list')
        cat('\n    ret <- c(stat1 = NaN, stat2 = NaN)\n    ret\n}')
        cat('\n\n')
        cat('Summarise <- function(condition, results, fixed_objects = NULL, parameters_list = NULL) {')
        if(comments) cat('\n    # Summarise the simulation results ...\n')
        if(comments) cat('\n    # Return a named vector of results')
        cat('\n    ret <- c(bias = NaN, RMSE = NaN)\n    ret\n}\n\n')
    }

    TAIL <- function(){
        LINE()
        if(comments) cat('\n### Run the simulation\n')
        cat('\nresults <- runSimulation(design=Design, replications=1000, ')
        cat('\n    generate=Generate, analyse=Analyse, summarise=Summarise, edit=\'none\')')
        cat('\n\n')
    }

    #main
    if(is.null(filename) || singlefile){
        if(singlefile)
            if(!is.null(filename)) sink(paste0(filename, '.R'))
        HEAD()
        FUNCTIONS()
        TAIL()
        if(!is.null(filename)) sink()
    } else {
        sink(paste0(filename, '.R'))
        HEAD()
        TAIL()
        sink()
        sink(paste0(filename, '-functions.R'))
        FUNCTIONS()
        sink()
    }
    invisible()
}
