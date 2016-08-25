#' Skeleton functions for simulations
#'
#' This function prints skeleton versions of the required functions and work-flow required
#' to run simulations, complete with the correct inputs, class of outputs, and option comments to
#' help with the initial definitions. Use this at the start when defining your simulation. The
#' recommended approach when using the \code{RStudio} IDE is to write the simulation template to two
#' separate files for easier debugging/sourcing.
#'
#' The function \code{SimDesign_functions} is deprecated and will be
#' removed in a future release.
#'
#' @param filename a character vector indicating whether the output should be saved to two respective files
#'   containing the simulation design and the functional components, respectively. Using this option
#'   is generally the recommended approach when beginning to write a Monte Carlo simulation
#'
#' @param comments logical; include helpful comments? Default is \code{FALSE}
#'
#' @param singlefile logical; when \code{filename} is included, put output in one files? When \code{FALSE} the
#'   output is saved to two separate files containing the functions and design definitions. Default is
#'   \code{FALSE}
#'
#' @param summarise include \code{summarise} function? Default is \code{TRUE}
#'
#' @aliases SimFunctions
#'
#' @export SimFunctions
#'
#' @examples
#'
#' SimFunctions()
#' SimFunctions(comments = TRUE) #with helpful comments
#'
#'\dontrun{
#'
#' # write output to two files (recommended)
#' SimFunctions('mysim')
#'
#' # write output files to a single file with comments
#' SimFunctions('mysim', singlefile = TRUE, comments = TRUE)
#' }
#'
SimFunctions <- function(filename = NULL, comments = FALSE, singlefile = FALSE, summarise = TRUE){
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
        cat('Analyse <- function(condition, dat, fixed_objects = NULL) {')
        if(comments) cat('\n    # Run statistical analyses of interest ... \n')
        if(comments) cat('\n    # Return a named vector or list')
        cat('\n    ret <- c(stat1 = NaN, stat2 = NaN)\n    ret\n}')
        cat('\n\n')
        if(summarise){
            cat('Summarise <- function(condition, results, fixed_objects = NULL) {')
            if(comments) cat('\n    # Summarise the simulation results ...\n')
            if(comments) cat('\n    # Return a named vector of results')
            cat('\n    ret <- c(bias = NaN, RMSE = NaN)\n    ret\n}\n\n')
        }
    }

    TAIL <- function(){
        LINE()
        if(comments) cat('\n### Run the simulation\n')
        cat('\nresults <- runSimulation(design=Design, replications=1000, ')
        cat(sprintf('\n    generate=Generate, analyse=Analyse, %sedit=\'none\')',
                    if(summarise) 'summarise=Summarise, ' else ''))
        cat('\n\n')
    }

    #main
    if(!is.null(filename)){
        if(file.exists(paste0(filename, '.R')))
            stop('File already exists! Please rename input or rename/remove existing files', call.=FALSE)
    }
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
