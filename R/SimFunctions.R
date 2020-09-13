#' Template-based generation of the Generate-Analyse-Summarise functions
#'
#' This function prints template versions of the required \code{Design} and Generate-Analyse-Summarise functions
#' for \code{SimDesign} to run simulations. Templated output comes complete with the correct inputs,
#' class of outputs, and optional comments to help with the initial definitions.
#' Use this at the start of your Monte Carlo simulation study. Following
#' the definition of the \code{SimDesign} template file please refer to detailed the information
#' in \code{\link{runSimulation}} for how to edit this template to make a working simulation study.
#'
#' The recommended approach to organizing Monte Carlo simulation files is to first save the template generated
#' by this function to the hard-drive by passing a suitable \code{filename} argument (which, if users are interacting
#' with R via the RStudio IDE, will also open the template file after it has been saved). For larger simulations, two
#' separate files could also be used (achieved by passing \code{singlefile = FALSE}),
#' and may be easier for debugging/sourcing the simulation code; however, this is a
#' matter of preference and does not change any functionality in the package.
#'
#' @param filename a character vector indicating whether the output should be saved to two respective files
#'   containing the simulation design and the functional components, respectively. Using this option
#'   is generally the recommended approach when beginning to write a Monte Carlo simulation
#'
#' @param dir the directory to write the files to. Default is the working directory
#'
#' @param comments logical; include helpful comments? Default is \code{FALSE}
#'
#' @param singlefile logical; when \code{filename} is included, put output in one files? When \code{FALSE} the
#'   output is saved to two separate files containing the functions and design definitions. The two-file format
#'   often makes organization and debugging slightly easier, especially for larger Monte Carlo simulations.
#'   Default is \code{TRUE}
#'
#' @param summarise include \code{summarise} function? Default is \code{TRUE}
#'
#' @param generate include \code{generate} function? Default is \code{TRUE}
#'
#' @param openFiles logical; after files have been generated, open them in your text editor (e.g., if Rstudio is running
#'   the scripts will open in a new tab)?
#'
#' @aliases SimFunctions
#'
#' @seealso \code{\link{runSimulation}}
#'
#' @export SimFunctions
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#'
#' SimFunctions()
#' SimFunctions(comments = TRUE) #with helpful comments
#'
#'\dontrun{
#'
#' # write output to two files (recommended for larger MCSs)
#' SimFunctions('mysim', singlefile = FALSE)
#'
#' # write output files to a single file with comments
#' SimFunctions('mysim', comments = TRUE)
#' }
#'
SimFunctions <- function(filename = NULL, dir = getwd(), comments = FALSE,
                         singlefile = TRUE, summarise = TRUE, generate = TRUE,
                         openFiles = TRUE){
    LINE <- function()
        cat('#-------------------------------------------------------------------\n')
    HEAD <- function(){
        LINE()
        cat('\nlibrary(SimDesign)\n')
        if(comments)
            cat('\n### Define design conditions')
        cat('\nDesign <- createDesign(factor1 = NA,
                       factor2 = NA)\n\n')
        if(!is.null(filename) && !singlefile){
            if(comments) cat('### Source in essential functions\n')
            cat('# setwd(\"', dir, '\")', sep='')
            cat('\nsource(\"', paste0(filename, '-functions.R\"'), ')\n\n', sep='')
        }
    }

    FUNCTIONS <- function(){
        LINE()
        if(comments) cat('\n### Define essential simulation functions\n')
        if(generate){
            cat('\nGenerate <- function(condition, fixed_objects = NULL) {')
            if(comments) cat('\n    # Define data generation code ...\n')
            if(comments) cat('\n    # Return a vector, matrix, data.frame, or list')
            cat('\n    dat <- data.frame()')
            cat('\n    dat\n}')
            cat('\n')
        }
        cat('\nAnalyse <- function(condition, dat, fixed_objects = NULL) {')
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
        cat('\nres <- runSimulation(design=Design, replications=1000,',
            if(generate) 'generate=Generate, ')
        cat(sprintf('\n                     analyse=Analyse%s',
                    if(summarise) ', summarise=Summarise)' else ')'))
        cat('\nres\n\n')
    }

    #main
    if(!is.null(filename)){
        if(file.exists(paste0(filename, '.R')))
            stop('File already exists! Please rename input or rename/remove existing files', call.=FALSE)
    }
    if(is.null(filename) || singlefile){
        if(singlefile){
            if(!is.null(filename)){
                cat(sprintf('Writing simulation components to file \"%s\" in \n  directory \"%s\"',
                            paste0(filename, '.R'), dir))
                sink(paste0(filename, '.R'))
            }
        }
        HEAD()
        FUNCTIONS()
        TAIL()
        if(!is.null(filename)) sink()
    } else {
        cat(sprintf('Writing simulation components to files to \"%s\" and \"%s\" in \n  directory \"%s\"',
                    paste0(filename, '.R'), paste0(filename, '-functions.R'), dir))
        sink(paste0(filename, '.R'))
        HEAD()
        TAIL()
        sink()
        sink(paste0(filename, '-functions.R'))
        FUNCTIONS()
        sink()
    }
    if(!is.null(filename) && openFiles){
        message('\n\nOpening file(s) in your current text editor...')
        if(!singlefile) file.show(paste0(filename, '-functions.R'))
        file.show(paste0(filename, '.R'))
    }
    invisible()
}
