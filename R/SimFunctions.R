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
#' by this function to the hard-drive by passing a suitable \code{filename} argument
#' (which, if users are interacting
#' with R via the RStudio IDE, will also open the template file after it has been saved).
#' For larger simulations, two
#' separate files could also be used (achieved by changing \code{out.files}),
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
#' @param save_structure character indicating the number of files to break the simulation code into
#'   when \code{filename} is included (default is 'single' for one file). When \code{save_structure = 'double'} the
#'   output is saved to two separate files containing the functions and design definitions,
#'   and when \code{save_structure = 'all'} the generate, analyse, summarise, and execution code area all saved into
#'   separate files. The purpose for this structure is because multiple structured files
#'   often makes organization and debugging slightly easier larger Monte Carlo simulations, though in principle
#'   all files could be stored into a single R script
#'
#' @param extra_file logical; should and extra file be saved containing user-defined functions or objects?
#'   Default is \code{FALSE}
#'
#' @param summarise include \code{summarise} function? Default is \code{TRUE}
#'
#' @param nAnalyses number of analysis functions to create (default is 1). Increasing the value
#'   of this argument when independent analysis are being performed allows function definitions
#'   to be better partitioned and potentially more modular
#'
#' @param nGenerate number of generate functions to create (default is 1). Increase the value
#'   of this argument when when the data generation functions are very different and should
#'   be isolated from each other (otherwise, if there is much in common between the generate
#'   steps, the default of 1 should be preferred). Otherwise, if \code{nGenerate == 0}
#'   then no generate function will be provided and instead this data-generation
#'   step can be defined in the analysis function(s) (only recommended for smaller simulations)
#'
#' @param openFiles logical; after files have been generated, open them in your text editor
#'   (e.g., if Rstudio is running the scripts will open in a new tab)?
#'
#' @param SimSolve logical; should the template be generated that is intended for a
#'   \code{\link{SimSolve}} implementation? Default is \code{FALSE}
#'
#' @param spin_header logical; include a basic \code{knitr::spin} header to allow the simulation
#'   to be knitted? Default is \code{TRUE}. For those less familiar with \code{spin} documents
#'   see \code{https://bookdown.org/yihui/rmarkdown-cookbook/spin.html} for further details
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
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
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
#' \dontrun{
#'
#' # write output files to a single file with comments
#' SimFunctions('mysim', comments = TRUE)
#'
#' # Multiple analysis functions for optional partitioning
#' SimFunctions(nAnalyses = 2)
#' SimFunctions(nAnalyses = 3)
#'
#' # Multiple analysis + generate functions
#' SimFunctions(nAnalyses = 2, nGenerate=2)
#'
#' # save multiple files for the purpose of designing larger simulations
#' #  (also include extra_file for user-defined objects/functions)
#' SimFunctions('myBigSim', save_structure = 'all',
#'    nAnalyses = 3, nGenerate=2, extra_file = TRUE)
#'
#'
#' }
#'
SimFunctions <- function(filename = NULL, dir = getwd(),
                         save_structure = 'single', extra_file = FALSE,
                         nAnalyses = 1, nGenerate = 1,
                         summarise = TRUE, comments = FALSE, openFiles = TRUE,
                         spin_header = TRUE, SimSolve = FALSE){
    generate <- TRUE
    if(nGenerate == 0L)
        generate <- FALSE
    if(save_structure == 'single'){
        out.files <- 1
    } else if(save_structure == 'double'){
        out.files <- 2
    } else if(save_structure == 'all'){
        out.files <- 4
    } else {
        stop('Specified save_structure is invalid', call.=FALSE)
    }
    stopifnot(out.files %in% c(1,2,4))
    stopifnot(nAnalyses >= 1)

    SPIN <- function(){
        if(is.null(filename)) return(invisible(NULL))
        cat('#\' --- \n')
        cat('#\' title: \"Simulation title\"\n')
        cat('#\' output:\n')
        cat('#\'   html_document:\n')
        cat('#\'     theme: readable\n')
        cat('#\'     code_download: true\n')
        cat('#\' ---\n\n\n')
    }
    LINE <- function()
        cat('#-------------------------------------------------------------------\n')
    HEAD <- function(){
        LINE()
        cat('\nlibrary(SimDesign)\n')
        if(comments)
            cat('\n### Define design conditions')
        cat('\nDesign <- createDesign(factor1 = NA,
                       factor2 = NA)\n\n')
        if(!is.null(filename) && out.files == 2L){
            if(comments) cat('### Source in essential functions\n')
            cat('# setwd(\"', dir, '\")', sep='')
            cat('\nsource(\"', paste0(filename, '-functions.R\"'), ')', sep='')
            if(extra_file)
                cat('\nsource(\"', paste0(filename, '-extras.R\"'), ')', sep='')
            cat('\n\n')
        }
        if(!is.null(filename) && out.files == 4L){
            if(comments) cat('### Source in essential functions\n')
            cat('# setwd(\"', dir, '\")', sep='')
            if(generate) cat('\nsource(\"', paste0(filename, '-generate.R\"'), ')', sep='')
            cat('\nsource(\"', paste0(filename, '-analyse.R\"'), ')', sep='')
            if(summarise) cat('\nsource(\"', paste0(filename, '-summarise.R\"'), ')', sep='')
            if(extra_file)
                cat('\nsource(\"', paste0(filename, '-extras.R\"'), ')\n', sep='')
            cat('\n\n')
        }
    }

    FUNCTIONS <- function(add.gen=TRUE, add.analyse=TRUE, add.summarise=TRUE){
        LINE()
        if(comments) cat('\n### Define essential simulation functions\n')
        if(generate && add.gen){
            if(nGenerate == 1L){
                cat('\nGenerate <- function(condition, fixed_objects = NULL) {')
                if(comments) cat('\n    # Define data generation code ...\n')
                if(comments) cat('\n    # Return a vector, matrix, data.frame, or list')
                cat('\n    dat <- data.frame()')
                cat('\n    dat\n}')
                cat('\n')
            } else {
                for(i in 1L:nGenerate){
                    cat(sprintf('\nGenerate.G%i <- function(condition, fixed_objects = NULL) {', i))
                    if(i < nGenerate) cat("\n    GenerateIf(TRUE)")
                    if(comments) cat('\n    # Define data generation code ...\n')
                    if(comments) cat('\n    # Return a vector, matrix, data.frame, or list')
                    cat('\n    dat <- data.frame()')
                    cat('\n    dat\n}')
                    cat('\n')
                }
                cat('\n')
                LINE()
            }
        }
        if(add.analyse){
            if(nAnalyses == 1L){
                cat('\nAnalyse <- function(condition, dat, fixed_objects = NULL) {')
                if(comments) cat('\n    # Run statistical analyses of interest ... \n')
                if(comments) cat('\n    # Return a named vector or list')
                cat('\n    ret <- nc(stat1 = NaN, stat2 = NaN)\n    ret\n}')
                cat('\n\n')
            } else {
                for(i in 1L:nAnalyses){
                    cat(sprintf('\nAnalyse.A%i <- function(condition, dat, fixed_objects = NULL) {', i))
                    if(comments) cat('\n    # Run statistical analyses of interest ... \n')
                    if(comments) cat('\n    # Return a named vector or list')
                    cat('\n    ret <- nc(stat1 = NaN, stat2 = NaN)\n    ret\n}')
                    cat('\n')
                }
                cat('\n')
                LINE()
                cat('\n')
            }
        }
        if(summarise && add.summarise){
            cat('Summarise <- function(condition, results, fixed_objects = NULL) {')
            if(comments) cat('\n    # Summarise the simulation results ...\n')
            if(comments) cat('\n    # Return a named vector of results')
            if(SimSolve) cat('\n    ret <- EDR(results)\n    ret\n}\n\n')
            else cat('\n    ret <- c(bias = NaN, RMSE = NaN)\n    ret\n}\n\n')
            LINE()
            cat('\n')
        }
    }

    TAIL <- function(){
        if(comments) cat('### Run the simulation\n')
        if(nAnalyses==1L && nGenerate==1L){
            if(SimSolve){
                cat('res <- SimSolve(design=Design, b=VALUE, inverval=RANGE,',
                    if(generate) 'generate=Generate, ')
                cat(sprintf('\n                     analyse=Analyse%s',
                            if(summarise) ', summarise=Summarise)' else ')'))

            } else {
                cat('res <- runSimulation(design=Design, replications=1000,',
                    if(generate) 'generate=Generate, ')
                cat(sprintf('\n                     analyse=Analyse%s',
                            if(summarise) ', summarise=Summarise)' else ')'))
            }
        } else {
            Analyse_string <- if(nAnalyses > 1L)
                sprintf("list(%s)",paste0(paste0('A', 1L:nAnalyses, sep='='),
                                          paste0('Analyse.A', 1L:nAnalyses), collapse=', '))
            else "Analyse"
            Generate_string <- if(nGenerate > 1L)
                sprintf("list(%s)",paste0(paste0('G', 1L:nGenerate, sep='='),
                                          paste0('Generate.G', 1L:nGenerate), collapse=', '))
            else "Generate"
            genspace <- if(nGenerate > 1L) '\n                     ' else ""
            if(SimSolve) cat('res <- SimSolve(design=Design, b=VALUE, inverval=RANGE,')
            else cat('res <- runSimulation(design=Design, replications=1000,')
            if(generate)
                cat(sprintf('%sgenerate=%s, ', genspace, Generate_string))
            cat(sprintf('\n                     analyse=%s%s', Analyse_string,
                        if(summarise) ', \n                     summarise=Summarise)' else ')'))

        }
        cat('\nres\n\n')
    }

    #main
    if(!is.null(filename)){
        if(file.exists(paste0(filename, '.R')))
            stop('File already exists! Please rename input or rename/remove existing files',
                 call.=FALSE)
    }
    if(is.null(filename) || out.files == 1L){
        if(out.files == 1L){
            if(!is.null(filename)){
                cat(sprintf('Writing simulation components to file \"%s\" in \n  directory \"%s\"',
                            paste0(filename, '.R'), dir))
                sink(paste0(filename, '.R'))
            }
        }
        if(spin_header) SPIN()
        HEAD()
        FUNCTIONS()
        TAIL()
        if(!is.null(filename)) sink()
    } else {
        if(out.files == 2L){
            cat(sprintf('Writing simulation components to files to \"%s\" and \"%s\" in \n  directory \"%s\"',
                        paste0(filename, '.R'), paste0(filename, '-functions.R'), dir))
            sink(paste0(filename, '.R'))
            if(spin_header) SPIN()
            HEAD()
            TAIL()
            sink()
            sink(paste0(filename, '-functions.R'))
            FUNCTIONS()
            sink()
            if(extra_file){
                sink(paste0(filename, '-extras.R'))
                cat('# File for extra user-defined function and object definitions\n')
                sink()
            }
        } else if(out.files == 4L){
            cat(sprintf('Writing simulation components to multiple files (main file is \"%s\") in \n  directory \"%s\"',
                        paste0(filename, '.R'), dir))
            sink(paste0(filename, '.R'))
            if(spin_header) SPIN()
            HEAD()
            TAIL()
            sink()
            if(generate){
                sink(paste0(filename, '-generate.R'))
                FUNCTIONS(add.analyse = FALSE, add.summarise = FALSE)
                sink()
            }
            sink(paste0(filename, '-analyse.R'))
            FUNCTIONS(add.gen = FALSE, add.summarise = FALSE)
            sink()
            if(summarise){
                sink(paste0(filename, '-summarise.R'))
                FUNCTIONS(add.gen = FALSE, add.analyse = FALSE)
                sink()
            }
            if(extra_file){
                sink(paste0(filename, '-extras.R'))
                cat('# File for extra user-defined function and object definitions\n')
                sink()
            }
        }
    }
    if(!is.null(filename) && openFiles){
        message('\n\nOpening file(s) in your current text editor...')
        if(out.files > 1L){
            if(out.files == 2L){
                file.show(paste0(filename, '-functions.R'))
            } else {
                if(file.exists(paste0(filename, '-summarise.R')))
                    file.show(paste0(filename, '-summarise.R'))
                if(file.exists(paste0(filename, '-analyse.R')))
                    file.show(paste0(filename, '-analyse.R'))
                if(file.exists(paste0(filename, '-generate.R')))
                    file.show(paste0(filename, '-generate.R'))
            }
        }
        if(file.exists(paste0(filename, '-extras.R')))
            file.show(paste0(filename, '-extras.R'))
        file.show(paste0(filename, '.R'))
    }
    invisible(NULL)
}
