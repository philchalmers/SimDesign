#' Check for missing files in array simulations
#'
#' Given the saved files from a \code{\link{runArraySimulation}} remote
#' evaluation check whether all \code{.rds} files have been saved. If missing
#' the missing row condition numbers will be returned
#'
#' @param files vector of file names referring to the saved simulation files.
#'   E.g. \code{c('mysim-1.rds', 'mysim-2.rds', ...)}
#'
#' @param min minimum number after the \code{'-'} deliminator. Default is 1
#'
#' @param max maximum number after the \code{'-'} deliminator. If not specified
#'   is taken to be the highest number of the \code{files} input
#'
#' @seealso \code{\link{runArraySimulation}}
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' setwd('mysimfiles/')
#' files <- dir()
#' SimCheck()
#'
#' }
#'
SimCheck <- function(files, min = 1L, max = NULL){
    filename <- strsplit(files[1], '-')[[1L]][1L]
    if(is.null(max)){
        subfiles <- gsub(paste0(filename, '-'), files, replacement = '')
        subfiles <- gsub('.rds', subfiles, replacement = '')
        max <- max(as.integer(subfiles))
    }
    minmax <- min:max
    notin <- !(paste0(filename, '-', minmax, '.rds') %in% files)
    if(any(notin)){
        cat(sprintf('The following row conditions were missing:\n%s\n',
            paste0(minmax[notin], collapse=',')))
    } else
        cat(sprintf('No missing conditions from %i to %i were detected\n', min, max))
    invisible(NULL)
}
