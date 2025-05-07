#' Check for missing files in array simulations
#'
#' Given the saved files from a \code{\link{runArraySimulation}} remote
#' evaluation check whether all \code{.rds} files have been saved. If missing
#' the missing row condition numbers will be returned.
#'
#' @param dir character vector input indicating the directory
#'   containing the \code{.rds} files (see \code{files})
#'
#' @param files vector of file names referring to the saved simulation files.
#'   E.g. \code{c('mysim-1.rds', 'mysim-2.rds', ...)}
#'
#' @param min minimum number after the \code{'-'} deliminator. Default is 1
#'
#' @param max maximum number after the \code{'-'} deliminator. If not specified
#'   is extracted from the attributes in the first file
#'
#' @seealso \code{\link{runArraySimulation}}, \code{\link{SimCollect}}
#'
#' @return returns an invisible list of indices of empty, missing and
#'   empty-and-missing row conditions.. If no missing then an empty list is
#'   returned
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
#' # if files are in mysimfiles/ directory
#' SimCheck('mysimfiles')
#'
#' # specifying files explicility
#' setwd('mysimfiles/')
#' SimCheck(files=dir())
#'
#' }
#'
SimCheck <- function(dir = NULL, files = NULL, min = 1L, max = NULL){
    if(is.null(dir) && is.null(files))
        stop('either dir or files must be specified')
    if(!is.null(dir) && !is.null(files))
        stop('dir OR files must be specified, not both')
    if(!is.null(dir)){
        files <- dir(dir)
    } else dir <- './'
    files <- paste0(dir, files)
    filename <- strsplit(files[1], '-')[[1L]][1L]
    if(is.null(max)){
        tmp <- readRDS(files[1])
        max <- attr(tmp, 'extra_info')$number_of_conditions
    }
    minmax <- min:max
    mainlist <- paste0(filename, '-', minmax, '.rds')
    have <- mainlist %in% files
    notin <- !have
    names(have) <- mainlist
    ret <-  list()
    if(any(notin)){
        ret$Missing_Row_Conditions <- minmax[notin]
        warning(sprintf('The following row conditions were missing:\n%s\n',
                        paste0(ret$Missing_Row_Conditions, collapse=',')))
    }
    empty_file <- (sapply(names(have)[have], file.size, USE.NAMES = FALSE) == 0)
    if(any(empty_file)) {
        ret$Empty_Row_Conditions <- minmax[empty_file]
        warning(sprintf('The following row conditions have nothing saved:\n%s\n',
                        paste0(ret$Empty_Row_Conditions, collapse=',')))
    }
    if(any(notin) || any(!nonzero)){
        ret$Empty_Missing_Row_Conditions <- c(minmax[notin], minmax[empty_file]) |>
            unique() |>
            sort()
    } else integer()

    invisible(ret)
}
