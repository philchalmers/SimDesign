#' Function to extract extra information from SimDesign objects
#'
#' Function used to extract any error or warnings messages, the seeds associated
#' with any error or warning messages, and any analysis results that were stored in the
#' final simulation object.
#'
#' @param object object returned from \code{\link{runSimulation}}
#'
#' @param what character indicating what information to extract. Possible inputs
#'   include \code{'errors'} to return a \code{tibble} object containing counts of any
#'   error messages, \code{'warnings'} to return a \code{data.frame} object containing
#'   counts of any warning messages, \code{'error_seeds'} and \code{'warning_seeds'}
#'   to extract the associated \code{.Random.seed} values associated with the ERROR/WARNING messages,
#'   and \code{'results'} to extract the simulation results if the option \code{store_results} was passed to
#'   \code{\link{runSimulation}}. Note that \code{'warning_seeds'} are not stored automatically in
#'   simulations and require passing \code{store_warning_seeds = TRUE} to \code{\link{runSimulation}}.
#'
#' @export
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
#' \dontrun{
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'     int <- sample(1:10, 1)
#'     if(int > 5) warning('GENERATE WARNING: int greater than 5')
#'     if(int == 1) stop('GENERATE WARNING: integer is 1')
#'     rnorm(5)
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'     int <- sample(1:10, 1)
#'     if(int > 5) warning('ANALYSE WARNING: int greater than 5')
#'     if(int == 1) stop('ANALYSE WARNING: int is 1')
#'     c(ret = 1)
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
#'     mean(results)
#' }
#'
#' res <- runSimulation(replications = 100, seed=1234, verbose=FALSE,
#'                      generate=Generate, analyse=Analyse, summarise=Summarise)
#' res
#'
#' SimExtract(res, what = 'errors')
#' SimExtract(res, what = 'warnings')
#' seeds <- SimExtract(res, what = 'error_seeds')
#' seeds[,1:3]
#'
#' # replicate a specific error for debugging (type Q to exit debugger)
#' res <- runSimulation(replications = 100, load_seed=seeds[,1], debug='analyse',
#'                      generate=Generate, analyse=Analyse, summarise=Summarise)
#'
#'
#'
#' }
SimExtract <- function(object, what){
    stopifnot(is(object, "SimDesign"))
    what <- tolower(what)
    pick <- attr(object, 'design_names')$design
    Design <- if(any(pick != 'dummy_run'))
        object[,attr(object, 'design_names')$design]
        else dplyr::tibble(.rows = nrow(object))
    if(what == 'design') return(Design)
    if(missing(what)) stop('Please specify what you want to extract')
    ret <- if(what == 'results'){
        extract_results(object)
    } else if(what == 'errors'){
        cbind(Design, extract_errors(object))
    } else if(what == 'error_seeds'){
        extract_error_seeds(object)
    } else if(what == 'warnings'){
        cbind(Design, extract_warnings(object))
    } else if(what == 'warning_seeds'){
        extract_warning_seeds(object)
    } else stop('Input provided to \"what" is not supported')
    ret
}

extract_errors <- function(object){
    attr(object, 'ERROR_msg')
}

extract_warnings <- function(object){
    attr(object, 'WARNING_msg')
}

extract_results <- function(object){
    extra_info <- attr(object, 'extra_info')
    if(is.null(extra_info$stored_results)) return(NULL)
    design_names <- attr(object, "design_names")
    pick <- design_names$design
    if(length(extra_info$stored_results) == 1L)
        return(extra_info$stored_results[[1L]])
    design <- subset(as.data.frame(object), select=pick)
    nms <- colnames(design)
    nms2 <- matrix(character(0L), nrow(design), ncol(design))
    for(i in 1L:ncol(design))
        nms2[,i] <- paste0(nms[i], '=', design[,i], if(i < ncol(design)) '; ')
    nms2 <- apply(nms2, 1L, paste0, collapse='')
    ret <- extra_info$stored_results
    names(ret) <- nms2
    ret
}

extract_error_seeds <- function(object){
    extra_info <- attr(object, 'extra_info')
    ret <- extra_info$error_seeds
    ret
}

extract_warning_seeds <- function(object){
    extra_info <- attr(object, 'extra_info')
    ret <- extra_info$warning_seeds
    ret
}
