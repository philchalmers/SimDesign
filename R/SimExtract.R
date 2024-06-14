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
#'   counts of any warning messages, \code{'seeds'}  for the specified random number
#'   generation seeds,  \code{'Random.seeds'} for the complete list of
#'   \code{.Random.seed} states across replications (only stored when
#'   \code{runSimulation(..., control = list(store_Random.seeds=TRUE))}),
#'   \code{'error_seeds'} and \code{'warning_seeds'}
#'   to extract the associated \code{.Random.seed} values associated with the ERROR/WARNING messages,
#'   \code{'results'} to extract the simulation results if the option \code{store_results} was passed to
#'   \code{\link{runSimulation}}, \code{'filename'} and \code{'save_results_dirname'} for extracting
#'   the saved file/directory name information (if used),
#'   and \code{'summarise'} if the \code{\link{Summarise}}
#'   definition returned a named \code{list} rather than a named numeric vector.
#'
#'   Note that \code{'warning_seeds'} are not stored automatically in
#'   simulations and require passing \code{store_warning_seeds = TRUE} to \code{\link{runSimulation}}.
#'
#' @param fuzzy logical; use fuzzy string matching to reduce effectively identical messages?
#'   For example, when attempting to invert a matrix the error message
#'   \emph{"System is computationally singular: reciprocal condition number = 1.92747e-17"} and
#'   \emph{"System is computationally singular: reciprocal condition number = 2.15321e-16"} are
#'   effectively the same, and likely should be reported in the same columns of the extracted output
#'
#' @param append logical; append the design conditions when extracting error/warning messages?
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
#' Generate <- function(condition, fixed_objects) {
#'     int <- sample(1:10, 1)
#'     if(int > 5) warning('GENERATE WARNING: int greater than 5')
#'     if(int == 1) stop('GENERATE ERROR: integer is 1')
#'     rnorm(5)
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects) {
#'     int <- sample(1:10, 1)
#'     if(int > 5) warning('ANALYSE WARNING: int greater than 5')
#'     if(int == 1) stop('ANALYSE ERROR: int is 1')
#'     c(ret = 1)
#' }
#'
#' Summarise <- function(condition, results, fixed_objects) {
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
SimExtract <- function(object, what, fuzzy = TRUE, append = TRUE){
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
        err <- extract_errors(object, fuzzy=fuzzy)
        if(length(err) && append) cbind(Design, err) else err
    } else if(what == 'summarise'){
        extract_summarise(object)
    }  else if(what == 'seeds'){
        extract_seeds(object)
    } else if(what == 'random.seeds'){
        extract_Random.seeds(object)
    } else if(what == 'error_seeds'){
        extract_error_seeds(object)
    } else if(what == 'warnings'){
        wrn <- extract_warnings(object, fuzzy=fuzzy)
        if(length(wrn) && append) cbind(Design, wrn) else wrn
    } else if(what == 'warning_seeds'){
        extract_warning_seeds(object)
    } else if(what == 'save_results_dirname'){
        attr(object, 'extra_info')$save_info['save_results_dirname']
    } else if(what == 'filename'){
        attr(object, 'extra_info')$save_info['filename']
    } else stop('Input provided to \"what" is not supported')
    ret
}

extract_errors <- function(object, fuzzy){
    ret <- attr(object, 'ERROR_msg')
    if(fuzzy)
        ret <- fuzzy_reduce(ret)
    ret
}

extract_warnings <- function(object, fuzzy){
    ret <- attr(object, 'WARNING_msg')
    if(fuzzy)
        ret <- fuzzy_reduce(ret)
    ret
}

extract_results <- function(object){
    extra_info <- attr(object, 'extra_info')
    if(is.null(extra_info$stored_results)) return(NULL)
    if(is(extra_info$stored_results, 'tbl_df'))
        return(extra_info$stored_results)
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

extract_Random.seeds <- function(object){
    extra_info <- attr(object, 'extra_info')
    ret <- extra_info$stored_Random.seeds_list
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

extract_seeds <- function(object){
    extra_info <- attr(object, 'extra_info')
    ret <- extra_info$seeds
    ret
}

extract_summarise <- function(object){
    extra_info <- attr(object, 'extra_info')
    Design <- SimExtract(object, 'Design')
    nms <- apply(Design, 1L, function(x)
        paste0(colnames(Design), "=", x, collapse = ' ; '))
    ret <- extra_info$summarise_list
    names(ret) <- nms
    ret
}

fuzzy_reduce <- function(df){
    if(!length(df)) return(df)
    nms <- colnames(df)
    matched <- logical(length(nms))
    unames <- c()
    udf <- df[,0]
    for(i in 1L:length(nms)){
        if(matched[i]) next
        unames <- c(unames, nms[i])
        udf <- cbind(udf, df[,i])
        temp_matched <- agrepl(nms[i], nms)
        udf[,ncol(udf)] <- rowSums(df[,temp_matched], na.rm = TRUE)
        matched <- matched | temp_matched
    }
    udf
}
