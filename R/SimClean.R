#' Removes/cleans files and folders that have been saved
#'
#' This function is mainly used in pilot studies where results and datasets have been temporarily saved
#' by \code{\link{runSimulation}} but should be removed before beginning the full
#' Monte Carlo simulation (e.g., remove files and folders which contained bugs/biased results).
#'
#' @param ... one or more character objects indicating which files to remove. Used to remove
#'   \code{.rds} files which were saved with \code{\link{saveRDS}} or when using the \code{save}
#'   and \code{filename} inputs to \code{\link{runSimulation}}
#'
#' @param dirs a character vector indicating which directories to remove
#'
# @param generate_data logical; remove the \code{.rds} data-set files
#   saved when passing \code{save_generate_data = TRUE}?
#'
#' @param results logical; remove the \code{.rds} results files
#'   saved when passing \code{save_results = TRUE}?
#'
#' @param seeds logical; remove the seed files
#'   saved when passing \code{save_seeds = TRUE}?
#'
#' @param temp logical; remove the temporary file saved when passing \code{save = TRUE}?
#'
#' @param save_details a list pertaining to information about how and where files were saved
#'   (see the corresponding list in \code{\link{runSimulation}})
#'
#' @seealso \code{\link{runSimulation}}
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
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # remove file called 'results.rds'
#' SimClean('results.rds')
#'
#' # remove default temp file
#' SimClean()
#'
#' # remove customized saved-results directory called 'mydir'
#' SimClean(results = TRUE, save_details = list(save_results_dirname = 'mydir'))
#'
#' }
SimClean <- function(..., dirs = NULL, temp = TRUE, results = FALSE,
                     seeds = FALSE, save_details = list()){
    generate_data <- FALSE
    compname <- save_details$compname; tmpfilename <- save_details$tmpfilename
    out_rootdir <- save_details$out_rootdir
    if(!is.null(out_rootdir)){
        gtw <- getwd()
        setwd(out_rootdir)
    }
    save_results_dirname <- save_details$save_results_dirname
    save_seeds_dirname <- save_details$save_seeds_dirname
    save_generate_data_dirname <- save_details$save_generate_data_dirname
    if(is.null(compname)) compname <- Sys.info()['nodename']
    if(is.null(save_results_dirname))
        save_results_dirname <- paste0('SimDesign-results_', compname)
    if(is.null(save_generate_data_dirname))
        save_generate_data_dirname <- paste0('SimDesign-generate-data_', compname)
    files <- list(...)
    if(length(files)) file.remove(...)
    if(!is.null(dirs))
        for(d in dirs) unlink(d, recursive = TRUE, force = TRUE)
    if(is.null(save_seeds_dirname)) save_seeds_dirname <- paste0('SimDesign-seeds_', compname)
    if(generate_data) unlink(save_generate_data_dirname, recursive = TRUE, force = TRUE)
    if(results) unlink(save_results_dirname, recursive = TRUE, force = TRUE)
    if(seeds) unlink(save_seeds_dirname, recursive = TRUE, force = TRUE)
    if(temp){
        fs <- dir()
        file.remove(fs[grepl('SIMDESIGN-TEMPFILE_', fs)])
    }
    if(!is.null(out_rootdir)) setwd(gtw)
    invisible(NULL)
}
