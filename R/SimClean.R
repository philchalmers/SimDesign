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
#' @param dirs a character vector indiciating which directories to remove
#'
#' @param generate_data logical; remove the \code{.rds} data-set files
#'   saved when passing \code{save_generate_data = TRUE}?
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
#' SimClean(temp = TRUE)
#'
#' # remove default saved-data directory
#' SimClean(generate_data = TRUE)
#'
#' # remove customized saved-results directory called 'mydir'
#' SimClean(results = TRUE, save_details = list(save_results_dirname = 'mydir'))
#'
#' }
SimClean <- function(..., dirs = NULL, generate_data = FALSE, results = FALSE,
                     seeds = FALSE, temp = FALSE, save_details = list()){
    compname <- save_details$compname; tmpfilename <- save_details$tempfilename
    save_results_dirname <- save_details$save_results_dirname
    save_seeds_dirname <- save_details$save_seeds_dirname
    save_generate_data_dirname <- save_details$save_generate_data_dirname
    if(is.null(compname)) compname <- Sys.info()['nodename']
    if(is.null(tmpfilename))
        tmpfilename <- paste0('SIMDESIGN-TEMPFILE_', compname, '.rds')
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
    if(temp) file.remove(tmpfilename)
    invisible(NULL)
}
