#' Function to read in saved simulation results
#'
#' If \code{\link{runSimulation}} was passed the flag \code{save_results = TRUE} then the
#' row results corresponding to the \code{design} object will be stored to a suitable
#' sub-directory as individual \code{.rds} files. While users could use \code{\link{readRDS}} directly
#' to read these files in themselves, this convenience function will read the desired rows in
#' automatically given the returned object
#' from the simulation. Can be used to read in 1 or more \code{.rds} files at once (if more than 1 file
#' is read in then the result will be stored in a list).
#'
#' @param obj object returned from \code{\link{runSimulation}} where \code{save_results = TRUE}
#'   or \code{store_results} was used. If the former then the remaining function arguments can
#'   be useful for reading in specific files.
#'
#'   Alternatively, the object can be from the \code{Spower} package, evaluated
#'   using either \code{Spower()} or \code{SpowerBatch()}
#'
#' @param which a numeric vector indicating which rows should be read in. If missing, all rows will be
#'   read in
#'
#' @param prefix character indicating prefix used for stored files
#'
#' @param wd working directory; default is found with \code{\link{getwd}}.
#'
#' @param rbind logical; should the results be combined by row or returned as
#'   a list? Only applicable when the supplied \code{obj} was obtained from the
#'   function \code{Spower::SpowerBatch()}
#'
#' @export
#'
#' @return the returned result is either a nested list (when \code{length(which) > 1}) or a single list
#'   (when \code{length(which) == 1}) containing the simulation results. Each read-in result refers to
#'   a list of 4 elements:
#'   \describe{
#'     \item{\code{condition}}{the associate row (ID) and conditions from the
#'       respective \code{design} object}
#'     \item{\code{results}}{the object with returned from the \code{analyse} function, potentially
#'       simplified into a matrix or data.frame}
#'     \item{\code{errors}}{a table containing the message and number of errors that caused
#'       the generate-analyse steps to be rerun. These should be inspected carefully as they
#'       could indicate validity issues with the simulation that should be noted}
#'     \item{\code{warnings}}{a table containing the message and number of non-fatal warnings
#'       which arose from the analyse step. These should be inspected carefully as they
#'       could indicate validity issues with the simulation that should be noted}
#'   }
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
#' # store results (default behaviour)
#' sim <- runSimulation(..., store_results = TRUE)
#' SimResults(sim)
#'
#' # store results to drive if RAM issues are present
#' obj <- runSimulation(..., save_results = TRUE)
#'
#' # row 1 results
#' row1 <- SimResults(obj, 1)
#'
#' # rows 1:5, stored in a named list
#' rows_1to5 <- SimResults(obj, 1:5)
#'
#' # all results
#' rows_all <- SimResults(obj)
#'
#' }
SimResults <- function(obj, which, prefix = "results-row", wd = getwd(),
                       rbind = FALSE){
    stopifnot(!missing(obj))
    if(is(obj, 'SpowerBatch')){
        out <- lapply(obj, \(x) SimResults(x, which=which))
        if(rbind)
            out <- do.call(base::rbind, out)
        return(out)
    }
    results <- SimExtract(obj, what='results')
    if(!is.null(results)) return(results)
    wdold <- getwd()
    on.exit(setwd(wdold))
    so <- summary(obj)
    if(missing(which)) which <- 1L:so$number_of_conditions
    path <- so$save_info["save_results_dirname"]
    if(is.na(path))
        stop('results object was not run with save_results = TRUE')
    setwd(paste0(wd, '/', path))
    files <- file_nums <- dir()
    file_nums <- gsub(paste0(prefix, '-'), '', file_nums)
    file_nums <- as.numeric(gsub('.rds', '', file_nums))
    files <- data.frame(file_nums, files, stringsAsFactors = FALSE)
    stored_Results_list <- vector('list', length(which))
    for(i in seq_len(length(which))){
        pick <- which(files$file_num == which[i])
        stored_Results_list[[i]] <- readRDS(files$files[pick])
    }
    design <- SimExtract(obj, 'design')
    if(is(stored_Results_list[[1L]]$results, 'data.frame') ||
       is(stored_Results_list[[1L]]$results, 'matrix')){
        for(i in seq_len(length(stored_Results_list)))
            stored_Results_list[[i]] <- cbind(design[i,],
                                              stored_Results_list[[i]]$results, row.names=NULL)
        stored_Results_list <- dplyr::bind_rows(stored_Results_list)
        stored_Results_list$ID <- NULL
        stored_Results_list <- dplyr::as_tibble(stored_Results_list)
    }
    stored_Results_list
}
