#' Function to read in saved simulation results
#'
#' If \code{\link{runSimulation}} was passed the flag \code{save_results = TRUE} then the
#' row results corresponding to the \code{design} object will be stored to a suitable
#' sub-directory as individual \code{.rds} files. While users could use \code{\link{readRDS}} directly
#' to read these files in themselves, this conviencience function will read the desired rows in
#' automatically given the returned object
#' from the simulation. Can be used to read in 1 or more .rds files at once (if more than 1 file
#' is read in then the result will be stored in a list).
#'
#' @param results object returned from \code{\link{runSimulation}} where \code{save_results = TRUE}
#'   was used
#'
#' @param which a numeric vector indicating which rows should be read in. If missing, all rows will be
#'   read in
#'
#' @param wd working directory; default is found with \code{\link{getwd}}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' results <- runSimulation(..., save_results = TRUE)
#'
#' # row 1 results
#' row1 <- SimResults(results, 1)
#'
#' # rows 1:5, stored in a named list
#' rows_1to5 <- SimResults(results, 1:5)
#'
#' # all results
#' rows_all <- SimResults(results)
#'
#' }
SimResults <- function(results, which, wd = getwd()){
    stopifnot(!missing(results))
    wdold <- getwd()
    on.exit(setwd(wdold))
    so <- summary(results)
    if(missing(which)) which <- 1L:so$number_of_conditions
    path <- so$save_info["save_results_dirname"]
    if(is.na(path))
        stop('results object was not run with save_results = TRUE')
    setwd(paste0(wd, '/', path))
    files <- file_nums <- dir()
    file_nums <- gsub('results-row-', '', file_nums)
    file_nums <- as.numeric(gsub('.rds', '', file_nums))
    files <- data.frame(file_nums, files, stringsAsFactors = FALSE)
    ret <- vector('list', length(which))
    for(i in 1L:length(which)){
        pick <- which(files$file_num == which[i])
        ret[[i]] <- readRDS(files$files[pick])
    }
    if(length(which) == 1L) ret <- ret[[1]]
    ret
}
