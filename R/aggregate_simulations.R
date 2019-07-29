#' Collapse separate simulation files into a single result
#'
#' This function grabs all \code{.rds} files in the working directory and aggregates them into a single
#' \code{data.frame} object or combines all the saved results directories and combines them into one.
#' This is generally useful when results are run piecewise on one node or run independently across
#' different nodes/computers which are not on the same network.
#'
#' @param files a \code{character} vector containing the names of the simulation files. If \code{NULL},
#'   all files in the working directory ending in \code{.rds} will be used
#'
#' @param dirs a \code{character} vector containing the names of the \code{save_results} directories to be
#'   aggregated. A new folder will be created and placed in the \code{results_dirname} output folder
#'
#' @param results_dirname the new directory to place the aggregated results files
#'
#' @return if \code{files} is used the function returns a \code{data.frame} with the (weighted) average
#'   of the simulation results. Otherwise, if \code{dirs} is used, the function returns NULL
#'
#' @aliases aggregate_simulations
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @seealso \code{\link{runSimulation}}
#'
#' @export aggregate_simulations
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' setwd('my_working_directory')
#'
#' ## run simulations to save the .rds files (or move them to the working directory)
#' # runSimulation(..., filename='file1')
#' # runSimulation(..., filename='file2')
#'
#' final <- aggregate_simulations()
#' saveRDS(final, 'my_final_simulation.rds')
#'
#' # aggregate saved results
#' # runSimulation(..., save_results = TRUE, save_details = list(save_results_dirname = 'dir1'))
#' # runSimulation(..., save_results = TRUE, save_details = list(save_results_dirname = 'dir2'))
#'
#' # place new saved results in 'SimDesign_results/' directory by default
#' aggregate_simulations(dirs = c('dir1', 'dir2'))
#'
#'
#' }
aggregate_simulations <- function(files = NULL, dirs = NULL, results_dirname = 'SimDesign_aggregate_results'){
    if(!is.null(dirs)){
        if(!all(sapply(dirs, dir.exists))) stop('One or more directories not found')
        files <- lapply(dirs, function(x) dir(x))
        if(!all(sapply(files, function(x) all(x == files[[1L]]))))
            stop('File names are not all the same')
        files <- files[[1L]]
        ndirs <- length(dirs)
        dir.create(results_dirname)
        for(f in files){
            readin <- lapply(1:ndirs, function(x) readRDS(paste0(dirs[x], '/', f)))
            ret <- readin[[1L]]
            collapse <- !is.list(ret$results) || is.data.frame(ret$results)
            results <- lapply(readin, function(x) x$results)
            ret$results <- do.call(if(collapse) rbind else c, results)
            tmp <- do.call(c, lapply(readin, function(x) x$warnings))
            nms <- names(tmp)
            if(length(nms))
                ret$warnings <- table(do.call(c, lapply(1:length(nms), function(x) rep(nms[x], each = tmp[x]))))
            tmp <- do.call(c, lapply(readin, function(x) x$errors))
            nms <- names(tmp)
            if(length(nms))
                ret$errors <- table(do.call(c, lapply(1:length(nms), function(x) rep(nms[x], each = tmp[x]))))
            saveRDS(ret, paste0(results_dirname, '/', f))
        }
        return(invisible(NULL))
    }
    filenames <- dir()
    filenames <- filenames[grepl('*\\.rds', tolower(filenames))]
    if(!length(filenames)) stop('There are no .rds files in the working directory')
    if(!is.null(files)) filenames <- files
    readin <- vector('list', length(filenames))
    for(i in 1:length(filenames))
        readin[[i]] <- readRDS(filenames[i])
    errors <- lapply(readin, function(x) x[ ,grepl('ERROR', colnames(x)), drop=FALSE])
    nms <- unique(do.call(c, lapply(errors, function(x) colnames(x))))
    try_errors <- as.data.frame(matrix(0, nrow(readin[[1L]]), length(nms)))
    names(try_errors) <- nms
    readin <- lapply(readin, function(x) x[ ,!grepl('ERROR', colnames(x)), drop=FALSE])
    if(length(unique(sapply(readin, ncol))) > 1L)
        stop('Number of columns in the replications not equal')
    ret <- readin[[1L]]
    pick <- sapply(readin[[1L]], is.numeric)
    ret[, pick] <- 0
    pick <- pick & !(colnames(readin[[1L]]) %in% c('SIM_TIME', 'REPLICATIONS', 'SEED'))
    weights <- sapply(readin, function(x) x$REPLICATIONS[1L])
    stopifnot(all(sapply(readin, function(x) length(unique(x$REPLICATIONS)) == 1L)))
    weights <- weights / sum(weights)
    message('Aggregating ', length(filenames), ' simulation files.')
    for(i in seq_len(length(filenames))){
        tmp <- stats::na.omit(match(nms, names(errors[[i]])))
        if(length(tmp) > 0L){
            try_errors[,match(nms, names(try_errors))] <- errors[[i]][ ,tmp] +
                try_errors[,match(nms, names(try_errors))]
        }
        ret$REPLICATIONS <- ret$REPLICATIONS + readin[[i]]$REPLICATIONS
        ret$SIM_TIME <- ret$SIM_TIME + readin[[i]]$SIM_TIME
        ret[ ,pick] <- ret[ ,pick] + weights[i] * readin[[i]][ ,pick]
    }
    out <- dplyr::as_tibble(data.frame(ret, try_errors, check.names = FALSE))
    out$SEED <- NULL
    out
}
