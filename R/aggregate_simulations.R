#' Collapse separate simulation files into a single result
#'
#' This function grabs all .rds files in the working directory and aggregates them into a single
#' data.frame object.
#'
#' @param files a character vector containing the names of the simulation files. If NULL, all files
#'   in the working directory ending in .rds will be used
#'
#' @return a data.frame with the (weighted) average of the simulation results
#'
#' @aliases aggregate_simulations
#'
#' @seealso \code{\link{runSimulation}}
#'
#' @export aggregate_simulations
#'
#' @examples
#' \dontrun{
#'
#' setwd('my_working_directory')
#'
#' final <- aggregate_simulations()
#' head(final)
#'
#' saveRDS(final, 'my_final_simulation.rds')
#'
#' }
aggregate_simulations <- function(files = NULL){
    filenames <- dir()
    filenames <- filenames[grepl('*\\.rds', tolower(filenames))]
    if(!length(filenames)) stop('There are no .rds files in the working directory')
    if(!is.null(files)) filenames <- files
    readin <- vector('list', length(filenames))
    for(i in 1:length(filenames))
        readin[[i]] <- readRDS(filenames[i])
    errors <- lapply(readin, function(x) x[ ,grepl('ERROR_MESSAGE', colnames(x)), drop=FALSE])
    nms <- unique(do.call(c, lapply(errors, function(x) colnames(x))))
    try_errors <- as.data.frame(matrix(0, nrow(readin[[1L]]), length(nms)))
    names(try_errors) <- nms
    readin <- lapply(readin, function(x) x[ ,!grepl('ERROR_MESSAGE', colnames(x)), drop=FALSE])
    if(length(unique(sapply(readin, ncol))) > 1L)
        stop('Number of columns in the replications not equal')
    ret <- readin[[1L]]
    pick <- sapply(readin[[1L]], is.numeric)
    ret[, pick] <- 0
    pick <- pick & !(colnames(readin[[1L]]) %in% c('SIM_TIME', 'REPLICATIONS', 'SEED'))
    weights <- sapply(readin, function(x) x$REPLICATIONS[1L])
    weights <- weights / sum(weights)
    for(i in 1L:length(filenames)){
        tmp <- stats::na.omit(match(nms, names(errors[[i]])))
        if(length(tmp) > 0L){
            try_errors[,match(nms, names(try_errors))] <- errors[[i]][ ,tmp] +
                try_errors[,match(nms, names(try_errors))]
        }
        ret$REPLICATIONS <- ret$REPLICATIONS + readin[[i]]$REPLICATIONS
        ret$SIM_TIME <- ret$SIM_TIME + readin[[i]]$SIM_TIME
        ret[ ,pick] <- ret[ ,pick] + weights[i] * readin[[i]][ ,pick]
    }
    out <- data.frame(ret, try_errors, check.names = FALSE)
    out$SEED <- NULL
    out
}
