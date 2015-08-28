#' Collapse seperate simulation files into a single result
#'
#' This function grabs all .rds files in the working directory and aggregates them into a single
#' data.frame object. Weights are infered from the last numbers before .rds, in the form
#' "name_of_sim_100.rds".
#'
#' @param files a character vector containing the names of the simulation files. If NULL, all files
#'   in the working directory ending in .rds will be used
#'
#' @return a data.frame with the (weighted) average of the simulation results
#'
#' @aliases aggregate_simulations
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
    ret <- readin[[1L]]
    pick <- sapply(readin[[1L]], is.numeric)
    ret[, pick] <- 0
    pick <- pick & !(colnames(readin[[1L]]) %in% c('SIM_TIME', 'N_CELL_RUNS'))
    splt <- strsplit(filenames, '_')
    weights <- sapply(splt, function(x){
        y <- x[length(x)]
        as.integer(strsplit(y, '.rds')[[1L]])
    })
    weights <- weights / sum(weights)
    for(i in 1L:length(filenames)){
        ret$N_CELL_RUNS <- ret$N_CELL_RUNS + readin[[i]]$N_CELL_RUNS
        ret$SIM_TIME <- ret$SIM_TIME + readin[[i]]$SIM_TIME
        ret[ ,pick] <- ret[ ,pick] + weights[i] * readin[[i]][ ,pick]
    }
    ret
}
