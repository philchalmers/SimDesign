#' Generate data
#'
#' Generate data from a single row in the \code{design} input (see \code{\link{runSimulation}}).
#'
#' @param condition a single row from the \code{design} input (as a \code{data.frame}), indicating the
#'   simulation conditions
#'
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @return returns a single object containing the data to be analyzed (usually a
#'   \code{vector}, \code{matrix}, or \code{data.frame}),
#'   or \code{list})
#'
#' @aliases Generate
#'
#' @seealso \code{\link{add_missing}}, \code{\link{attach_condition}}
#'
#' @examples
#' \dontrun{
#'
#' mygenerate <- function(condition, fixed_objects = NULL){
#'     N1 <- condition$sample_sizes_group1
#'     N2 <- condition$sample_sizes_group2
#'     sd <- condition$standard_deviations
#'
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
#'                       DV = c(group1, group2))
#'     # just a silly example of a simulated parameter
#'     pars <- list(random_number = rnorm(1))
#'
#'     list(dat=dat, parameters=pars)
#' }
#'
#' # similar to above, but using the attach_condition() function instead of indexing
#' mygenerate <- function(condition, fixed_objects = NULL){
#'     attach_condition(condition)
#'     N1 <- sample_sizes_group1
#'     N2 <- sample_sizes_group2
#'     sd <- standard_deviations
#'
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
#'                       DV = c(group1, group2))
#'     dat
#' }
#'
#' mygenerate2 <- function(condition, fixed_objects = NULL){
#'     mu <- sample(c(-1,0,1), 1)
#'     dat <- rnorm(100, mu)
#'     dat        #return simple vector (discard mu information)
#' }
#'
#' mygenerate3 <- function(condition, fixed_objects = NULL){
#'     mu <- sample(c(-1,0,1), 1)
#'     dat <- data.frame(DV = rnorm(100, mu))
#'     dat
#' }
#'
#' }
#'
Generate <- function(condition, fixed_objects = NULL) NULL

#=================================================================================================#

#' Compute estimates and statistics
#'
#' Computes all relevant test statistics, parameter estimates, detection rates, and so on.
#' This is the computational heavy lifting portion of the Monte Carlo simulation.
#'
#' In some cases, it may be easier to change the output to a named \code{list} containing
#' different parameter configurations (e.g., when
#' determining RMSE values for a large set of population parameters).
#'
#' The use of \code{\link{try}} functions is generally not required because the function
#' is internally wrapped in a \code{\link{try}} call. Therefore, if a function stops early
#' then this will cause the function to halt iternally, the message which triggered the \code{\link{stop}}
#' will be recorded, and \code{\link{Generate}} will be called again to obtain a different dataset.
#' That being said, it may be useful for users to throw their own \code{\link{stop}} commands if the data
#' should be redrawn for other reasons (e.g., a model terminated correctly but the maximum number of
#' iterations were reached).
#'
#' @param dat the \code{dat} object returned from the \code{\link{Generate}} function
#'   (usually a \code{data.frame}, \code{matrix}, \code{vector}, or \code{list})
#'
#' @param condition a single row from the design input (as a \code{data.frame}), indicating the
#'   simulation conditions
#'
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @return returns a named \code{numeric} vector or \code{data.frame} with the values of interest
#'   (e.g., p-values, effects sizes, etc), or a \code{list} containing values of interest
#'   (e.g., separate matrix and vector of parameter estimates corresponding to elements in
#'   \code{parameters}). If a \code{data.frame} is returned with more than 1 row then these
#'   objects will be wrapped into \code{list} objects
#'
#' @seealso \code{\link{stop}}
#' @aliases Analyse
#'
#' @examples
#' \dontrun{
#'
#' myanalyse <- function(condition, dat, fixed_objects = NULL){
#'
#'     # require packages/define functions if needed, or better yet index with the :: operator
#'     require(stats)
#'     mygreatfunction <- function(x) print('Do some stuff')
#'
#'     #wrap computational statistics in try() statements to control estimation problems
#'     welch <- t.test(DV ~ group, dat)
#'     ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)
#'
#'     # In this function the p values for the t-tests are returned,
#'     #  and make sure to name each element, for future reference
#'     ret <- c(welch = welch$p.value,
#'              independent = ind$p.value)
#'
#'     return(ret)
#' }
#'
#' }
Analyse <- function(condition, dat, fixed_objects = NULL) NULL




#=================================================================================================#

#' Summarise simulated data using various population comparison statistics
#'
#' This collapses the simulation results within each condition to composite
#' estimates such as RMSE, bias, Type I error rates, coverage rates, etc.
#'
#' @param results a \code{data.frame} (if \code{Analyse} returned a numeric vector) or a \code{list}
#'   (if \code{Analyse} returned a list or multirowed data.frame) containing the analysis
#'   results from \code{\link{Analyse}},
#'   where each cell is stored in a unique row/list element
#'
#' @param condition a single row from the \code{design} input from \code{\link{runSimulation}}
#'   (as a \code{data.frame}), indicating the simulation conditions
#'
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @aliases Summarise
#'
#' @return must return a named \code{numeric} vector or \code{data.frame}
#'   with the desired meta-simulation results
#'
#' @seealso \code{\link{bias}}, \code{\link{RMSE}}, \code{\link{RE}}, \code{\link{EDR}},
#'   \code{\link{ECR}}, \code{\link{MAE}}
#'
#' @examples
#' \dontrun{
#'
#' mysummarise <- function(condition, results, fixed_objects = NULL){
#'
#'     #find results of interest here (alpha < .1, .05, .01)
#'     lessthan.05 <- EDR(results, alpha = .05)
#'
#'     # return the results that will be appended to the design input
#'     ret <- c(lessthan.05=lessthan.05)
#'     return(ret)
#' }
#'
#' }
#'
Summarise <- function(condition, results, fixed_objects = NULL) NULL


#=================================================================================================#

# Main subroutine
#
# This function runs one condition at a time exactly once. This is for
# repeating the simulation for each condition a number of times, and is where the
# Monte Carlo flow is controlled. Generally speaking, you shouldn't need to edit this
# function, therefore \emph{only replace it if you really know what you are doing}.
#
# @param index an integer place-holder value indexed from the \code{1:replications} input, where each value
#   represents a particular draw given the \code{replications} argument in \code{\link{runSimulation}}
# @param condition a single row from the design input (as a data.frame), indicating the
#   simulation conditions
# @param fixed_objects object passed down from \code{\link{runSimulation}}
# @param generate the \code{\link{Generate}} function defined above (required for parallel computing)
# @param analyse the \code{\link{Analyse}} function defined above (required for parallel computing)
#
# @return must return a named list with the 'result' and 'parameters' elements for the
#   computational results and \code{parameters} from \code{\link{Generate}}
#
# @aliases main
#
# @export main
#
# @examples
# \dontrun{
#
# # This is the default main function
# print(SimDesign::main)
#
# }
mainsim <- function(index, condition, generate, analyse, fixed_objects, max_errors,
                    save_generate_data, save_generate_data_dirname,
                    save_seeds, save_seeds_dirname, load_seed, packages = NULL){

    load_packages(packages)
    try_error <- character()

    while(TRUE){

        current_Random.seed <- .GlobalEnv$.Random.seed
        if(save_seeds){
            filename_stem <- paste0(save_seeds_dirname, '/design-row-', condition$ID,
                                    '/seed-')
            filename <- paste0(filename_stem, index)
            count <- 1L
            while(file.exists(filename)){
                filename <- paste0(filename_stem, index, '-', count)
                count <- count + 1L
            }
            write(current_Random.seed, filename, sep = ' ')
        }
        if(!is.null(load_seed))
            .GlobalEnv$.Random.seed <- as.integer(scan(load_seed, sep = ' ', quiet = TRUE))
        simlist <- try(generate(condition=condition, fixed_objects=fixed_objects), TRUE)
        if(is(simlist, 'try-error'))
            stop(paste0('generate function threw an error.',
                        '\n\nError message was: ', simlist), call.=FALSE)
        if(save_generate_data){
            filename_stem <- paste0(save_generate_data_dirname, '/design-row-', condition$ID,
                                    '/generate-data-')
            filename <- paste0(filename_stem, index, '.rds')
            count <- 1L
            while(file.exists(filename)){
                filename <- paste0(filename_stem, index, '-', count, '.rds')
                count <- count + 1L
            }
            saveRDS(simlist, filename)
        }
        Warnings <- NULL
        wHandler <- function(w) {
            Warnings <<- c(Warnings, list(w))
            invokeRestart("muffleWarning")
        }
        res <- try(withCallingHandlers(analyse(dat=simlist, condition=condition,
                           fixed_objects=fixed_objects), warning=wHandler), silent=TRUE)
        if(!is.null(Warnings)){
            Warnings <- sapply(1L:length(Warnings), function(i, Warnings) {
                paste0('Warning in ', deparse(Warnings[[i]]$call), ' : ', Warnings[[i]]$message)
            }, Warnings)
        }
        if(any(is.na(res))){
            NA_names <- names(res)[is.na(res)]
            res <- try(stop(sprintf('The following return NA/NaN and required redrawing: %s',
                                    paste(NA_names, sep=',')),
                            call.=FALSE), silent=TRUE)
        }

        # if an error was detected in compute(), try again
        if(is(res, 'try-error')){
            try_error <- c(try_error, res[1L])
            if(length(try_error) == max_errors){
                res[1L] <-
                    gsub('Error in analyse\\(dat = simlist, condition = condition, fixed_objects = fixed_objects) : \\n  ',
                         replacement = 'Error : ', res[1L])
                stop(paste0('Row ', condition$ID, ' in design was terminated because it had ', max_errors,
                            ' consecutive errors. \n\nLast error message was: \n\n  ', res[1L]), call.=FALSE)
            }
            next
        }
        if(!is.list(res) && !is.numeric(res))
            stop('analyse() did not return a list or numeric vector', call.=FALSE)

        attr(res, 'try_errors') <- try_error
        attr(res, 'warnings') <- Warnings
        return(res)
    }
}
