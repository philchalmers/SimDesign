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
#'   or a list with the elements \code{'dat'} and \code{'parameters'}.
#'
#'   If a list is returned the \code{'dat'} element should be the observed data object while the
#'   \code{'parameters'} element should be a named list containing the simulated parameters
#'   (if there are any. Otherwise, this could just be an empty list),
#'   or any other objects that would be useful
#'   in the \code{\link{Analyse}} and \code{\link{Summarise}} functions. If, on the other hand,
#'   the objects are only useful in the \code{\link{Analyse}} function and NOT
#'   \code{\link{Summarise}} then simply adding \code{\link{attributes}} to the returned object
#'   is sufficent (and requires less RAM)
#'
#' @aliases Generate
#'
#' @seealso \code{\link{add_missing}}
#'
#' @examples
#' \dontrun{
#'
#' mygenerate <- function(condition, fixed_objects = NULL){
#'
#'     #require packages/define functions if needed, or better yet index with the :: operator
#'
#'     N1 <- condition$sample_sizes_group1
#'     N2 <- condition$sample_sizes_group2
#'     sd <- condition$standard_deviations
#'
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
#'     pars <- list(random_number = rnorm(1)) # just a silly example of a simulated parameter
#'
#'     #could just use return(dat) if no parameters should be tracked for Summerise
#'     return(list(dat=dat, parameters=pars))
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
#'     dat <- rnorm(100, mu)
#'     attr(dat, 'mu') <- mu    # store mu as an attribute 'mu'
#'     dat
#' }
#'
#' # in the Analyse function, use attr(dat, 'mu') to pull out the mu object for further use
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
#'   (usually a \code{data.frame}, \code{matrix}, or \code{vector}).
#'
#' @param parameters the (optional) list object named 'parameters' returned from the
#'   \code{\link{Generate}} function when a list is returned. Otherwise, this will be an just an
#'   empty list
#'
#' @param condition a single row from the design input (as a \code{data.frame}), indicating the
#'   simulation conditions
#'
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @return returns a named \code{numeric} vector with the values of interest (e.g., p-values,
#'   effects sizes, etc), or a \code{list} containing values of interest (e.g., separate matrix
#'   and vector of parameter estimates corresponding to elements in \code{parameters})
#'
#' @seealso \code{\link{stop}}
#' @aliases Analyse
#'
#' @examples
#' \dontrun{
#'
#' myanalyse <- function(condition, dat, fixed_objects = NULL, parameters = NULL){
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
Analyse <- function(condition, dat, fixed_objects = NULL, parameters = NULL) NULL




#=================================================================================================#

#' Summarise simulated data using various population comparison statistics
#'
#' This collapses the simulation results within each condition to composite
#' estimates such as RMSE, bias, Type I error rates, coverage rates, etc.
#'
#' @param results a \code{data.frame} (if \code{Analyse} returned a numeric vector) or a \code{list}
#'   (if \code{Analyse} returned a list) containing the simulation results from \code{\link{Analyse}},
#'   where each cell is stored in a unique row/list element
#' @param parameters_list an (optional) list containing all the 'parameters' elements generated
#'   from \code{\link{Generate}}, where each repetition is stored in a unique element. If a \code{list}
#'   was not returned from \code{\link{Generate}} then this will be \code{NULL}
#' @param condition a single row from the \code{design} input from \code{\link{runSimulation}}
#'   (as a \code{data.frame}), indicating the simulation conditions
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @aliases Summarise
#'
#' @return must return a named \code{numeric} vector with the desired meta-simulation results
#'
#' @seealso \code{\link{bias}}, \code{\link{RMSE}}, \code{\link{RE}}, \code{\link{EDR}},
#'   \code{\link{ECR}}, \code{\link{MAE}}
#'
#' @examples
#' \dontrun{
#'
#' mysummarise <- function(condition, results, fixed_objects = NULL, parameters_list = NULL){
#'
#'     #convert to matrix for convenience (if helpful)
#'     cell_results <- do.call(rbind, results)
#'
#'     # silly test for bias and RMSE of a random number from 0
#'     pop_value <- 0
#'     bias.random_number <- bias(sapply(parameters_list, function(x) x$random_number), pop_value)
#'     RMSE.random_number <- RMSE(sapply(parameters_list, function(x) x$random_number), pop_value)
#'
#'     #find results of interest here (alpha < .1, .05, .01)
#'     nms <- c('welch', 'independent')
#'     lessthan.05 <- EDR(results[,nms], alpha = .05)
#'
#'     # return the results that will be appended to the design input
#'     ret <- c(bias.random_number=bias.random_number,
#'              RMSE.random_number=RMSE.random_number,
#'              lessthan.05=lessthan.05)
#'     return(ret)
#' }
#'
#' }
#'
Summarise <- function(condition, results, fixed_objects = NULL, parameters_list = NULL) NULL


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
                    save_generate_data, save_generate_data_dirname, packages = NULL){

    load_packages(packages)
    try_error <- character()

    while(TRUE){

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
        if(is.data.frame(simlist) || !is.list(simlist)) simlist <- list(dat=simlist)
        if(length(names(simlist)) > 1L)
            if(!all(names(simlist) %in% c('dat', 'parameters')))
                stop('generate() did not return a list with elements \'dat\' and \'parameters\'', call.=FALSE)
        Warnings <- NULL
        wHandler <- function(w) {
            Warnings <<- c(Warnings, list(w))
            invokeRestart("muffleWarning")
        }
        res <- try(withCallingHandlers(analyse(dat=simlist$dat, parameters=simlist$parameters, condition=condition,
                           fixed_objects=fixed_objects), warning=wHandler), silent=TRUE)
        if(!is.null(Warnings)){
            Warnings <- sapply(1L:length(Warnings), function(i, Warnings) {
                paste0('Warning in ', deparse(Warnings[[i]]$call), ' : ', Warnings[[i]]$message)
            }, Warnings)
        }

        # if an error was detected in compute(), try again
        if(is(res, 'try-error')){
            try_error <- c(try_error, res[1L])
            if(length(try_error) == max_errors){
                res[1L] <-
                    gsub('Error in analyse\\(dat = simlist\\$dat, parameters = simlist\\$parameters, condition = condition,  : \\n  ',
                         replacement = 'Manual Error : ', res[1L])
                stop(paste0('Row ', condition$ID, ' in design was terminated because it had ', max_errors,
                            ' consecutive errors. \n\nLast error message was: \n\n  ', res[1L]), call.=FALSE)
            }
            next
        }
        if(!is.list(res) && !is.numeric(res))
            stop('analyse() did not return a list or numeric vector', call.=FALSE)

        ret <- list(result=res, parameters=simlist$parameters)
        attr(ret, 'try_errors') <- try_error
        attr(ret, 'warnings') <- Warnings
        return(ret)
    }
}
