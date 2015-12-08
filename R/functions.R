#' Generate data
#'
#' Generate data from a single row in the \code{design} input (see \code{\link{runSimulation}}).
#'
#' @param condition a single row from the design input (as a data.frame), indicating the
#'   simulation conditions
#'
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @return returns a single object containing the data to be analysed (usually a vector, matrix, or data.frame),
#'   or a list with a \code{'dat'} and \code{'parameters'} element. If a list is returned, the \code{'dat'}
#'   element should be the observed data object while the
#'   \code{'parameters'} element should be a named list containing the simulated parameters
#'   (if there are any. Otherwise, this could just be an empty list)
#'
#' @aliases generate
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
#'     #could just use return(dat) if no parameters should be tracked
#'     return(list(dat=dat, parameters=pars))
#' }
#'
#' }
#'
generate <- function(condition, fixed_objects = NULL) NULL

#=================================================================================================#

#' Compute estimates and statistics
#'
#' Computes all relevant test statistics, parameter estimates, detection rates, and so on.
#' This is the computational heavy lifting portion of the Monte Carlo simulation.
#' In some cases, it may be easier to change
#' the output to a named list containing different parameter configurations (i.e., when
#' determining RMSE values for a large set of population parameters).
#'
#' Be sure to make heavy use
#' of \code{\link{try}} combinations and throw a \code{\link{stop}}/\code{\link{check_error}}
#' if an iterative function fails
#' to converge. This will cause the function to stop, and \code{\link{generate}} will be called again
#' to obtain a different dataset.
#'
#' @param dat the 'dat' object returned from the \code{\link{generate}} function (usually a data.frame, matrix,
#'   or vector) if a list was returned, otherwise just the raw object defined from \code{\link{generate}}
#' @param parameters the (optional) list object named 'parameters' returned from the
#'   \code{\link{generate}} function when a list is returned. Otherwise, this will be an empty list
#' @param condition a single row from the design input (as a data.frame), indicating the
#'   simulation conditions
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @return returns a named numeric vector with the values of interest (e.g., p-values,
#'   effects sizes, etc), or a list containing values of interest (e.g., separate matrix
#'   and vector of parameter estimates corresponding to elements in \code{parameters})
#'
#' @seealso \code{\link{try}}, \code{\link{check_error}}, \code{\link{stop}}
#' @aliases analyse
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
#'     welch <- try(t.test(DV ~ group, dat), silent=TRUE)
#'     ind <- try(stats::t.test(DV ~ group, dat, var.equal=TRUE), silent=TRUE)
#'
#'     # check if error, and if so stop and return an 'error'. This will re-draw the data
#'     if(is(welch, 'try-error')) stop('Welch error message')
#'     if(is(ind, 'try-error')) stop('Independent t-test error message')
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
analyse <- function(condition, dat, fixed_objects = NULL, parameters = NULL) NULL




#=================================================================================================#

#' Summarise simulated data using various population comparison statistics
#'
#' This collapses the simulation results within each condition to composite
#' estimates such as RMSE, bias, Type I error rates, coverage rates, etc.
#'
#' @param results a data.frame (if \code{analyse} returned a numeric vector) or a list (if
#'   \code{analyse} returned a list) containing the simulation results from \code{\link{analyse}},
#'   where each cell is stored in a unique row/list element
#' @param parameters_list an (optional) list containing all the 'parameters' elements generated
#'   from \code{\link{generate}}, where each repetition is stored in a unique element. If a list was
#'   not returned from \code{\link{generate}} then this will be NULL
#' @param condition a single row from the \code{design} input from \code{\link{runSimulation}}
#'   (as a data.frame), indicating the simulation conditions
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @aliases summarise
#'
#' @return must return a named numeric vector with the desired meta-simulation results
#'
#' @seealso \code{\link{bias}}, \code{\link{RMSE}}, \code{\link{RE}}, \code{\link{EDR}}, \code{\link{ECR}},
#'   \code{\link{MAE}}
#'
#' @examples
#' \dontrun{
#'
#' mysummarise <- function(condition, results, fixed_conditoins = NULL, parameters_list = NULL){
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
summarise <- function(condition, results, fixed_objects = NULL, parameters_list = NULL) NULL

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
# @param generate the \code{\link{generate}} function defined above (required for parallel computing)
# @param analyse the \code{\link{analyse}} function defined above (required for parallel computing)
#
# @return must return a named list with the 'result' and 'parameters' elements for the
#   computational results and \code{parameters} from \code{\link{generate}}
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
                    save_generate_data, save_generate_data_dirname){

    require('SimDesign') #this is required if SimDesign functions are called (e.g., bias(), RMSE())
    try_error <- character()

    while(TRUE){

        simlist <- try(generate(condition=condition, fixed_objects=fixed_objects), TRUE)
        if(is(simlist, 'try-error'))
            stop(paste0('generate function threw an error. Please make sure the function does not throw errors.',
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
        res <- try(analyse(dat=simlist$dat, parameters=simlist$parameters, condition=condition,
                           fixed_objects=fixed_objects), silent=TRUE)

        # if an error was detected in compute(), try again
        if(is(res, 'try-error')){
            try_error <- c(try_error, res[1L])
            if(length(try_error) == max_errors)
                stop(paste0('Row ', condition$ID, ' in design was terminated because it had ', max_errors,
                            ' consecutive errors. Please fix.\n\nLast error message was \n', res[1L]), call.=FALSE)
            next
        }
        if(!is.list(res) && !is.numeric(res))
            stop('analyse() did not return a list or numeric vector', call.=FALSE)

        ret <- list(result=res, parameters=simlist$parameters)
        attr(ret, 'try_errors') <- try_error
        return(ret)
    }
}
