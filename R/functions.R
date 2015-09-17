#' Generate data
#'
#' @param condition a single row from the design input (as a data.frame), indicating the
#'   simulation conditions
#'
#' @return returns a list with a 'dat' and 'parameters' element.  The 'dat' element should be
#'   the observed data object (i.e., a vector, matrix, or data.frame), and the 'parameters'
#'   element should be a named list containing the simulated parameters (if any, otherwise,
#'   this could just be an empty list)
#'
#' @aliases generate
#'
#' @examples
#' \dontrun{
#'
#' mygenerate <- function(condition){
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
#'     return(list(dat=dat, parameters=pars))
#' }
#'
#' }
#'
generate <- function(condition) NULL

#=================================================================================================#

#' Compute estimates and statistics
#'
#' Compute all relevant test statistics and parameter estimates here.
#' This is the computational heavy lifting section. In some cases, it may be easier to change
#' the output to a named list containing different parameter configurations (i.e., when
#' determining RMSD values for a large set of population parameters).
#'
#' Be sure to make heavy use
#' of \code{\link{try}} combinations and throw a \code{\link{stop}} if an iterative function fails
#' to converge. This will cause the function to stop, and \code{\link{generate}} will be called again
#' to obtain a different dataset.
#'
#' @param simlist a list containing the data.frame ('dat') and parameters ('parameters')
#'   generated from the sim() function
#' @param condition a single row from the design input (as a data.frame), indicating the
#'   simulation conditions
#'
#' @return returns a named numeric vector with the values of interest (e.g., p-values,
#'   effects sizes, etc), or a list containing values of interest (e.g., seperate matrix
#'   and vector of parameter estimates corresponding to elements in simlist$parameters)
#'
#' @aliases analyse
#'
#' @examples
#' \dontrun{
#'
#' myanalyse <- function(simlist, condition){
#'
#'     # require packages/define functions if needed, or better yet index with the :: operator
#'     require(stats)
#'     mygreatfunction <- function(x) print('Do some stuff')
#'
#'     # begin extracting the data elements
#'     dat <- simlist$dat
#'     parameters <- simlist$parameters
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
analyse <- function(simlist, condition) NULL




#=================================================================================================#

#' Summarise simulated data using various population comparison statistics
#'
#' This collapses across the simulation results within each condition for computing composite
#' estimates such as RMSD, bias, Type I error, etc.
#'
#' @param results a data.frame (if \code{analyse} returned a numeric vector) or a list (if
#'   \code{analyse} returned a list) containing the simulation results from analyse(), where each cell
#'   is stored in a unique row/list element
#' @param parameters a list containing all the 'parameters' elements generated from generate(), where
#'   each repetition is stored in a unique list element
#' @param condition a single row from the design input (as a data.frame), indicating the
#'   simulation conditions
#'
#' @aliases summarise
#'
#' @return must return a named numeric vector with the desired meta-simulation results
#'
#' @examples
#' \dontrun{
#'
#' mysummarise <- function(results, parameters, condition){
#'
#'     #convert to matrix for convenience (if helpful)
#'     cell_results <- do.call(rbind, results)
#'
#'     # silly test for bias and RMSD of a random number from 0
#'     pop_value <- 0
#'     bias.random_number <- bias(sapply(parameters, function(x) x$random_number), pop_value)
#'     MSE.random_number <- MSE(sapply(parameters, function(x) x$random_number), pop_value)
#'
#'     #find results of interest here (alpha < .1, .05, .01)
#'     nms <- c('welch', 'independent')
#'     lessthan.10 <- colMeans(cell_results[,nms] < .10)
#'     lessthan.05 <- colMeans(cell_results[,nms] < .05)
#'     lessthan.01 <- colMeans(cell_results[,nms] < .01)
#'
#'     # return the results that will be appended to the design input
#'     ret <- c(bias.random_number=bias.random_number,
#'              MSE.random_number=MSE.random_number,
#'              lessthan.10=lessthan.10,
#'              lessthan.05=lessthan.05,
#'              lessthan.01=lessthan.01)
#'     return(ret)
#' }
#'
#' }
#'
summarise <- function(results, parameters, condition) NULL

#=================================================================================================#

#' Main subroutine
#'
#' This function runs one condition at a time exactly once. This is for
#' repeating the simulation for each condition a number of times, and is where the
#' Monte Carlo flow is controlled. Generally speaking, you shouldn't need to edit this
#' function.
#'
#' @param index an integer place-holder value indexed from the 1:each input
#' @param condition a single row from the design input (as a data.frame), indicating the
#'   simulation conditions
#' @param generate the \code{\link{generate}} function defined above (required for parallel computing)
#' @param analyse the \code{\link{analyse}} function defined above (required for parallel computing)
#'
#' @return must return a named list with the 'result' and 'parameters' elements for the
#'   computational results and Monte Carlo parameters from generate()
#'
#' @aliases main
#'
#' @export main
#'
#' @examples
#' \dontrun{
#'
#' # This is the default main function
#' print(SimDesign::main)
#'
#' }
main <- function(index, condition, generate, analyse){

    count <- 0L

    while(TRUE){

        count <- count + 1L
        simlist <- generate(condition)
        if(!is.list(simlist) || !all(names(simlist) %in% c('dat', 'parameters')))
            stop('generate() did not return a list with elements dat and parameters', call.=FALSE)
        res <- try(analyse(simlist=simlist, condition=condition), silent=TRUE)

        # if an error was detected in compute(), try again
        if(is(res, 'try-error')) next
        if(!is.list(res) && !is.numeric(res))
            stop('analyse() did not return a list or numeric vector', call.=FALSE)

        # else return the result with simulation parameters
        if(!is.list(res)){
            result <- c(res, n_cell_runs=count)
        } else {
            result <- res
            result$n_cell_runs <- count
        }
        return(list(result=result, parameters=simlist$parameters))
    }
}
