#' Generate data
#'
#' Generate data from a single row in the \code{design} input (see \code{\link{runSimulation}}). R contains
#' numerous approaches to generate data, some of which are contained in the base package, as well
#' as in \code{SimDesign} (e.g., \code{\link{rmgh}}, \code{\link{rValeMaurelli}}, \code{\link{rHeadrick}}).
#' However the majority can be found in external packages. See CRAN's list of possible distributions here:
#' \url{https://CRAN.R-project.org/view=Distributions}. Note that this function technically
#' can be omitted if the data generation is provided in the \code{\link{Analyse}} step, though
#' in general this is not recommended.
#'
#' The use of \code{\link{try}} functions is generally not required in this function because \code{Generate}
#' is internally wrapped in a \code{\link{try}} call. Therefore, if a function stops early
#' then this will cause the function to halt internally, the message which triggered the \code{\link{stop}}
#' will be recorded, and \code{Generate} will be called again to obtain a different dataset.
#' That said, it may be useful for users to throw their own \code{\link{stop}} commands if the data
#' should be re-drawn for other reasons (e.g., an estimated model terminated correctly
#' but the maximum number of iterations were reached).
#'
#' @param condition a single row from the \code{design} input (as a \code{data.frame}), indicating the
#'   simulation conditions
#'
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @return returns a single object containing the data to be analyzed (usually a
#'   \code{vector}, \code{matrix}, or \code{data.frame}),
#'   or \code{list}
#'
#' @aliases Generate
#'
#' @seealso \code{\link{addMissing}}, \code{\link{Attach}},
#'   \code{\link{rmgh}}, \code{\link{rValeMaurelli}}, \code{\link{rHeadrick}}
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
#' @examples
#' \dontrun{
#'
#' generate <- function(condition, fixed_objects) {
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
#' # similar to above, but using the Attach() function instead of indexing
#' generate <- function(condition, fixed_objects) {
#'     Attach(condition)
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
#' generate2 <- function(condition, fixed_objects) {
#'     mu <- sample(c(-1,0,1), 1)
#'     dat <- rnorm(100, mu)
#'     dat        #return simple vector (discard mu information)
#' }
#'
#' generate3 <- function(condition, fixed_objects) {
#'     mu <- sample(c(-1,0,1), 1)
#'     dat <- data.frame(DV = rnorm(100, mu))
#'     dat
#' }
#'
#' }
#'
Generate <- function(condition, fixed_objects) NULL

#=================================================================================================#

#' Compute estimates and statistics
#'
#' Compute all relevant test statistics, parameter estimates, detection rates, and so on.
#' This is the computational heavy lifting portion of the Monte Carlo simulation. Users
#' may define a single Analysis function to perform all the analyses in the same function environment,
#' or may define a \code{list} of named functions to \code{\link{runSimulation}} to allow for a more
#' modularized approach to performing the analyses in independent blocks (but that share the same generated
#' data). Note that if a suitable \code{\link{Generate}} function was not supplied then this function
#' can be used to be generate and analyse the Monte Carlo data (though in general this
#' setup is not recommended for larger simulations).
#'
#' In some cases, it may be easier to change the output to a named \code{list} containing
#' different parameter configurations (e.g., when
#' determining RMSE values for a large set of population parameters).
#'
#' The use of \code{\link{try}} functions is generally not required in this function because \code{Analyse}
#' is internally wrapped in a \code{\link{try}} call. Therefore, if a function stops early
#' then this will cause the function to halt internally, the message which triggered the \code{\link{stop}}
#' will be recorded, and \code{\link{Generate}} will be called again to obtain a different dataset.
#' That said, it may be useful for users to throw their own \code{\link{stop}} commands if the data
#' should be re-drawn for other reasons (e.g., an estimated model terminated correctly
#' but the maximum number of iterations were reached).
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
#'   objects will be wrapped into suitable \code{list} objects
#'
#' @seealso \code{\link{stop}}, \code{\link{AnalyseIf}}, \code{\link{manageWarnings}}
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
#' @aliases Analyse
#'
#' @examples
#' \dontrun{
#'
#' analyse <- function(condition, dat, fixed_objects) {
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
#' # A more modularized example approach
#'
#' analysis_welch <- function(condition, dat, fixed_objects) {
#'     welch <- t.test(DV ~ group, dat)
#'     ret <- c(p=welch$p.value)
#'     ret
#' }
#'
#' analysis_ind <- function(condition, dat, fixed_objects) {
#'     ind <- t.test(DV ~ group, dat, var.equal=TRUE)
#'     ret <- c(p=ind$p.value)
#'     ret
#' }
#'
#' # pass functions as a named list
#' # runSimulation(..., analyse=list(welch=analyse_welch, independent=analysis_ind))
#'
#' }
Analyse <- function(condition, dat, fixed_objects) NULL




#=================================================================================================#

#' Summarise simulated data using various population comparison statistics
#'
#' This collapses the simulation results within each condition to composite
#' estimates such as RMSE, bias, Type I error rates, coverage rates, etc. See the
#' \code{See Also} section below for useful functions to be used within \code{Summarise}.
#'
#' @param results a \code{tibble} data frame (if \code{Analyse} returned a named numeric vector of any
#'   length) or a \code{list} (if \code{Analyse} returned a \code{list} or multi-rowed \code{data.frame})
#'   containing the analysis results from \code{\link{Analyse}},
#'   where each cell is stored in a unique row/list element
#'
#' @param condition a single row from the \code{design} input from \code{\link{runSimulation}}
#'   (as a \code{data.frame}), indicating the simulation conditions
#'
#' @param fixed_objects object passed down from \code{\link{runSimulation}}
#'
#' @aliases Summarise
#'
#' @return for best results should return a named \code{numeric} vector or \code{data.frame}
#'   with the desired meta-simulation results. Named \code{list} objects can also be returned,
#'   however the subsequent results must be extracted via \code{\link{SimExtract}}
#'
#' @seealso \code{\link{bias}}, \code{\link{RMSE}}, \code{\link{RE}}, \code{\link{EDR}},
#'   \code{\link{ECR}}, \code{\link{MAE}}, \code{\link{SimExtract}}
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
#' @examples
#' \dontrun{
#'
#' summarise <- function(condition, results, fixed_objects) {
#'
#'     #find results of interest here (alpha < .1, .05, .01)
#'     lessthan.05 <- EDR(results, alpha = .05)
#'
#'     # return the results that will be appended to the design input
#'     ret <- c(lessthan.05=lessthan.05)
#'     ret
#' }
#'
#' }
#'
Summarise <- function(condition, results, fixed_objects) NULL


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
# @references
#
# Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
# with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
# \doi{10.20982/tqmp.16.4.p248}
#
# Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
# Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
# \doi{10.1080/10691898.2016.1246953}
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
mainsim <- function(index, condition, condition.row, generate, analyse, fixed_objects, max_errors, save_results_out_rootdir,
                    save, allow_na, allow_nan, save_seeds, save_seeds_dirname, load_seed, max_time, max_time.start,
                    warnings_as_errors, store_Random.seeds, store_warning_seeds, use_try, include_replication_index,
                    useGenerate, useAnalyseHandler, logging, p = NULL, future = FALSE, allow_gen_errors = TRUE){

    if(!is.null(p)) p(sprintf("replication = %g", index))
    if(include_replication_index) condition$REPLICATION <- index
    try_error <- try_error_seeds <- warning_message_seeds <- NULL

    generate_time <- analyse_time <- 0
    while(TRUE){

        Warnings <- NULL
        wHandler <- function(w) {
            Warnings <<- c(Warnings, list(w))
            invokeRestart("muffleWarning")
        }
        current_Random.seed <- .GlobalEnv$.Random.seed
        if(save_seeds){
            filename_stem <- paste0(save_seeds_dirname, '/design-row-', condition$ID,
                                    '/seed-')
            filename <- paste0(filename_stem, index)
            count <- 1L
            while(file.exists(file.path(save_results_out_rootdir, filename))){
                filename <- paste0(filename_stem, index, '-', count)
                count <- count + 1L
            }
            write(current_Random.seed, file.path(save_results_out_rootdir, filename), sep = ' ')
        }
        if(!is.null(load_seed))
            .GlobalEnv$.Random.seed <- load_seed
        start_time <- proc.time()[3L]
        simlist <- if(useGenerate){
            if(allow_gen_errors)
                try(withCallingHandlers(generate(condition=condition,
                                                            fixed_objects=fixed_objects), warning=wHandler), TRUE)
            else generate(condition=condition, fixed_objects=fixed_objects)
        } else NA
        if(!use_try){
            if(is(simlist, 'try-error')){
                .GlobalEnv$.Random.seed <- current_Random.seed
                debug(generate)
                on.exit(myundebug(generate), add = TRUE)
                generate(condition=condition, fixed_objects=fixed_objects)
                myundebug(generate)
            }
        }
        if(is(simlist, 'try-error')){
            simlist[1L] <-
                gsub('Error in generate\\(condition = condition, fixed_objects = fixed_objects) : \\n  ',
                     replacement = '', simlist[1L])
            try_error <- c(try_error, simlist[1L])
            if(length(try_error) == max_errors){
                try_error_seeds <- rbind(try_error_seeds, current_Random.seed)
                rownames(try_error_seeds) <- paste0('Error_seed_', 1L:nrow(try_error_seeds))
                stop(paste0('Row ', condition$ID, ' in design was terminated because it had ', max_errors,
                            ' consecutive errors. \n\nLast error message was: \n\n  ', simlist[1L]),
                     call.=FALSE)
            }
            try_error_seeds <- rbind(try_error_seeds, current_Random.seed)
            next
        }
        end_time <- proc.time()[3L]
        generate_time <- generate_time + (end_time - start_time)
        start_time <- end_time
        res <- if(useAnalyseHandler)
            try(withCallingHandlers(analyse(dat=simlist, condition=condition,
                               fixed_objects=fixed_objects), warning=wHandler), silent=TRUE)
        else try(analyse(dat=simlist, condition=condition,
                         fixed_objects=fixed_objects), silent=TRUE)
        if(!valid_results(res))
            stop("Invalid object returned from Analyse()", call.=FALSE)
        end_time <- proc.time()[3L]
        analyse_time <- analyse_time + (end_time - start_time)
        if(!use_try){
            if(is(res, 'try-error')){
                debug(analyse)
                on.exit(myundebug(analyse), add = TRUE)
                .GlobalEnv$.Random.seed <- current_Random.seed
                simlist <- generate(condition=condition, fixed_objects=fixed_objects)
                try(analyse(dat=simlist, condition=condition, fixed_objects=fixed_objects), TRUE)
                myundebug(analyse)
            }
        }
        if(!is.null(Warnings)){
            Warnings <- sapply(1L:length(Warnings), function(i, warn) {
                paste0('Warning in ', paste0(deparse(warn[[i]]$call), collapse = ''),
                       ' : ', warn[[i]]$message)
            }, warn=Warnings)
            Warnings <- gsub('Warning in generate(condition = condition, fixed_objects = fixed_objects) : ',
                             replacement = '', Warnings, fixed = TRUE)
            Warnings <- gsub('Warning in analyse(dat = simlist, condition = condition, fixed_objects = fixed_objects) : ',
                 replacement = '', Warnings, fixed = TRUE)
            Warnings <-
                gsub('Warning in ANALYSE_FUNCTIONS\\[\\[i\\]\\]\\(condition = condition, dat = dat, fixed_objects = fixed_objects) : ',
                     replacement = '', Warnings)
            if(warnings_as_errors){
                res <- try(stop(Warnings[1L]), TRUE)
                res[1L] <- Warnings[1L]
                Warnings <- NULL
            }
            if(store_warning_seeds){
                for(i in seq_len(length(Warnings)))
                    warning_message_seeds <- rbind(warning_message_seeds, current_Random.seed)
            }
        }
        if(!allow_nan && !is.list(res) && any(is.nan(res))){
            NA_names <- names(res)[is.nan(res)]
            res <- try(stop(sprintf('The following return NaN and required redrawing: %s',
                                    paste(NA_names, sep=',')),
                            call.=FALSE), silent=TRUE)
        }
        if(!allow_na && !is.list(res) && any(is.na(res))){
            NA_names <- names(res)[is.na(res)]
            res <- try(stop(sprintf('The following return NA and required redrawing: %s',
                                    paste(NA_names, sep=',')),
                            call.=FALSE), silent=TRUE)
        }

        # if an error was detected in analyse(), try again
        if(is(res, 'try-error')){
            res[1L] <-
                gsub('Error in analyse\\(dat = simlist, condition = condition, fixed_objects = fixed_objects) : \\n  ',
                     replacement = '', res[1L])
            res[1L] <-
                gsub('Error in ANALYSE_FUNCTIONS\\[\\[i\\]\\]\\(condition = condition, dat = dat, fixed_objects = fixed_objects) : \\n  ',
                     replacement = '', res[1L])
            try_error <- c(try_error, res[1L])
            if(length(try_error) == max_errors){
                try_error_seeds <- rbind(try_error_seeds, current_Random.seed)
                rownames(try_error_seeds) <- paste0('Error_seed_', 1L:nrow(try_error_seeds))
                stop(paste0('Row ', condition$ID, ' in design was terminated because it had ', max_errors,
                            ' consecutive errors. \n\nLast error message was: \n\n  ', res[1L]), call.=FALSE)

            }
            try_error_seeds <- rbind(try_error_seeds, current_Random.seed)
            next
        }
        if(!is.list(res) && !is.numeric(res) && !is.logical(res))
            stop('analyse() did not return a list or numeric vector', call.=FALSE)

        if(length(try_error))
            rownames(try_error_seeds) <- try_error
        if(store_warning_seeds)
            rownames(warning_message_seeds) <- Warnings
        if(store_Random.seeds)
            attr(res, 'current_Random.seed') <- current_Random.seed
        attr(res, 'generate_analyse_time') <-
            unname(c(generate_time, analyse_time))
        if(logging == 'verbose')
            cat(sprintf("\nDesign row: %s, Replication: %i \n Time (seconds): generate = %s, analyse = %s",
                condition.row, index,
                timeFormater(generate_time/60, output = 'sec'), timeFormater(analyse_time/60, output = 'sec')))
        attr(res, 'try_errors') <- try_error
        attr(res, 'try_error_seeds') <- try_error_seeds
        attr(res, 'warnings') <- Warnings
        attr(res, 'warning_message_seeds') <- warning_message_seeds
        return(res)
    }
}

mainsim_maxtime <- function(max_time, max_time.start, ...){
    st <- proc.time()['elapsed']
    time_left <- max_time - (st - max_time.start)
    out <- R.utils::withTimeout(mainsim(...),
                                timeout = time_left,
                                onTimeout = 'warning')
    out
}
