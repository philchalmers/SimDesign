#' Combine two separate SimDesign objects by row
#'
#' This function combines two Monte Carlo simulations executed by
#' \code{SimDesign}'s \code{\link{runSimulation}} function which, for all
#' intents and purposes, could have been executed in a single run.
#' This situation arises when a simulation has been completed, however
#' the \code{Design} object was later modified to include more levels in the
#' defined simulation factors. Rather than re-executing the previously completed
#' simulation combinations, only the new combinations need to be evaluated
#' into a different object and then \code{rbind} together to create the complete
#' object combinations.
#'
#' @param ... two or more \code{SimDesign} objects that should be
#'   combined by rows
#'
#' @return same object that is returned by \code{\link{runSimulation}}
#'
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' # modified example from runSimulation()
#'
#' Design <- createDesign(N = c(10, 20),
#'                        SD = c(1, 2))
#'
#' Generate <- function(condition, fixed_objects = NULL){
#'     dat <- with(condition, rnorm(N, 10, sd=SD))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL){
#'     ret <- mean(dat) # mean of the sample data vector
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
#'     ret
#' }
#'
#' Final1 <- runSimulation(design=Design, replications=1000,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' Final1
#'
#' ###
#' # later decide that N = 30 should have also been investigated. Rather than
#' # running the following object ....
#' newDesign <- createDesign(N = c(10, 20, 30),
#'                           SD = c(1, 2))
#'
#' # ... only the new subset levels are executed to save time
#' subDesign <- subset(newDesign, N == 30)
#' subDesign
#'
#' Final2 <- runSimulation(design=subDesign, replications=1000,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' Final2
#'
#' # glue results together by row into one object as though the complete 'Design'
#' # object were run all at once
#' Final <- rbind(Final1, Final2)
#' Final
#'
#' summary(Final)
#'
#' }
rbind.SimDesign <- function(...){
    dots <- list(...)
    stopifnot(all(sapply(dots, function(x) is(x, 'SimDesign'))))
    ret <- as.data.frame(dots[[1L]])
    extra_info <- attr(dots[[1L]], 'extra_info')
    design_names <- attr(dots[[1L]], 'design_names')
    for(i in 2L:length(dots)){
        ret <- plyr::rbind.fill(ret, as.data.frame(dots[[i]]))
        tmp_design_names <- attr(dots[[i]], 'design_names')
        tmp_extra_info <- attr(dots[[i]], 'extra_info')
        design_names$errors <- unique(design_names$errors, tmp_design_names$errors)
        design_names$warnings <- unique(design_names$warnings, tmp_design_names$warnings)
        extra_info$number_of_conditions <- extra_info$number_of_conditions + tmp_extra_info$number_of_conditions
        extra_info$total_elapsed_time <- extra_info$total_elapsed_time + tmp_extra_info$total_elapsed_time
        extra_info$error_seeds <- c(extra_info$error_seeds, tmp_extra_info$error_seeds)

        # throw warning if package versions differ
        lapply(extra_info$sessionInfo$otherPkgs, function(x, tmp){
            if(x$Version != tmp$sessionInfo$otherPkgs[[x$Package]]$Version)
                warning(sprintf('Different "%s" package used across simulation objects (%s and %s).',
                                x$Package, x$Version, tmp$sessionInfo$otherPkgs[[x$Package]]$Version), call.=FALSE)
        }, tmp=tmp_extra_info)
    }

    ret <- dplyr::as_tibble(ret)
    attr(ret, 'extra_info') <- extra_info
    attr(ret, 'design_names') <- design_names
    class(ret) <- c('SimDesign', class(ret))
    ret
}
