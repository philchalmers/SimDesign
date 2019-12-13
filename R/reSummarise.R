#' Run a summarise step for results that have been saved to the hard drive
#'
#' When \code{runSimulation()} uses the option \code{save_results = TRUE}
#' the R replication results from the Generate-Analyse functions are
#' stored to the hard drive. As such, additional summarise components
#' may be required at a later time, whereby the respective \code{.rds} files
#' must be read back into R to be summarised. This function performs
#' the reading of these files, application of a provided summarise function,
#' and final collection of the respective results.
#'
#' @param summarise a summarise function to apply to the read-in files.
#'   See \code{\link{runSimulation}} for details
#'
#' @param dir directory pointing to the .rds files to be
#'   read-in that were saved from \code{runSimulation(..., save_results=TRUE)}.
#'   If \code{NULL}, it is assumed the current working directory contains
#'   the .rds files
#'
#' @param files (optional) names of files to read-in. If \code{NULL} all files
#'   located within \code{dir} will be used
#'
#' @param fixed_objects (optional) see \code{\link{runSimulation}} for details
#'
#' @param bootSE logical; perform a non-parametric bootstrap to compute bootstrap standard error
#'   estimates for the respective meta-statistics computed by the \code{Summarise} function?
#'   See \code{\link{runSimulation}} for details
#'
#' @param boot_draws number of non-parametric bootstrap draws to sample for the \code{summarise}
#'   function after the generate-analyse replications are collected. Default is 1000
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @examples
#'
#' \dontrun{
#'
#' Design <- data.frame(N = c(10, 20, 30))
#'
#' Generate <- function(condition, fixed_objects = NULL){
#'     dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL){
#'     ret <- mean(dat) # mean of the sample data vector
#'     ret
#' }
#'
#' # run the simulation
#' runSimulation(design=Design, replications=50,
#'               generate=Generate, analyse=Analyse,
#'               summarise=NA, save_results=TRUE,
#'               save_details = list(save_results_dirname='simresults'))
#'
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     ret <- c(mu=mean(results), SE=sd(results))
#'     ret
#' }
#'
#' SimClean('SimDesign-results.rds')
#'
#' results <- reSummarise(Summarise, dir = 'simresults/')
#' results
#'
#' Summarise2 <- function(condition, results, fixed_objects = NULL){
#'     mean(results)
#' }
#'
#' results2 <- reSummarise(Summarise2, dir = 'simresults/')
#' results2
#'
#' SimClean('simresults/')
#' }
reSummarise <- function(summarise, dir = NULL, files = NULL,
                        fixed_objects = NULL, bootSE = FALSE, boot_draws = 1000L){
    current_wd <- getwd()
    on.exit(setwd(current_wd))
    if(!is.null(dir)) setwd(dir)
    if(is.null(files)) files <- dir()
    expect_filenames <- paste0(paste0('results-row-'), 1L:length(files), '.rds')
    if(!all(files %in% expect_filenames))
        stop('Filenames in select directory did not follow the \'results-row-#\' pattern. Please fix')
    res <- vector('list', length(files))
    conditions <- vector('list', length(files))

    for(i in 1L:length(files)){
        inp <- readRDS(files[i])
        conditions[[i]] <- inp$condition
        summ <- try(summarise(condition=inp$condition, results=inp$results,
                          fixed_objects=fixed_objects))
        if(is(summ, 'try-error'))
            stop(sprintf("File \'%s\' threw an error in the summarise() function", files[i]))
        res[[i]] <- try(sim_results_check(summ))
        if(is(res[[i]], 'try-error'))
            stop(sprintf("File \'%s\' did not return a valid summarise() output", files[i]))
        if(bootSE){
            SE_sim_results <- sapply(1L:boot_draws, function(r){
                pick <- rint(n = replications, min = 1L, max = replications)
                tmp <- if(!is.data.frame(inp$results)) inp$results[pick]
                else inp$results[pick, , drop=FALSE]
                summarise(results=tmp, condition=inp$condition, fixed_objects=fixed_objects)
            })
            if(!is.matrix(SE_sim_results)) SE_sim_results <- matrix(SE_sim_results, nrow=1L)
            SE_sim_results <- apply(SE_sim_results, 1L, sd)
            names(SE_sim_results) <- paste0("BOOT_SE.", names(res[[i]]))
            res[[i]] <- c(res[[i]], SE_sim_results)
        }
    }
    res <- cbind(plyr::rbind.fill(conditions), do.call(rbind, res))
    res$REPLICATION <- res$ID <- NULL
    dplyr::as_tibble(res)
}
