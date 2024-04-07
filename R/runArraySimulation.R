#' Run a Monte Carlo simulation using array job submissions per condition
#'
#' This function has the same purpose as \code{\link{runSimulation}}, however
#' rather than evaluating each row in a \code{design} object (potentially with
#' parallel computing architecture) this function evaluates the simulation
#' per independent row condition. This is mainly useful when distributing the
#' jobs to HPC clusters where a job array number is available (e.g., via SLURM),
#' where the simulation results must be saved to independent files as they
#' complete. Use of \code{\link{expandDesign}} is useful for distributing replications
#' to different jobs, while \code{\link{gen_seeds}} is required to ensure high-quality
#' random number generation across the array submissions. See the associated
#' vignette for a brief tutorial of this setup.
#'
#' Due to the nature of how the replication are split it is important that
#' the L'Ecuyer-CMRG (2002) method of random seeds is used across all
#' array ID submissions (cf. \code{\link{runSimulation}}'s \code{parallel}
#' approach, which uses this method to distribute random seeds within
#' each isolated condition rather than between all conditions). As such, this
#' function requires the seeds to be generated using
#' \code{\link{gen_seeds}} with the \code{iseed} and \code{arrayID}
#' inputs to ensure that each job is analyzing a high-quality
#' set of random numbers via L'Ecuyer-CMRG's (2002) method.
#'
#' Additionally, for timed simulations on HPC clusters it is also recommended to pass a
#' \code{control = list(max_time = number_of_hours)} to avoid discarding
#' conditions that require more than the specified time in the shell script.
#' The \code{max_time} value should be less than the maximum time allocated
#' on the HPC cluster (e.g., approximately 90% of this time or less, though
#' depends on how long each replication takes). Simulations with missing
#' replication information should submit a new set of jobs at a later time
#' to collect the missing replication information.
#'
#' @param design design object containing simulation conditions on a per row basis.
#'   This function is design to submit each row as in independent job on a HPC cluster.
#'   See \code{\link{runSimulation}} for further details
#'
#' @param replications number of independent replications to perform per
#'   condition (i.e., each row in \code{design}). See \code{\link{runSimulation}}
#'   for further details
#'
#' @param arrayID array identifier from the scheduler. Must be a number between
#'   1 and \code{nrow(design)}. If not specified then \code{\link{getArrayID}} will
#'   be called automatically, which assumes the environmental variables are available
#'   according the SLURM scheduler
#'
#' @param save_details optional list of extra file saving details.
#'   See \code{\link{runSimulation}}
#'
#' @param filename file name to save simulation files to (does not need to
#'   specify extension). However, the array ID will be appended to each
#'   \code{filename} (see \code{filename_suffix}). For example, if
#'   \code{filename = 'mysim'} then files stored will be \code{'mysim-1.rds'},
#'   \code{'mysim-2.rds'}, and so on for each row in \code{design}
#'
#' @param filename_suffix suffix to add to the \code{filename};
#'   default add '-' with the \code{arrayID}
#'
#' @param iseed initial seed to be passed to \code{\link{gen_seeds}}'s argument
#'   of the same name, along with the supplied \code{arrayID}
#'
#' @param addArrayInfo logical; should the array ID and original design row number
#'   be added to the \code{SimExtract(..., what='results')} output?
#'
#' @param ... additional arguments to be passed to \code{\link{runSimulation}}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
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
#'
#' @seealso \code{\link{runSimulation}}, \code{\link{expandDesign}},
#'   \code{\link{gen_seeds}}, \code{\link{aggregate_simulations}}, \code{\link{getArrayID}}
#'
#' @examples
#'
#' Design <- createDesign(N = c(10, 20, 30))
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'     dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'     ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     colMeans(results)
#' }
#'
#' \dontrun{
#'
#' # define initial seed (do this only once to keep it constant!)
#' # iseed <- gen_seeds()
#' iseed <- 554184288
#'
#' ### On cluster submission, the active array ID is obtained via getArrayID(),
#' ###   and therefore should be used in real SLURM submissions
#' arrayID <- getArrayID(type = 'slurm')
#'
#' # However, for the following example array ID is set to first row only
#' arrayID <- 1L
#'
#' # run the simulation (results not caught on job submission, only files saved)
#' res <- runArraySimulation(design=Design, replications=50,
#'                       generate=Generate, analyse=Analyse,
#'                       summarise=Summarise, arrayID=arrayID,
#'                       iseed=iseed, filename='mysim') # saved as 'mysim-1.rds'
#' res
#' SimResults(res) # condition and replication count stored
#'
#' dir()
#' SimClean('mysim-1.rds')
#'
#' ########################
#' # Same submission job as above, however split the replications over multiple
#' # evaluations and combine when complete
#' Design5 <- expandDesign(Design, 5)
#' Design5
#'
#' # iseed <- gen_seeds()
#' iseed <- 554184288
#'
#' # arrayID <- getArrayID(type = 'slurm')
#' arrayID <- 14L
#'
#' # run the simulation (replications reduced per row, but same in total)
#' runArraySimulation(design=Design5, replications=10,
#'                    generate=Generate, analyse=Analyse,
#'                    summarise=Summarise, iseed=iseed,
#'                    filename='mylongsim', arrayID=arrayID)
#'
#' res <- readRDS('mylongsim-14.rds')
#' res
#' SimResults(res) # condition and replication count stored
#'
#' SimClean('mylongsim-14.rds')
#'
#'
#' ###
#' # emulate the arrayID distribution, storing all results in a 'sim/' folder
#' dir.create('sim/')
#'
#' # Emulate distribution to nrow(Design5) = 15 independent job arrays
#' sapply(1:nrow(Design5), \(arrayID)
#'        runArraySimulation(design=Design5, replications=10,
#'              generate=Generate, analyse=Analyse,
#'              summarise=Summarise, iseed=iseed, arrayID=arrayID,
#'              filename='sim/condition',   # saved to 'sim/condition-#.rds'
#'              control = list(max_time = 4))) |> invisible()
#'
#' #  If necessary, conditions above will manually terminate before
#' #  4 hours, returning any successfully completed results before the HPC
#' #  session times out (provided shell specified more than 4 hours)
#'
#' # list saved files
#' dir('sim/')
#'
#' setwd('sim')
#' condition14 <- readRDS('condition-14.rds')
#' condition14
#' SimResults(condition14)
#'
#' # aggregate simulation results into single file
#' final <- aggregate_simulations(files=dir())
#' final
#'
#' SimResults(final) |> View()
#'
#' setwd('..')
#' SimClean(dirs='sim/')
#'
#' }
#'
runArraySimulation <- function(design, ..., replications,
                               iseed, filename, arrayID = getArrayID(),
                               filename_suffix = paste0("-", arrayID),
                               addArrayInfo = TRUE,
                               save_details = list()){
    dots <- list(...)
    if(!is.null(dots$save_results) && isTRUE(dots$save_results))
        stop('save_results not supported for array jobs. Please use store_results only')
    if(!is.null(dots$save_seeds) && isTRUE(dots$save_seeds))
        stop(c('save_seeds not supported for array jobs. If this is truely',
               ' necessary pass control = list(store_Random.seeds=TRUE) instead'))
    rngkind <- RNGkind()
    RNGkind("L'Ecuyer-CMRG")
    on.exit(RNGkind(rngkind[1L]))
    stopifnot(!missing(design))
    if(is.null(attr(design, 'condition')))
        attr(design, 'condition') <- 1L:nrow(design)
    stopifnot(!missing(iseed))
    stopifnot(!missing(filename))
    stopifnot(nrow(design) > 1L)
    stopifnot(!missing(replications))
    if(length(replications) == 1L)
        replications <- rep(replications, nrow(design))
    stopifnot(length(replications) == nrow(design))
    stopifnot("arrayID is not a single integer identifier"=
                  length(arrayID) == 1L && is.numeric(arrayID) && !is.na(arrayID))
    stopifnot(arrayID %in% 1L:nrow(design))
    if(!is.null(filename))
        filename <- paste0(filename, filename_suffix)
    save_details$arrayID <- arrayID
    seed <- gen_seeds(design, iseed=iseed, arrayID=arrayID)

    ret <- runSimulation(design=design[arrayID, , drop=FALSE],
                         replications=replications[arrayID],
                         filename=filename, seed=seed,
                         verbose=FALSE, save_details=save_details, ...)
    if(addArrayInfo && (is.null(dots$store_results) ||
       (!is.null(dots$store_results) && isTRUE(dots$store_results)))){
        results <- SimExtract(ret, 'results')
        condition <- attr(design, 'condition')
        results <- dplyr::mutate(results, arrayID=arrayID, .before=1L)
        results <- dplyr::mutate(results, condition=condition[arrayID], .before=1L)
        attr(ret, "extra_info")$stored_results <- results
        saveRDS(ret, paste0(filename, '.rds'))
    }
    invisible(ret)
}
