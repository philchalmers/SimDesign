#' Run a Monte Carlo simulation using array job submissions per condition
#'
#' This function has the same purpose as \code{\link{runSimulation}}, however
#' rather than evaluating each row in a \code{design} object (potentially with
#' parallel computing architecture) this function evaluates the simulation
#' per independent row condition. This is mainly useful when distributing the
#' jobs to HPC clusters where a job array number is available (e.g., via SLURM),
#' where the simulation results must be saved to independent files as they
#' complete. Use of \code{\link{expandDesign}} is useful for distributing replications
#' to different jobs.
#'
#' Due to the nature of how the replication are split it is important that
#' the L'Ecuyer-CMRG (2002) method of random seeds is used across all
#' array ID submissions (cf. \code{\link{runSimulation}}'s \code{parallel}
#' approach, which uses this method to distribute random seeds within
#' each isolated condition rather than between all conditions). As such, this
#' function requires a \code{list} of seeds to be generated using
#' \code{\link{gen_seeds}} with the \code{type = "L'Ecuyer-CMRG"} method to ensure
#' that each job is analyzing a high-quality set of random numbers.
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
#'   1 and \code{nrow(design)}
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
#' @param seeds list of seeds to use from the L'Ecuyer-CMRG method. This
#'   is constructed from \code{\link{gen_seeds}} with
#'   \code{type = "L'Ecuyer-CMRG"}. The length of this input must equal the
#'   number of rows in \code{design}
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
#' # generate unique seed for each condition to be distributed
#' (CMRG.seed <- gen_seeds(1L))
#' seeds <- gen_seeds(Design, CMRG.seed=CMRG.seed)
#' str(seeds)
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
#'                       seeds=seeds, filename='mysim') # saved as 'mysim-1.rds'
#' res
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
#' # generate unique seed for each condition to be distributed
#' (CMRG.seed <- gen_seeds(1L))
#' seeds <- gen_seeds(Design5, CMRG.seed=CMRG.seed)
#'
#' # arrayID <- getArrayID(type = 'slurm')
#' arrayID <- 1L
#'
#' # run the simulation (replications reduced per row, but same in total)
#' runArraySimulation(design=Design5, replications=10,
#'                    generate=Generate, analyse=Analyse,
#'                    summarise=Summarise, seeds=seeds,
#'                    filename='mylongsim', arrayID=arrayID)
#'
#' res <- readRDS('mylongsim-1.rds')
#' res
#'
#' SimClean('mylongsim-1.rds')
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
#'              summarise=Summarise, seeds=seeds, arrayID=arrayID,
#'              filename='sim/condition',   # saved to 'sim/condition-#.rds'
#'              control = list(max_time = 4))) |> invisible()
#'
#' #  If necessary, conditions above would manually terminate before
#' #  4 hours, returning any successfully completed results before the HPC
#' #  session times out (provided shell specified more than 4 hours)
#'
#' # list saved files
#' dir('sim/')
#'
#' # aggregate simulation results into single file
#' setwd('sim')
#' result <- aggregate_simulations(files=dir())
#' result
#'
#' setwd('..')
#' SimClean(dirs='sim/')
#'
#' }
#'
runArraySimulation <- function(design, ..., replications, arrayID,
                               seeds, filename,
                               filename_suffix = paste0("-", arrayID),
                               save_details = list()){
    rngkind <- RNGkind()
    RNGkind("L'Ecuyer-CMRG")
    on.exit(RNGkind(rngkind[1L]))
    stopifnot(!missing(design))
    stopifnot(!missing(seeds))
    stopifnot(is.list(seeds))
    stopifnot(!missing(filename))
    stopifnot(nrow(design) > 1L)
    stopifnot(!missing(replications))
    if(length(replications) == 1L)
        replications <- rep(replications, nrow(design))
    stopifnot(length(replications) == nrow(design))
    stopifnot(length(arrayID) == 1L && is.numeric(arrayID))
    stopifnot(arrayID %in% 1L:nrow(design))
    if(!is.null(filename))
        filename <- paste0(filename, filename_suffix)
    save_details$arrayID <- arrayID

    ret <- runSimulation(design=design[arrayID, , drop=FALSE],
                         replications=replications[arrayID],
                         filename=filename, seed=seeds[arrayID],
                         verbose=FALSE, save_details=save_details, ...)
    invisible(ret)
}
