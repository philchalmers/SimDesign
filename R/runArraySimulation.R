#' Run a Monte Carlo simulation using array job submissions per condition
#'
#' This function has the same purpose as \code{\link{runSimulation}}, however
#' rather than evaluating each row in a \code{design} object (potentially with
#' parallel computing architecture) this function evaluates the simulation
#' per independent row condition. This is mainly useful when distributing the
#' jobs to HPC clusters where a job array number is available (e.g., via SLURM).
#' Use of \code{\link{expandDesign}} is useful for distributing replications
#' to different jobs.
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
#' @param filename optional filename to save simulation to (does not need to
#'   specify extension). See \code{\link{runSimulation}} for further details
#'
#' @param filename_suffix suffix to add to the filename; default add '-' with the
#'   \cpde{arrayID}
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
#'   \code{\link{aggregate_simulations}}, \code{\link{getArrayID}}
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
#' ### On cluster submission, the active array ID is obtained via getArrayID(),
#' ###   and therefore should be used in real SLURM submissions
#' arrayID <- getArrayID(type = 'slurm')
#'
#' # However, for the following example array ID is set to first row only
#' arrayID <- 1L
#'
#' # run the simulation
#' res <- runArraySimulation(design=Design, replications=50,
#'                    generate=Generate, analyse=Analyse,
#'                    summarise=Summarise, arrayID=arrayID,
#'                    filename='mysim') # saved as 'mysim-1.rds'
#' res
#'
#'
#' ########################
#' # Same submission job as above, however split the replications over multiple
#' # evaluations and combine when complete
#' Design5 <- expandDesign(Design, 5)
#' Design5
#'
#' # arrayID <- getArrayID(type = 'slurm')
#' arrayID <- 1L
#'
#' # run the simulation (replications reduced per row, but same in total)
#' res <- runArraySimulation(design=Design5, replications=10,
#'                    generate=Generate, analyse=Analyse,
#'                    summarise=Summarise, arrayID=arrayID)
#' res
#'
#' # emulate the arrayID distribution, storing all results in a 'sim/' folder
#' dir.create('sim/')
#'
#' # emulate distribution to nrow(Design5) = 15 independent job arrays
#' sapply(1:nrow(Design5), \(arrayID)
#'    runArraySimulation(design=Design5, replications=10,
#'           generate=Generate, analyse=Analyse,
#'           summarise=Summarise, arrayID=arrayID,
#'           filename='sim/condition')) # saved as 'sim/condition-#.rds'
#'
#' # list saved files
#' dir('sim/')
#'
#' # aggregate simulation results into single file
#' setwd('sim')
#' result <- aggregate_simulations(files=dir())
#' result
#'
#' # setwd("../")
#' # SimClean(dirs='sim/')
#'
#' }
#'
runArraySimulation <- function(design, ..., replications, arrayID,
                               filename = NULL,
                               filename_suffix = paste0("-", arrayID),
                               save_details = list()){
    stopifnot(!missing(design))
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
                         filename=filename,
                         verbose=FALSE, save_details=save_details, ...)
    ret
}
