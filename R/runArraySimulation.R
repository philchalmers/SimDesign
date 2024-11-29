#' Run a Monte Carlo simulation using array job submissions per condition
#'
#' This function has the same purpose as \code{\link{runSimulation}}, however
#' rather than evaluating each row in a \code{design} object (potentially with
#' parallel computing architecture) this function evaluates the simulation
#' per independent row condition. This is mainly useful when distributing the
#' jobs to HPC clusters where a job array number is available (e.g., via SLURM),
#' where the simulation results must be saved to independent files as they
#' complete. Use of \code{\link{expandDesign}} is useful for distributing replications
#' to different jobs, while \code{\link{genSeeds}} is required to ensure high-quality
#' random number generation across the array submissions. See the associated
#' vignette for a brief tutorial of this setup.
#'
#' Due to the nature of how the replication are split it is important that
#' the L'Ecuyer-CMRG (2002) method of random seeds is used across all
#' array ID submissions (cf. \code{\link{runSimulation}}'s \code{parallel}
#' approach, which uses this method to distribute random seeds within
#' each isolated condition rather than between all conditions). As such, this
#' function requires the seeds to be generated using
#' \code{\link{genSeeds}} with the \code{iseed} and \code{arrayID}
#' inputs to ensure that each job is analyzing a high-quality
#' set of random numbers via L'Ecuyer-CMRG's (2002) method, incremented using
#' \code{\link[parallel]{nextRNGStream}}.
#'
#' Additionally, for timed simulations on HPC clusters it is also recommended to pass a
#' \code{control = list(max_time)} value to avoid discarding
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
#'   \code{filename}. For example, if
#'   \code{filename = 'mysim'} then files stored will be \code{'mysim-1.rds'},
#'   \code{'mysim-2.rds'}, and so on for each row ID in \code{design}
#'
#' @param dirname directory to save the files associated with \code{filename}
#'   to. If omitted the files will be stored in the same working directory
#'   where the script was submitted
#'
#' @param parallel logical; use parallel computations via the a "SOCK" cluster?
#'   Only useful when the instruction shell file requires more than 1 core
#'   (number of cores detected via \code{ncores}). For this application
#'   the random seeds further distributed using \code{\link[parallel]{nextRNGSubStream}}
#'
#' @param cl cluster definition. If omitted a "SOCK" cluster will be defined
#'
#' @param ncores number of cores to use when \code{parallel=TRUE}. Note that
#'   the default uses 1 minus the number of available cores, therefore this
#'   will only be useful when \code{ncores > 2} as defined in the shell instruction
#'   file
#'
#' @param iseed initial seed to be passed to \code{\link{genSeeds}}'s argument
#'   of the same name, along with the supplied \code{arrayID}
#'
#' @param addArrayInfo logical; should the array ID and original design row number
#'   be added to the \code{SimResults(...)} output?
#'
#' @param array2row user defined function with the single argument \code{arrayID}.
#'   Used to convert the detected \code{arrayID}
#'   into a suitable row index in the \code{design} object input. By default
#'   each \code{arrayID} is associated with its respective row in \code{design}.
#'
#'   For example, if each \code{arrayID} should evaluate 10 rows in
#'   the \code{design} object then the function
#'   \code{function(arrayID){1:10 + 10 * (arrayID-1)}} can be passed to \code{array2row}
#'
#' @param control control list passed to \code{\link{runSimulation}}.
#'   In addition to the original \code{control} elements two
#'   additional arguments have been added:
#'   \code{max_time} and \code{max_RAM}, both of which as specified as
#'   character vectors with one element.
#'
#'   \code{max_time} specifies the maximum time allowed for a
#'   single simulation condition to execute (default does not set
#'   any time limits), and is formatted according to the specification in
#'   \code{\link{timeFormater}}. This is primarily useful when the HPC cluster
#'   will time out after some known elapsed time.
#'   In general, this input should be set to somewhere around
#'   80-90% of the true termination time so that any evaluations completed
#'   before the cluster is terminated can be saved. Default applies no time limit
#'
#'   Similarly, \code{max_RAM} controls the
#'   (approximate) maximum size that the simulation storage objects can grow
#'   before RAM becomes an issue. This can be specified either in terms of megabytes
#'   (MB), gigabytes (GB), or terabytes (TB). For example, \code{max_RAM = "4GB"} indicates that
#'   if the simulation storage objects are larger than 4GB then the workflow
#'   will terminate early, returning only the successful results up to this point).
#'   Useful for larger HPC cluster jobs with RAM constraints that could terminate abruptly.
#'   As a rule of thumb this should be set to around 90% of the maximum possible storage
#'   available. Default applies no memory limit
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
#' @seealso \code{\link{runSimulation}}, \code{\link{expandDesign}},
#'   \code{\link{genSeeds}}, \code{\link{SimCheck}},
#'   \code{\link{SimCollect}}, \code{\link{getArrayID}}
#'
#' @examples
#'
#' library(SimDesign)
#'
#' Design <- createDesign(N = c(10, 20, 30))
#'
#' Generate <- function(condition, fixed_objects) {
#'     dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects) {
#'     ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects){
#'     colMeans(results)
#' }
#'
#' \dontrun{
#'
#' # define initial seed (do this only once to keep it constant!)
#' # iseed <- genSeeds()
#' iseed <- 554184288
#'
#' ### On cluster submission, the active array ID is obtained via getArrayID(),
#' ###   and therefore should be used in real SLURM submissions
#' arrayID <- getArrayID(type = 'slurm')
#'
#' # However, the following example arrayID is set to
#' #  the first row only for testing purposes
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
#' # same, but evaluated with multiple cores
#' res <- runArraySimulation(design=Design, replications=50,
#'                       generate=Generate, analyse=Analyse,
#'                       summarise=Summarise, arrayID=arrayID,
#'                       parallel=TRUE, ncores=3,
#'                       iseed=iseed, filename='myparsim')
#' res
#' SimResults(res) # condition and replication count stored
#'
#' dir()
#' SimClean(c('mysim-1.rds', 'myparsim-1.rds'))
#'
#' ########################
#' # Same submission job as above, however split the replications over multiple
#' # evaluations and combine when complete
#' Design5 <- expandDesign(Design, 5)
#' Design5
#'
#' # iseed <- genSeeds()
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
#' # Emulate the arrayID distribution, storing all results in a 'sim/' folder
#' # (if 'sim/' does not exist in runArraySimulation() it will be
#' # created automatically)
#' dir.create('sim/')
#'
#' # Emulate distribution to nrow(Design5) = 15 independent job arrays
#' ##  (just used for presentation purposes on local computer)
#' sapply(1:nrow(Design5), \(arrayID)
#'      runArraySimulation(design=Design5, replications=10,
#'           generate=Generate, analyse=Analyse,
#'           summarise=Summarise, iseed=iseed, arrayID=arrayID,
#'           filename='condition', dirname='sim', # files: "sim/condition-#.rds"
#'           control = list(max_time="04:00:00", max_RAM="4GB"))) |> invisible()
#'
#' #  If necessary, conditions above will manually terminate before
#' #  4 hours and 4GB of RAM are used, returning any
#' #  successfully completed results before the HPC session times
#' #  out (provided .slurm script specified more than 4 hours)
#'
#' # list saved files
#' dir('sim/')
#'
#' # check that all files saved (warnings will be raised if missing files)
#' SimCheck('sim/') |> isTRUE()
#'
#' condition14 <- readRDS('sim/condition-14.rds')
#' condition14
#' SimResults(condition14)
#'
#' # aggregate simulation results into single file
#' final <- SimCollect('sim/')
#' final
#'
#' # clean simulation directory
#' SimClean(dirs='sim/')
#'
#'
#' ############
#' # same as above, however passing different amounts of information depending
#' # on the array ID
#' array2row <- function(arrayID){
#'   switch(arrayID,
#'     "1"=1:8,
#'     "2"=9:14,
#'     "3"=15)
#' }
#'
#' # arrayID 1 does row 1 though 8, arrayID 2 does 9 to 14
#' array2row(1)
#' array2row(2)
#' array2row(3)  # arrayID 3 does 15 only
#'
#' # emulate remote array distribution with only 3 arrays
#' sapply(1:3, \(arrayID)
#'      runArraySimulation(design=Design5, replications=10,
#'           generate=Generate, analyse=Analyse,
#'           summarise=Summarise, iseed=iseed, arrayID=arrayID,
#'           filename='condition', dirname='sim', array2row=array2row)) |> invisible()
#'
#' # list saved files
#' dir('sim/')
#'
#' # note that all row conditions are still stored separately, though note that
#' #  arrayID is now 2 instead
#' condition14 <- readRDS('sim/condition-14.rds')
#' condition14
#' SimResults(condition14)
#'
#' # aggregate simulation results into single file
#' final <- SimCollect('sim/')
#' final
#'
#' # clean simulation directory
#' SimClean(dirs='sim/')
#'
#' }
#'
runArraySimulation <- function(design, ..., replications,
                               iseed, filename, dirname = NULL,
                               arrayID = getArrayID(),
                               array2row = function(arrayID) arrayID,
                               addArrayInfo = TRUE,
                               parallel = FALSE, cl = NULL,
                               ncores = parallelly::availableCores(omit = 1L),
                               save_details = list(),
                               control = list()){
    dots <- list(...)
    if(parallel && ncores == 1L) parallel <- FALSE
    if(!is.null(dots$save_results) && isTRUE(dots$save_results))
        stop('save_results not supported for array jobs. Please use store_results only')
    if(!is.null(control$save_seeds) && isTRUE(control$save_seeds))
        stop(c('save_seeds not supported for array jobs. If this is truely',
               ' necessary use store_Random.seeds instead'))
    control$from.runArraySimulation <- TRUE
    rngkind <- RNGkind()
    RNGkind("L'Ecuyer-CMRG")
    on.exit(RNGkind(rngkind[1L]))
    stopifnot(!missing(design))
    if(is.null(attr(design, 'Design.ID')))
        attr(design, 'Design.ID') <- 1L:nrow(design)
    stopifnot(!missing(iseed))
    stopifnot(!missing(filename))
    if(grepl('-', filename))
        stop('filename cannot contain the character \"-\" as this is used in the suffix')
    stopifnot(nrow(design) > 1L)
    stopifnot("arrayID is not a single integer identifier"=
                  length(arrayID) == 1L && is.numeric(arrayID) && !is.na(arrayID))
    rowpick <- array2row(arrayID)
    filename_suffix <- paste0("-", rowpick)
    stopifnot(!missing(replications))
    if(length(replications) > 1L)
        replications <- replications[rowpick]
    stopifnot(rowpick %in% 1L:nrow(design))
    if(!is.null(filename))
        filename <- paste0(filename, filename_suffix)
    if(!is.null(dirname)){
        if(!dir.exists(dirname))
            dir.create(dirname, showWarnings = FALSE)
        filename <- file.path(dirname, filename)
        filename <- gsub("//", "/", filename)
    }
    save_details$arrayID <- arrayID
    if(parallel){
        if(is.null(cl)){
            cl <- parallel::makeCluster(ncores, type="SOCK")
            on.exit(parallel::stopCluster(cl), add=TRUE)
        }
    }
    for(i in 1L:length(rowpick)){
        row <- rowpick[i]
        seed <- genSeeds(design, iseed=iseed, arrayID=row)
        dsub <- design[row, , drop=FALSE]
        attr(dsub, 'Design.ID') <- attr(design, 'Design.ID')[row]
        ret <- runSimulation(design=dsub, replications=replications, seed=seed,
                             verbose=FALSE, save_details=save_details,
                             parallel=parallel, cl=cl,
                             control=control, save=FALSE, ...)
        attr(ret, 'extra_info')$number_of_conditions <- nrow(design)
        if(addArrayInfo && (is.null(dots$store_results) ||
           (!is.null(dots$store_results) && isTRUE(dots$store_results)))){
            results <- SimExtract(ret, 'results')
            condition <- attr(design, 'Design.ID')
            if(is(results, 'tbl_df')){
                results <- dplyr::mutate(results, arrayID=arrayID, .before=1L)
                results <- dplyr::mutate(results, condition=condition[row], .before=1L)
            } else {
                results <- lapply(results,
                                  \(x) c(arrayID=arrayID, condition=condition[row], x))
                names(results) <- NULL
            }
            attr(ret, "extra_info")$stored_results <- results
        }
        filename.u <- unique_filename(filename[i], safe=TRUE, verbose=FALSE)
        saveRDS(ret, filename.u)
    }
    if(length(rowpick) > 1L) ret <- NULL
    invisible(ret)
}
