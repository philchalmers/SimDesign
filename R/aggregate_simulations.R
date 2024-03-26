#' Collapse separate simulation files into a single result
#'
#' This function aggregates the results from SimDesign's \code{\link{runSimulation}} into a single
#' objects suitable for post-analyses, or combines all the saved results directories and combines
#' them into one. This is useful when results are run piecewise on one node (e.g., 500 replications
#' in one batch, 500 again at a later date) or run independently across different
#' nodes/computers that are not on the same network.
#'
#' @param files a \code{character} vector containing the names of the simulation's final \code{.rds} files
#'
#' @param filename (optional) name of .rds file to save aggregate simulation file to. If not specified
#'   then the results will only be returned in the R console
#'
#' @param dirs a \code{character} vector containing the names of the \code{save_results} directories to be
#'   aggregated. A new folder will be created and placed in the \code{results_dirname} output folder
#'
#' @param results_dirname the new directory to place the aggregated results files
#'
#' @return if \code{files} is used the function returns a \code{data.frame/tibble} with the (weighted) average
#'   of the simulation results. Otherwise, if \code{dirs} is used, the function returns NULL
#'
#' @aliases aggregate_simulations
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
#' @seealso \code{\link{runSimulation}}
#'
#' @export aggregate_simulations
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' setwd('my_working_directory')
#'
#' ## run simulations to save the .rds files (or move them to the working directory)
#' # ret1 <- runSimulation(..., filename='file1')
#' # ret2 <- runSimulation(..., filename='file2')
#'
#' # saves to the hard-drive and stores in workspace
#' final <- aggregate_simulations(files = c('file1.rds', 'file2.rds'))
#' final
#'
#' # If filename not included, can be extracted from results
#' # files <- c(SimExtract(ret1, 'filename'), SimExtract(ret2, 'filename'))
#' # final <- aggregate_simulations(files = files)
#'
#' # aggregate saved results for .rds files and results directories
#' # runSimulation(..., save_results = TRUE, save_details = list(save_results_dirname = 'dir1'))
#' # runSimulation(..., save_results = TRUE, save_details = list(save_results_dirname = 'dir2'))
#'
#' # place new saved results in 'SimDesign_results/' by default
#' aggregate_simulations(files = c('file1.rds', 'file2.rds'),
#'                       filename='aggreged_sim.rds',
#'                       dirs = c('dir1', 'dir2'))
#'
#' # If dirnames not included, can be extracted from results
#' # dirs <- c(SimExtract(ret1, 'save_results_dirname'),
#'             SimExtract(ret2, 'save_results_dirname'))
#' # aggregate_simulations(dirs = dirs)
#'
#' #################################################
#' # Example where simulation run in multiple batches
#'
#' Design <- createDesign(N  = c(30, 60),
#'                        mu = c(0,5))
#'
#' # want 1000 replications in total, but execute two batches
#' replications <- 1000 / 2
#'
#' #-------------------------------------------------------------------
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'     dat <- with(condition, rnorm(N, mean=mu))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'     ret <- c(mean=mean(dat), SD=sd(dat))
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
#'     ret <- colMeans(results)
#'     ret
#' }
#'
#' #-------------------------------------------------------------------
#'
#' # Generate fixed seeds to be distributed (two sets of seeds)
#' set.seed(4321)
#' seeds <- gen_seeds(Design, nsets=2)
#' seeds
#'
#' # distribute both jobs independently (total reps / 2 )
#' runSimulation(design=Design, replications = 1000 / 2,
#'                  generate=Generate, analyse=Analyse, summarise=Summarise,
#'                  filename='job-1', seed=seeds[,1])
#'
#' runSimulation(design=Design, replications = 1000 / 2,
#'                  generate=Generate, analyse=Analyse, summarise=Summarise,
#'                  filename='job-2', seed=seeds[,2])
#'
#' # aggregate into single object, and save to drive as 'collected.rds'
#' sim <- aggregate_simulations(files = c('job-1.rds', 'job-2.rds'),
#'                              filename='collected.rds')
#' sim
#' file.exists('collected.rds')
#'
#'
#' #################################################
#' # Example where each row condition is repeated, evaluated independently,
#' # and later collapsed into a single analysis object
#'
#' # Each condition repeated four times (hence, replications
#' # should be set to desired.reps/4)
#' Design <- createDesign(N  = c(30, 60),
#'                        mu = c(0,5),
#'                        repeat_conditions = 4L)
#'
#' #-------------------------------------------------------------------
#'
#' # Generate-Analyse-Summarise same as previous example
#'
#' #-------------------------------------------------------------------
#'
#' # Generate fixed seeds to be distributed
#' set.seed(1234)
#' seeds <- gen_seeds(Design)
#' seeds
#'
#' # replications vector (constant is fine if the same across conditions;
#' # below is vectorized to demonstrate that this could change)
#' replications <- rep(250, nrow(Design))
#'
#' # create directory to store all final simulation files
#' dir.create('sim_files/')
#'
#' # distribute jobs independently (explicitly parallelize here on cluster)
#' sapply(1:nrow(Design), \(i) {
#'   runSimulation(design=Design[i, ], replications=replications[i],
#'                 generate=Generate, analyse=Analyse, summarise=Summarise,
#'                 filename=paste0('sim_files/job-', i))
#' })
#'
#' # aggregate into single object
#' sim <- aggregate_simulations(files = paste0('sim_files/job-',
#'                                      1:nrow(Design), ".rds"))
#' sim
#'
#' }
aggregate_simulations <- function(files = NULL, filename = NULL,
                                  dirs = NULL, results_dirname = 'SimDesign_aggregate_results'){
    oldfiles <- files
    if(!is.null(dirs)){
        if(!all(sapply(dirs, dir.exists))) stop('One or more directories not found')
        files <- lapply(dirs, function(x) dir(x))
        if(!all(sapply(files, function(x) all(x == files[[1L]]))))
            stop('File names are not all the same')
        files <- files[[1L]]
        ndirs <- length(dirs)
        if(dir.exists(results_dirname))
            stop(sprintf('Directory \'%s/\' already exists. Please fix', results_dirname),
                 call.=FALSE)
        dir.create(results_dirname)
        message(sprintf('Writing aggregate results folders to \"%s\"', results_dirname))
        for(f in files){
            readin <- lapply(1:ndirs, function(x) readRDS(paste0(dirs[x], '/', f)))
            ret <- readin[[1L]]
            collapse <- !is.list(ret$results) || is.data.frame(ret$results)
            results <- lapply(readin, function(x) x$results)
            ret$results <- do.call(if(collapse) rbind else c, results)
            tmp <- do.call(c, lapply(readin, function(x) x$warnings))
            nms <- names(tmp)
            if(length(nms))
                ret$warnings <- table(do.call(c, lapply(1:length(nms),
                                                        function(x) rep(nms[x], each = tmp[x]))))
            tmp <- do.call(c, lapply(readin, function(x) x$errors))
            nms <- names(tmp)
            if(length(nms))
                ret$errors <- table(do.call(c, lapply(1:length(nms),
                                                      function(x) rep(nms[x], each = tmp[x]))))
            saveRDS(ret, paste0(results_dirname, '/', f))
        }
    }
    files <- oldfiles
    if(!is.null(files)){
        filenames <- files
    } else {
        return(invisible(NULL))
    }
    if(!is.null(filename))
        if(filename %in% dir())
            stop(sprintf('File \'%s\' already exists in working directory', filename),
                 call.=FALSE)
    readin <- vector('list', length(filenames))
    for(i in 1:length(filenames))
        readin[[i]] <- readRDS(filenames[i])
    errors <- lapply(readin, function(x) x[ ,grepl('ERROR', colnames(x)), drop=FALSE])
    nms <- unique(do.call(c, lapply(errors, function(x) colnames(x))))
    try_errors <- as.data.frame(matrix(0, nrow(readin[[1L]]), length(nms)))
    names(try_errors) <- nms
    readin <- lapply(readin, function(x) x[ ,!grepl('ERROR', colnames(x)), drop=FALSE])
    if(length(unique(sapply(readin, ncol))) > 1L)
        stop('Number of columns in the replications not equal')
    designs <- lapply(readin, \(x) SimExtract(x, 'Design'))

    identical_set <- integer(0)
    set.count <- 1L
    set.index <- rep(NA, length(designs))
    while(TRUE){
        left <- setdiff(1L:length(designs), identical_set)
        pick_design <- designs[[min(left)]]
        matched <- which(sapply(designs, \(x) all(x == pick_design)))
        set.index[matched] <- rep(set.count, length(matched))
        set.count <- set.count + 1L
        identical_set <- c(identical_set, matched)
        if(all(!is.na(set.index))) break
        if(set.count > length(designs)) # while(sanity_check)
            stop('while() loop counter is broken (contact package maintainer for fix)')
    }
    unique.set.index <- unique(set.index)
    full_out <- vector('list', length(unique.set.index))
    readin.old <- readin
    for(j in unique.set.index){
        readin <- readin.old[which(j == set.index)]
        ret <- readin[[1L]]
        pick <- sapply(readin[[1L]], is.numeric)
        ret[, pick] <- 0
        pick <- pick & !(colnames(readin[[1L]]) %in% c('SIM_TIME', 'REPLICATIONS', 'SEED'))
        weights <- sapply(readin, function(x) x$REPLICATIONS[1L])
        stopifnot(all(sapply(readin, function(x) length(unique(x$REPLICATIONS)) == 1L)))
        weights <- weights / sum(weights)
        has_stored_results <- !is.null(SimExtract(ret, 'results'))
        for(i in seq_len(length(readin))){
            tmp <- stats::na.omit(match(nms, names(errors[[i]])))
            if(length(tmp) > 0L){
                try_errors[,match(nms, names(try_errors))] <- errors[[i]][ ,tmp] +
                    try_errors[,match(nms, names(try_errors))]
            }
            ret$REPLICATIONS <- ret$REPLICATIONS + readin[[i]]$REPLICATIONS
            ret$SIM_TIME <- ret$SIM_TIME + readin[[i]]$SIM_TIME
            ret[ ,pick] <- ret[ ,pick] + weights[i] * readin[[i]][ ,pick]
            if(has_stored_results & i > 1L)
                attr(ret, 'extra_info')$stored_results <-
                rbind(attr(ret, 'extra_info')$stored_results,
                      attr(readin[[i]], 'extra_info')$stored_results)
        }
        if(has_stored_results)
            results <- attr(ret, 'extra_info')$stored_results
        out <- dplyr::as_tibble(data.frame(ret, try_errors, check.names = FALSE))
        out$SEED <- NULL
        if(has_stored_results)
            attr(out, 'extra_info') <- list(stored_results=results)
        full_out[[j]] <- out
    }
    out <- if(length(unique.set.index) == 1L){
        full_out[[1L]]
    } else {
        do.call(rbind, full_out)
    }
    class(out) <- c('SimDesign', class(out))
    if(!is.null(filename)){
        message(sprintf('Writing combinded file from %i simulations to \"%s\"',
                        length(filenames), filename))
        saveRDS(out, filename)
    }
    if(length(unique(out$REPLICATIONS)) != 1L)
        warning("Simulation results do not contain the same number of REPLICATIONS")
    invisible(out)
}
