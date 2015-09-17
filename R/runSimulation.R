#' Run a Monte Carlo simulation given a data.frame of conditions and simulation functions
#'
#' This function runs a Monte Carlo simulation study given the simulation functions, the design conditions,
#' and the number of replications. Results can be saved as temporary files in case of interruptions
#' and may be restored by rerunning the exact function calls again, provided that the respective temp
#' file can be found in the working directory. Supports parallel and cluster computing, global and
#' local debugging, and is designed to be cross-platform.  For a skeleton version of the workflow
#' which may be useful when initially defining a simulation, see \code{\link{SimDesign_functions}}.
#'
#' The strategy for organizing the Monte Carlo simulation work-flow is to
#'
#' \describe{
#'    \item{1)}{Define a suitable \code{design} data.frame. This is often expedited by using the
#'       \code{\link{expand.grid}} function}
#'    \item{2)}{Define the three step functions to simulate the data (\code{\link{generate}}),
#'       analyse the generated data by computing the respective parameter estimates, detection rates,
#'       etc (\code{\link{analyse}}), and finally summarise the results across the total
#'       number of replications (\code{\link{summarise}})
#'    }
#'    \item{3)}{Pass the above objects to the \code{runSimulation} function, and define the
#'       number of replications with the \code{replications} input}
#'    \item{4)}{Analyze the output from \code{runSimulation}, possibly using techniques from ANOVA
#'      and generating suitable plots}
#' }
#'
#' Two constants for each condition are returned by default:
#' N_CELL_RUNS to indicate the number of Monte Carlo runs it took to obtain valid results (this will
#' be greater than the number of replications requested if, for example, models failed to converge
#' and had to be re-drawn), and SIM_TIME to indicate how long (in seconds) it took to complete
#' all the Monte Carlo replications for each respective condition.
#'
#' In the event of a computer crash, power outage, etc, if the 'save_every' command has been
#' used to save temporary results then the original code in main.R need only be rerun again.
#' The temp file will be read into the design, and the simulation will continue where it left
#' off before the simulation was terminated. Upon completion, a data.frame with the simulation
#' will be returned in the R session and a '.rds' file will be saved to the hard-drive (with the
#' file name corresponding to the 'filename' argument below).
#'
#' @section Cluster computing:
#'
#' If the package is installed across a cluster of computers, and all the computers are accessible on
#' the same LAN network, then the package may be run in cluster mode using the MPI paradigm. This simply
#' requires that the computers be setup using the usual MPI requirements (typically, running some flavor
#' of Linux, have password-less openSSH access, addresses have been added to the /etc/hosts file, etc),
#' and in the code the argument \code{MPI = TRUE} should be passed.
#'
#' For instances, if the following code is run on the master node through a terminal then 16 processes
#' will be summoned (1 master, 15 slaves) across the computers named localhost, slave1, and slave2.
#'
#' mpirun -np 16 -H localhost,slave1,slave2 R --slave -f simulation.R
#'
#' @section Poor man's cluster computing:
#'
#' In the event that you do not have access to a Beowulf-type cluster, but have multiple personal
#' computers, then the simulation code can be manually distributed across each computer instead.
#' This simply requires passing a smaller value to the \code{each} argument on each computer, and later
#' aggregating the results using the \code{\link{aggregate_simulations}} function.
#'
#' For instance, if you have two computers available and wanted 500 replications you
#' could pass \code{replications = 300} to one computer and \code{replications = 200} to the other.
#' This will create two .rds files which can be combined later with the
#' \code{\link{aggregate_simulations}} function.
#'
#' @param design a data.frame object containing the Monte carlo simulation conditions to
#'   be studied, where each row represents a unique condition
#'
#' @param generate user-defined data and parameter generating function.
#'   See \code{\link{generate}} for details
#'
#' @param analyse user-defined computation function which acts on the dat generated from
#'   \code{\link{generate}}. See \code{\link{analyse}} for details
#'
#' @param summarise user-defined summary function to be used after all the replications have completed.
#'    See \code{\link{summarise}} for details
#'
#' @param main (optional) user-defined organization function defining how the simulation should be
#'    organized. When NULL, the internal function definition is used (and the majority of the time
#'    this is sufficient). See \code{\link{main}} for further details
#'
#' @param replications number of replication to perform per condition (i.e., each row in \code{design})
#'
#' @param parallel logical; use parallel processing from the `parallel` package over each
#'   unique condition?
#'
#' @param save_every a number indicating how often to temporarily save your simulation results to
#'   disk. Default is 1 to save after every condition is complete, but set to NA if you don't
#'   want to save any temp files
#'
#' @param ncores number of cores to be used in parallel execution. Default uses all available
#'
#' @param clean logical; remove any temp files that are created after the simulation is complete?
#'   Default is TRUE
#'
#' @param filename the name of the file to save the results to. Default is the system name with
#'   the number of replications and 'Final' appended to the string
#'
#' @param tmpfilename the name of the temporary file, default is the system name with 'tmpsim.rds'
#'   appended at the end. This file will be
#'   read in if it is in the working directory, and the simulation will continue where at the last
#'   point this file was saved (useful in case of power outages or broken nodes).
#'   This file will be deleted when the simulation is complete
#'
#' @param MPI logical; use the \code{doMPI} package to run simulation in parallel on
#'   a cluster? Default is FALSE
#'
#' @param save logical; save the final simulation and temp files to the hard-drive? Default is FALSE
#'
#' @param compname name of the computer running the simulation. Normally this doesn't need to be modified,
#'   but in the event that a node breaks down while running a simulation the results from the tmp files
#'   may be resumed on another computer by changing the name of the node to match the broken computer
#'
#' @param edit a string indicating where to initiate a `browser()` call for editing and debugging.
#'   Options are 'none' (default), 'main' to edit the main function calls loop, 'generate' to edit the
#'   data simulation function, 'analyse' to edit the computational function, and 'summarise' to
#'   edit the collection function. Alternatively, users may place \code{\link{browser}} calls within their
#'   own code for debugging at specific lines (note: parallel computation flags will
#'   automatically be disabled when this is detected)
#'
#' @param verbose logical; print messages to the R console?
#'
#' @aliases runSimulation
#'
#' @seealso \code{\link{generate}}, \code{\link{analyse}}, \code{\link{summarise}},
#'   \code{\link{SimDesign_functions}}
#'
#' @export runSimulation
#'
#' @examples
#'
#' \dontrun{
#'
#' #### Step 1 --- Define your conditions under study and create design data.frame
#'
#' # helpful to use the following skeleton version
#' SimDesign_functions()
#'
#' # (use EXPLICIT names, avoid things like N <- 100. That's fine in functions, not here)
#' sample_sizes <- c(10, 20, 50, 100)
#' standard_deviations <- c(1, 4, 8)
#'
#' Design <- expand.grid(sample_sizes_group1=sample_sizes,
#'                       sample_sizes_group2=sample_sizes,
#'                       standard_deviations=standard_deviations)
#' dim(Design)
#' head(Design)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define sim, compute, and collect functions
#'
#' # skeleton functions to be edited
#' SimDesign_functions()
#'
#' # help(generate)
#' Generate <- function(condition){
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
#' # help(analyse)
#'
#' Analyse <- function(simlist, condition){
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
#' # help(summarise)
#'
#' Summarise <- function(results, parameters, condition){
#'
#'     # handy functions
#'     bias <- function(observed, population) mean(observed - population)
#'     RMSD <- function(observed, population) sqrt(mean((observed - population)^2))
#'
#'     # silly test for bias and RMSD of a random number from 0
#'     pop_value <- 0
#'     bias.random_number <- bias(sapply(parameters, function(x) x$random_number), pop_value)
#'     RMSD.random_number <- RMSD(sapply(parameters, function(x) x$random_number), pop_value)
#'
#'     #find results of interest here (alpha < .1, .05, .01)
#'     nms <- c('welch', 'independent')
#'     lessthan.10 <- colMeans(results[,nms] < .10)
#'     lessthan.05 <- colMeans(results[,nms] < .05)
#'     lessthan.01 <- colMeans(results[,nms] < .01)
#'
#'     # return the results that will be appended to the design input
#'     ret <- c(bias.random_number=bias.random_number,
#'              RMSD.random_number=RMSD.random_number,
#'              lessthan.10=lessthan.10,
#'              lessthan.05=lessthan.05,
#'              lessthan.01=lessthan.01)
#'     return(ret)
#' }
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Collect results by looping over the rows in design
#'
#' # this simulation does not save temp files or the final result to disk (save=FALSE)
#' Final <- runSimulation(design=Design, replications=1000, parallel=TRUE,
#'                        generate=Generate, analyse=Analyse, summerise=Summarise)
#'
#' ## Debug the generate function (not run). See ?browser for help on debugging
#' # runSimulation(design=Design, replications=1000,
#' #               generate=Generate, analyse=Analyse, summerise=Summarise,
#' #               parallel=TRUE, edit='generate')
#'
#'
#'
#'
#' ## EXTRA: To run the simulation on a MPI cluster, use the following setup on each node (not run)
#' # library(doMPI)
#' # cl <- startMPIcluster()
#' # registerDoMPI(cl)
#' # Final <- runSimulation(design=Design, replications=1000, MPI=TRUE,
#' #                        generate=Generate, analyse=Analyse, summerise=Summarise)
#' # closeCluster(cl)
#' # mpi.quit()
#'
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' # Step 4 --- Post-analysis: Create a new R file for analyzing the Final data.frame with R based
#' # regression stuff, so use the lm() function to find main effects, interactions, plots, etc.
#' # This is where you get to be a data analyst!
#' head(Final)
#' # View(Final)
#'
#' psych::describe(Final)
#' psych::describeBy(Final, group = Final$standard_deviations)
#'
#' # define new variables, if they are helpful
#' Final$SSratio <- with(Final, sample_sizes_group1 / sample_sizes_group2)
#'
#' # make numerics into factors for lm() analyses
#' Final$fSSratio <- with(Final, factor(SSratio))
#' Final$fSD <- with(Final, factor(standard_deviations))
#'
#' #lm analysis (might want to change DV to a logit for better stability)
#' mod <- lm(lessthan.05.welch ~ fSSratio * fSD, Final)
#' car::Anova(mod)
#'
#' mod2 <- lm(lessthan.05.independent ~ fSSratio * fSD, Final)
#' car::Anova(mod2)
#'
#' library(effects)
#' plot(allEffects(mod2))
#'
#' # make some plots
#' library(ggplot2)
#' library(reshape2)
#' welch_ind <- Final[,c('SSratio', "standard_deviations",
#'     "lessthan.05.welch", "lessthan.05.independent")]
#' dd <- melt(welch_ind, id.vars = names(welch_ind)[1:2])
#'
#' ggplot(dd, aes(factor(SSratio), value)) +
#'     geom_abline(intercept=0.05, slope=0, col = 'red') +
#'     geom_abline(intercept=0.075, slope=0, col = 'red', linetype='dotted') +
#'     geom_abline(intercept=0.025, slope=0, col = 'red', linetype='dotted') +
#'     geom_boxplot() + facet_wrap(~variable)
#' ggplot(dd, aes(factor(SSratio), value, fill = factor(standard_deviations))) +
#'     geom_abline(intercept=0.05, slope=0, col = 'red') +
#'     geom_abline(intercept=0.075, slope=0, col = 'red', linetype='dotted') +
#'     geom_abline(intercept=0.025, slope=0, col = 'red', linetype='dotted') +
#'     geom_boxplot() + facet_grid(standard_deviations~variable)
#'
#'
#' }
#'
runSimulation <- function(design, replications, generate, analyse, summarise,
                          parallel = FALSE, MPI = FALSE,
                          save = TRUE, save_every = 1, clean = TRUE,
                          compname = Sys.info()['nodename'],
                          filename = paste0(compname,'_Final_', replications, '.rds'),
                          tmpfilename = paste0(compname, '_tmpsim.rds'), main = NULL,
                          ncores = parallel::detectCores(), edit = 'none', verbose = TRUE)
{
    stopifnot(!missing(generate) || !missing(analyse) || !missing(summarise))
    Functions <- list(generate=generate, analyse=analyse, summarise=summarise, main=main)
    stopifnot(!missing(design))
    stopifnot(!missing(replications))
    FunNames <- names(Functions)
    if(is.null(main)) Functions$main <- SimDesign::main
    for(i in names(Functions)){
        fms <- names(formals(Functions[[i]]))
        truefms <- switch(i,
                          main = c('index', 'condition', 'generate', 'analyse'),
                          generate  = c('condition'),
                          analyse = c('simlist', 'condition'),
                          summarise = c('results', 'parameters', 'condition'))
        if(!all(truefms %in% fms))
            stop(paste0('Function arguments for ', i, ' are not correct.'), call. = FALSE)
    }
    if(MPI){
        parallel <- FALSE
        verbose <- FALSE
    }
    for(i in 1L:length(Functions)){
        tmp <- deparse(substitute(Functions[[i]]))
        if(any(grepl('browser\\(', tmp))) parallel <- MPI <- FALSE
    }
    if(!is.data.frame(design))
        stop('design must be a data.frame object', call. = FALSE)
    if(replications < 2L)
        stop('number of replications must be greater than or equal to 2', call. = FALSE)
    if(!is.na(save_every))
        if(save_every > nrow(design))
            warning('save_every is too large to be useful', call. = FALSE)
    if(!(edit %in% c('none', 'sim', 'compute', 'main', 'collect')))
        stop('edit location is not valid', call. = FALSE)

    if(is.null(design$ID))
        design <- data.frame(ID=1L:nrow(design), design)

    if(edit != 'none'){
        parallel <- MPI <- FALSE
        debug(Functions[[edit]])
        on.exit(undebug(Functions[[edit]]))
    }

    cl <- NULL
    if(parallel) cl <- parallel::makeCluster(ncores)

    start <- 1L
    Result_list <- vector('list', nrow(design))
    names(Result_list) <- rownames(design)
    time0 <- time1 <- proc.time()[3]
    files <- dir()
    if(!MPI && any(files == tmpfilename)){
        if(verbose)
            message(paste('Resuming simulation from the temporary results found in:', tmpfilename))
        Result_list <- readRDS(tmpfilename)
        start <- min(which(sapply(Result_list, is.null)))
    }
    for(i in start:nrow(design)){
        stored_time <- do.call(c, lapply(Result_list, function(x) x$SIM_TIME))
        if(verbose)
            cat(sprintf('\rCompleted: %i%s,   Previous cell time: %.1f,  Total elapsed time: %.1f ',
                        round((i-1)/(nrow(design))*100), '%', time1 - time0, sum(stored_time)))
        time0 <- proc.time()[3]
        Result_list[[i]] <- as.data.frame(c(as.list(design[i, ]),
                                            as.list(Analysis(Functions=Functions,
                                                             condition=design[i,],
                                                             replications=replications,
                                                             cl=cl, MPI=MPI))))
        time1 <- proc.time()[3]
        Result_list[[i]]$SIM_TIME <- time1 - time0
        if(save && !is.na(save_every))
            if((i %% save_every) == 0L) saveRDS(Result_list, tmpfilename)
    }
    Final <- do.call(rbind, Result_list)

    #save file
    files <- dir()
    filename0 <- filename
    count <- 2L
    # create a new file name if old one exists, and throw warning
    while(TRUE){
        if(filename %in% files){
            filename <- paste0(count, '_', filename0)
            count <- count + 1L
        } else break
    }
    if(filename0 != filename)
        if(verbose)
            message(paste0('\nWARNING:\n', filename0, ' existed in the working directory already.
                           A new unique name was created.\n'))
    if(save){
        if(verbose)
            message(paste('\nSaving simulation results to file:', filename))
        saveRDS(Final, filename)
    }
    if(clean) system(paste0('rm -f ', tmpfilename))
    if(parallel) parallel::stopCluster(cl)
    return(invisible(Final))
}
