#' Run a Monte Carlo simulation given a data.frame of conditions and simulation functions
#'
#' This function runs a Monte Carlo simulation study given the simulation functions, the design conditions,
#' and the number of replications. Results can be saved as temporary files in case of interruptions
#' and may be restored by rerunning the exact function calls again, provided that the respective temp
#' file can be found in the working directory. To conserve RAM, temporary objects (such as
#' generated data across conditions and replications) are discarded.
#' Supports parallel and cluster computing, global and
#' local debugging, and is designed to be cross-platform.  For a skeleton version of the work-flow
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
#'      and generating suitable plots and tables}
#' }
#'
#' Two constants for each condition are returned by default:
#' \code{N_CELL_RUNS} to indicate the number of Monte Carlo runs it took to obtain valid results (this will
#' be greater than the number of replications requested if, for example, models failed to converge
#' and had to be re-drawn), and \code{SIM_TIME} to indicate how long (in seconds) it took to complete
#' all the Monte Carlo replications for each respective condition.
#'
#' In the event of a computer crash, power outage, etc, if the \code{save_every} command has been
#' used to save temporary results (along with the logical \code{save = TRUE}) then the original code in
#' the main source file need only be rerun again to resume the simulation.
#' The saved temp file will be read into the function, and the simulation will continue where it left
#' off before the simulation was terminated. Upon completion, a data.frame with the simulation
#' will be returned in the R session and a '.rds' file will be saved to the hard-drive (with the
#' file name corresponding to the \code{filename} argument below).
#'
#' @section Cluster computing:
#'
#' If the package is installed across a cluster of computers, and all the computers are accessible on
#' the same LAN network, then the package may be run within the MPI paradigm. This simply
#' requires that the computers be setup using the usual MPI requirements (typically, running some flavor
#' of Linux, have password-less openSSH access, addresses have been added to the \code{/etc/hosts} file, etc).
#' To setup the R code for an MPI cluster one need only add the argument \code{MPI = TRUE} and submit the
#' files using the suitable BASH commands.
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
#' could pass \code{replications = 300} to one computer and \code{replications = 200} to the other along
#' with a \code{save = TRUE} argument. This will create two distinct .rds files which can be
#' combined later with the \code{\link{aggregate_simulations}} function. The benifit of this approach over
#' MPI is that computers need not be linked over a LAN network, and should the need arise the temporary
#' simulation results can be migrated to another computer in case of a complete hardware failure by modifying
#' the suitable \code{compname} input (or, if the \code{filename} and \code{tmpfilename} were modified,
#' matching those files as well).
#'
#' @param design a \code{data.frame} object containing the Monte Carlo simulation conditions to
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
#' @param parallel logical; use parallel processing from the \code{parallel} package over each
#'   unique condition?
#'
#'   NOTE: When using packages other than the basic packages which are attached by default (e.g., \code{stats},
#'   \code{graphics}, \code{utils}, etc) then you must either a) explicitly load the packages within
#'   the respective defined functions with a \code{library()} or \code{require()} call, or b) use the
#'   \code{::} operator to locate the public functions that are not visible in the R session (e.g.,
#'   \code{psych::describe()})
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
#' @param save logical; save the final simulation and temp files to the hard-drive? This is useful
#'   for simulations which require an extended amount of time. Default is FALSE
#'
#' @param compname name of the computer running the simulation. Normally this doesn't need to be modified,
#'   but in the event that a node breaks down while running a simulation the results from the tmp files
#'   may be resumed on another computer by changing the name of the node to match the broken computer
#'
#' @param edit a string indicating where to initiate a \code{browser()} call for editing and debugging.
#'   Options are \code{'none'} (default), 'main' to edit the main function calls loop, \code{'generate'}
#'   to edit the data simulation function, \code{'analyse'} to edit the computational function, and
#'   \code{'summarise'} to  edit the aggregation function. Alternatively, users may place
#'   \code{\link{browser}} calls within the respective functions for debugging at specific lines
#'   (note: parallel computation flags will automatically be disabled when this is detected)
#'
#' @param seed a vector of integers (or single number)
#'   to be used for reproducibility. The length of the vector must be
#'   equal to either 1 or the number of rows in \code{design}; if 1, this will be repeated for each
#'   condition. This argument calls \code{\link{set.seed}} or
#'   \code{\link{clusterSetRNGStream}}, respectively, but will not be run when \code{MPI = TRUE}.
#'   Default is NULL, indicating that no seed is set
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
#' # (use EXPLICIT names, avoid things like N <- 100. That's fine in functions, not here)
#' sample_sizes <- c(10, 20, 50, 100)
#' standard_deviation_ratios <- c(1, 4, 8)
#'
#' Design <- expand.grid(sample_size_group1=sample_sizes,
#'                       sample_size_group2=sample_sizes,
#'                       standard_deviation_ratio=standard_deviation_ratios)
#' dim(Design)
#' head(Design)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define generate, analyse, and summerise functions
#'
#' # skeleton functions to be edited
#' SimDesign_functions()
#'
#' # help(generate)
#' Generate <- function(condition){
#'
#'     #require packages/define functions if needed, or better yet index with the :: operator
#'
#'     N1 <- condition$sample_size_group1
#'     N2 <- condition$sample_size_group2
#'     sd <- condition$standard_deviation_ratio
#'
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
#'
#'     return(dat)
#' }
#'
#' # help(analyse)
#'
#' Analyse <- function(condition, dat, parameters = NULL){
#'
#'     # require packages/define functions if needed, or better yet index with the :: operator
#'     require(stats)
#'     mygreatfunction <- function(x) print('Do some stuff')
#'
#'     #wrap computational statistics in try() statements to control estimation problems
#'     welch <- try(t.test(DV ~ group, dat), silent=TRUE)
#'     ind <- try(t.test(DV ~ group, dat, var.equal=TRUE), silent=TRUE)
#'
#'     # check if any errors occured. This will re-draw the data
#'     check_error(welch, ind)
#'
#'     # In this function the p values for the t-tests are returned,
#'     #  and make sure to name each element, for future reference
#'     ret <- c(welch = welch$p.value, independent = ind$p.value)
#'
#'     return(ret)
#' }
#'
#' # help(summarise)
#'
#' Summarise <- function(condition, results, parameters_list = NULL){
#'
#'     #find results of interest here (e.g., alpha < .1, .05, .01)
#'     nms <- c('welch', 'independent')
#'     lessthan.05 <- EDR(results[,nms], alpha = .05)
#'
#'     # return the results that will be appended to the design input
#'     ret <- c(lessthan.05=lessthan.05)
#'     return(ret)
#' }
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Collect results by looping over the rows in design
#'
#' # this simulation does not save temp files or the final result to disk (save=FALSE)
#' Final <- runSimulation(design=Design, replications=1000, parallel=TRUE,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' head(Final)
#' View(Final)
#'
#' ## Debug the generate function. See ?browser for help on debugging
#' ##   Type help to see available commands (e.g., n, c, where, ...),
#' ##   ls() to see what has been defined, and type Q to quit the debugger
#' runSimulation(design=Design, replications=1000,
#'               generate=Generate, analyse=Analyse, summarise=Summarise,
#'               parallel=TRUE, edit='generate')
#'
#' ## Alternatively, place a browser() within the desired function line to
#' ##   jump to a specific location
#' Summarise <- function(condition, results, parameters_list = NULL){
#'
#'     #find results of interest here (e.g., alpha < .1, .05, .01)
#'     nms <- c('welch', 'independent')
#'     lessthan.05 <- EDR(results[,nms], alpha = .05)
#'
#'     browser()
#'
#'     # return the results that will be appended to the design input
#'     ret <- c(lessthan.05=lessthan.05)
#'     return(ret)
#' }
#'
#' runSimulation(design=Design, replications=1000,
#'               generate=Generate, analyse=Analyse, summarise=Summarise,
#'               parallel=TRUE)
#'
#'
#'
#'
#' ## EXTRA: To run the simulation on a MPI cluster, use the following setup on each node (not run)
#' # library(doMPI)
#' # cl <- startMPIcluster()
#' # registerDoMPI(cl)
#' # Final <- runSimulation(design=Design, replications=1000, MPI=TRUE,
#' #                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' # closeCluster(cl)
#' # mpi.quit()
#'
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' # Step 4 --- Post-analysis: Create a new R file for analyzing the Final data.frame with R based
#' # regression stuff, so use the lm() function to find main effects, interactions, plots, etc.
#' # This is where you get to be a data analyst!
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
                          save = FALSE, save_every = 1, clean = TRUE, seed = NULL,
                          compname = Sys.info()['nodename'],
                          filename = paste0(compname,'_Final_', replications, '.rds'),
                          tmpfilename = paste0(compname, '_tmpsim.rds'), main = NULL,
                          ncores = parallel::detectCores(), edit = 'none', verbose = TRUE)
{
    stopifnot(!missing(generate) || !missing(analyse) || !missing(summarise))
    Functions <- list(generate=generate, analyse=analyse, summarise=summarise, main=main)
    stopifnot(!missing(design))
    stopifnot(!missing(replications))
    if(!is.null(seed)){
        if(length(seed) == 1L) seed <- rep(seed, nrow(design))
        stopifnot(nrow(design) == length(seed))
    }
    FunNames <- names(Functions)
    edit <- tolower(edit)
    if(is.null(main)) Functions$main <- SimDesign::main
    for(i in names(Functions)){
        fms <- names(formals(Functions[[i]]))
        truefms <- switch(i,
                          main = c('index', 'condition', 'generate', 'analyse'),
                          generate  = c('condition'),
                          analyse = c('dat', 'parameters', 'condition'),
                          summarise = c('results', 'parameters_list', 'condition'))
        if(!all(truefms %in% fms))
            stop(paste0('Function arguments for ', i, ' are not correct.'), call. = FALSE)
    }
    if(MPI){
        parallel <- FALSE
        verbose <- FALSE
    }
    for(i in 1L:length(Functions)){
        tmp <- deparse(substitute(Functions[[i]]))
        if(any(grepl('browser\\(', tmp))){
            if(verbose && parallel)
                message('A browser() call was detected. Parallel processing will be disabled while visible')
            parallel <- MPI <- FALSE
        }
    }
    if(!is.data.frame(design))
        stop('design must be a data.frame object', call. = FALSE)
    if(replications < 2L)
        stop('number of replications must be greater than or equal to 2', call. = FALSE)
    if(!is.na(save_every))
        if(save_every > nrow(design))
            warning('save_every is too large to be useful', call. = FALSE)
    if(!(edit %in% c('none', 'analyse', 'generate', 'main', 'summarise')))
        stop('edit location is not valid', call. = FALSE)
    if(is.null(design$ID)){
        design <- data.frame(ID=1L:nrow(design), design)
    } else stopifnot(length(unique(design$ID)) == nrow(design))
    if(edit != 'none'){
        parallel <- MPI <- FALSE
        debug(Functions[[edit]])
        on.exit(undebug(Functions[[edit]]))
    }
    cl <- NULL
    if(parallel){
        cl <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cl))
    }
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
                                                             cl=cl, MPI=MPI, seed=seed))))
        time1 <- proc.time()[3]
        Result_list[[i]]$SIM_TIME <- time1 - time0
        if(!(length(unique(sapply(Result_list, length))) %in% c(1L, 2L)))
            stop(c('Summerise() results are not all of the same length. This may require splitting up',
                   '\nthe design input to allow for different result lengths.'), call.=FALSE)
        if(save && !is.na(save_every))
            if((i %% save_every) == 0L) saveRDS(Result_list, tmpfilename)
    }
    Final <- do.call(rbind, Result_list)
    Final$ID <- NULL
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
    return(invisible(Final))
}
