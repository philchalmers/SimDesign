#' Optimized target quantities in simulation experiments (ProBABLI)
#'
#' This function provides a stochastic optimization approach to solving
#' specific quantities in simulation experiments (e.g., solving for a specific
#' sample size to meet a target power rate) using the
#' Probablistic Bisection Algorithm with Bolstering and Interpolations
#' (ProBABLI; Chalmers, in review). The structure follows the
#' steps outlined in \code{\link{runSimulation}}, however portions of
#' the \code{design} input are taken as variables to be estimated rather than
#' fixed, and the constant \code{b} is required in order to
#' solve the root equation \code{f(x) - b = 0}.
#'
#' Optimization is performed using the probabilistic bisection algorithm
#' (\code{\link{PBA}}) to find the associated root given the noisy simulation
#' objective function evaluations. Information is also collected throughout
#' the iterations in order to make more aggressive predictions about the
#' associated root via interpolation and extrapolation.
#'
#' @param design a \code{tibble} or \code{data.frame} object containing
#'   the Monte Carlo simulation conditions to be studied, where each row
#'   represents a unique condition and each column a factor  to be varied
#'   (see also \code{\link{createDesign}}). However, exactly one column of this
#'   object must be specified with \code{NA} placeholders to indicate
#'   that the missing value should be solved via the stochastic optimizer
#'
#' @param b a single constant used to solve the root equation \code{f(x) - b = 0}
#'
#' @param replications a named list or vector indicating the number of replication to
#'   use for each design condition per PBA iteration. By default the input is a
#'   \code{list} with the arguments \code{burnin = 15L}, specifying the number
#'   of burn-in iterations to used, \code{burnin.reps = 100L} to indicate how many
#'   replications to use in each burn-in iteration, \code{max.reps = 500L} to
#'   prevent the replications from increasing higher than this number, and
#'   \code{increase.by = 10L} to indicate how many replications to increase
#'   after the burn-in stage. Unless otherwise specified these defaults will
#'   be used, but can be overwritten by explicit definition (e.g.,
#'   \code{replications = list(increase.by = 25L)})
#'
#'   Vector inputs can specify the exact replications
#'   for each iterations. As a general rule, early iterations
#'   should be relatively low for initial searches to avoid unnecessary computations
#'   for locating the approximate root, though the number of replications should
#'   gradually increase to reduce the sampling variability as the PBA approaches
#'   the root.
#'
#' @param generate generate function. See \code{\link{runSimulation}}
#'
#' @param analyse analysis function. See \code{\link{runSimulation}}
#'
#' @param summarise summary function that returns a single number corresponding
#'   to a function evaluation \code{f(x)} in the equation
#'   \code{f(x) = b} to be solved as a root \code{f(x) - b = 0}.
#'   Unlike in the standard \code{runSimulation()} definitions this input
#'   is required. For further information on this function specification,
#'   see \code{\link{runSimulation}}
#'
#' @param interval a vector of length two, or matrix with \code{nrow(design)}
#'   and two columns, containing the end-points of the interval to be searched.
#'   If a vector then the interval will be used for all rows in the supplied
#'   \code{design} object
#'
#' @param integer logical; should the values of the root be considered integer
#'   or numeric? If \code{TRUE} then bolstered directional decisions will be
#'   made in the \code{pba} function based on the collected sampling history
#'   throughout the search
#'
#' @param save logical; store temporary file in case of crashes. If detected
#'   in the working directory will automatically be loaded to resume (see
#'   \code{\link{runSimulation}} for similar behavior)
#'
#' @param verbose logical; print information to the console?
#'
#' @param control a \code{list} of the algorithm control parameters. If not specified,
#'   the defaults described below are used.
#'
#' \describe{
#'    \item{\code{tol}}{tolerance criteria for early termination (.1 for
#'      \code{integer = TRUE} searches; .001 for non-integer searches}
#'    \item{\code{rel.tol}}{relative tolerance criteria for early termination (default .00001)}
#'    \item{\code{k.success}}{number of consecutive tolerance success given \code{rel.tol} and
#'      \code{tol} criteria (default is 3)}
#'    \item{\code{bolster}}{logical; should the PBA evaluations use bolstering based on previous
#'      evaluations? Default is \code{TRUE}, though only applicable when \code{integer = TRUE} }
#'    \item{\code{single_step.iter}}{when \code{integer = TRUE}, do not take steps larger than
#'      size 1 after this iteration number (default is 40). This prevents wide oscillations
#'      around the probable root}
#'    \item{\code{interpolate.R}}{number of replications to collect prior to performing
#'      the interpolation step (default is 3000 after accounting for data exclusion
#'      from \code{burnin}). Setting this to 0 will disable any
#'      interpolation computations}
#'    \item{\code{include_reps}}{logical; include a column in the \code{condition}
#'      elements to indicate how many replications are currently being evaluated? Mainly
#'      useful when further precision tuning within each ProBABLI iteration is
#'      desirable (e.g., for bootstrapping). Default is \code{FALSE}}
#'    \item{\code{summarise.reg_data}}{logical; should the aggregate results from \code{Summarise}
#'      (along with its associated weights) be used for the interpolation steps, or the
#'      raw data from the \code{Analyse} step? Set this to \code{TRUE} when the individual
#'      results from \code{Analyse} give less meaningful information}
#'    }
#'
# @param interpolate.burnin integer indicating the number of initial iterations
#      to discard from the interpolation computations. This is included to further
#      remove the effect of early estimates that are far away from the solution
#
#' @param maxiter the maximum number of iterations (default 100)
#'
#' @param parallel for parallel computing for slower simulation experiments
#'   (see \code{\link{runSimulation}} for details)
#'
#' @param cl see \code{\link{runSimulation}}
#'
#' @param ncores see \code{\link{runSimulation}}
#'
#' @param type type of cluster object to define
#'
#' @param formula regression formula to use when \code{interpolate = TRUE}. Default
#'   fits an orthogonal polynomial of degree 2
#'
#' @param family \code{family} argument passed to \code{\link{glm}}. By default
#'   the \code{'binomial'} family is used, as this function defaults to power
#'   analysis setups where isolated results passed to \code{summarise} will
#'   return 0/1s, however other families should be used had \code{summarise}
#'   returned something else (e.g., if solving for a particular standard error
#'   then a \code{'gaussian'} family would be more appropriate).
#'
#'   Note that if individual  results from the \code{analyse} steps should
#'   not be used (i.e., only the aggregate from \code{summarise} is meaningful)
#'   then set \code{control = list(summarise.reg_data = TRUE)} to override the default
#'   behaviour, thereby using only the aggregate information and weights
#'
#' @param ... additional arguments to be pasted to \code{\link{PBA}}
#'
#' @return the filled-in \code{design} object containing the associated lower and upper interval
#'   estimates from the stochastic optimization
#'
#' @export
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#'
#' \dontrun{
#'
#' # TASK: Find specific sample size in each group for independent t-test
#' # corresponding to a power rate of .8
#' #
#' # For ease of the setup, assume the groups are the same size, and the mean
#' # difference corresponds to Cohen's d values of .2, .5, and .8
#' # This example can be solved numerically using the pwr package (see below),
#' # though the following simulation setup is far more general and can be
#' # used for any generate-analyse combination of interest
#'
#' # SimFunctions(SimSolve=TRUE)
#'
#' #### Step 1 --- Define your conditions under study and create design data.frame.
#' #### However, use NA placeholder for sample size as it must be solved,
#' #### and add desired power rate to object
#'
#' Design <- createDesign(N = NA,
#'                        d = c(.2, .5, .8),
#'                        sig.level = .05)
#' Design    # solve for NA's
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define generate, analyse, and summarise functions
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'     Attach(condition)
#'     group1 <- rnorm(N)
#'     group2 <- rnorm(N, mean=d)
#'     dat <- data.frame(group = gl(2, N, labels=c('G1', 'G2')),
#'                       DV = c(group1, group2))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'     p <- t.test(DV ~ group, dat, var.equal=TRUE)$p.value
#'     p
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
#'     # Must return a single number corresponding to f(x) in the
#'     # root equation f(x) = b
#'
#'     ret <- EDR(results, alpha = condition$sig.level)
#'     ret
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Optimize N over the rows in design
#' # Initial search between N = [10,500] for each row using the default
#' # integer solver (integer = TRUE)
#'
#' # In this example, b = target power
#' solved <- SimSolve(design=Design, b=.8, interval=c(10, 500),
#'                 generate=Generate, analyse=Analyse,
#'                 summarise=Summarise)
#' solved
#' summary(solved)
#' plot(solved, 1)
#' plot(solved, 2)
#' plot(solved, 3)
#'
#' # also can plot median history and estimate precision
#' plot(solved, 1, type = 'history')
#' plot(solved, 1, type = 'density')
#'
#' # verify with true power from pwr package
#' library(pwr)
#' pwr.t.test(d=.2, power = .8, sig.level = .05)
#' pwr.t.test(d=.5, power = .8, sig.level = .05)
#' pwr.t.test(d=.8, power = .8, sig.level = .05)
#'
#' # use estimated N results to see how close power was
#' N <- solved$N
#' pwr.t.test(d=.2, n=N[1], sig.level = .05)
#' pwr.t.test(d=.5, n=N[2], sig.level = .05)
#' pwr.t.test(d=.8, n=N[3], sig.level = .05)
#'
#' # with rounding
#' N <- ceiling(solved$N)
#' pwr.t.test(d=.2, n=N[1], sig.level = .05)
#' pwr.t.test(d=.5, n=N[2], sig.level = .05)
#' pwr.t.test(d=.8, n=N[3], sig.level = .05)
#'
#' # failing analytic formula, confirm results with more precise
#' #  simulation via runSimulation()
#' csolved <- solved
#' csolved$N <- ceiling(solved$N)
#' confirm <- runSimulation(design=csolved, replications=10000, parallel=TRUE,
#'                          generate=Generate, analyse=Analyse,
#'                          summarise=Summarise)
#' confirm
#'
#'
#' ############
#' # Similar setup as above, however goal is now to solve d given sample
#' # size and power inputs (inputs for root no longer required to be an integer)
#'
#' Design <- createDesign(N = c(100, 50, 25),
#'                        d = NA,
#'                        sig.level = .05)
#' Design    # solve for NA's
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define generate, analyse, and summarise functions (same as above)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Optimize d over the rows in design
#' # search between d = [.1, 2] for each row
#'
#' # In this example, b = target power
#' # note that integer = FALSE to allow smooth updates of d
#' solved <- SimSolve(design=Design, b = .8, interval=c(.1, 2),
#'                 generate=Generate, analyse=Analyse,
#'                 summarise=Summarise, integer=FALSE)
#' solved
#' summary(solved)
#' plot(solved, 1)
#' plot(solved, 2)
#' plot(solved, 3)
#'
#' # verify with true power from pwr package
#' library(pwr)
#' pwr.t.test(n=100, power = .8, sig.level = .05)
#' pwr.t.test(n=50, power = .8, sig.level = .05)
#' pwr.t.test(n=25, power = .8, sig.level = .05)
#'
#' # use estimated d results to see how close power was
#' pwr.t.test(n=100, d = solved$d[1], sig.level = .05)
#' pwr.t.test(n=50, d = solved$d[2], sig.level = .05)
#' pwr.t.test(n=25, d = solved$d[3], sig.level = .05)
#'
#' # failing analytic formula, confirm results with more precise
#' #  simulation via runSimulation()
#' confirm <- runSimulation(design=solved, replications=10000, parallel=TRUE,
#'                          generate=Generate, analyse=Analyse,
#'                          summarise=Summarise)
#' confirm
#'
#' }
SimSolve <- function(design, interval, b, generate, analyse, summarise,
                     replications = list(burnin = 15L, burnin.reps = 100L,
                                         max.reps = 500L, increase.by = 10L),
                     integer = TRUE, formula = y ~ poly(x, 2), family = 'binomial',
                     parallel = FALSE, cl = NULL, save = TRUE,
                     ncores = parallel::detectCores() - 1L,
                     type = ifelse(.Platform$OS.type == 'windows', 'PSOCK', 'FORK'),
                     maxiter = 100L, verbose = TRUE, control = list(), ...){

    # robust <- FALSE
    burnin <- 15L
    if(is.list(replications)){
        if(is.null(replications$burnin)) replications$burnin <- burnin else
            burnin <- replications$burnin
        if(is.null(replications$burnin.reps)) replications$burnin.reps <- 100L
        if(is.null(replications$max.reps)) replications$max.reps <- 500L
        if(is.null(replications$increase.by)) replications$increase.by <- 10L
        replications <- with(replications,
                             pmin(max.reps, c(rep(burnin.reps, burnin),
                                              seq(burnin.reps, by=increase.by,
                                                  length.out=maxiter-burnin))))
    }
    ANALYSE_FUNCTIONS <- NULL
    .SIMDENV$ANALYSE_FUNCTIONS <- ANALYSE_FUNCTIONS <- analyse
    if(is.character(parallel)){
        useFuture <- tolower(parallel) == 'future'
        parallel <- TRUE
    } else useFuture <- FALSE
    if(is.null(control$tol)) control$tol <- if(integer) .1 else .001
    if(is.null(control$summarise.reg_data))
        control$summarise.reg_data <- FALSE
    if(is.null(control$rel.tol)) control$rel.tol <- .0001
    if(is.null(control$k.sucess)) control$k.success <- 3L
    if(is.null(control$interpolate.R)) control$interpolate.R <- 3000L
    if(is.null(control$bolster)) control$bolster <- TRUE
    if(is.null(control$single_step.iter)) control$single_step.iter <- 40L
    if(is.null(control$include_reps)) control$include_reps <- FALSE

    on.exit(.SIMDENV$stored_results <- .SIMDENV$stored_medhistory <-
                .SIMDENV$stored_history <- .SIMDENV$include_reps <- NULL,
            add = TRUE)
    on.exit(.SIMDENV$FromSimSolve <- NULL, add = TRUE)

    if(missing(generate) && !missing(analyse))
        generate <- function(condition, dat, fixed_objects = NULL){}
    stopifnot(!missing(b))
    stopifnot(length(b) == 1L)
    stopifnot(!missing(interval))
    if(length(replications) == 1L) replications <- rep(replications, maxiter)
    stopifnot(length(replications) == maxiter)
    interpolate <- control$interpolate.R > 0L
    interpolate.after <- max(which(cumsum(replications) <=
                                       control$interpolate.R)) + 1L
    ReturnSimSolveInternals <- FALSE
    if(!is.null(attr(verbose, 'ReturnSimSolveInternals')))
        ReturnSimSolveInternals <- TRUE

    root.fun <- function(x, b, design.row, replications, store = TRUE, ...){
        design.row[1L, which(is.na(design.row))] <- x
        if(.SIMDENV$include_reps) design.row$REPLICATIONS <- replications
        attr(design.row, 'SimSolve') <- TRUE
        ret <- runSimulation(design=design.row, replications=replications,
                             generate=generate, analyse=analyse,
                             parallel=parallel, cl=cl,
                             summarise=summarise, save=FALSE, verbose=FALSE, ...)
        val <- ifelse(is.list(ret), ret[[1L]], ret[1L])
        if(store){
            pick <- min(which(sapply(.SIMDENV$stored_results, is.null)))
            .SIMDENV$stored_results[[pick]] <- ret$summary_results
            .SIMDENV$stored_medhistory[[pick]] <- x
            .SIMDENV$stored_history[[pick]] <-
                data.frame(y=val, x=x, reps=replications)
        }
        ret <- val - b
        ret
    }

    if(missing(design))
        stop('design object required', call.=FALSE)
    apply(design, 1L, function(x){
        if(sum(is.na(x)) != 1L)
            stop('design object must have exactly 1 NA element per row')
    })
    if(missing(analyse))
        stop('analyse function for root required', call.=FALSE)
    if(missing(interval))
        stop('analyse function for root required', call.=FALSE)
    if(!is.matrix(interval)){
        stopifnot(ncol(interval) == 2L)
        interval <- matrix(interval, nrow=nrow(design), ncol=2, byrow=TRUE)
    }
    stopifnot(is.matrix(interval))
    stopifnot(all(interval[,1] < interval[,2]))
    roots <- vector('list', nrow(design))
    .SIMDENV$SimSolveInteger <- integer
    if(is.character(parallel)){
        useFuture <- tolower(parallel) == 'future'
        parallel <- TRUE
    } else useFuture <- FALSE
    if(parallel){
        if(!useFuture && is.null(cl)){
            cl <- parallel::makeCluster(ncores, type=type)
            on.exit(parallel::stopCluster(cl), add = TRUE)
        }
    }
    export_funs <- parent_env_fun()
    if(parallel){
        if(!useFuture && is.null(cl)){
            cl <- parallel::makeCluster(ncores, type=type)
            on.exit(parallel::stopCluster(cl), add = TRUE)
        }
        if(!useFuture){
            parallel::clusterExport(cl=cl, export_funs, envir = parent.frame(1L))
            parallel::clusterExport(cl=cl, "ANALYSE_FUNCTIONS", envir = environment())
            if(verbose)
                message(sprintf("\nNumber of parallel clusters in use: %i", length(cl)))
        }
    }
    compname <- Sys.info()['nodename']
    tmpfilename <- paste0('SIMSOLVE-TEMPFILE_', compname, '.rds')
    start <- 1L
    if(file.exists(tmpfilename)){
        roots <- readRDS(tmpfilename)
        start <- min(which(sapply(roots, is.null)))
        if(verbose)
            message(paste0('\nContinuing SimSolve() run at design row ', start))
    }
    for(i in start:nrow(design)){
        if(verbose){
            cat(sprintf('\n\n#############\nDesign row %s:\n\n', i))
            print(cbind(as.data.frame(design[i,]), b = b))
            cat("\n")
        }
        .SIMDENV$stored_results <- vector('list', maxiter)
        .SIMDENV$stored_medhistory <- rep(NA, maxiter)
        .SIMDENV$stored_history <- vector('list', maxiter)
        .SIMDENV$include_reps <- control$include_reps
        .SIMDENV$FromSimSolve <- list(interpolate=interpolate,
                                      interpolate.after=interpolate.after,
                                      family=family,
                                      formula=formula,
                                      replications=replications,
                                      tol=control$tol,
                                      rel.tol=control$rel.tol,
                                      b=b,
                                      bolster=control$bolster,
                                      k.success=control$k.success,
                                      single_step.iter=control$single_step.iter,
                                      control=control,
                                      # robust = robust,
                                      interpolate.burnin=burnin)
        roots[[i]] <- try(PBA(root.fun, interval=interval[i, , drop=TRUE], b=b,
                          design.row=as.data.frame(design[i,]),
                          integer=integer, verbose=verbose, maxiter=maxiter, ...))
        if(is(roots[[i]], 'try-error')){
            is_below <- grepl("*below*", as.character(roots[[i]]))
            if(is_below || grepl("*above*", as.character(roots[[i]])))
                roots[[i]] <- list(root=ifelse(is_below, Inf, -Inf))
            else roots[[i]] <- list(root=NA)
            next
        }
        tab <- .SIMDENV$stored_history[!sapply(.SIMDENV$stored_history, is.null)]
        if(ReturnSimSolveInternals) return(tab)
        attr(roots[[i]], 'stored_tab') <- if(integer) reduceTable(tab) else tab
        if(verbose){
            cat("\r")
            tmp <- as.data.frame(design[i,])
            cat(sprintf("\nSolution for %s: %.3f",
                colnames(design)[which(is.na(tmp))], roots[[i]]$root))
        }
        if(save && i < nrow(design)) saveRDS(roots, tmpfilename)
    }
    if(file.exists(tmpfilename)) file.remove(tmpfilename)
    ret <- design
    vals <- sapply(roots, function(x) x$root)
    ret[, which(is.na(ret[i,])), drop=TRUE] <- vals
    attr(ret, 'roots') <- roots
    attr(ret, 'summarise_fun') <- summarise
    class(ret) <- c('SimSolve', class(ret))
    ret
}

#' @rdname SimSolve
#' @param object object of class \code{'SimSolve'}
#' @param tab.only logical; print only the (reduce) table of estimates?
#' @param reps.cutoff integer indicating the rows to omit from output
#'  if the number of replications do no reach this value
#' @export
summary.SimSolve <- function(object, tab.only = FALSE, reps.cutoff = 300, ...)
{
    ret <- attr(object, 'roots')
    if(ret[[1L]]$integer){
        stored_tab <- lapply(ret, function(x) attr(x, "stored_tab"))
        stored_tab <- lapply(stored_tab, function(x)
            x[x$reps > reps.cutoff, , drop=FALSE])
        if(tab.only) return(stored_tab)
    } else stored_tab <- NULL
    for(i in 1L:length(ret)){
        attr(ret[[i]], "stored_tab") <- NULL
        ret[[i]]$tab <- stored_tab[[i]]
    }
    names(ret) <- paste0('DesignRow_', 1L:length(ret))
    ret
}

#' @rdname SimSolve
#' @param x object of class \code{'SimSolve'}
#' @param y design row to plot. If omitted defaults to 1
#' @export
plot.SimSolve <- function(x, y, ...)
{
    if(missing(y)) y <- 1L
    roots <- attr(x, 'roots')[[y]]
    dots <- list(...)
    if(!is.null(dots$type) && dots$type == 'density'){
        so <- summary(x, ...)
        tab <- so[[y]]$tab
        with(tab, plot(density(x, weights=reps/sum(reps)),
                       main = 'Density Using Replication Weights', las=1))
    } else plot(roots, las=1, ...)
    return(invisible(NULL))
}
