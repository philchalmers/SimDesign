#' Probabilistic Bisection Algorithm
#'
#' The function \code{PBA} searches a specified \code{interval} for a root
#' (i.e., zero) of the function \code{f(x)} with respect to its first argument.
#' However, this function differs from deterministic cousins such as
#' \code{\link{uniroot}} in that \code{f} may contain stochastic error
#' components, and instead provides a Bayesian interval where the root
#' is likely to lie. Note that it is assumed that \code{E[f(x)]} is non-decreasing
#' in \code{x} and that the root is between the search interval.
#' See Waeber, Frazier, and Henderson (2013) for details.
#'
#' @param f noisy function for which the root is sought
#'
#' @param f.prior density function indicating the likely location of the prior
#'   (e.g., if root is within [0,1] then \code{\link{dunif}} works, otherwise custom
#'   functions will be required)
#'
#' @param interval a vector containing the end-points of the interval
#'   to be searched for the root
#'
#' @param tol tolerance criteria for convergence based on average of the
#'   \code{f(x)} evaluations
#'
# @param integer.tol a vector of length two indicating the termination
#   criteria when \code{integer = TRUE}. The first element indicates the
#   number of previous median values to inspect from the iteration history
#   (default checks the last 15), while the second criteria indicates the number
#   of unique values that are allowed to appear in the last of these iterations
#   (default is 3). The idea is that if the same median estimates are appearing
#   in several of the most recent estimates then the algorithm has likely reached
#   the stationary location
#
#' @param ... additional named arguments to be passed to \code{f}
#'
#' @param p assumed constant for probability of correct responses (must be > 0.5)
#'
#' @param maxiter the maximum number of iterations
#'
#' @param integer logical; should the values of the root be considered integer
#'   or numeric? The former uses a discreet grid to track the updates, while the
#'   latter currently creates a grid with \code{resolution} points
#'
#' @param check.interval logical; should an initial check be made to determine
#'    whether \code{f(interval[1L])} and \code{f(interval[2L])} have opposite
#'    signs? Default is TRUE
#'
#' @param check.interval.only logical; return only TRUE or FALSE to test
#'   whether there is a likely root given \code{interval}? Setting this to TRUE
#'   can be useful when you are unsure about the root location interval and
#'   may want to use a higher \code{replication} input from \code{\link{SimSolve}}
#'
#' @param resolution constant indicating the
#'   number of equally spaced grid points to track when \code{integer = FALSE}.
#'
#' @param mean_window last n iterations used to compute the final estimate of the root.
#'   This is used to avoid the influence of the early bisection steps in the
#'   final root estimate
#'
# @param CI advertised confidence level for Bayes interval
#'
#' @param verbose logical; should the iterations and estimate be printed to the
#'   console?
#'
#' @references
#'
#' Horstein, M. (1963). Sequential transmission using noiseless feedback.
#' IEEE Trans. Inform. Theory, 9(3):136-143.
#'
#' Waeber, R.; Frazier, P. I. & Henderson, S. G. (2013). Bisection Search
#' with Noisy Responses. SIAM Journal on Control and Optimization,
#' Society for Industrial & Applied Mathematics (SIAM), 51, 2261-2279.
#'
#' @export
#'
#' @seealso \code{\link{uniroot}}
#'
#' @examples
#'
#' # find x that solves f(x) - b = 0 for the following
#' f.root <- function(x, b = .6) 1 / (1 + exp(-x)) - b
#' f.root(.3)
#' retuni <- uniroot(f.root, c(0,1))
#' retuni
#' retuni$root
#'
#' # PBA without noisy root
#' retpba <- PBA(f.root, c(0,1))
#' retpba
#' retpba$root
#' plot(retpba)
#' plot(retpba, type = 'history')
#'
#' # Same problem, however root function is noisy. Hence, need to solve
#' #  fhat(x) - b + e = 0, where E(e) = 0
#' f.root_noisy <- function(x) 1 / (1 + exp(-x)) - .6 + rnorm(1, sd=.02)
#' sapply(rep(.3, 10), f.root_noisy)
#'
#' # uniroot "converges" unreliably
#' set.seed(123)
#' uniroot(f.root_noisy, c(0,1))$root
#' uniroot(f.root_noisy, c(0,1))$root
#' uniroot(f.root_noisy, c(0,1))$root
#'
#' # probabilistic bisection provides better convergence
#' retpba.noise <- PBA(f.root_noisy, c(0,1))
#' retpba.noise
#' plot(retpba.noise)
#' plot(retpba.noise, type = 'history')
#'
PBA <- function(f, interval, ..., p = .6,
                integer = FALSE, tol = if(integer) .01 else .0001,
                maxiter = 300L, mean_window = 100L,
                f.prior = NULL, resolution = 10000L,
                check.interval = TRUE, check.interval.only = FALSE,
                verbose = TRUE){

    stopifnot(length(p) == 1L)
    if(p <= 0.5)
        stop('Probability must be > 0.5')
    if(integer){
        if(any((interval - floor(interval)) > 0))
            stop('interval supplied contains decimals while integer = TRUE. Please fix',
                 call.=FALSE)
    }

    bool.f <- function(f.root, median, ...){
        val <- valp <- f.root(median, ...)
        if(integer && !is.null(.SIMDENV$FromSimSolve)){
            if(!all(is.na(.SIMDENV$stored_medhistory))){
                whc <- which(median == .SIMDENV$stored_medhistory)
                whc <- whc[-1L]
                if(length(whc)){
                    dots <- list(...)
                    cmp <- dplyr::bind_rows(.SIMDENV$stored_history[whc])
                    valp <- sum((cmp$y - .SIMDENV$FromSimSolve$b) * cmp$reps,
                             val * dots$replications) /
                        sum(cmp$reps, dots$replications)
                }
            }
        }
        z <- valp < 0
        c(z, val)
    }

    logp <- log(p)
    logq <- log(1-p)

    dots <- list(...)
    FromSimSolve <- .SIMDENV$FromSimSolve
    if(!is.null(FromSimSolve)){
        family <- FromSimSolve$family
        interpolate <- FromSimSolve$interpolate
        interpolate.after <- FromSimSolve$interpolate.after
        formula <- FromSimSolve$formula
        replications <- FromSimSolve$replications
        tol <- FromSimSolve$tol
        rel.tol <- FromSimSolve$rel.tol
        # robust <- FromSimSolve$robust
        interpolate.burnin <- FromSimSolve$interpolate.burnin
        glmpred.last <- glmpred <- c(NA, NA)
        k.success <- FromSimSolve$k.success
        k.successes <- 0L
    } else interpolate <- FALSE
    x <- if(integer) interval[1L]:interval[2L]
        else seq(interval[1L], interval[2L], length.out=resolution[1L])
    fx <- if(is.null(f.prior)) rep(1, length(x)) else f.prior(x, ...)
    fx <- log(fx / sum(fx)) # in log units
    medhistory <- roothistory <- numeric(maxiter)
    e.froot <- NA
    start_time <- proc.time()[3L]

    if(check.interval){
        if(!is.null(FromSimSolve)){
            upper <- bool.f(f.root=f, interval[2L], replications=replications[1L],
                            store = FALSE, ...)
            lower <- bool.f(f.root=f, interval[1L], replications=replications[1L],
                            store = FALSE, ...)
        } else {
            upper <- bool.f(f.root=f, interval[2L], ...)
            lower <- bool.f(f.root=f, interval[1L], ...)
        }
        no_root <- (upper[1L] + lower[1L]) != 1L
        if(no_root){
            msg <- sprintf('interval range supplied appears to be %s the probable root.',
                           ifelse(upper[1L] == 0, '*above*', '*below*'))
            stop(msg, call.=FALSE)
        }
        if(check.interval.only) return(!no_root)
    }

    for(iter in 1L:maxiter){
        med <- getMedian(fx, x)
        medhistory[iter] <- med
        feval <- if(!is.null(FromSimSolve))
            bool.f(f.root=f, med, replications=replications[iter], ...)
        else bool.f(f.root=f, med, ...)
        z <- feval[1]
        roothistory[iter] <- feval[2]
        if(z){
            fx <- fx + ifelse(x >= med, logp, logq)
        } else {
            fx <- fx + ifelse(x >= med, logq, logp)
        }
        w <- if(!is.null(FromSimSolve)) 1/sqrt(replications) else rep(1/iter, iter)
        e.froot <- sum(roothistory[iter:1L] * w[iter:1] / sum(w[iter:1]))

        if(interpolate && iter > interpolate.after){
            SimSolveData <- SimSolveData(burnin=interpolate.burnin)
            # SimMod <- if(robust)
            #     suppressWarnings(robustbase::glmrob(formula = formula,
            #                          data=SimSolveData, family=family))
            #     else
            SimMod <- try(suppressWarnings(glm(formula = formula,
                                          data=SimSolveData, family=family)), silent=TRUE)
            glmpred <- if(is(SimMod, 'try-error')){
                c(NA, NA)
            } else {
                suppressWarnings(SimSolveUniroot(SimMod=SimMod,
                                                 b=dots$b, interval=interval,
                                                 median=med))
            }

            # Should termination occur early when this changes very little?
            if(!any(is.na(c(glmpred[1L], glmpred.last[1L])))){
                abs_diff <- abs(glmpred.last[1L] - glmpred[1L])
                rel_diff <- abs_diff / abs(glmpred.last[1L])
                if(abs_diff <= tol || rel_diff <= rel.tol){
                    k.successes <- k.successes + 1L
                    if(k.successes == k.success) break
                } else k.successes <- 0L
            } else k.successes <- 0L
            glmpred.last <- glmpred
        }
        if(!interpolate && abs(e.froot) < tol && iter > mean_window) break

        if(verbose){
            if(integer)
                cat(sprintf("\rIteration: %i; Median: %i; E(f(x)) = %.3f",
                            iter, med, e.froot))
            else cat(sprintf("\rIteration: %i; Median: %.3f; E(f(x)) = %.3f",
                             iter, med, e.froot))
            if(!is.null(FromSimSolve))
                cat('; Reps =', replications[iter])
            if(interpolate && iter > interpolate.after && !is.na(glmpred[1L]))
                cat(sprintf('; Pred = %.3f', glmpred[1L]))
        }
    }
    converged <- iter < maxiter
    if(verbose)
        cat("\n")
    fx <- exp(fx) / sum(exp(fx)) # normalize final result
    medhistory <- medhistory[1L:(iter-1L)]
    # BI <- belief_interval(x, fx, CI=CI)
    root <- if(!interpolate) mean(medhistory[length(medhistory):
                                                 (length(medhistory)-mean_window+1L)])
        else glmpred[1L]
    ret <- list(iter=iter, root=root, converged=converged, integer=integer,
                e.froot=e.froot, x=x, fx=fx, medhistory=medhistory,
                time=as.numeric(proc.time()[3L]-start_time))
    if(!is.null(FromSimSolve)) ret$total.replications <- sum(replications[1L:iter])
    class(ret) <- 'PBA'
    ret
}

#' @rdname PBA
#' @param x an object of class \code{PBA}
#' @export
print.PBA <- function(x, ...)
{
    out <- with(x,
         list(root = root,
              converged=converged,
              time=noquote(timeFormater(time)),
              iterations = iter))
    if(!is.null(x$total.replications))
        out$total.replications <- x$total.replications
    if(x$integer && !is.null(x$tab))
        out$tab <- x$tab
    print(out, ...)
}

#' @rdname PBA
#' @param main plot title
#' @param type type of plot to draw for PBA object. Can be either 'posterior' or
#'   'history' to plot the PBA posterior distribution or the mediation iteration
#'    history
#' @export
plot.PBA <- function(x, type = 'posterior',
                     main = 'Probabilistic Bisection Posterior', ...)
{
    if(type == 'posterior'){
        with(x, plot(fx ~ x,
                     main = main, type = 'l',
                     ylab = 'density', ...))
    } else if(type == 'history'){
        with(x, plot(medhistory,
                     main = 'Median history', type = 'b',
                     ylab = 'Median Estimate',
                     xlab = 'Iteration', pch = 16, ...))
    }

}

getMedian <- function(fx, x){
    expfx <- exp(fx)  # on original pdf
    alpha <- sum(expfx)/2

    # ad-hoc sum approach is clunky, but seems to work okay for now
    if(rint(1, min=0, max=1)){
        ret <- x[cumsum(expfx) <= alpha]
        if(!length(ret)) ret <- x[1L]
    } else {
        xs <- x[length(x):1L]
        expfxs <- expfx[length(x):1L]
        ret <- xs[cumsum(expfxs) <= alpha]
        if(!length(ret)) ret <- xs[1L]
    }
    ret[length(ret)]
}

# belief_interval <- function(x, fx, CI = .95){
#     expfx <- exp(fx)
#     expfx <- expfx / sum(expfx)
#     eps <- (1 - CI)/2
#     L <- x[cumsum(expfx) < eps]
#     if(!length(L)) L <- x[1L]
#     xs <- x[length(x):1L]
#     expfxs <- expfx[length(x):1L]
#     R <- xs[cumsum(expfxs) < eps]
#     if(!length(R)) R <- xs[1L]
#     ret <- c(L=L[length(L)], R=R[length(R)])
#     ret
# }
