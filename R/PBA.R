#' Probabilistic Bisection Algorithm
#'
#' The function \code{PBA} searches a specified \code{interval} for a root
#' (i.e., zero) of the function \code{f(x)} with respect to its first argument.
#' However, this function differs from deterministic cousins such as
#' \code{\link{uniroot}} in that \code{f} may contain stochastic error
#' components, and instead provides a Bayesian interval where the root
#' is likely to lie. Note that it is assumed that \code{E[f(x)]} is non-decreasing
#' in \code{x} and that the root is between the search interval (evaluated
#' approximately when \code{check.interval=TRUE}).
#' See Waeber, Frazier, and Henderson (2013) for details.
#'
#' @param f.root noisy function for which the root is sought
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
#'
#' @param wait.time (optional) instead of terminating after specific estimate criteria
#'   are satisfied (e.g., \code{tol}), terminate after a specific
#'   wait time. Input is specified either as a numeric vector in seconds or as a character
#'   vector to be formatted by \code{\link{timeFormater}}.
#'   Note that users should increase the number of \code{maxiter} as well
#'   so that termination can occur if either the maximum iterations are satisfied
#'   or the specified wait time has elapsed (whichever occurs first)
#
#' @param ... additional named arguments to be passed to \code{f}
#'
#' @param p assumed constant for probability of correct responses (must be > 0.5)
#'
#' @param maxiter the maximum number of iterations (default 300)
#'
#' @param miniter minimum number of iterations (default 100)
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
#' Waeber, R., Frazier, P. I. & Henderson, S. G. (2013). Bisection Search
#' with Noisy Responses. SIAM Journal on Control and Optimization,
#' Society for Industrial & Applied Mathematics (SIAM), 51, 2261-2279.
#'
#' @export
#'
#' @seealso \code{\link{uniroot}}, \code{\link{RobbinsMonro}}
#'
#' @examples
#'
#' # find x that solves f(x) - b = 0 for the following
#' f.root <- function(x, b = .6) 1 / (1 + exp(-x)) - b
#' f.root(.3)
#'
#' xs <- seq(-3,3, length.out=1000)
#' plot(xs, f.root(xs), type = 'l', ylab = "f(x)", xlab='x', las=1)
#' abline(h=0, col='red')
#'
#' retuni <- uniroot(f.root, c(0,1))
#' retuni
#' abline(v=retuni$root, col='blue', lty=2)
#'
#' # PBA without noisy root
#' retpba <- PBA(f.root, c(0,1))
#' retpba
#' retpba$root
#' plot(retpba)
#' plot(retpba, type = 'history')
#'
#' # Same problem, however root function is now noisy. Hence, need to solve
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
#' \dontrun{
#' # ignore termination criteria and instead run for 30 seconds or 30000 iterations
#' retpba.noise_30sec <- PBA(f.root_noisy, c(0,1), wait.time = "0:30", maxiter=30000)
#' retpba.noise_30sec
#'
#' }
#'
PBA <- function(f.root, interval, ..., p = .6,
                integer = FALSE, tol = if(integer) .01 else .0001,
                maxiter = 300L, miniter = 100L, wait.time = NULL,
                f.prior = NULL, resolution = 10000L,
                check.interval = TRUE, check.interval.only = FALSE,
                verbose = TRUE){

    if(maxiter < miniter) maxiter <- miniter
    if(!is.null(wait.time))
        wait.time <- timeFormater(wait.time)
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
        if(integer && !is.null(.SIMDENV$FromSimSolve) && .SIMDENV$FromSimSolve$bolster){
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
        min.total.reps <- FromSimSolve$min.total.reps
        tol <- FromSimSolve$tol
        rel.tol <- FromSimSolve$rel.tol
        control <- FromSimSolve$control
        # robust <- FromSimSolve$robust
        predCI <- FromSimSolve$predCI
        predCI.tol <- FromSimSolve$predCI.tol
        if(!is.null(predCI.tol)){
            tol <- predCI.tol
            rel.tol <- 0
        }
        interpolate.burnin <- FromSimSolve$interpolate.burnin
        glmpred.last <- glmpred <- c(NA, NA)
        k.success <- FromSimSolve$k.success
        k.successes <- 0L
    } else{
        interpolate <- FALSE
        interpolate.burnin <- NULL
    }
    x <- if(integer) interval[1L]:interval[2L]
        else seq(interval[1L], interval[2L], length.out=resolution[1L])
    fx <- if(is.null(f.prior)) rep(1, length(x)) else f.prior(x, ...)
    fx <- log(fx / sum(fx)) # in log units
    medhistory <- roothistory <- numeric(maxiter)
    e.froot <- NA
    start_time <- proc.time()[3L]

    if(check.interval){
        if(!is.null(FromSimSolve)){
            upper <- bool.f(f.root=f.root, interval[2L], replications=replications[1L],
                            store = FALSE, ...)
            lower <- bool.f(f.root=f.root, interval[1L], replications=replications[1L],
                            store = FALSE, ...)
        } else {
            upper <- bool.f(f.root=f.root, interval[2L], ...)
            lower <- bool.f(f.root=f.root, interval[1L], ...)
        }
        no_root <- (upper[1L] + lower[1L]) != 1L
        if(no_root){
            msg <- sprintf(paste0('supplied interval range appears to be %s the probable root.',
                           '\nResulting root estimates were [%.3f, %.3f]'),
                           ifelse(upper[1L] == 0, '*above*', '*below*'),
                           lower[2L], upper[2L])
            old.opts <- options()
            options(warn=1)
            warning(msg, call.=FALSE)
            options(old.opts)
        }
        if(check.interval.only) return(!no_root)
    }
    glmpred.converged <- FALSE
    iter <- 0L

    while(TRUE){
        iter <- iter + 1L
        med <- getMedian(fx, x)
        if(!is.null(FromSimSolve)){
            if(integer && iter > 6L){
                tmp <- if(replications[iter] == max(replications))
                    medhistory[iter:(iter-3L)-1L] else medhistory[iter:(iter-5L)-1L]
                if(length(unique(tmp)) == 2L && (max(tmp) - min(tmp)) == 2L)
                    med <- max(tmp) - 1L
            }
            if(replications[iter] == max(replications) && integer && iter > 4L){
                tmp <- medhistory[iter:(iter-3L)-1L]
                if(length(unique(tmp)) == 2L && (max(tmp) - min(tmp)) == 2L)
                    med <- max(tmp) - 1L
            }
        }
        medhistory[iter] <- med
        feval <- if(!is.null(FromSimSolve))
            bool.f(f.root=f.root, med, replications=replications[iter], ...)
        else bool.f(f.root=f.root, med, ...)
        z <- feval[1]
        roothistory[iter] <- feval[2]
        if(z){
            fx <- fx + ifelse(x >= med, logp, logq)
        } else {
            fx <- fx + ifelse(x >= med, logq, logp)
        }
        w <- if(!is.null(FromSimSolve)) 1/sqrt(replications) else rep(1/iter, iter)
        e.froot <- sum(roothistory[iter:1L] * w[iter:1] / sum(w[iter:1]))

        if(interpolate && iter > interpolate.after && iter > interpolate.burnin){
            SimSolveData <- SimSolveData(burnin=interpolate.burnin,
                                         full=!control$summarise.reg_data)
            # SimMod <- if(robust)
            #     suppressWarnings(robustbase::glmrob(formula = formula,
            #                          data=SimSolveData, family=family))
            #     else
            SimMod <- try(suppressWarnings(glm(formula = formula,
                                               data=SimSolveData, family=family,
                                               weights=weights)), silent=TRUE)
            glmpred <- glmpred0 <- if(is(SimMod, 'try-error')){
                c(NA, NA)
            } else {
                suppressWarnings(SimSolveUniroot(SimMod=SimMod,
                                                 b=dots$b,
                                                 interval=quantile(medhistory[medhistory != 0],
                                                                   probs = c(.05, .95)),
                                                 max.interval=interval,
                                                 median=med, CI=if(!is.null(predCI.tol)) predCI else NULL))
            }
            if(!is.null(predCI.tol)){
                glmpred[1L] <- glmpred[2L]
                glmpred.last[1L] <- glmpred[3L]
            }
            if(is.na(glmpred[1L])){
                glmpred.converged <- FALSE
                glmpred0[1L] <- med
            }

            # Should termination occur early when this changes very little?
            if(!any(is.na(c(glmpred[1L], glmpred.last[1L])))){
                abs_diff <- abs(glmpred.last[1L] - glmpred[1L])
                if(!is.null(predCI.tol)){
                    if(glmpred0[2L] < (dots$b - predCI.tol/2)) abs_diff <- tol*2
                    if(glmpred0[3L] > (dots$b + predCI.tol/2)) abs_diff <- tol*2
                }
                rel_diff <- abs_diff / abs(glmpred.last[1L])
                if(abs_diff <= tol || rel_diff <= rel.tol){
                    k.successes <- k.successes + 1L
                    if(k.successes == k.success && is.null(wait.time)) break
                } else {
                    k.successes <- max(c(k.successes - 1L, 0L))
                }
            } else k.successes <- 0L
            if(sum(replications[1L:iter]) < min.total.reps)
                k.successes <- 0L
            glmpred.last <- glmpred
        }
        if(is.null(wait.time))
            if(!interpolate && abs(e.froot) < tol && iter > miniter) break

        if(verbose){
            if(integer)
                cat(sprintf("\rIter: %i; Median = %i", iter, med))
            else cat(sprintf("\rIter: %i; Median = %.3f", iter, med))
            cat(sprintf("; E(f(x)) = %.2f", abs(e.froot)))
            if(!is.null(FromSimSolve))
                cat('; Total.reps =', sum(replications[1L:iter]))
            if(interpolate && iter > interpolate.after && !is.na(glmpred[1L]))
                cat(sprintf(paste0('; k.tol = %i; Pred = %',
                                   if(integer) ".1f" else ".3f"),
                            k.successes, glmpred0[1L]))
            utils::flush.console()
        }

        if(!is.null(wait.time))
            if(proc.time()[3L] - start_time > wait.time) break

        if(iter == maxiter) break
    }
    converged <- iter < maxiter
    predCIs <- c(NA, NA, NA)
    predCIs_root <- c(NA, NA)
    if(!is.null(FromSimSolve)){
        names(predCIs_root) <- paste0('CI_', predCI*100)
        predCIs <- SimSolveUniroot(SimMod=SimMod, b=dots$b,
                               interval=quantile(medhistory[medhistory != 0],
                                                 probs = c(.05, .95)),
                               max.interval=interval,median=med, CI=predCI)
        predCIs_root[1L] <- SimSolveUniroot(SimMod=SimMod, b=predCIs[2L],
                                            interval=c(min(medhistory[medhistory != 0]), predCIs[1]),
                                            max.interval=c(interval[1], predCIs[1]), median=med, CI=predCI)[1L]
        predCIs_root[2L] <- SimSolveUniroot(SimMod=SimMod, b=predCIs[3L],
                                            interval=c(predCIs[1], max(medhistory[medhistory != 0])),
                                            max.interval=c(predCIs[1], interval[2]), median=med, CI=predCI)[1L]
        if(any(is.na(predCIs_root[1:2]))){
            SimMod2 <- try(suppressWarnings(glm(formula = y~x,
                                                data=SimSolveData, family=family,
                                                weights=weights)), silent=TRUE)
            if(is.na(predCIs_root[1]))
                predCIs_root[1L] <- SimSolveUniroot(SimMod=SimMod2, b=predCIs[2L],
                                                    interval=c(min(medhistory[medhistory != 0]), predCIs[1]),
                                                    max.interval=c(interval[1], predCIs[1]), median=med, CI=predCI)[1L]
            if(is.na(predCIs_root[2]))
                predCIs_root[2L] <- SimSolveUniroot(SimMod=SimMod2, b=predCIs[3L],
                                                    interval=c(predCIs[1], max(medhistory[medhistory != 0])),
                                                    max.interval=c(predCIs[1], interval[2]), median=med, CI=predCI)[1L]
        }
    }
    if(verbose)
        cat("\n")
    fx <- exp(fx) / sum(exp(fx)) # normalize final result
    medhistory <- medhistory[1L:(iter-1L)]
    # BI <- belief_interval(x, fx, CI=CI)
    root <- if(!interpolate) medhistory[length(medhistory)] else glmpred0[1L]
    ret <- list(iter=iter, root=root, terminated_early=converged, integer=integer,
                e.froot=e.froot, x=x, fx=fx, medhistory=medhistory,
                time=as.numeric(proc.time()[3L]-start_time),
                burnin=interpolate.burnin, b=dots$b,
                predCIs=predCIs[-1L], predCIs_root=predCIs_root)
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
              terminated_early=terminated_early,
              time=noquote(timeFormater_internal(time)),
              iterations = iter))
    if(!all(is.na(x$predCIs)))
        out <- append(out, list(predCI.root = x$predCIs_root,
                                b = x$b,
                                predCI.b = x$predCIs), 1L)
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
                     ylab = 'density', las=1, ...))
    } else if(type == 'history'){
        with(x, plot(medhistory,
                     main = 'Median history', type = 'b',
                     ylab = 'Median Estimate',
                     xlab = 'Iteration', pch = 16, las=1, ...))
    }

}

getMedian <- function(fx, x){
    expfx <- exp(fx - max(fx))  # on original pdf
    expfx <- expfx / sum(expfx)
    alpha <- .5
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
