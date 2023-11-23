#' Robbins-Monro (1951) stochastic root-finding algorithm
#'
#' Function performs stochastic root solving for the provided \code{f(x)}
#' using the Robbins-Monro (1951) algorithm. Differs from deterministic
#' cousins such as \code{\link{uniroot}} in that \code{f} may contain stochastic error
#' components, where the root is obtained through the running average method
#' provided by noise filter (see also \code{\link{PBA}}).
#' Assumes that \code{E[f(x)]} is non-decreasing in \code{x}.
#'
#' @param f noisy function for which the root is sought
#'
#' @param p vector of starting values to be passed as \code{f(p, ...)}
#'
#' @param tol tolerance criteria for convergence on the changes in the
#'   updated \code{p} elements. Must be achieved on \code{k} (default 3)
#'   successive occasions
#
#' @param ... additional named arguments to be passed to \code{f}
#'
#' @param maxiter the maximum number of iterations (default 300)
#'
#' @param miniter minimum number of iterations (default 100)
#'
#' @param k number of consecutive \code{tol} criteria required before terminating
#'
#' @param fn.a function to create the \code{a} coefficient in the Robbins-Monro
#'   noise filter. Requires the first argument is the current iteration (\code{iter})
#'   and must provide one or more arguments and (optionally) the \code{...}. Note that
#'   if a different function is provided it must satisfy the property
#'   that \eqn{\sum^\infty_{i=1} a_i = \infty} and
#'   \eqn{\sum^\infty_{i=1} a_i^2 < \infty}
#'
#' @param verbose logical; should the iterations and estimate be printed to the
#'   console?
#'
#' @references
#'
#' Robbins, H. and Monro, S. (1951). A stochastic approximation method. Ann.Math.Statistics,
#'   22:400-407.
#'
#' @export
#'
#' @seealso \code{\link{uniroot}}, \code{\link{PBA}}
#'
#' @examples
#'
#' # find x that solves f(x) - b = 0 for the following
#' f.root <- function(x, b = .6) 1 / (1 + exp(-x)) - b
#' f.root(.3)
#'
#' xs <- seq(-3,3, length.out=1000)
#' plot(xs, f.root(xs), type = 'l', ylab = "f(x)", xlab='x')
#' abline(h=0, col='red')
#'
#' retuni <- uniroot(f.root, c(0,1))
#' retuni
#' abline(v=retuni$root, col='blue', lty=2)
#'
#' # Robbins-Monro without noisy root, start with p=.9
#' retrm <- RobbinsMonro(f.root, .9)
#' retrm
#' plot(retrm)
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
#' # Robbins-Monro provides better convergence
#' retrm.noise <- RobbinsMonro(f.root_noisy, .9)
#' retrm.noise
#' plot(retrm.noise)
#'
#' # different power (b) for fn.a()
#' retrm.b2 <- RobbinsMonro(f.root_noisy, .9, b = .1)
#' retrm.b2
#' plot(retrm.b2)
#'
RobbinsMonro <- function(f, p, ...,
                         maxiter = 300L, miniter = 100L, k = 3L,
                         tol = .00001, verbose = TRUE,
                         fn.a = function(iter, b = 1/2, ...) (1 / iter)^b)
{
    if(maxiter < miniter) maxiter <- miniter
    history <- rbind(p, matrix(NA, nrow=maxiter, ncol=length(p)))
    k.succ <- 0

    for(i in 1L:maxiter){
        a <- fn.a(iter=i, ...)
        fp <- f(p)
        p <- p - a * fp
        history[i + 1L, ] <- p
        change <- max(abs(history[i,]-p))
        if(verbose)
            cat(sprintf("\rIter: %i; Max change in p = %.3f",
                             i, change))
        if(i > miniter && all(change < tol)){
            k.succ <- k.succ + 1L
            if(k.succ == k) break
        } else k.succ <- 0L
    }

    converged <- i < maxiter
    history <- history[0L:i + 1L, , drop=FALSE]
    ret <- list(iter=i, root=p, terminated_early=converged,
                history=history)
    class(ret) <- 'RM'
    ret
}

#' @rdname RobbinsMonro
#' @param x an object of class \code{RM}
#' @export
print.RM <- function(x, ...)
{
    print(x$root)
}

#' @rdname RobbinsMonro
#' @param main plot title
#' @param par which parameter in the original vector \code{p} to include in the plot
#' @export
plot.RM <- function(x, par = 1,
                    main = paste0('Robbins-Monro history (p = ', par, ')'), ...)
{
    with(x, plot(1L:nrow(history), history[,par],
                 main = main, type = 'b',
                 ylab = 'f(p)',
                 xlab = 'Iteration', pch = 16, ...))
}
