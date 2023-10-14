#' Gold section search algorithm
#'
#' Implementation of univariate gold-section search algorithm. Algorithm
#' assumes that the objective function has one unique minimum (unimodal)
#' within the specified search interval. Supports integer searches for input
#' parameter.
#'
#' @param f function to be minimized, where the first argument corresponds
#'   to the parameter of interest
#'
#' @param interval interval with which to search, specified as
#'   \code{c(lower, upper)}
#'
#' @param integer logical; should the parameters be constrained as
#'   integer values?
#'
#' @param tol convergence tolerance. Ignored when \code{integer = TRUE}
#'   (converges when lowest integer is found between competing boundaries)
#'
#' @param maxiter maximum number of iterations
#'
#' @param ... additional arguments passed to \code{f}
#'
#' @return a \code{list} of the converge and solution information
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @examples
#'
#' y <- c(10, 20, 50)
#' y <- y - mean(y)
#' x <- 1:3
#' f <- function(beta, y, x) sum( (y - beta*x)^2)
#'
#' betas <- seq(0, 10, length.out=1000)
#' fs <- sapply(betas, f, y=y, x=x)
#' plot(betas, fs, type = 'l', las=1)
#'
#' res <- gold_section(f, interval = c(0,10), y=y, x=x)
#' res
#'
#' # if beta must be an integer
#' betas <- -10:20
#' fs <- sapply(betas, f, y=y, x=x)
#' plot(betas, fs, las=1)
#'
#' res <- gold_section(f, interval = c(-10,20), integer=TRUE, y=y, x=x)
#' res
#'
gold_section <- function(f, interval, integer = FALSE,
                         tol = .001, maxiter = 100L, ...){
    if(integer) interval <- round(interval)
    xl <- interval[1L]
    xu <- interval[2L]
    stopifnot(xl < xu)
    fl <- f(xl, ...)
    fu <- f(xu, ...)

    d.c <- (sqrt(5) - 1) / 2
    d <-  d.c * (xu - xl)
    x1 <- xl + d
    x2 <- xu - d
    if(integer){
        x1 <- round(x1)
        x2 <- round(x2)
    }
    f1 <- f(x1, ...)
    f2 <- f(x2, ...)

    if(!all(f1 < c(fl, fu) | f2 < c(fl, fu)))
        stop('Function does not contain unique minimum', call.=FALSE)

    for(i in 1L:maxiter){
        if(f1 < f2){
            xl <- x2; fl <- f2
            x2 <- x1; f2 <- f1
            x1 <- xl + d.c * (xu - xl)
            if(integer) x1 <- floor(x1)
            f1 <- f(x1, ...)
        } else {
            xu <- x1; fu <- f1
            x1 <- x2; f1 <- f2
            x2 <- xu - d.c * (xu - xl)
            if(integer) x2 <- ceiling(x2)
            f2 <- f(x2, ...)
        }
        if(integer) if(abs(xu - xl) <= 1L) break
        if(abs(xu - xl) < tol) break
    }

    converged <- i < maxiter
    minimum <- mean(c(xu, xl))
    if(integer && converged)
        minimum <- c(xu, xl)[which.min(c(fu, fl))]
    objective <- f(minimum, ...)
    list(minimum=minimum, objective=objective,
         iterations=i, converged=converged)
}
