#' Rejection sampling (i.e., accept-reject method)
#'
#' This function supports the rejection sampling (i.e., accept-reject) approach
#' to drawing values from seemingly difficult (probability) density functions
#' by sampling values from more manageable proxy distributions.
#'
#' The accept-reject algorithm is a flexible approach to obtaining i.i.d.'s from
#' a difficult to sample from (probability) density function  where either the
#' transformation method fails or inverse transform method is
#' difficult to manage. The algorithm does so by sampling from
#' a more "well-behaved" proxy distribution (with identical support, up to some
#' proportionality constant \code{M} that reshapes the proposal density
#' to envelope the target density), and accepts the
#' draws if they are likely within the target density. Hence, the closer the
#' shape of \code{dg(x)} is to the desired \code{df(x)}, the more likely the draws
#' are to be accepted; otherwise, many iterations of the accept-reject algorithm
#' may be required, which decreases the computational efficiency.
#'
#' @param n number of samples to draw
#'
#' @param df the desired (potentially un-normed) density function to draw
#'   independent samples from. Must be in the form of a \code{function} with a
#'   single input corresponding to the values sampled from \code{rg}. Function
#'   is assumed to be vectorized (if not, see \code{\link{Vectorize}})
#'
#' @param dg the proxy (potentially un-normed) density function to
#'   draw samples from in lieu of drawing samples from \code{df}.
#'   The support for this density function should be the same as \code{df}
#'   (i.e., when \code{df(x) > 0} then \code{dg(x) > 0}).
#'   Must be in the form of a \code{function} with a single input
#'   corresponding to the values sampled from \code{rg}. Function is
#'   assumed to be vectorized (if not, see \code{\link{Vectorize}})
#'
#' @param rg the proxy random number generation function, associated with
#'   \code{dg}, used to draw proposal samples from.
#'   Must be in the form of a \code{function} with a single input corresponding
#'   to the number of values to draw, while the output can either be a vector
#'   or a matrix (if a matrix, each independent observation must be stored in
#'   a unique row). Function is assumed to be vectorized (if not, see
#'   \code{\link{Vectorize}})
#'
#' @param M the upper-bound of the ratio of probability density functions to help
#'   minimize the number of discarded draws and define the corresponding
#'   rescaled proposal envelope. When missing, \code{M} is computed
#'   internally by finding a reasonable maximum of \code{log(df(x)) - log(dg(x))},
#'   and this value is returned to the console.
#'   When both \code{df} and \code{dg} are true probability density functions
#'   (i.e., integrate to 1) the acceptance probability is equal to 1/M
#'
#' @param interval when M is missing, for univariate density function draws,
#'   the interval to search within via \code{\link{optimize}}.
#'   If not specified, a sample of 5000 values from the \code{rg}
#'   function definition will be
#'   collected, and the min/max will be obtained via this random sample
#'
#' @param method when M is missing, the optimization of M is done either by
#'   finding the mode of the log-density values (\code{"optimize"}) or by
#'   using the "Empirical Supremum Rejection Sampling" method (\code{"ESRS"})
#'
#' @param logfuns logical; have the \code{df} and \code{dg} function been
#'   written so as to return log-densities instead of the original densities?
#'   The FALSE default assumes the original densities are returned
#'   (use TRUE when higher accuracy is required when generating each density
#'   definition)
#'
#' @param maxM logical; if when optimizing M the value is greater than this
#'   cut-off then stop; ampler would likelihood be too efficient,
#'   or optimization is failing
#'
#' @param parstart starting value vector for optimization of M in
#'   multidimensional distributions
#'
#' @param ESRS_Mstart starting M value for the ESRS algorithm
#'
#' @return returns a vector or matrix of draws (corresponding to the
#'   output class from \code{rg}) from the desired \code{df}
#'
#' @references
#'
#' Caffo, B. S., Booth, J. G., and Davison, A. C. (2002). Empirical supremum
#'   rejection sampling. \code{Biometrika}, 89, 745--754.
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable
#'   Monte Carlo Simulations with the SimDesign Package.
#'   \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#'   \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics
#'   with Monte Carlo simulation. \code{Journal of Statistics Education, 24}(3),
#'   136-156. \doi{10.1080/10691898.2016.1246953}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' # Generate X ~ beta(a,b), where a and b are a = 2.7 and b = 6.3,
#' # and the support is Y ~ Unif(0,1)
#' dfn <- function(x) dbeta(x, shape1 = 2.7, shape2 = 6.3)
#' dgn <- function(x) dunif(x, min = 0, max = 1)
#' rgn <- function(n) runif(n, min = 0, max = 1)
#'
#' # when df and dg both integrate to 1, acceptance probability = 1/M
#' M <- rejectionSampling(df=dfn, dg=dgn, rg=rgn)
#' M
#' dat <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, M=M)
#' hist(dat, 100)
#' hist(rbeta(10000, 2.7, 6.3), 100) # compare
#'
#' # obtain empirical estimate of M via ESRS method
#' M <- rejectionSampling(1000, df=dfn, dg=dgn, rg=rgn, method='ESRS')
#' M
#'
#' # generate using better support function (here, Y ~ beta(2,6)),
#' #   and use log setup in initial calls (more numerically accurate)
#' dfn <- function(x) dbeta(x, shape1 = 2.7, shape2 = 6.3, log = TRUE)
#' dgn <- function(x) dbeta(x, shape1 = 2, shape2 = 6, log = TRUE)
#' rgn <- function(n) rbeta(n, shape1 = 2, shape2 = 6)
#' M <- rejectionSampling(df=dfn, dg=dgn, rg=rgn, logfuns=TRUE) # better M
#' M
#' ## Alternative estimation of M
#' ## M <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, logfuns=TRUE,
#' ##                        method='ESRS')
#' dat <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, M=M, logfuns=TRUE)
#' hist(dat, 100)
#'
#' #------------------------------------------------------
#' # sample from wonky (and non-normalized) density function, like below
#' dfn <- function(x){
#'     ret <- numeric(length(x))
#'     ret[x <= .5] <- dnorm(x[x <= .5])
#'     ret[x > .5] <-  dnorm(x[x > .5]) + dchisq(x[x > .5], df = 2)
#'     ret
#' }
#' y <- seq(-5,5, length.out = 1000)
#' plot(y, dfn(y), type = 'l', main = "Function to sample from")
#'
#' # choose dg/rg functions that have support within the range [-inf, inf]
#' rgn <- function(n) rnorm(n, sd=4)
#' dgn <- function(x) dnorm(x, sd=4)
#'
#' ## example M height from above graphic
#' ##  (M selected using ESRS to help stochastically avoid local mins)
#' M <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, method='ESRS')
#' M
#' lines(y, dgn(y)*M, lty = 2)
#' dat <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, M=M)
#' hist(dat, 100, prob=TRUE)
#'
#' # true density (normalized)
#' C <- integrate(dfn, -Inf, Inf)$value
#' ndfn <- function(x) dfn(x) / C
#' curve(ndfn, col='red', lwd=2, add=TRUE)
#'
#'
#' #-----------------------------------------------------
#' # multivariate distribution
#' dfn <- function(x) sum(log(c(dnorm(x[1]) + dchisq(x[1], df = 5),
#'                    dnorm(x[2], -1, 2))))
#' rgn <- function(n) c(rnorm(n, sd=3), rnorm(n, sd=3))
#' dgn <- function(x) sum(log(c(dnorm(x[1], sd=3), dnorm(x[1], sd=3))))
#'
#' # M <- rejectionSampling(df=dfn, dg=dgn, rg=rgn, logfuns=TRUE)
#' dat <- rejectionSampling(5000, df=dfn, dg=dgn, rg=rgn, M=4.6, logfuns=TRUE)
#' hist(dat[,1], 30)
#' hist(dat[,2], 30)
#' plot(dat)
#'
#' }
#'
rejectionSampling <- function(n, df, dg, rg, M, method = 'optimize',
                              interval = NULL, logfuns = FALSE,
                              maxM = 1e5, parstart = rg(1L),
                              ESRS_Mstart = 1.0001) {
    stopifnot(!missing(rg))
    stopifnot(!missing(dg))
    stopifnot(!missing(df))
    stopifnot(tolower(method) %in% c("optimize", 'esrs'))
    ESRS <- tolower(method) == 'esrs' && missing(M)
    npar <- length(parstart)
    multipar <- npar > 1L
    if(logfuns) log.df_dg <- function(y) df(y) - dg(y)
    else log.df_dg <- function(y) log(df(y)) - log(dg(y))

    if(missing(M) && !ESRS){
        if(is.null(interval) && npar == 1L){
            tmp <- rg(5000L)
            interval <- c(min(tmp), max(tmp))
            rm(tmp)
        }
        logM <- if(!multipar){
            try(optimize(log.df_dg, interval = interval,
                                 maximum = TRUE)$objective, TRUE)
        } else {
            try(optim(par = parstart, log.df_dg,
                              control = list(fnscale = -1))$value, TRUE)
        }
        if(is(logM, "try-error"))
            stop(c("Optimizer could not find suitable maximum for M input. ",
                   "Please explicitly provide a value for M"))
        return(exp(logM))
    }
    logM <- if(ESRS) log(ESRS_Mstart) else log(M)

    stopifnot(exp(logM) < maxM)
    stopifnot(!missing(n))
    res <- if(multipar) matrix(0, nrow = n, ncol = npar) else numeric(n)
    n.remaining <- n
    lowest <- 1L
    if(ESRS) iter <- 0L
    while_iter <- 0L

    while(n.remaining != 0L) {
        while_iter <- while_iter + 1L
        if(while_iter == 30L)
            if(n.remaining == n)
                stop("No sucessful draws computed after 30 iterations")
        y <- rg(n.remaining)
        u <- runif(n.remaining, 0, 1)
        pick <- if(multipar){
            y <- matrix(y, ncol = npar)
            log_diff <-
                apply(y, MARGIN = 1L,
                      function(y) log.df_dg(y) - logM)
            log(u) <= log_diff
        } else {
            log_diff <- log.df_dg(y) - logM
            log(u) <= log_diff
        }
        sumpick <- sum(pick)
        if(sumpick >= 1L){
            if(multipar){
                whc <- which(pick)
                for(i in lowest:(lowest + sumpick - 1L))
                    res[i, ] <- y[whc[i - lowest + 1L], ]
            } else res[lowest:(lowest + sumpick - 1L)] <- y[pick]
            lowest <- lowest + sumpick
            n.remaining <- n - lowest
        }
        if(ESRS){
            iter <- iter + 1L
            if(iter > 100)
                stop('ESRS estimate not stable. Final value of M: ',
                     round(exp(logM), 3))
            logM <- max(c(logM, log_diff + logM))
            stopifnot(exp(logM) < maxM)
        }
    }
    if(ESRS) return(exp(logM))
    res
}
