#' Rejection sampling (i.e., accept-reject method) to draw samples from difficult probability density functions
#'
#' This function supports the rejection sampling (i.e., accept-reject) approach to drawing values
#' from seemingly difficult, and potentially non-normed, probability density functions by
#' sampling values from a more manageable proxy distribution. This function is
#' optimized to work efficiently when the defined functions are vectorized; otherwise,
#' the accept-reject algorithm will loop over candidate sample-draws in isolation.
#'
#' The accept-reject algorithm is a flexible approach to obtaining i.i.d.'s from a
#' difficult to sample from probability density function (pdf) where either the
#' transformation method fails or inverse of the cumulative distribution function
#' is too difficult to manage. The algorithm does so by sampling from
#' a more "well-behaved" proxy distribution (with identical support, up to some
#' proportionality constant \code{M}), and accepts the
#' draws if they are likely within the proposed pdf. Hence, the closer the
#' shape of \code{dg(x)} is to the desired \code{df(x)}, the more likely the draws
#' are to be accepted; otherwise, many iterations of the accept-reject algorithm
#' may be required, which decreases the computational efficiency.
#'
#' @param n number of samples to draw
#'
#' @param df the desired (potentially un-normed) probability density function to
#'   draw samples from. Must be in the form of a \code{function} with a single input
#'   corresponding to the values sampled from \code{rg}
#'
#' @param dg the proxy (potentially un-normed) probability density function to
#'   draw samples from in lieu of drawing samples from \code{df}. The support for
#'   this density function should be the same as \code{df}
#'   (i.e., when \code{df(x) > 0} then \code{dg(x) > 0}).
#'   Must be in the form of a \code{function} with a single input
#'   corresponding to the values sampled from \code{rg}
#'
#' @param rg the proxy random number generation function, associated with \code{dg},
#'   used to draw samples from in lieu of drawing samples from \code{df}.
#'   Must be in the form of a \code{function} with a single input
#'   corresponding to the number of values to draw, while the output can either
#'   be a vector or a matrix (if a matrix, each independent observation must be stored in
#'   a unique row)
#'
#' @param M the upper-bound of the ratio of probability density functions to help
#'   minimize the number of discarded draws. By default, \code{M} is computed
#'   internally by finding the maximum of the ratio \code{df(x) / dg(x)} within the
#'   range [0,1]. When both \code{df} and \code{dg} are true probability density functions
#'   (i.e., integrate to 1) then the acceptance probability is equal to 1/M
#'
#' @param returnM logical; return the value of \code{M} located from the internal
#'   optimization results?
#'
#' @param vectorized logical; have the input function been vectorized (i.e., do they
#'   support a vector of input values rather than only a single sample)? This can
#'   be disabled, however it's recommended to redefine the input functions to be
#'   vectorized instead since these are more efficient when \code{n} is large or
#'   1/M is small
#'
#' @return returns a vector or matrix of draws (corresponding to the
#'   output class from \code{rg}) from the desired \code{df}
#'
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
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' # Generate X ~ beta(a,b), where a and b are a = 2.7 and b = 6.3,
#' # and the support is Y ~ Unif(0,1)
#' df <- function(x) dbeta(x, shape1 = 2.7, shape2 = 6.3)
#' dg <- function(x) dunif(x, min = 0, max = 1)
#' rg <- function(n) runif(n, min = 0, max = 1)
#'
#' dat <- rejectionSampling(10000, df=df, dg=dg, rg=rg)
#' hist(dat, 100)
#' hist(rbeta(10000, 2.7, 6.3), 100) # compare
#'
#' # when df and dg both integrate to 1, acceptance probability = 1/M
#' rejectionSampling(df=df, dg=dg, rg=rg, returnM=TRUE)
#'
#' # user supplied M. Here, M = 4, indicating 25% acceptance rate
#' dat2 <- rejectionSampling(10000, df=df, dg=dg, rg=rg, M=4)
#' hist(dat2, 100)
#'
#' # generate using better support function (here, Y ~ beta(2,6))
#' dg <- function(x) dbeta(x, shape1 = 2, shape2 = 6)
#' rg <- function(n) rbeta(n, shape1 = 2, shape2 = 6)
#' rejectionSampling(10000, df=df, dg=dg, rg=rg, returnM=TRUE) # more efficient
#' dat <- rejectionSampling(10000, df=df, dg=dg, rg=rg)
#' hist(dat, 100)
#'
#' #------------------------------------------------------
#' # sample from wonky (and non-normed) pdf, like below
#' df <- function(x){
#'     ret <- numeric(length(x))
#'     ret[x <= .5] <- dnorm(x[x <= .5])
#'     ret[x > .5] <-  dnorm(x[x > .5]) + dchisq(x[x > .5], df = 2)
#'     ret
#' }
#' y <- seq(-5,5, length.out = 1000)
#' plot(y, df(y), type = 'l', main = "pdf to sample")
#'
#' # choose dg/rg functions that have support within the range [-inf, inf]
#' rg <- function(n) rnorm(n, sd=2)
#' dg <- function(x) dnorm(x, sd=2)
#' dat <- rejectionSampling(10000, df=df, dg=dg, rg=rg)
#' hist(dat, 100, prob=TRUE)
#' lines(density(dat), col = 'red')
#'
#' # same as above, but df not vectorized (much slower)
#' df2 <- function(x){
#'     ret <- if(x <= .5) dnorm(x)
#'     else if(x > .5) dnorm(x) + dchisq(x, df = 2)
#'     ret
#' }
#' system.time(dat2 <-
#'    rejectionSampling(100000, df=df2, dg=dg, rg=rg, vectorized=FALSE))
#' system.time(dat <-
#'    rejectionSampling(100000, df=df, dg=dg, rg=rg))
#'
#' #-----------------------------------------------------
#' # multivariate distribution
#' df <- function(x) prod(c(dnorm(x[1]) + dchisq(x[1], df = 5),
#'                    dnorm(x[2], -1, 2)))
#' rg <- function(n) c(rnorm(n, sd=3), rnorm(n, sd=3))
#' dg <- function(x) prod(c(dnorm(x[1], sd=3), dnorm(x[1], sd=3)))
#'
#' dat <- rejectionSampling(5000, df=df, dg=dg, rg=rg, M=10)
#' hist(dat[,1], 30)
#' hist(dat[,2], 30)
#' plot(dat)
#'
#' }
#'
rejectionSampling <- function(n, df, dg, rg, M = NULL, returnM = FALSE,
                              vectorized = TRUE) {
    stopifnot(!missing(rg))
    stopifnot(!missing(dg))
    stopifnot(!missing(df))
    npar <- length(rg(1L))
    multipar <- npar > 1L
    df_dg <- function(y) df(y) / dg(y)
    if(is.null(M)){
        if(!multipar){
            M <- try(optimize(df_dg, interval = c(0,1),
                              maximum = TRUE)$objective, TRUE)
            if(is(M, "try-error"))
                stop(c("Optimizer could not find suitable maximum for M input. ",
                       "Please explicitly provide a value for M"))
            if(returnM) return(M)
        } else stop("Multivariate functions require M to be specified by the user")
    }
    stopifnot(!missing(n))
    res <- if(multipar) matrix(0, nrow = n, ncol = npar) else numeric(n)
    n.remaining <- n
    lowest <- 1L

    while(n.remaining != 0L) {
        y <- if(vectorized) rg(n.remaining) else rg(1L)
        u <- if(vectorized) runif(n.remaining, 0, 1) else runif(1L, 0, 1)
        pick <- if(multipar){
            y <- matrix(y, ncol = npar)
            u <= apply(y, MARGIN = 1L, function(y, M) df(y)/(M * dg(y)), M=M)
        } else u <= df(y)/(M * dg(y))
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
    }
    res
}
