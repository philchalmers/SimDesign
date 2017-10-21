#' Generate a random set of values within a truncated range
#'
#' Function generates data given a supplied random number generating function that
#' are constructed to fall within a particular range. Sampled values outside this range
#' are discarded and re-sampled until the desired criteria has been met.
#'
#' In simulations it is often useful to draw numbers from truncated distributions
#' rather than across the full theoretical range. For instance, sampling parameters within
#' the range [-4,4] from a normal distribution. The \code{rtruncate} function has been
#' designed to accept any sampling function, where the first argument is the number of
#' values to sample, and will draw values iteratively until the number of values
#' within the specified bound are obtained. In situations where it is unlikely for the bounds
#' to be located (e.g., sampling from a normal distribution where all values are within [-10,-6])
#' then the sampling scheme will throw an error if too many re-sampling executions are required
#' (default will stop if more that 100 calls to \code{rfun} are required).
#'
#' @param n number of observations to generate. This should be the first argument passed
#'   to \code{rfun}
#'
#' @param range a numeric vector of length two, where the first element indicates the
#'   lower bound and the second the upper bound. When values are generated outside these
#'   two bounds then data are redrawn until the bounded criteria is met. When the
#'   output of \code{rfun} is a matrix then this input can be specified as a matrix with
#'   two rows, where each the first row corresponds to the lower bound and the second row
#'   the upper bound for each generated column in the output
#'
#' @param rfun a function to generate random values. Function can return
#'   a numeric/integer vector or matrix, and additional arguments
#'   requred for this function are passed through the argument \code{...}
#'
#' @param ... additional arguments to be passed to \code{rfun}
#'
#' @param redraws the maximum number of redraws to take before terminating the iterative
#'   sequence. This is in place as a safety in case the \code{range} is too small given the
#'   random number generator, causing too many consecutive rejections. Default is 100
#'
#' @return either a numeric vector or matrix, where all values are within the
#'   desired \code{range}
#'
#' @seealso \code{\link{runSimulation}}
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \url{http://www.tandfonline.com/doi/full/10.1080/10691898.2016.1246953}
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @export
#'
#' @examples
#'
#' # n = 1000 truncated normal vector between [-2,3]
#' vec <- rtruncate(1000, rnorm, c(-2,3))
#' summary(vec)
#'
#' # truncated correlated multivariate normal between [-1,4]
#' mat <- rtruncate(1000, rmvtnorm, c(-1,4),
#'    sigma = matrix(c(2,1,1,1),2))
#' summary(mat)
#'
#' # truncated correlated multivariate normal between [-1,4] for the
#' #  first column and [0,3] for the second column
#' mat <- rtruncate(1000, rmvtnorm, cbind(c(-1,4), c(0,3)),
#'    sigma = matrix(c(2,1,1,1),2))
#' summary(mat)
#'
#' # truncated chi-square with df = 4 between [2,6]
#' vec <- rtruncate(1000, rchisq, c(2,6), df = 4)
#' summary(vec)
#'
rtruncate <- function(n, rfun, range, ..., redraws = 100){
    ret <- rfun(n, ...)
    stopifnot(is.numeric(ret))
    is_matrix <- is.matrix(ret)
    if(is_matrix && !is.matrix(range))
        range <- matrix(range, nrow=2L, ncol=ncol(ret))
    draw <- 1L
    while(draw < redraws){
        if(is_matrix){
            pick <- sapply(1L:ncol(ret), function(i){
                ret[,i] > range[2L, i] | ret[,i] < range[1L, i]
            })
            pick <- rowSums(pick) > 0L
            if(!any(pick)) break
            ret[pick, ] <- rfun(sum(pick), ...)
        } else {
            pick <- ret > range[2L] | ret < range[1L]
            if(!any(pick)) break
            ret[pick] <- rfun(sum(pick), ...)
        }
        draw <- draw + 1L
    }
    if(draw == redraws)
        stop("rtruncate() redrew data too often and was terminated", call.=FALSE)
    ret
}
