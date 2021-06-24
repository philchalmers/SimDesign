#' Create a subset of a list from a larger named list object
#'
#' A convenience function used to create a smaller list subset from a larger named
#' list. Useful to put out commonly named objects in a larger list to create smaller,
#' more homogeneous list objects.
#'
#' @param lst the larger list object with which to create a sublist from
#'
#' @param what a character input indicating the name of the object to extract out
#'
#' @param bind_rows logical; should the returned list be collapsed into a suitable
#'   row-bind matrix object instead? Default is FALSE
#'
#' @seealso \code{\link{Summarise}}
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable
#'   Monte Carlo Simulations with the SimDesign Package.
#'   \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#'    \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics
#'   with Monte Carlo simulation. \code{Journal of Statistics Education, 24}(3),
#'   136-156. \doi{10.1080/10691898.2016.1246953}
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @export
#'
#' @examples
#'
#' # example behaviour when returning list from Analyse()
#' results <- list(list(means = rnorm(3, 10, 2),
#'                  sigma = matrix(diag(rnorm(3, mean=10)), 3)),
#'             list(means = rnorm(3, 10, 2),
#'                  sigma = matrix(diag(rnorm(3, mean=10)), 3)),
#'             list(means = rnorm(3, 10, 2),
#'                  sigma = matrix(diag(rnorm(3, mean=10)), 3)))
#'
#' str(results)
#'
#' # following code is potentially useful in a Summarise() definition
#' means <- sublist(results, 'means')
#' sigmas <- sublist(results, 'sigma')
#'
#' # meta-statistics on objects
#' popmeans <- c(M1=10, M2=10, M3=10)
#' popsigma <- diag(popmeans)
#'
#' # means
#' bias(means, popmeans)
#' RMSD(means, popmeans)
#'
#' # sigma
#' bias(sigmas, popsigmas)
#' RMSD(sigmas, popsigmas)
#'
sublist <- function(lst, what, bind_rows = FALSE){
    if(is.null(lst[[1L]][[what]]))
        stop('specied name cannot be found in list')
    ret <- lapply(lst, function(x) x[[what]])
    if(bind_rows) ret <- dplyr::bind_rows(ret)
    ret
}
