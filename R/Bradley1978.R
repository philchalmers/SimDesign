#' Bradley's (1978) empirical robustness interval
#'
#' Robustness interval criteria for empirical detection rate estimates defined
#' by Bradley (1978). See \code{\link{EDR}} to obtain such estimates.
#'
#' @param p (optional) numeric vector containing the empirical detection
#'   rate(s) estimates. If supplied a character vector with elements defined in
#'   \code{out.labels} or a logical vector will be returned indicating whether the
#'   detection rate estimate is considered 'robust'. If missing, the
#'   interval criteria will be printed to the console
#'
#' @param alpha Type I error rate to evaluated (default is .05)
#'
#' @param type character vector indicating the type of interval classification to use.
#'   Default is 'liberal', however can be 'stringent' to use Bradley's more
#'   stringent robustness criteria
#'
#' @param out.logical logical; should the output vector be TRUE/FALSE indicating whether
#'   the supplied empirical detection rate should be considered "robust"? Default is
#'   FALSE, in which case the out.labels elements are used instead
#'
#' @param out.labels character vector of length three indicating the classification
#'   labels according to the desired robustness interval
#'
#' @param unname logical; apply \code{\link{unname}} to the results to remove any variable
#'   names?
#'
#' @seealso \code{\link{EDR}}, \code{\link{Serlin2000}}
#'
#' @references
#'
#' Bradley, J. V. (1978). Robustness? \emph{British Journal of Mathematical and
#' Statistical Psychology, 31}, 144-152.
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
#'
#' # interval criteria used
#' Bradley1978()
#' Bradley1978(type = 'stringent')
#' Bradley1978(alpha = .01, type = 'stringent')
#'
#' # intervals applied to empirical detection rate estimates
#' edr <- c(test1=.05, test2=.027, test3=.051, test4=.076, test5=.024)
#'
#' Bradley1978(edr)
#' Bradley1978(edr, out.logical=TRUE) # is robust?
#'
Bradley1978 <- function(p, alpha = .05, type = 'liberal', unname = FALSE,
                        out.logical = FALSE,
                        out.labels = c('conservative', 'robust', 'liberal')){
    stopifnot(type %in% c('liberal', 'stringent'))
    stopifnot(length(alpha) == 1L)
    stopifnot(alpha <= 1 && alpha >= 0)
    if(type == 'stringent') bnds <- c(0.9, 1.1)
    if(type == 'liberal') bnds <- c(.5, 1.5)
    bounds <- bnds * alpha
    if(missing(p)){
        ret <- bounds
        names(ret) <- paste0(type, c('.lower', '.upper'))
    } else {
        if(is.data.frame(p) || is.matrix(p)) p <- as.numeric(p)
        stopifnot(all(p <= 1 & p >= 0))
        if(out.logical){
            ret <- p >= bounds[1L] & p <= bounds[2]
        } else {
            ret <- rep(out.labels[2], length(p))
            names(ret) <- names(p)
            ret[p < bounds[1L]] <- out.labels[1L]
            ret[p > bounds[2L]] <- out.labels[3L]
        }
        if(unname) ret <- unname(ret)
    }
    ret
}
