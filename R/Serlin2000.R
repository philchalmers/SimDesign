#' Empirical detection robustness method suggested by Serlin (2000)
#'
#' Hypothesis test to determine whether an observed empirical detection rate,
#' coupled with a given robustness interval, statistically differs from the
#' population value. Uses the methods described by Serlin (2000) as well to
#' generate critical values (similar to confidence intervals, but define a fixed
#' window of robustness). Critical values may be computed without performing the simulation
#' experiment (hence, can be obtained a priori).
#'
#' @param p (optional) a vector containing the empirical detection rate(s) to be tested.
#'   Omitting this input will compute only the CV1 and CV2 values, while including this
#'   input will perform a one-sided hypothesis test for robustness
#'
#' @param R number of replications used in the simulation
#'
#' @param alpha Type I error rate (e.g., often set to .05)
#'
#' @param delta (optional) symmetric robustness interval around \code{alpha} (e.g., a value
#'   of .01 when \code{alpha = .05} would test the robustness window .04-.06)
#'
#' @param CI confidence interval for \code{alpha} as a proportion. Default of 0.95
#'   indicates a 95\% interval
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Serlin, R. C. (2000). Testing for Robustness in Monte Carlo Studies.
#' \emph{Psychological Methods, 5}, 230-240.
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
#' # Cochran's criteria at alpha = .05 (i.e., 0.5 +- .01), assuming N = 2000
#' Serlin2000(p = .051, alpha = .05, delta = .01, R = 2000)
#'
#' # Bradley's liberal criteria given p = .06 and .076, assuming N = 1000
#' Serlin2000(p = .060, alpha = .05, delta = .025, R = 1000)
#' Serlin2000(p = .076, alpha = .05, delta = .025, R = 1000)
#'
#' # multiple p-values
#' Serlin2000(p = c(.05, .06, .07), alpha = .05, delta = .025, R = 1000)
#'
#' # CV values computed before simulation performed
#' Serlin2000(alpha = .05, R = 2500)
#'
Serlin2000 <- function(p, alpha, delta, R, CI = .95){
    stopifnot(!missing(alpha))
    stopifnot(!missing(R))
    cs <- c(2, 1)
    if(alpha > .5) cs <- c(1, 2)
    z_crit <- abs(qnorm((1 - CI)/cs[1L]))
    fn <- function(val){
        crit <- abs(val - alpha) / sqrt(val * (1 - val) / R)
        crit - z_crit
    }
    CV1 <- uniroot(fn, interval = c(0, alpha))$root
    z_crit <- abs(qnorm((1 - CI)/cs[2L]))
    CV2 <- uniroot(fn, interval = c(alpha, 1 - 1e-10))$root
    if(!missing(p)){
        stopifnot(!missing(delta))
        sigma2 <- alpha * (1 - alpha) / R
        crit <- (abs(p - alpha) - delta) / sqrt(sigma2)
        crit_p <- pnorm(abs(crit), lower.tail = FALSE) # one-tailed test
        robust <- ifelse(crit_p > .1, "no",
                         ifelse(crit_p > .05, "maybe", "yes"))
        ret <- data.frame("p" = p,
                          "z(|p-a| - d))" = crit,
                          "Pr(>|z|)" = crit_p,
                          "robust" = robust,
                          "CV1" = CV1,
                          "CV2" = CV2, check.names = FALSE)
    } else {
        ret <- data.frame("CV1" = CV1, "CV2" = CV2)
    }
    ret
}
