#' Empirical detection robustness method suggested by Serlin (2000)
#'
#' Hypothesis test to determine whether an observed empirical detection rate,
#' coupled with a given robustness interval, statistically differs from the
#' population value. Uses the methods described by Serlin (2000) as well to
#' generate critical values (similar to confidence intervals, but define a fixed
#' window of robustness).
#'
#' @param p a vector containing the empirical detection rate(s) to be tested
#'
#' @param R number of replications used in the simulation
#'
#' @param alpha Type I error rate (e.g., often set to .05)
#'
#' @param delta symmetric robustness interval around \code{alpha} (e.g., a value
#'   of .01 when \code{alpha = .05} would test the robustness window .04-.06)
#'
#' @param CI confidence interval for \code{alpha} as a proportion. Default of 0.95
#'   indicates a 95\% interval
#'
#' @references
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
Serlin2000 <- function(p, alpha, delta, R, CI = .95){
    sigma2 <- alpha * (1 - alpha) / R
    crit <- (abs(p - alpha) - delta) / sqrt(sigma2)
    crit_p <- pnorm(abs(crit), lower.tail = FALSE) # one-tailed test
    robust <- ifelse(crit_p > .1, "no",
                     ifelse(crit_p > .05, "maybe", "yes"))
    z_crit <- abs(qnorm((1 - CI)/2))
    fn <- function(val){
        crit <- abs(val - alpha) / sqrt(val * (1 - val) / R)
        crit - z_crit
    }
    CV1 <- try(uniroot(fn, interval = c(0, alpha))$root, TRUE)
    if(is(CV1, 'try-error')) CV1 <- NA
    z_crit <- abs(qnorm((1 - CI)))
    CV2 <- try(uniroot(fn, interval = c(alpha, 1 - 1e-10))$root, TRUE)
    if(is(CV2, 'try-error')) CV2 <- NA
    ret <- data.frame("p" = p,
                      "z(|p-a| - d))" = crit,
                      "Pr(>|z|)" = crit_p, check.names = FALSE,
                      "robust" = robust,
                      "CV1" = CV1,
                      "CV2" = CV2)
    ret
}
