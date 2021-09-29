#' Perform a test that indicates whether a given \code{Analyse()} function should be executed
#'
#' This function is designed to prevent specific analysis function executions when the
#' design conditions are not met. Primarily useful when the \code{analyse} argument to
#' \code{\link{runSimulation}} was a input as a named list object, however some of the
#' analysis functions are not interesting/compatible with the generated data and should
#' therefore be skipped.
#'
#' @param x logical statement to evaluate. If the statement evaluates to \code{TRUE}
#'   then the remainder of the defined function will be evaluated
#'
#' @param data (optional) the current design condition. This does not need to be supplied
#'   if the expression in \code{x} evaluates to valid logical (e.g., use \code{Attach(condition)}
#'   prior to using \code{AnalyseIf}, or use \code{with(condition, AnalyseIf(someLogicalTest))})
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
#' @seealso \code{\link{Analyse}}, \code{\link{runSimulation}}
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' Design <- createDesign(N=c(10,20,30), var.equal = c(TRUE, FALSE))
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'   Attach(condition)
#'   dat <- data.frame(DV = rnorm(N*2), IV = gl(2, N, labels=c('G1', 'G2')))
#'   dat
#' }
#'
#' # always run this analysis for each row in Design
#' Analyse1 <- function(condition, dat, fixed_objects = NULL) {
#'   mod <- t.test(DV ~ IV, data=dat)
#'   mod$p.value
#' }
#'
#' # Only perform analysis when variances are equal and N = 20 or 30
#' Analyse2 <- function(condition, dat, fixed_objects = NULL) {
#'   AnalyseIf(var.equal && N %in% c(20, 30), condition)
#'   mod <- t.test(DV ~ IV, data=dat, var.equal=TRUE)
#'   mod$p.value
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
#'   ret <- EDR(results, alpha=.05)
#'   ret
#' }
#'
#' #-------------------------------------------------------------------
#'
#' # append names 'Welch' and 'independent' to associated output
#' res <- runSimulation(design=Design, replications=100, generate=Generate,
#'                      analyse=list(Welch=Analyse1, independent=Analyse2),
#'                      summarise=Summarise)
#' res
#'
#' # leave results unnamed
#' res <- runSimulation(design=Design, replications=100, generate=Generate,
#'                      analyse=list(Analyse1, Analyse2),
#'                      summarise=Summarise)
#'
#'
#' }
#'
AnalyseIf <- function(x, data = NULL){
    e <- substitute(x)
    r <- eval(e, data, parent.frame())
    if (!is.logical(r) || length(r) != 1L)
        stop("AnalyseIf must return a single logical value", call.=FALSE)
    if(!isTRUE(r))
        stop('ANALYSEIF RAISED ERROR', call.=FALSE)
    invisible(NULL)
}
