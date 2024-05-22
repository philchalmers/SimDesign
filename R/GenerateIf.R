#' Perform a test that indicates whether a given \code{Generate()} function should be executed
#'
#' This function is designed to prevent specific generate function executions when the
#' design conditions are not met. Primarily useful when the \code{generate} argument to
#' \code{\link{runSimulation}} was input as a named list object, however should only be
#' applied for some specific design condition (otherwise, the data generation moves to the
#' next function in the list).
#'
#' @param x logical statement to evaluate. If the statement evaluates to \code{TRUE}
#'   then the remainder of the defined function will be evaluated
#'
#' @param condition (optional) the current design condition. This does not need to be supplied
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
#' # SimFunctions(nGenerate = 2)
#'
#' Design <- createDesign(N=c(10,20,30), var.equal = c(TRUE, FALSE))
#'
#' Generate.G1 <- function(condition, fixed_objects) {
#'   GenerateIf(condition$var.equal == FALSE) # only run when unequal vars
#'   Attach(condition)
#'   dat <- data.frame(DV = c(rnorm(N), rnorm(N, sd=2)),
#'                     IV = gl(2, N, labels=c('G1', 'G2')))
#'   dat
#' }
#'
#' Generate.G2 <- function(condition, fixed_objects) {
#'   Attach(condition)
#'   dat <- data.frame(DV = rnorm(N*2), IV = gl(2, N, labels=c('G1', 'G2')))
#'   dat
#' }
#'
#' # always run this analysis for each row in Design
#' Analyse <- function(condition, dat, fixed_objects) {
#'   mod <- t.test(DV ~ IV, data=dat)
#'   mod$p.value
#' }
#'
#' Summarise <- function(condition, results, fixed_objects) {
#'   ret <- EDR(results, alpha=.05)
#'   ret
#' }
#'
#' #-------------------------------------------------------------------
#'
#' # append names 'Welch' and 'independent' to associated output
#' res <- runSimulation(design=Design, replications=1000,
#'                      generate=list(G1=Generate.G1, G2=Generate.G2),
#'                      analyse=Analyse,
#'                      summarise=Summarise)
#' res
#'
#' }
#'
GenerateIf <- function(x, condition = NULL){
    e <- substitute(x)
    r <- eval(e, condition, parent.frame())
    if (!is.logical(r) || length(r) != 1L)
        stop("GenerateIf must return a single logical value", call.=FALSE)
    if(!isTRUE(r))
        stop('GENERATEIF RAISED ERROR', call.=FALSE)
    invisible(NULL)
}
