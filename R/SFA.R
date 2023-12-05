#' Surrogate Function Approximation via the Generalized Linear Model
#'
#' Given a simulation that was executed with \code{\link{runSimulation}},
#' potentially with the argument \code{store_results} to store the
#' unsummarised analysis results, fit a surrogate function approximation (SFA)
#' model to the results and (optionally) perform a root-solving
#' step to solve a target quantity. See Schoemann et al. (2014) for details.
#'
#' @param results data returned from \code{\link{runSimulation}}. This can be
#'   the original results object or the extracted results stored when using
#'   \code{store_results = TRUE} included to store the analysis results.
#'
#' @param formula formula to specify for the regression model
#'
#' @param family character vector indicating the family of GLMs to use
#'   (see \code{\link{family}})
#'
#' @param target optional. Target quantity to use for root solving given the fitted
#'  surrogate function (e.g., find sample size associated with SFA implied power of .80)
#'
#' @param root.var character vector indicating the name of the root to solve for
#'
#' @param CI advertised confidence interval of SFA prediction around solved target
#'
#' @param ... additional arguments to pass to \code{\link{glm}}
#'
#' @seealso \code{\link{runSimulation}}, \code{\link{SimSolve}}
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Schoemann, A. M., Miller, P., Pornprasertmanit, S., and Wu, W. (2014).
#' Using Monte Carlo simulations to determine power and sample size for planned
#' missing designs. \emph{International Journal of Behavioral Development,
#' SAGE Publications, 38}, 471-479.
#'
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' # create long Design object to fit surrogate over
#' Design <- createDesign(N = 100:500,
#'                        d = .2)
#' Design
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define generate, analyse, and summarise functions
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'     Attach(condition)
#'     group1 <- rnorm(N)
#'     group2 <- rnorm(N, mean=d)
#'     dat <- data.frame(group = gl(2, N, labels=c('G1', 'G2')),
#'                       DV = c(group1, group2))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'     p <- c(p = t.test(DV ~ group, dat, var.equal=TRUE)$p.value)
#'     p
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
#'     ret <- EDR(results, alpha = .05)
#'     ret
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Estimate power over N
#'
#' # Use small number of replications given range of sample sizes
#' ## note that due to the lower replications disabling the
#' ## RAM printing will help reduce overhead
#' sim <- runSimulation(design=Design, replications=10,
#'                      generate=Generate, analyse=Analyse,
#'                      summarise=Summarise, store_results=TRUE,
#'                      control=list(print_RAM=FALSE))
#' sim
#'
#' # total of 4010 replication
#' sum(sim$REPLICATIONS)
#'
#' # use the unsummarised results for the SFA, and include p.values < alpha
#' sim_results <- SimExtract(sim, what = 'results')
#' sim_results <- within(sim_results, sig <- p < .05)
#' sim_results
#'
#' # fitted model
#' sfa <- SFA(sim_results, formula = sig ~ N)
#' sfa
#' summary(sfa)
#'
#' # plot the observed and SFA expected values
#' plot(p ~ N, sim, las=1, pch=16, main='Rejection rates with R=10')
#' pred <- predict(sfa, type = 'response')
#' lines(sim_results$N, pred, col='red', lty=2)
#'
#' # fitted model + root-solved solution given target power of .8
#' sfa.root <- SFA(sim_results, formula = sig ~ N, target = .8, root.var = "N")
#' sfa.root
#'
#' # true root
#' pwr::pwr.t.test(power=.8, d=.2)
#'
#'
#' ################
#' # example with smaller range but higher precision
#' Design <- createDesign(N = 375:425,
#'                        d = .2)
#' Design
#'
#' sim <- runSimulation(design=Design, replications=100,
#'                      generate=Generate, analyse=Analyse,
#'                      summarise=Summarise, store_results=TRUE,
#'                      control=list(print_RAM=FALSE))
#' sim
#' sum(sim$REPLICATIONS)
#'
#' # use the unsummarised results for the SFA, and include p.values < alpha
#' sim_results <- SimExtract(sim, what = 'results')
#' sim_results <- within(sim_results, sig <- p < .05)
#' sim_results
#'
#' # fitted model
#' sfa <- SFA(sim_results, formula = sig ~ N)
#' sfa
#' summary(sfa)
#'
#' # plot the observed and SFA expected values
#' plot(p ~ N, sim, las=1, pch=16, main='Rejection rates with R=10')
#' pred <- predict(sfa, type = 'response')
#' lines(sim_results$N, pred, col='red', lty=2)
#'
#' # fitted model + root-solved solution given target power of .8
#' sfa.root <- SFA(sim_results, formula = sig ~ N, target = .8, root.var = "N")
#' sfa.root
#'
#' }
#'
SFA <- function(results, formula, target = NULL, root.var = NULL,
                family = 'binomial', CI = .95, ...){
    fn <- function(x, mod, target, root.var){
        newdata <- data.frame(x)
        colnames(newdata) <- root.var
        predict(mod, newdata=newdata, type='response') - target
    }

    mod <- glm(formula, data=results, family=family, ...)
    if(is.null(target)) return(mod)

    root <- uniroot(fn, interval = c(min(results[root.var]),
                                     max(results[root.var])),
                    mod=mod, target=target, root.var=root.var)
    newdata <- data.frame(root$root)
    colnames(newdata) <- root.var
    pred <- predict(mod, newdata=newdata, type='link', se.fit=TRUE)
    CI <- c((1-CI)/2, CI + (1-CI)/2)
    CIs <- mod$family$linkinv(pred$fit + qnorm(CI) * pred$se.fit)
    ret <- list(root=root$root, CI=CIs, mod=mod, uniroot=root, target=target)
    class(ret) <- 'SFA'
    ret
}

#' @rdname SFA
#' @param x an object of class \code{SFA}
#' @export
print.SFA <- function(x, ...)
{
    out <- data.frame(root=x$root,
                      target=x$target,
                      CI.lower=x$CI[1L],
                      CI.upper=x$CI[2L])
    rownames(out) <- ""
    print(out, ...)
}
