#' Surrogate Function Approximation via the Generalized Linear Model
#'
#' Given a simulation that was executed with \code{\link{runSimulation}},
#' potentially with the argument \code{store_results = TRUE} to store the
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
#' @param b (optional) Target quantity to use for root solving given the fitted
#'  surrogate function (e.g., find sample size associated with SFA implied power of .80)
#'
#' @param design (optional) \code{data.frame} object containing all the information
#'   relevant for the surrogate model (passed to \code{newdata} in
#'   \code{\link{predict}}) with an \code{NA} value in the variable to be solved
#'
#' @param CI advertised confidence interval of SFA prediction around solved target
#'
#' @param interval interval to be passed to \code{\link{uniroot}} if not specified then
#'   the lowest and highest values from \code{results} for the respective variable
#'   will be used
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
#' Generate <- function(condition, fixed_objects) {
#'     Attach(condition)
#'     group1 <- rnorm(N)
#'     group2 <- rnorm(N, mean=d)
#'     dat <- data.frame(group = gl(2, N, labels=c('G1', 'G2')),
#'                       DV = c(group1, group2))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects) {
#'     p <- c(p = t.test(DV ~ group, dat, var.equal=TRUE)$p.value)
#'     p
#' }
#'
#' Summarise <- function(condition, results, fixed_objects) {
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
#'
#' sim <- runSimulation(design=Design, replications=10,
#'                      generate=Generate, analyse=Analyse,
#'                      summarise=Summarise, store_results=TRUE, save=FALSE,
#'                      progress=FALSE, control=list(print_RAM=FALSE))
#' sim
#'
#' # total of 4010 replication
#' sum(sim$REPLICATIONS)
#'
#' # use the unsummarised results for the SFA, and include p.values < alpha
#' sim_results <- SimResults(sim)
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
#' # fitted model + root-solved solution given f(.) = b,
#' #   where b = target power of .8
#' design <- data.frame(N=NA, d=.2)
#' sfa.root <- SFA(sim_results, formula = sig ~ N,
#'                 b=.8, design=design)
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
#' sim2 <- runSimulation(design=Design, replications=100,
#'                      generate=Generate, analyse=Analyse,
#'                      summarise=Summarise, store_results=TRUE, save=FALSE,
#'                      progress=FALSE, control=list(print_RAM=FALSE))
#' sim2
#' sum(sim2$REPLICATIONS) # more replications in total
#'
#' # use the unsummarised results for the SFA, and include p.values < alpha
#' sim_results <- SimResults(sim2)
#' sim_results <- within(sim_results, sig <- p < .05)
#' sim_results
#'
#' # fitted model
#' sfa <- SFA(sim_results, formula = sig ~ N)
#' sfa
#' summary(sfa)
#'
#' # plot the observed and SFA expected values
#' plot(p ~ N, sim2, las=1, pch=16, main='Rejection rates with R=100')
#' pred <- predict(sfa, type = 'response')
#' lines(sim_results$N, pred, col='red', lty=2)
#'
#' # fitted model + root-solved solution given f(.) = b,
#' #   where b = target power of .8
#' design <- data.frame(N=NA, d=.2)
#' sfa.root <- SFA(sim_results, formula = sig ~ N,
#'                 b=.8, design=design, interval=c(100, 500))
#' sfa.root
#'
#' # true root
#' pwr::pwr.t.test(power=.8, d=.2)
#'
#' ###################
#' # vary multiple parameters (e.g., sample size + effect size) to fit
#' # multi-parameter surrogate
#'
#' Design <- createDesign(N = seq(from=10, to=500, by=10),
#'                        d = seq(from=.1, to=.5, by=.1))
#' Design
#'
#' sim3 <- runSimulation(design=Design, replications=50,
#'                       generate=Generate, analyse=Analyse,
#'                       summarise=Summarise, store_results=TRUE, save=FALSE,
#'                       progress=FALSE, control=list(print_RAM=FALSE))
#' sim3
#' sum(sim3$REPLICATIONS)
#'
#' # use the unsummarised results for the SFA, and include p.values < alpha
#' sim_results <- SimResults(sim3)
#' sim_results <- within(sim_results, sig <- p < .05)
#' sim_results
#'
#' # additive effects (logit(sig) ~ N + d)
#' sfa0 <- SFA(sim_results, formula = sig ~ N+d)
#' sfa0
#'
#' # multiplicative effects (logit(sig) ~ N + d + N:d)
#' sfa <- SFA(sim_results, formula = sig ~ N*d)
#' sfa
#'
#' # multiplicative better fit (sample size interacts with effect size)
#' anova(sfa0, sfa, test = "LRT")
#' summary(sfa)
#'
#' # plot the observed and SFA expected values
#' library(ggplot2)
#' sim3$pred <- predict(sfa, type = 'response', newdata=sim3)
#' ggplot(sim3, aes(N, p, color = factor(d))) +
#'   geom_point() + geom_line(aes(y=pred)) +
#'   facet_wrap(~factor(d))
#'
#' # fitted model + root-solved solution given f(.) = b,
#' #   where b = target power of .8
#' design <- data.frame(N=NA, d=.2)
#' sfa.root <- SFA(sim_results, formula = sig ~ N * d,
#'                 b=.8, design=design, interval=c(100, 500))
#' sfa.root
#'
#' # true root
#' pwr::pwr.t.test(power=.8, d=.2)
#'
#' # root prediction where d *not* used in original data
#' design <- data.frame(N=NA, d=.25)
#' sfa.root <- SFA(sim_results, formula = sig ~ N * d,
#'                 b=.8, design=design, interval=c(100, 500))
#' sfa.root
#'
#' # true root
#' pwr::pwr.t.test(power=.8, d=.25)
#'
#' }
#'
SFA <- function(results, formula, family = 'binomial',
                b = NULL, design = NULL, CI = .95, interval = NULL, ...){
    fn <- function(x, mod, b, root.var, newdata){
        newdata[root.var] <- x
        predict(mod, newdata=newdata, type='response') - b
    }

    mod <- glm(formula=formula, data=results, family=family, ...)
    if(is.null(b)) return(mod)
    stopifnot("Must specify design with one missing NA element" = !is.null(design))
    stopifnot(is.data.frame(design) && nrow(design) == 1L && sum(is.na(design)) == 1L)
    root.var <- names(design)[which(is.na(design))]
    if(is.null(interval)) interval <- c(min(results[root.var]),
                                       max(results[root.var]))
    root <- uniroot(fn, interval=interval,
                    mod=mod, b=b, root.var=root.var, newdata=design)
    design[root.var] <- root$root
    pred <- predict(mod, newdata=design, type='link', se.fit=TRUE)
    CI <- c((1-CI)/2, CI + (1-CI)/2)
    CIs <- mod$family$linkinv(pred$fit + qnorm(CI) * pred$se.fit)
    ret <- list(root=root$root, CI=CIs, mod=mod, uniroot=root, b=b)
    class(ret) <- 'SFA'
    ret
}

#' @rdname SFA
#' @param x an object of class \code{SFA}
#' @export
print.SFA <- function(x, ...)
{
    out <- data.frame(root=x$root,
                      b=x$b,
                      CI.lower=x$CI[1L],
                      CI.upper=x$CI[2L])
    rownames(out) <- ""
    print(out, ...)
}
