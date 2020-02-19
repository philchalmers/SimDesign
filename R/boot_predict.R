#' Compute prediction estimates for the replication size using bootstrap MSE estimates
#'
#' This function computes bootstrap mean-square error estimates to approximate the sampling behavior
#' of the meta-statistics in SimDesign's \code{summarise} functions. A single design condition is
#' supplied, and a simulation with \code{max(Rstar)} replications is performed whereby the
#' generate-analyse results are collected. After obtaining these replication values, the
#' replications are further drawn from (with replacement) using the differing sizes in \code{Rstar}
#' to approximate the bootstrap MSE behavior given different replication sizes. Finally, given these
#' bootstrap estimates linear regression models are fitted using the predictor term
#' \code{one_sqrtR = 1 / sqrt(Rstar)} to allow extrapolation to replication sizes not observed in
#' \code{Rstar}. For more information about the method and subsequent bootstrap MSE plots,
#' refer to Koehler, Brown, and Haneuse (2009).
#'
#' @param condition a \code{data.frame} consisting of one row from the original \code{design}
#'   input object used within \code{\link{runSimulation}}
#'
#' @param generate see \code{\link{runSimulation}}
#'
#' @param analyse see \code{\link{runSimulation}}
#'
#' @param summarise see \code{\link{runSimulation}}
#'
#' @param fixed_objects see \code{\link{runSimulation}}
#'
#' @param ... additional arguments to be passed to \code{\link{runSimulation}}
#'
#' @param Rstar a vector containing the size of the bootstrap subsets to obtain. Default
#'   investigates the vector [100, 200, 300, 400, 500] to compute the respective MSE terms
#'
#' @param boot_draws number of bootstrap replications to draw. Default is 1000
#'
#' @return returns a list of linear model objects (via \code{\link{lm}}) for each
#'    meta-statistics returned by the \code{summarise()} function
#'
#' @references
#'
#' Koehler, E., Brown, E., & Haneuse, S. J.-P. A. (2009). On the Assessment of Monte Carlo Error in
#' Simulation-Based Statistical Analyses. \emph{The American Statistician, 63}, 155-162.
#'
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
#' set.seed(4321)
#' Design <- createDesign(sigma = c(1, 2))
#'
#' #-------------------------------------------------------------------
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'     dat <- rnorm(100, 0, condition$sigma)
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'     CIs <- t.test(dat)$conf.int
#'     names(CIs) <- c('lower', 'upper')
#'     ret <- c(mean = mean(dat), CIs)
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
#'     ret <- c(mu_bias = bias(results[,1], 0),
#'              mu_coverage = ECR(results[,2:3], parameter = 0))
#'     ret
#' }
#'
#' \dontrun{
#' # boot_predict supports only one condition at a time
#' out <- boot_predict(condition=Design[1L, , drop=FALSE],
#'     generate=Generate, analyse=Analyse, summarise=Summarise)
#' out # list of fitted linear model(s)
#'
#' # extract first meta-statistic
#' mu_bias <- out$mu_bias
#'
#' dat <- model.frame(mu_bias)
#' print(dat)
#'
#' # original R metric plot
#' R <- 1 / dat$one_sqrtR^2
#' plot(R, dat$MSE, type = 'b', ylab = 'MSE', main = "Replications by MSE")
#'
#' plot(MSE ~ one_sqrtR, dat, main = "Bootstrap prediction plot", xlim = c(0, max(one_sqrtR)),
#'      ylim = c(0, max(MSE)), ylab = 'MSE', xlab = expression(1/sqrt(R)))
#' beta <- coef(mu_bias)
#' abline(a = 0, b = beta, lty = 2, col='red')
#'
#' # what is the replication value when x-axis = .02? What's its associated expected MSE?
#' 1 / .02^2 # number of replications
#' predict(mu_bias, data.frame(one_sqrtR = .02)) # y-axis value
#'
#' # approximately how many replications to obtain MSE = .001?
#' (beta / .001)^2
#' }
#'
boot_predict <- function(condition, generate, analyse, summarise, fixed_objects = NULL, ...,
                        Rstar = seq(100, 500, by=100), boot_draws = 1000){
    replications <- max(Rstar)
    results <- runSimulation(design=condition, generate=generate, analyse=analyse,
                             replications=replications, fixed_objects=fixed_objects, ...)
    #if(!is.null(dots$cl)) parallel::parSapply()
    nms <- names(summarise(results=results, condition=condition, fixed_objects=fixed_objects))
    tmp_SEs <- vector("list", length(Rstar))
    for(r in 1L:length(Rstar)){
        boots <- sapply(1L:boot_draws, function(ind, r, results, replications, boot_draws, summarise, condition){
            pick <- rint(n = r, min = 1L, max = replications)
            # results could be a list? TODO
            tmp <- results[pick, , drop=FALSE]
            summarise(results=tmp, condition=condition, fixed_objects=fixed_objects)
        }, r=Rstar[r], results=results, replications=replications, boot_draws=boot_draws,
        summarise=summarise, condition=condition)
        if(is.vector(boots)) boots <- matrix(boots, nrow=1)
        tmp_SEs[[r]] <- apply(boots, 1L, sd)
    }
    mat <- do.call(rbind, tmp_SEs)
    rownames(mat) <- Rstar
    mods <- apply(mat, 2L, function(y, R){
        dat <- data.frame(MSE = y, R = R, one_sqrtR = 1/sqrt(R))
        mod <- lm(MSE ~ -1 + one_sqrtR, dat)
        mod
    }, R = Rstar)
    names(mods) <-  nms
    mods
}
