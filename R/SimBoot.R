#' Function to present bootstrap standard errors estimates for Monte Carlo simulation meta-statistics
#'
#' This function generates confidence intervals for the meta-statistics called within the
#' \code{summarise} function with \code{\link{runSimulation}}
#' that included the arguemnt \code{bootSE = TRUE}.
#'
#' @param results object returned from \code{\link{runSimulation}} where \code{bootSE = TRUE}
#'   was used
#'
#' @param CI desired confidence interval level for each meta-statistic using the bootstrap
#'  SE estimate. Default is .99, which constructs a 99\% confidence interval
#'
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @export
#'
#' @examples
#'
#' #SimFunctions()
#'
#' Design <- data.frame(N = c(10, 20, 30))
#'
#' Generate <- function(condition, fixed_objects = NULL){
#'     dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL){
#'     CIs <- t.test(dat)$conf.int # t-based CIs
#'     xbar <- mean(dat) # mean of the sample data vector
#'     ret <- c(mean=xbar, lowerCI=CIs[1], upperCI=CIs[2])
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     ret <- c(mu=mean(results[,1]), SE=sd(results[,1]), # mean and SD summary of the sample means
#'              coverage=ECR(results[,2:3], parameter = 10))
#'     ret
#' }
#'
#' res <- runSimulation(design=Design, replications=250, bootSE=TRUE,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#'
#' res
#' SimBoot(res)
#'
#' # larger R
#' res2 <- runSimulation(design=Design, replications=2500, bootSE=TRUE,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#'
#' # point estimates more accurate, smaller BOOT_SE terms
#' res2
#' SimBoot(res2) # more reasonable CI range
#'
SimBoot <- function(results, CI = .99){
    nms <- attr(results, 'design_names')
    q <- -qnorm((1 - .99)/2)
    results <- as.data.frame(results)
    bootSE <- subset(results, select = colnames(results) %in% nms$bootSE)
    sims <- subset(results, select = colnames(results) %in% nms$sim)
    lower <- sims - q * bootSE
    upper <- sims + q * bootSE
    ret <- cbind(lower, upper)
    ret[,seq(1L, ncol(ret), by = 2L)] <- lower
    ret[,seq(2L, ncol(ret), by = 2L)] <- upper
    colnames(ret) <- paste0(as.vector(sapply(paste0(nms$sim, ":("), function(x)
        paste0(x, c( (1 - .99)/2 * 100, (1 - (1 - .99)/2)*100)))), "%)")
    ret <- data.frame(subset(results, select = colnames(results) %in% nms$design), ret,
                      check.names = FALSE)
    ret
}
