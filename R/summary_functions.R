#' Compute bias summary statistic
#'
#' Computes the bias of a sample estimate from the population value.
#'
#' @param observed a numeric vector of parameter estimates, where the length is equal to the number of
#'   replications
#'
#' @param population a numeric scalar indicating the fixed population value
#'
#' @return returns a single number indicating the overall bias in the estimates
#'
#' @aliases bias
#'
#' @examples
#' \dontrun{
#'
#' pop <- 1
#' samp <- rnorm(100, 1, sd = 0.5)
#' bias(samp, pop)
#'
#' }
#'
bias <- function(observed, population) mean(observed - population)



#' Compute the mean square error
#'
#' Computes the average deviation (mean square error) of a sample estimate from the population value.
#'
#' @param observed a numeric vector of parameter estimates, where the length is equal to the number of
#'   replications
#'
#' @param population a numeric scalar indicating the fixed population value
#'
#' @return returns a single number indicating the overall bias in the estimates
#'
#' @aliases RMSD
#'
#' @examples
#' \dontrun{
#'
#' pop <- 1
#' samp <- rnorm(100, 1, sd = 0.5)
#' RMSD(samp, pop)
#'
#' }
#'
MSE <- function(observed, population) sqrt(mean((observed - population)^2))




#' Compute the relative efficiency of multiple estimators
#'
#' Computes the relateive efficiency given the MSE values for multiple estimators
#'
#' @param MSEs a vector of mean square error values (see \code{\link{MSE}})
#'
#' @return returns a vector of ratios indicating the relative efficiency compared to the first
#'   estimator (which by default will be equal to 1). Values less than 1 indicate worse efficiency, while
#'   values greater than 1 indicate better efficiency
#'
#' @aliases RE
#'
#' @examples
#' \dontrun{
#'
#' pop <- 1
#' samp1 <- rnorm(100, 1, sd = 0.5)
#' MSE1 <- MSE(samp1, pop)
#' samp2 <- rnorm(100, 1, sd = 1)
#' MSE2 <- MSE(samp2, pop)
#'
#' RE(c(MSE1, MSE2))
#'
#' }
#'
RE <- function(MSEs) MSEs[1L] / MSEs



#' Compute the empirical detection rate for Type I errors and Power
#'
#' Computes the detection rate for determining empirical Type I error and power rates
#'
#' @param p a vector or matrix of p-values from the desired statistical estimator. If a matrix,
#'   each statistic must be organized by column where the number of rows is equal to the number
#'   of replications
#'
#' @param alpha the nominal detection rate to be studied (typical values are .10, .05, and .01)
#'
#' @aliases EDR
#'
#' @examples
#' \dontrun{
#'
#' rates <- numeric(100)
#' for(i in 1:100){
#'    dat <- rnorm(100)
#'    rates[i] <- t.test(dat)$p.value
#' }
#'
#' EDR(rates, alpha = .05)
#'
#' }
#'
EDR <- function(p, alpha){
    stopifnot(all(p <= 1 && p >= 0))
    stopifnot(alpha <= 1 && alpha >= 0)
    if(is.vector(p)) p <- matrix(p)
    colMeans(p <= alpha)
}

