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
#' @export bias
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
bias <- function(observed, population){
    stopifnot(is.vector(observed))
    stopifnot(length(population) == 1L || length(observed) == length(population))
    mean(observed - population)
}



#' Compute the root mean square error
#'
#' Computes the average deviation (root mean square error; also known as the root mean square deviation)
#' of a sample estimate from the population value.
#'
#' @param observed a numeric vector of parameter estimates, where the length is equal to the number of
#'   replications
#'
#' @param population a numeric scalar indicating the fixed population value
#'
#' @return returns a single number indicating the overall bias in the estimates
#'
#' @aliases RMSE
#'
#' @export RMSE
#'
#' @examples
#' \dontrun{
#'
#' pop <- 1
#' samp <- rnorm(100, 1, sd = 0.5)
#' RMSE(samp, pop)
#'
#' }
#'
RMSE <- function(observed, population){
    stopifnot(is.vector(observed))
    stopifnot(length(population) == 1L || length(population) == length(observed))
    sqrt(mean((observed - population)^2))
}




#' Compute the relative efficiency of multiple estimators
#'
#' Computes the relateive efficiency given the RMSE values for multiple estimators
#'
#' @param RMSEs a vector of mean square error values (see \code{\link{RMSE}})
#'
#' @return returns a vector of ratios indicating the relative efficiency compared to the first
#'   estimator (which by default will be equal to 1). Values less than 1 indicate worse efficiency, while
#'   values greater than 1 indicate better efficiency
#'
#' @aliases RE
#'
#' @export RE
#'
#' @examples
#' \dontrun{
#'
#' pop <- 1
#' samp1 <- rnorm(100, 1, sd = 0.5)
#' RMSE1 <- RMSE(samp1, pop)
#' samp2 <- rnorm(100, 1, sd = 1)
#' RMSE2 <- RMSE(samp2, pop)
#'
#' RE(c(RMSE1, RMSE2))
#'
#' }
#'
RE <- function(RMSEs){
    stopifnot(is.vector(RMSEs))
    RMSEs[1L] / RMSEs
}



#' Compute the empirical detection rate for Type I errors and Power
#'
#' Computes the detection rate for determining empirical Type I error and power rates using information
#' from p-values.
#'
#' @param p a vector or matrix of p-values from the desired statistical estimator. If a matrix,
#'   each statistic must be organized by column where the number of rows is equal to the number
#'   of replications
#'
#' @param alpha the nominal detection rate to be studied (typical values are .10, .05, and .01)
#'
#' @aliases EDR
#'
#' @export EDR
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
    stopifnot(length(alpha) == 1L)
    stopifnot(alpha <= 1 && alpha >= 0)
    if(is.vector(p)) p <- matrix(p)
    colMeans(p <= alpha)
}



#' Compute the empirical coverage rate for Type I errors and Power
#'
#' Computes the detection rate for determining empirical Type I error and power rates using information
#' from the confidence intervals. Note that using \code{1 - ECR(CIs, population)} will provide the empirical
#' detection rate.
#'
#' @param CIs a matrix of confidence interval values for a given population value, where the first
#'   column indicates the lower cofidence interval and the second column the upper confidence interval
#'
#' @param population a numeric scalar indicating the fixed population value
#'
#' @aliases ECR
#'
#' @export ECR
#'
#' @examples
#' \dontrun{
#'
#' CIs <- matrix(NA, 100, 2)
#' for(i in 1:100){
#'    dat <- rnorm(100)
#'    CIs[i,] <- t.test(dat)$conf.int
#' }
#'
#' ECR(CIs, 0)
#'
#' }
#'
ECR <- function(CIs, population){
    stopifnot(is.matrix(CIs))
    stopifnot(length(population) == 1L)
    if(CIs[1,1] > CIs[1,2]){
        CIs <- cbind(CIs[,2L], CIs[,1L])
    }
    mean(CIs[,1L] <= population & population <= CIs[,2L])
}

