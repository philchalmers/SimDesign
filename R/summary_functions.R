#' Compute (relative) bias summary statistic
#'
#' Computes the (relative) bias of a sample estimate from the population value.
#' Accepts observed and population values, as well as observed values which are in deviation form.
#' If relative bias is requested the \code{observed} and \code{population} inputs are both required.
#'
#' @param observed a numeric vector of parameter estimates, where the length is equal to the number of
#'   replications
#'
#' @param population a numeric scalar indicating the fixed population value. If NULL, then it will be assumed
#'   that the \code{observed} input is in a deviation form (therefore \code{mean(observed)} will be returned)
#'
#' @param relative logical; compute the relative bias statistic? Default is FALSE
#'
#' @return returns a single number indicating the overall (relative) bias in the estimates
#'
#' @seealso \code{\link{RMSE}}
#'
#' @aliases bias
#'
#' @export bias
#'
#' @examples
#'
#' pop <- 2
#' samp <- rnorm(100, 2, sd = 0.5)
#' bias(samp, pop)
#' bias(samp, pop, relative = TRUE)
#'
#' dev <- samp - pop
#' bias(dev)
#'
#' # equivalent here
#' bias(mean(samp), pop)
#'
#'
bias <- function(observed, population = NULL, relative = FALSE){
    if(relative){
        stopifnot(!is.null(population))
        ret <- (mean(observed) - population) / population
    } else {
        if(is.null(population)){
            ret <- mean(observed)
        } else {
            stopifnot(is.vector(observed))
            ret <- mean(observed - population)
        }
    }
    ret
}



#' Compute the (normalized) root mean square error
#'
#' Computes the average deviation (root mean square error; also known as the root mean square deviation)
#' of a sample estimate from the population value. Accepts observed and population values,
#' as well as observed values which are in deviation form.
#'
#' @param observed a numeric vector of parameter estimates, where the length is equal to the number of
#'   replications
#'
#' @param population a numeric scalar indicating the fixed population value. If NULL, then it will be assumed
#'   that the \code{observed} input is in a deviation form (therefore \code{sqrt(mean(observed^2))} will be
#'   returned)
#'
#' @param type type of deviation to compute. Can be 'RMSE' (default) for the root mean square-error,
#'   'NRMSE' for the normalized RMSE (RMSE / (max(observed) - min(observed))), or 'CV' for the coefficient of
#'   variation
#'
#' @return returns a single number indicating the overall average deviation in the estimates
#'
#' @aliases RMSE
#'
#' @seealso \code{\link{bias}}
#'
#' @export RMSE
#'
#' @seealso MAE
#'
#' @examples
#'
#' pop <- 1
#' samp <- rnorm(100, 1, sd = 0.5)
#' RMSE(samp, pop)
#'
#' dev <- samp - pop
#' RMSE(dev)
#'
#' RMSE(samp, pop, type = 'NRMSE')
#' RMSE(dev, type = 'NRMSE')
#' RMSE(samp, pop, type = 'CV')
#'
RMSE <- function(observed, population = NULL, type = 'RMSE'){
    if(is.null(population)){
        ret <- sqrt(mean(observed^2))
    } else {
        stopifnot(is.vector(observed))
        ret <- sqrt(mean((observed - population)^2))
    }
    if(type == 'NRMSE') ret <- ret / (max(observed) - min(observed))
    if(type == 'CV'){
        stopifnot(!is.null(population))
        ret <- ret / mean(observed)
    }
    ret
}




#' Compute the mean absolute error
#'
#' Computes the average absolute deviation of a sample estimate from the population value.
#' Accepts observed and population values,
#' as well as observed values which are in deviation form.
#'
#' @param observed a numeric vector of parameter estimates, where the length is equal to the number of
#'   replications
#'
#' @param population a numeric scalar indicating the fixed population value. If NULL, then it will be assumed
#'   that the \code{observed} input is in a deviation form (therefore \code{mean(abs(observed))} will be
#'   returned)
#'
#' @param type type of deviation to compute. Can be 'MAE' (default) for the mean absolute error, or
#'   'NMSE' for the normalized MAE (MAE / (max(observed) - min(observed)))
#'
#' @return returns a single number indicating the overall mean absolute error in the estimates
#'
#' @aliases MAE
#'
#' @export MAE
#'
#' @seealso RMSE
#'
#' @examples
#'
#' pop <- 1
#' samp <- rnorm(100, 1, sd = 0.5)
#' MAE(samp, pop)
#'
#' dev <- samp - pop
#' MAE(dev)
#' MAE(samp, pop, type = 'NMAE')
#'
MAE <- function(observed, population = NULL, type = 'MAE'){
    if(is.null(population)){
        ret <- mean(abs(observed))
    } else {
        stopifnot(is.vector(observed))
        ret <- mean(abs(observed - population))
    }
    if(type == 'NMAE') ret <- ret / (max(observed) - min(observed))
    ret
}



#' Compute the relative efficiency of multiple estimators
#'
#' Computes the relative efficiency given the RMSE values for multiple estimators
#'
#' @param RMSEs a vector or matrix of mean square error values (see \code{\link{RMSE}}), where the first
#'  element/row will be used as the reference
#'
#' @return returns a vector/matrix of ratios indicating the relative efficiency compared to the first
#'   estimator (which by default will be equal to 1). Values less than 1 indicate worse efficiency, while
#'   values greater than 1 indicate better efficiency
#'
#' @aliases RE
#'
#' @export RE
#'
#' @examples
#'
#' pop <- 1
#' samp1 <- rnorm(100, 1, sd = 0.5)
#' RMSE1 <- RMSE(samp1, pop)
#' samp2 <- rnorm(100, 1, sd = 1)
#' RMSE2 <- RMSE(samp2, pop)
#'
#' RE(c(RMSE1, RMSE2))
#'
RE <- function(RMSEs){
    RMSEs <- RMSEs^2
    if(!is.vector(RMSEs)){
        RMSEs[,1L] / RMSEs
    } else return(RMSEs[1L] / RMSEs)
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
#' @seealso \code{\link{ECR}}
#'
#' @export EDR
#'
#' @examples
#'
#' rates <- numeric(100)
#' for(i in 1:100){
#'    dat <- rnorm(100)
#'    rates[i] <- t.test(dat)$p.value
#' }
#'
#' EDR(rates, alpha = .05)
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
#'   column indicates the lower confidence interval and the second column the upper confidence interval
#'
#' @param population a numeric scalar indicating the fixed population value
#'
#' @aliases ECR
#'
#' @seealso \code{\link{EDR}}
#'
#' @export ECR
#'
#' @examples
#'
#' CIs <- matrix(NA, 100, 2)
#' for(i in 1:100){
#'    dat <- rnorm(100)
#'    CIs[i,] <- t.test(dat)$conf.int
#' }
#'
#' ECR(CIs, 0)
#'
ECR <- function(CIs, population){
    stopifnot(is.matrix(CIs))
    stopifnot(length(population) == 1L)
    if(CIs[1,1] > CIs[1,2]){
        warning('First column not less than second. Temporarily switching')
        CIs <- cbind(CIs[,2L], CIs[,1L])
    }
    mean(CIs[,1L] <= population & population <= CIs[,2L])
}

