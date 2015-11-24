#' Compute (relative) bias summary statistic
#'
#' Computes the (relative) bias of a sample estimate from the population value.
#' Accepts observed and population values, as well as observed values which are in deviation form.
#' If relative bias is requested the \code{observed} and \code{population} inputs are both required.
#'
#' @param observed a numeric vector or matrix/data.frame of parameter estimates. If a vector,
#'   the length is equal to the number of replications. If a matrix/data.frame,
#'   the number of rows must equal the number of replications
#'
#' @param population a numeric scalar/vector indicating the fixed population values.
#'   If a single value is supplied and \code{observed} is a matrix/data.frame then the value will be
#'   recycled for each column.
#'   If NULL, then it will be assumed that the \code{observed} input is in a deviation
#'   form (therefore \code{mean(observed))} will be returned)
#'
#' @param relative logical; compute the relative bias statistic? Default is FALSE
#'
#' @return returns a numeric vector indicating the overall (relative) bias in the estimates
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
#' # matrix input
#' mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' bias(mat, population = 2)
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' bias(df, population = c(2,2))
#'
#'
bias <- function(observed, population = NULL, relative = FALSE){
    if(is.vector(observed)){
        nms <- names(observed)
        observed <- matrix(observed)
        colnames(observed) <- nms
    } else if(is.data.frame(observed)) observed <- as.matrix(observed)
    stopifnot(is.matrix(observed))
    n_col <- ncol(observed)
    if(relative) stopifnot(!is.null(population))
    if(is.null(population)) population <- 0
    stopifnot(is.vector(population))
    if(length(population) == 1L) population <- rep(population, n_col)
    ret <- colMeans(t(t(observed) - population))
    if(relative) ret <- ret / population
    ret
}



#' Compute the (normalized) root mean square error
#'
#' Computes the average deviation (root mean square error; also known as the root mean square deviation)
#' of a sample estimate from the population value. Accepts observed and population values,
#' as well as observed values which are in deviation form.
#'
#' @param observed a numeric vector or matrix/data.frame of parameter estimates. If a vector,
#'   the length is equal to the number of replications. If a matrix/data.frame,
#'   the number of rows must equal the number of replications
#'
#' @param population a numeric scalar/vector indicating the fixed population values.
#'   If a single value is supplied and \code{observed} is a matrix/data.frame then the value will be
#'   recycled for each column.
#'   If NULL, then it will be assumed that the \code{observed} input is in a deviation
#'   form (therefore \code{sqrt(mean(observed^2))} will be returned)
#'
#' @param type type of deviation to compute. Can be 'RMSE' (default) for the root mean square-error,
#'   'NRMSE' for the normalized RMSE (RMSE / (max(observed) - min(observed))), or 'CV' for the coefficient of
#'   variation
#'
#' @return returns a numeric vector indicating the overall average deviation in the estimates
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
#' # matrix input
#' mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' RMSE(mat, population = 2)
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' RMSE(df, population = c(2,2))
#'
RMSE <- function(observed, population = NULL, type = 'RMSE'){
    if(is.vector(observed)){
        nms <- names(observed)
        observed <- matrix(observed)
        colnames(observed) <- nms
    } else if(is.data.frame(observed)) observed <- as.matrix(observed)
    stopifnot(is.matrix(observed))
    n_col <- ncol(observed)
    if(is.null(population)) population <- 0
    stopifnot(is.vector(population))
    if(length(population) == 1L) population <- rep(population, n_col)
    ret <- sqrt(colMeans(t( (t(observed) - population)^2 )))
    if(type == 'NRMSE'){
        diff <- apply(observed, 2, max) - apply(observed, 2, min)
        ret <- ret / diff
    }
    if(type == 'CV'){
        ret <- ret / colMeans(observed)
    }
    ret
}




#' Compute the mean absolute error
#'
#' Computes the average absolute deviation of a sample estimate from the population value.
#' Accepts observed and population values,
#' as well as observed values which are in deviation form.
#'
#' @param observed a numeric vector or matrix/data.frame of parameter estimates. If a vector,
#'   the length is equal to the number of replications. If a matrix/data.frame,
#'   the number of rows must equal the number of replications
#'
#' @param population a numeric scalar/vector indicating the fixed population values.
#'   If a single value is supplied and \code{observed} is a matrix/data.frame then the value will be
#'   recycled for each column.
#'   If NULL, then it will be assumed that the \code{observed} input is in a deviation
#'   form (therefore \code{mean(abs(observed))} will be returned)
#'
#' @param type type of deviation to compute. Can be 'MAE' (default) for the mean absolute error, or
#'   'NMSE' for the normalized MAE (MAE / (max(observed) - min(observed)))
#'
#' @return returns a numeric vector indicating the overall mean absolute error in the estimates
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
#' # matrix input
#' mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' MAE(mat, population = 2)
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' MAE(df, population = c(2,2))
#'
MAE <- function(observed, population = NULL, type = 'MAE'){
    if(is.vector(observed)){
        nms <- names(observed)
        observed <- matrix(observed)
        colnames(observed) <- nms
    } else if(is.data.frame(observed)) observed <- as.matrix(observed)
    stopifnot(is.matrix(observed))
    n_col <- ncol(observed)
    if(is.null(population)) population <- 0
    stopifnot(is.vector(population))
    if(length(population) == 1L) population <- rep(population, n_col)
    ret <- colMeans(t(abs(t(observed) - population)))
    if(type == 'NMAE'){
        diff <- apply(observed, 2, max) - apply(observed, 2, min)
        ret <- ret / diff
    }
    ret
}



#' Compute the relative efficiency of multiple estimators
#'
#' Computes the relative efficiency given the RMSE (default) or MSE values for multiple estimators.
#'
#' @param x a vector or matrix of mean square error values (see \code{\link{RMSE}}), where the first
#'  element/row will be used as the reference. Otherwise, the object could contain MSE values if the flag
#'  \code{MSE = TRUE} is also included
#' @param MSE logical; are the input value mean squared errors instead of root mean square errors?
#'
#' @return returns a vector/matrix of ratios indicating the relative efficiency compared to the first
#'   estimator (which by default will be equal to 1). Values less than 1 indicate better efficiency, while
#'   values greater than 1 indicate worse efficiency
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
#' # using MSE instead
#' mse <- c(RMSE1, RMSE2)^2
#' RE(mse, MSE = TRUE)
#'
RE <- function(x, MSE = FALSE){
    pow <- ifelse(MSE, 1, 2)
    x <- x^pow
    if(!is.vector(x)){
        x / x[,1L]
    } else return(x / x[1L])
}



#' Compute the empirical detection rate for Type I errors and Power
#'
#' Computes the detection rate for determining empirical Type I error and power rates using information
#' from p-values.
#'
#' @param p a vector or matrix/data.frame of p-values from the desired statistical estimator. If a matrix,
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

