#' Compute (relative) bias summary statistic
#'
#' Computes the (relative) bias of a sample estimate from the parameter value.
#' Accepts estimate and parameter values, as well as estimate values which are in deviation form.
#' If relative bias is requested the \code{estimate} and \code{parameter} inputs are both required.
#'
#' @param estimate a \code{numeric} vector or \code{matrix}/\code{data.frame}
#'   of parameter estimates. If a vector,
#'   the length is equal to the number of replications. If a \code{matrix}/\code{data.frame},
#'   the number of rows must equal the number of replications
#'
#' @param parameter a \code{numeric} scalar/vector indicating the fixed parameters.
#'   If a single value is supplied and \code{estimate} is a \code{matrix}/\code{data.frame}
#'   then the value will be recycled for each column.
#'   If \code{NULL} then it will be assumed that the \code{estimate} input is in a deviation
#'   form (therefore \code{mean(estimate))} will be returned)
#'
#' @param relative logical; compute the relative bias statistic (i.e., divide the bias by the value
#'   in \code{parameter})? Default is \code{FALSE}
#'
#' @return returns a \code{numeric} vector indicating the overall (relative) bias in the estimates
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
#' bias(mat, parameter = 2)
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' bias(df, parameter = c(2,2))
#'
#'
bias <- function(estimate, parameter = NULL, relative = FALSE){
    if(is.vector(estimate)){
        nms <- names(estimate)
        estimate <- matrix(estimate)
        colnames(estimate) <- nms
    } else if(is.data.frame(estimate)) estimate <- as.matrix(estimate)
    stopifnot(is.matrix(estimate))
    n_col <- ncol(estimate)
    if(relative) stopifnot(!is.null(parameter))
    if(is.null(parameter)) parameter <- 0
    stopifnot(is.vector(parameter))
    if(length(parameter) == 1L) parameter <- rep(parameter, n_col)
    ret <- colMeans(t(t(estimate) - parameter))
    if(relative) ret <- ret / parameter
    ret
}



#' Compute the (normalized) root mean square error
#'
#' Computes the average deviation (root mean square error; also known as the root mean square deviation)
#' of a sample estimate from the parameter value. Accepts estimate and parameter values,
#' as well as estimate values which are in deviation form.
#'
#' @param estimate a \code{numeric} vector or \code{matrix}/\code{data.frame} of parameter estimates.
#'   If a vector, the length is equal to the number of replications. If a
#'   \code{matrix}/\code{data.frame}, the number of rows must equal the number of replications
#'
#' @param parameter a \code{numeric} scalar/vector indicating the fixed parameter values.
#'   If a single value is supplied and \code{estimate} is a \code{matrix}/\code{data.frame} then
#'   the value will be recycled for each column.
#'   If \code{NULL} then it will be assumed that the \code{estimate} input is in a deviation
#'   form (therefore \code{sqrt(mean(estimate^2))} will be returned)
#'
#' @param type type of deviation to compute. Can be \code{'RMSE'} (default) for the root mean square-error,
#'   \code{'NRMSE'} for the normalized RMSE (RMSE / (max(estimate) - min(estimate))),
#'   \code{'NRMSE_SD'} for the normalized RMSE with the standard deviation (RMSE / sd(estimate)),
#'   or \code{'CV'} for the coefficient of variation
#'
#' @return returns a \code{numeric} vector indicating the overall average deviation in the estimates
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
#' RMSE(dev, pop, type = 'NRMSE_SD')
#' RMSE(samp, pop, type = 'CV')
#'
#' # matrix input
#' mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' RMSE(mat, parameter = 2)
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' RMSE(df, parameter = c(2,2))
#'
RMSE <- function(estimate, parameter = NULL, type = 'RMSE'){
    if(is.vector(estimate)){
        nms <- names(estimate)
        estimate <- matrix(estimate)
        colnames(estimate) <- nms
    } else if(is.data.frame(estimate)) estimate <- as.matrix(estimate)
    stopifnot(is.matrix(estimate))
    n_col <- ncol(estimate)
    if(is.null(parameter)) parameter <- 0
    stopifnot(is.vector(parameter))
    if(length(parameter) == 1L) parameter <- rep(parameter, n_col)
    ret <- sqrt(colMeans(t( (t(estimate) - parameter)^2 )))
    if(type == 'NRMSE'){
        diff <- apply(estimate, 2, max) - apply(estimate, 2, min)
        ret <- ret / diff
    } else if(type == 'NRMSE_SD'){
        diff <- apply(estimate, 2, sd)
        ret <- ret / diff
    } else if(type == 'CV'){
        ret <- ret / colMeans(estimate)
    }
    ret
}




#' Compute the mean absolute error
#'
#' Computes the average absolute deviation of a sample estimate from the parameter value.
#' Accepts estimate and parameter values, as well as estimate values which are in deviation form.
#'
#' @param estimate a \code{numeric} vector or \code{matrix}/\code{data.frame} of parameter estimates.
#'   If a vector, the length is equal to the number of replications. If a
#'   \code{matrix}/\code{data.frame} the number of rows must equal the number of replications
#'
#' @param parameter a \code{numeric} scalar/vector indicating the fixed parameter values.
#'   If a single value is supplied and \code{estimate} is a \code{matrix}/\code{data.frame} then the value will be
#'   recycled for each column.
#'   If \code{NULL}, then it will be assumed that the \code{estimate} input is in a deviation
#'   form (therefore \code{mean(abs(estimate))} will be returned)
#'
#' @param type type of deviation to compute. Can be \code{'MAE'} (default) for the mean absolute error,
#'   \code{'NMSE'} for the normalized MAE (MAE / (max(estimate) - min(estimate))), or
#'   \code{'NMSE_SD'} for the normalized MAE by the standard deviation (MAE / sd(estimate)),
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
#' MAE(samp, pop, type = 'NMAE_SD')
#'
#' # matrix input
#' mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' MAE(mat, parameter = 2)
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' MAE(df, parameter = c(2,2))
#'
MAE <- function(estimate, parameter = NULL, type = 'MAE'){
    if(is.vector(estimate)){
        nms <- names(estimate)
        estimate <- matrix(estimate)
        colnames(estimate) <- nms
    } else if(is.data.frame(estimate)) estimate <- as.matrix(estimate)
    stopifnot(is.matrix(estimate))
    n_col <- ncol(estimate)
    if(is.null(parameter)) parameter <- 0
    stopifnot(is.vector(parameter))
    if(length(parameter) == 1L) parameter <- rep(parameter, n_col)
    ret <- colMeans(t(abs(t(estimate) - parameter)))
    if(type == 'NMAE'){
        diff <- apply(estimate, 2, max) - apply(estimate, 2, min)
        ret <- ret / diff
    } else if(type == 'NMAE_SD'){
        diff <- apply(estimate, 2, sd)
        ret <- ret / diff
    }
    ret
}



#' Compute the relative efficiency of multiple estimators
#'
#' Computes the relative efficiency given the RMSE (default) or MSE values for multiple estimators.
#'
#' @param x a \code{numeric} vector of root mean square error values (see \code{\link{RMSE}}),
#'  where the first element will be used as the reference. Otherwise, the object could contain
#'  MSE values if the flag \code{MSE = TRUE} is also included
#'
#' @param MSE logical; are the input value mean squared errors instead of root mean square errors?
#'
#' @return returns a \code{vector} of variance ratios indicating the relative efficiency compared
#'   to the first estimator. Values less than 1 indicate better efficiency, while
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
#' @param p a \code{numeric} vector or \code{matrix}/\code{data.frame} of p-values from the
#'   desired statistical estimator. If a \code{matrix}, each statistic must be organized by
#'   column, where the number of rows is equal to the number of replications
#'
#' @param alpha the nominal detection rate to be studied (typical values are .10, .05, and .01). Default
#'   is .05
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
#' EDR(rates)
#' EDR(rates, alpha = .01)
#'
EDR <- function(p, alpha = .05){
    stopifnot(all(p <= 1 && p >= 0))
    stopifnot(length(alpha) == 1L)
    stopifnot(alpha <= 1 && alpha >= 0)
    if(is.vector(p)) p <- matrix(p)
    colMeans(p <= alpha)
}



#' Compute the empirical coverage rate for Type I errors and Power
#'
#' Computes the detection rate for determining empirical Type I error and power rates using information
#' from the confidence intervals. Note that using \code{1 - ECR(CIs, parameter)} will provide the empirical
#' detection rate.
#'
#' @param CIs a \code{numeric} vector or \code{matrix} of confidence interval values for a
#'   given parameter value, where the first element/column indicates the lower confidence interval
#'   and the second element/column the upper confidence interval. If a
#'   vector of length 2 is passed instead then the returned value will be either a 1 or 0 to indicate
#'   whether the parameter value was or was not within the interval, respectively
#'
#' @param parameter a numeric scalar indicating the fixed parameter value
#'
#' @param tails logical; when TRUE returns a vector of length 2 to indicate the proportion of times
#'   the parameter was lower or higher than the supplied interval, respectively. This is mainly only
#'   useful when the coverage region is not expected to be symmetric, and therefore is generally not
#'   required. Note that \code{1 - sum(ECR(CIs, parameter, tails=TRUE)) == ECR(CIs, parameter)}
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
#' ECR(CIs, 0, tails = TRUE)
#'
#' # single vector input
#' CI <- c(-1, 1)
#' ECR(CI, 0)
#' ECR(CI, 2)
#' ECR(CI, 2, tails = TRUE)
#'
ECR <- function(CIs, parameter, tails = FALSE){
    if(length(CIs) == 2L) CIs <- matrix(CIs, 1L, 2L)
    stopifnot(is.matrix(CIs))
    stopifnot(length(parameter) == 1L)
    if(CIs[1,1] > CIs[1,2]){
        warning('First column not less than second. Temporarily switching')
        CIs <- cbind(CIs[,2L], CIs[,1L])
    }
    ret <- if(tails){
        c(mean(CIs[,1L] >= parameter), mean(parameter >= CIs[,2L]))
    } else mean(CIs[,1L] <= parameter & parameter <= CIs[,2L])
    ret
}

