#' Compute (relative/standardized) bias summary statistic
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
#'   then the value will be recycled for each column; otherwise, each element will be associated
#'   with each respective column in the \code{estimate} input.
#'   If \code{NULL} then it will be assumed that the \code{estimate} input is in a deviation
#'   form (therefore \code{mean(estimate))} will be returned)
#'
#' @param type type of bias statistic to return. Default (\code{'bias'}) computes the standard bias
#'   (average difference between sample and population), \code{'relative'} computes
#'   the relative bias statistic (i.e., divide the bias by the value
#'   in \code{parameter}; note that multiplying this by 100 gives the "percent bias" measure),
#'   \code{'abs_relative'} computes the relative bias but the absoluate values of the parameters
#'   are used in the denominator rather than the (potentially) signed input values,
#'   and \code{'standardized'} computes the standardized bias estimate
#'   (standard bias divided by the standard deviation of the sample estimates)
#'
#' @param abs logical; find the absoluate bias between the parameters and estimates? This effectively
#'   just applies the \code{\link{abs}} transformation to the returned result. Default is FALSE
#'
#' @return returns a \code{numeric} vector indicating the overall (relative/standardized)
#'   bias in the estimates
#'
#' @seealso \code{\link{RMSE}}
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @aliases bias
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @export bias
#'
#' @examples
#'
#' pop <- 2
#' samp <- rnorm(100, 2, sd = 0.5)
#' bias(samp, pop)
#' bias(samp, pop, type = 'relative')
#' bias(samp, pop, type = 'standardized')
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
#' bias(mat, parameter = 2, type = 'relative')
#' bias(mat, parameter = 2, type = 'standardized')
#'
#' # different parameter associated with each column
#' mat <- cbind(M1=rnorm(1000, 2, sd = 0.25), M2 = rnorm(1000, 3, sd = .25))
#' bias(mat, parameter = c(2,3))
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' bias(df, parameter = c(2,2))
#'
#' # parameters of the same size
#' parameters <- 1:10
#' estimates <- parameters + rnorm(10)
#' bias(estimates, parameters)
#'
#' # relative difference dividing by the magnitude of parameters
#' bias(estimates, parameters, type = 'abs_relative')
#'
#'
bias <- function(estimate, parameter = NULL, type = 'bias', abs = FALSE){
    if(is.data.frame(estimate)) estimate <- as.matrix(estimate)
    if(is.vector(estimate)){
        nms <- names(estimate)
        estimate <- matrix(estimate)
        colnames(estimate) <- nms
    }
    stopifnot(is.matrix(estimate))
    stopifnot(type %in% c('bias', 'standardized', 'relative', 'abs_relative'))
    n_col <- ncol(estimate)
    if(type == "relative") stopifnot(!is.null(parameter))
    if(is.null(parameter)) parameter <- 0
    if(is.data.frame(parameter)) parameter <- unlist(parameter)
    stopifnot(is.vector(parameter))
    if(length(parameter) == 1L) parameter <- rep(parameter, n_col)
    equal_len <- length(estimate) == length(parameter)
    if(!equal_len)
        stopifnot(ncol(estimate) == length(parameter))
    diff <- t(t(estimate) - parameter)
    ret <- if(type == 'relative') colMeans(diff / parameter)
        else if(type == 'abs_relative') colMeans(diff / abs(parameter))
        else if(type == 'standardized') colMeans(diff) / apply(estimate, 2, sd)
        else colMeans(diff)
    if(abs) ret <- abs(ret)
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
#'   the value will be recycled for each column; otherwise, each element will be associated
#'   with each respective column in the \code{estimate} input.
#'   If \code{NULL} then it will be assumed that the \code{estimate} input is in a deviation
#'   form (therefore \code{sqrt(mean(estimate^2))} will be returned)
#'
#' @param type type of deviation to compute. Can be \code{'RMSE'} (default) for the root mean square-error,
#'   \code{'NRMSE'} for the normalized RMSE (RMSE / (max(estimate) - min(estimate))),
#'   \code{'NRMSE_SD'} for the normalized RMSE with the standard deviation (RMSE / sd(estimate)),
#'   \code{'CV'} for the coefficient of variation, or \code{'RMSLE'} for the root mean-square log-error
#'
#' @param MSE logical; return the mean square error equivalent of the results instead of the root
#'   mean-square error (in other words, the result is squared)? Default is \code{FALSE}
#'
#' @return returns a \code{numeric} vector indicating the overall average deviation in the estimates
#'
#' @aliases RMSE
#'
#' @seealso \code{\link{bias}}
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export RMSE
#'
#' @seealso MAE
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
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
#' RMSE(samp, pop, type = 'RMSLE')
#'
#' # matrix input
#' mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' RMSE(mat, parameter = 2)
#' RMSE(mat, parameter = c(2, 3))
#'
#' # different parameter associated with each column
#' mat <- cbind(M1=rnorm(1000, 2, sd = 0.25), M2 = rnorm(1000, 3, sd = .25))
#' RMSE(mat, parameter = c(2,3))
#'
#' # same, but with data.frame
#' df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
#' RMSE(df, parameter = c(2,2))
#'
#' # parameters of the same size
#' parameters <- 1:10
#' estimates <- parameters + rnorm(10)
#' RMSE(estimates, parameters)
#'
RMSE <- function(estimate, parameter = NULL, type = 'RMSE', MSE = FALSE){
    if(is.data.frame(estimate)) estimate <- as.matrix(estimate)
    if(is.vector(estimate)){
        nms <- names(estimate)
        estimate <- matrix(estimate)
        colnames(estimate) <- nms
    }
    stopifnot(is.matrix(estimate))
    n_col <- ncol(estimate)
    if(is.null(parameter)) parameter <- 0
    if(is.data.frame(parameter)) parameter <- unlist(parameter)
    stopifnot(is.vector(parameter))
    if(length(parameter) == 1L) parameter <- rep(parameter, n_col)
    ret <- sapply(1L:ncol(estimate), function(i)
        sqrt(mean((estimate[,i] - parameter[i])^2)))
    equal_len <- length(estimate) == length(parameter)
    if(!equal_len)
        stopifnot(ncol(estimate) == length(parameter))
    ret <- sqrt(colMeans(t( (t(estimate) - parameter)^2 )))
    if(type == 'NRMSE'){
        diff <- apply(estimate, 2, max) - apply(estimate, 2, min)
        ret <- ret / diff
    } else if(type == 'NRMSE_SD'){
        diff <- apply(estimate, 2, sd)
        ret <- ret / diff
    } else if(type == 'CV'){
        ret <- ret / colMeans(estimate)
    } else if(type == 'RMSLE'){
        ret <- sqrt(colMeans(t(t(log(estimate + 1)) - log(parameter + 1))^2))
    } else if(type != 'RMSE')
        stop('type argument not supported')
    if(MSE) ret <- ret^2
    ret
}

#' Compute the integrated root mean-square error
#'
#' Computes the average/cumulative deviation given two continuous functions and an optional
#' function representing the probability density function. Only one-dimensional integration
#' is supported.
#'
#' The integrated root mean-square error (IRMSE) is of the form
#' \deqn{IRMSE(\theta) = \sqrt{\int [f(\theta, \hat{\psi}) - f(\theta, \psi)]^2 g(\theta, ...)}}
#' where \eqn{g(\theta, ...)} is the density function used to marginalize the continuous sample
#' (\eqn{f(\theta, \hat{\psi})}) and population (\eqn{f(\theta, \psi)}) functions.
#'
#' @param estimate a vector of parameter estimates
#'
#' @param parameter a vector of population parameters
#'
#' @param fn a continuous function where the first argument is to be integrated and the second argument is
#'   a vector of parameters or parameter estimates. This function
#'   represents a implied continuous function which uses the sample estimates or population parameters
#'
#' @param density (optional) a density function used to marginalize (i.e., average), where the first
#'   argument is to be integrated, and must be of the form \code{density(theta, ...)} or
#'   \code{density(theta, param1, param2)}, where \code{param1} is a placeholder name for the
#'   hyper-parameters associated with the probability density function. If omitted then
#'   the cumulative different between the respective functions will be computed instead
#'
#' @param lower lower bound to begin numerical integration from
#'
#' @param upper upper bound to finish numerical integration to
#'
#' @param ... additional parameters to pass to \code{fnest}, \code{fnparam}, \code{density},
#'   and \code{\link{integrate}},
#'
#' @return returns a single \code{numeric} term indicating the average/cumulative deviation
#' given the supplied continuous functions
#'
#' @aliases IRMSE
#'
#' @seealso \code{\link{RMSE}}
#'
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export IRMSE
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#'
#' # logistic regression function with one slope and intercept
#' fn <- function(theta, param) 1 / (1 + exp(-(param[1] + param[2] * theta)))
#'
#' # sample and population sets
#' est <- c(-0.4951, 1.1253)
#' pop <- c(-0.5, 1)
#'
#' theta <- seq(-10,10,length.out=1000)
#' plot(theta, fn(theta, pop), type = 'l', col='red', ylim = c(0,1))
#' lines(theta, fn(theta, est), col='blue', lty=2)
#'
#' # cumulative result (i.e., standard integral)
#' IRMSE(est, pop, fn)
#'
#' # integrated RMSE result by marginalizing over a N(0,1) distribution
#' den <- function(theta, mean, sd) dnorm(theta, mean=mean, sd=sd)
#'
#' IRMSE(est, pop, fn, den, mean=0, sd=1)
#'
#' # this specification is equivalent to the above
#' den2 <- function(theta, ...) dnorm(theta, ...)
#'
#' IRMSE(est, pop, fn, den2, mean=0, sd=1)
#'
IRMSE <- function(estimate, parameter, fn, density = function(theta, ...) 1,
                  lower = -Inf, upper = Inf, ...){
    stopifnot(is.function(fn))
    stopifnot(is.function(density))
    intfn <- function(theta, estimate, parameter, ...)
        (fn(theta, estimate) - fn(theta, parameter))^2 * density(theta, ...)
    res <- integrate(intfn, lower=lower, upper=upper, estimate=estimate,
                     parameter=parameter, ...)
    sqrt(res$value)
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
#'   recycled for each column; otherwise, each element will be associated
#'   with each respective column in the \code{estimate} input.
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
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @seealso RMSE
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
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
#' # parameters of the same size
#' parameters <- 1:10
#' estimates <- parameters + rnorm(10)
#' MAE(estimates, parameters)
#'
MAE <- function(estimate, parameter = NULL, type = 'MAE'){
    if(is.data.frame(estimate)) estimate <- as.matrix(estimate)
    if(is.vector(estimate)){
        nms <- names(estimate)
        estimate <- matrix(estimate)
        colnames(estimate) <- nms
    }
    stopifnot(is.matrix(estimate))
    n_col <- ncol(estimate)
    if(is.null(parameter)) parameter <- 0
    if(is.data.frame(parameter)) parameter <- unlist(parameter)
    stopifnot(is.vector(parameter))
    if(length(parameter) == 1L) parameter <- rep(parameter, n_col)
    equal_len <- length(estimate) == length(parameter)
    if(!equal_len)
        stopifnot(ncol(estimate) == length(parameter))
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
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
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


#' Compute the relative performance behavior of collections of standard errors
#'
#' The mean-square relative standard error (MSRSE) compares standard error
#' estimates to the standard deviation of the respective
#' parameter estimates. Values close to 1 indicate that the behavior of the standard errors
#' closely matched the sampling variability of the parameter estimates.
#'
#' Mean-square relative standard error (MSRSE) is expressed as
#'
#' \deqn{MSRSE = \frac{E(SE(\psi)^2)}{SD(\psi)^2} =
#'   \frac{1/R * \sum_{r=1}^R SE(\psi_r)^2}{SD(\psi)^2} - 1}
#'
#' where \eqn{SE(\psi_r)} represents the estimate of the standard error at the \eqn{r}th
#' simulation replication, and \eqn{SD(\psi)} represents the standard deviation estimate
#' of the parameters across all \eqn{R} replications. Note that \eqn{SD(\psi)^2} is used,
#' which corresponds to the variance of \eqn{\psi}.
#'
#' @param SE a \code{numeric} scalar/vector indicating the average standard errors across
#'   the replications, or a \code{matrix} of collected standard error estimates themselves
#'   to be used to compute the average standard errors. Each column/element in this input
#'   corresponds to the column/element in \code{SD}
#'
#' @param SD a \code{numeric} scalar/vector indicating the standard deviation across
#'   the replications, or a \code{matrix} of collected parameter estimates themselves
#'   to be used to compute the standard deviations. Each column/element in this input
#'   corresponds to the column/element in \code{SE}
#'
#' @return returns a \code{vector} of ratios indicating the relative performance
#'   of the standard error estimates to the observed parameter standard deviation.
#'   Values less than 0 indicate that the standard errors were larger than the standard
#'   deviation of the parameters (hence, the SEs are interpreted as more conservative),
#'   while values greater than 0 were smaller than the standard deviation of the
#'   parameters (i.e., more liberal SEs)
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export
#'
#' @examples
#'
#' Generate <- function(condition, fixed_objects = NULL) {
#'    X <- rep(0:1, each = 50)
#'    y <- 10 + 5 * X + rnorm(100, 0, .2)
#'    data.frame(y, X)
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'    mod <- lm(y ~ X, dat)
#'    so <- summary(mod)
#'    ret <- c(SE = so$coefficients[,"Std. Error"],
#'             est = so$coefficients[,"Estimate"])
#'    ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
#'    MSRSE(SE = results[,1:2], SD = results[,3:4])
#' }
#'
#' results <- runSimulation(replications=500, generate=Generate,
#'                          analyse=Analyse, summarise=Summarise)
#' results
#'
#'
MSRSE <- function(SE, SD){
    if(is.matrix(SE) && nrow(SE) > 1L)
        SE <- apply(SE, 2L, mean)
    if(is.matrix(SD) && nrow(SD) > 1L)
        SD <- apply(SD, 2L, sd)
    SE^2 / SD^2 - 1
}


#' Compute the relative difference
#'
#' Computes the relative difference statistic of the form \code{(est - pop)/ pop}, which
#' is equivalent to the form \code{est/pop - 1}. If matrices are supplied then
#' an equivalent matrix variant will be used of the form
#' \code{(est - pop) * solve(pop)}. Values closer to 0 indicate better
#' relative parameter recovery.
#'
#' @param est a \code{numeric} vector or matrix containing the parameter estimates
#'
#' @param pop a \code{numeric} vector or matrix containing the true parameter values. Must be
#'   of the same dimensions as \code{est}
#'
#' @param as.vector logical; always wrap the result in a \code{\link{as.vector}} function
#'   before returning?
#'
#' @return returns a \code{vector} or \code{matrix} depending on the inputs and whether
#'   \code{as.vector} was used
#'
#' @aliases RD
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export RD
#'
#' @examples
#'
#' # vector
#' pop <- seq(1, 100, length.out=9)
#' est1 <- pop + rnorm(9, 0, .2)
#' (rds <- RD(est1, pop))
#' summary(rds)
#'
#' # matrix
#' pop <- matrix(c(1:8, 10), 3, 3)
#' est2 <- pop + rnorm(9, 0, .2)
#' RD(est2, pop, as.vector = FALSE)
#' (rds <- RD(est2, pop))
#' summary(rds)
#'
#'
RD <- function(est, pop, as.vector = TRUE){
    if(is.matrix(est)){
        slv <- try(solve(pop), TRUE)
        if(is(slv, 'try-error'))
            stop('pop matrix could not be inverted')
        ret <- (est - pop) %*% slv
        if(as.vector) ret <- as.vector(ret)
    } else {
        ret <- (est - pop) / pop
    }
    ret
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
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @seealso \code{\link{ECR}}
#'
#' @export EDR
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
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
#' # multiple rates at once
#' rates <- cbind(runif(1000), runif(1000))
#' EDR(rates)
#'
EDR <- function(p, alpha = .05){
    if(is.data.frame(p)) p <- as.matrix(p)
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
#' detection rate. Also supports computing the average width of the CIs, which may be useful when comparing
#' the efficiency of CI estimators.
#'
#' @param CIs a \code{numeric} vector or \code{matrix} of confidence interval values for a
#'   given parameter value, where the first element/column indicates the lower confidence interval
#'   and the second element/column the upper confidence interval. If a
#'   vector of length 2 is passed instead then the returned value will be either a 1 or 0 to indicate
#'   whether the parameter value was or was not within the interval, respectively. Otherwise,
#'   the input must be a matrix with an even number of columns
#'
#' @param parameter a numeric scalar indicating the fixed parameter value. Alternative, a \code{numeric}
#'    vector object with length equal to the number of rows as \code{CIs} (use to compare sets of parameters
#'    at once)
#'
#' @param tails logical; when TRUE returns a vector of length 2 to indicate the proportion of times
#'   the parameter was lower or higher than the supplied interval, respectively. This is mainly only
#'   useful when the coverage region is not expected to be symmetric, and therefore is generally not
#'   required. Note that \code{1 - sum(ECR(CIs, parameter, tails=TRUE)) == ECR(CIs, parameter)}
#'
#' @param CI_width logical; rather than returning the overall coverage rate, return the
#'   average width of the CIs instead? Useful when comparing the efficiency of different CI
#'   estimators
#'
#' @param names an optional character vector used to name the returned object. Generally useful
#'   when more than one CI estimate is investigated at once
#'
#' @aliases ECR
#'
#' @seealso \code{\link{EDR}}
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export ECR
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
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
#' # parameters of the same size as CI
#' parameters <- 1:10
#' CIs <- cbind(parameters - runif(10), parameters + runif(10))
#' parameters <- parameters + rnorm(10)
#' ECR(CIs, parameters)
#'
#' # average width of CIs
#' ECR(CIs, parameters, CI_width=TRUE)
#'
#' # ECR() for multiple CI estimates in the same object
#' parameter <- 10
#' CIs <- data.frame(lowerCI_1=parameter - runif(10),
#'                   upperCI_1=parameter + runif(10),
#'                   lowerCI_2=parameter - 2*runif(10),
#'                   upperCI_2=parameter + 2*runif(10))
#' head(CIs)
#' ECR(CIs, parameter)
#' ECR(CIs, parameter, tails=TRUE)
#' ECR(CIs, parameter, CI_width=TRUE)
#'
#' # often a good idea to provide names for the output
#' ECR(CIs, parameter, names = c('this', 'that'))
#' ECR(CIs, parameter, CI_width=TRUE, names = c('this', 'that'))
#' ECR(CIs, parameter, tails=TRUE, names = c('this', 'that'))
#'
ECR <- function(CIs, parameter, tails = FALSE, CI_width = FALSE, names = NULL){
    if(CI_width) tails <- FALSE
    if(is.data.frame(CIs)) CIs <- as.matrix(CIs)
    if(length(CIs) == 2L) CIs <- matrix(CIs, 1L, 2L)
    stopifnot(ncol(CIs) %% 2 == 0L)
    if(ncol(CIs) > 2L){
        ret <- c()
        ind <- 1L
        for(i in seq(1L, ncol(CIs), by=2L)){
            ret <- c(ret, ECR(CIs[,c(i, i+1L)], parameter=parameter,
                              tails=tails, names=names[ind],
                              CI_width=CI_width))
            ind <- ind + 1L
        }
        return(ret)
    }
    stopifnot(is.matrix(CIs))
    if(is.data.frame(parameter)) parameter <- unlist(parameter)
    stopifnot(is.vector(parameter))
    if(length(parameter) != 1L) stopifnot(length(parameter) == nrow(CIs))
    if(CIs[1,1] > CIs[1,2]){
        warning('First column not less than second. Temporarily switching')
        CIs <- cbind(CIs[,2L], CIs[,1L])
    }
    if(CI_width){
        ret <- mean(CIs[,2L] - CIs[,1L])
        if(!is.null(names))
            names(ret) <- names
        return(ret)
    }
    ends <- c(mean(CIs[,1L] > parameter), mean(parameter > CIs[,2L]))
    if(tails){
        ret <- ends
        if(!is.null(names))
            names(ret) <- paste0(names, c('_lower', '_upper'))
    } else {
        ret <- 1 - sum(ends)
        if(!is.null(names))
            names(ret) <- names
    }
    ret
}

