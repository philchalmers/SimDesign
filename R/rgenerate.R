#' Generate integer values within specified range
#'
#' Efficiently generate positive and negative integer values with (default) or without replacement.
#' This function is mainly a wrapper to the \code{\link{sample.int}} function (which itself is much
#' more efficient integer sampler than the more general \code{\link{sample}}), however is intended
#' to work with both positive and negative integer ranges since \code{sample.int} only returns
#' positive integer values that must begin at \code{1L}.
#'
#' @param n number of samples to draw
#' @param min lower limit of the distribution. Must be finite
#' @param max upper limit of the distribution. Must be finite
#' @param replace should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the vector being sampled
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2016). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#' @export
#' @examples
#'
#' set.seed(1)
#'
#' # sample 1000 integer values within 20 to 100
#' x <- rint(1000, min = 20, max = 100)
#' summary(x)
#'
#' # sample 1000 integer values within 100 to 10 billion
#' x <- rint(1000, min = 100, max = 1e8)
#' summary(x)
#'
#' # compare speed to sample()
#' system.time(x <- rint(1000, min = 100, max = 1e8))
#' system.time(x2 <- sample(100:1e8, 1000, replace = TRUE))
#'
#' # sample 1000 integer values within -20 to 20
#' x <- rint(1000, min = -20, max = 20)
#' summary(x)
#'
rint <- function(n, min, max, replace = TRUE, prob = NULL){
    stopifnot(!missing(n))
    stopifnot(!missing(min))
    stopifnot(!missing(max))
    min <- as.integer(min)
    max <- as.integer(max)
    range <- max - min + 1L
    ret <- sample.int(range, size = n, replace = replace, prob = prob) - 1L + min
    ret
}

#' Generate data with the multivariate g-and-h distribution
#'
#' Generate non-normal distributions using the multivariate g-and-h distribution. Can be used to
#' generate several different classes of univariate and multivariate distributions.
#'
#' @param n number of samples to draw
#' @param g the g parameter(s) which control the skew of a distribution in terms of both direction
#'   and magnitude
#' @param h the h parameter(s) which control the tail weight or elongation of a distribution and
#'   is positively related with kurtosis
#' @param mean a vector of k elements for the mean of the variables
#' @param sigma desired k x k covariance matrix between bivariate non-normal variables
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2016). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#' @export
#' @examples
#'
#' set.seed(1)
#'
#' # univariate
#' norm <- rmgh(10000,1e-5,0)
#' hist(norm)
#'
#' skew <- rmgh(10000,1/2,0)
#' hist(skew)
#'
#' neg_skew_platykurtic <- rmgh(10000,-1,-1/2)
#' hist(neg_skew_platykurtic)
#'
#' # multivariate
#' sigma <- matrix(c(2,1,1,4), 2)
#' mean <- c(-1, 1)
#' twovar <- rmgh(10000, c(-1/2, 1/2), c(0,0),
#'     mean=mean, sigma=sigma)
#' hist(twovar[,1])
#' hist(twovar[,2])
#' plot(twovar)
#'
rmgh <- function(n, g, h, mean = rep(0, length(g)), sigma = diag(length(mean))) {
    stopifnot(!missing(n))
    if(length(sigma) == 1L) sigma <- as.matrix(sigma)
    stopifnot(length(g) == length(h) && length(g) == length(mean))
    stopifnot(is.matrix(sigma))
    if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps),
                     check.attributes = FALSE)) {
        stop("sigma must be a symmetric matrix")
    }
    qgh <- function(q, g, h, mean, csigma) {
        gh <- sapply(1L:length(g), function(ind, q, g, h){
            ret <- if(g[ind] == 0) q[,ind]
            else (exp(g[ind] * q[,ind]) - 1) / g[ind] *
                exp((h[ind] * q[,ind]^2 / 2))
            ret
        }, q=q, g=g, h=h)
        t(csigma %*% t(gh) + mean)
    }
    q <- matrix(rnorm(n*ncol(sigma)), ncol=ncol(sigma))
    csigma <- t(chol(sigma))
    qgh(q, g, h, mean, csigma)
}

#' Generate non-normal data with Vale & Maurelli's (1983) method
#'
#' Generate multivariate non-normal distributions using the third-order polynomial method described
#' by Vale & Maurelli (1983). If only a single variable is generated then this function
#' is equivalent to the method described by Fleishman (1978).
#'
#' @param n number of samples to draw
#' @param mean a vector of k elements for the mean of the variables
#' @param sigma desired k x k covariance matrix between bivariate non-normal variables
#' @param skew a vector of k elements for the skewness of the variables
#' @param kurt a vector of k elements for the kurtosis of the variables
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2016). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' Fleishman, A. I. (1978). A method for simulating non-normal distributions.
#' \emph{Psychometrika, 43}, 521-532.
#'
#' Vale, C. & Maurelli, V. (1983). Simulating multivariate nonnormal distributions.
#' \emph{Psychometrika, 48}(3), 465-471.
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @aliases rValeMaurelli
#' @export
#' @examples
#'
#' set.seed(1)
#'
#' # univariate with skew
#' nonnormal <- rValeMaurelli(10000, mean=10, sigma=5, skew=1, kurt=3)
#' # psych::describe(nonnormal)
#'
#' # multivariate with skew and kurtosis
#' n <- 10000
#' r12 <- .4
#' r13 <- .9
#' r23 <- .1
#' cor <- matrix(c(1,r12,r13,r12,1,r23,r13,r23,1),3,3)
#' sk <- c(1.5,1.5,0.5)
#' ku <- c(3.75,3.5,0.5)
#'
#' nonnormal <- rValeMaurelli(n, sigma=cor, skew=sk, kurt=ku)
#' # cor(nonnormal)
#' # psych::describe(nonnormal)
#'
rValeMaurelli <- function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
                          skew = rep(0, nrow(sigma)), kurt = rep(0, nrow(sigma))) {
    stopifnot(!missing(n))
    if(length(sigma) == 1L) sigma <- as.matrix(sigma)
    stopifnot(is.matrix(sigma))
    if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps),
                     check.attributes = FALSE)) {
        stop("sigma must be a symmetric matrix")
    }
    if (length(mean) != nrow(sigma))
        stop("mean and sigma have non-conforming size")
    stopifnot(n > 0)
    stopifnot(ncol(sigma) == length(skew))
    stopifnot(ncol(sigma) == length(kurt))
    sds <- sqrt(diag(sigma))
    cor <- cov2cor(sigma)
    chol_corr <- t(chol(cor))
    k <- ncol(cor)
    for (i in 1:k){
        if (kurt[i] <= skew[i]^2 - 2){
            stop("Error: the ", i," th component of kurtosis is not bigger than skewness squared minus 2.\n")
        }
    }
	constant <- function(sk,ku,start){
		F <- function(x){
			F <- numeric(3)
			b <- x[1]
			c <- x[2]
			d <- x[3]
			F[1] <- b^2+6*b*d+2*c^2+15*d^2-1
			F[2] <- 2*c*(b^2+24*b*d+105*d^2+2)-sk
			F[3] <- 24*(b*d+c^2*(1+b^2+28*b*d)+d^2*(12+48*b*d+141*c^2+225*d^2))-ku
			F
		}
		obj.fun <- function(par){
		    sum(F(par)^2)
		}
		opt <- nlminb(start = start, objective = obj.fun,
		              control = list(abs.tol = 1e-10, rel.tol = 1e-10, eval.max = 1e6, iter.max = 1e6))
		if(opt$converge != 0 || opt$objective > 1e-5)
		    stop('optimizer could not find suitable solution for c0, c1, and c2')
        x0 <- opt$par
		x0
	}
	constants <- matrix(nrow=k, ncol=4)
	for(i in 1:k) {
		constants[i,2:4] <- t(constant(skew[i], kurt[i], start=c(1,0,0)))
		constants[i,1] <- -(constants[i,3])
	}
	solve.p12 <- function(r12,a1,a2,b1,b2,c1,c2,d1,d2) { #Start Internal Function 2
		ftn <- function(p12) {
			((b1*b2+3*b1*d2+3*d1*b2+9*d1*d2)*p12)+((2*c1*c2)*p12^2)+((6*d1*d2)*p12^3)-r12
		}
		root <- uniroot(ftn, c(-1, 1), check.conv = TRUE)
		p12 <- root$root
		p12
	}
	inter <- matrix(0, k, k)
	for(i in seq_len(k)) {
		for(j in i:k) {
		    if(i == j) next
			inter[i,j] <- solve.p12(cor[i,j],constants[i,1],constants[j,1],constants[i,2],constants[j,2],
			                        constants[i,3],constants[j,3],
			                        constants[i,4],constants[j,4])
			inter[j, i] <- inter[i,j]
		}
	}
	Z <- t(chol_corr %*% t(matrix(rnorm(n*k), ncol=k)))
	Z2 <- Z^2
	Z3 <- Z^3

	## Generate multivariate distribution with desired property
	Y <- matrix(0, nrow = n, ncol = k)
	for(i in 1:k) {
		Y[ ,i] <- constants[i,1] + constants[i,2]*Z[,i] +
		    constants[i,3]*Z2[,i] + constants[i,4]*Z3[,i]
	}
	Y <- t(t(Y) * sds + mean)
	Y
}

#' Generate non-normal data with Headrick's (2002) method
#'
#' Generate multivariate non-normal distributions using the fifth-order polynomial method described by Headrick (2002).
#'
#' This function is primarily a wrapper for the code written by Oscar L. Olvera Astivia (last edited Feb 26, 2015)
#' with some modifications (e.g., better starting values for the Newton optimizer, passing previously saved
#' coefs, etc).
#'
#' @param n number of samples to draw
#' @param mean a vector of k elements for the mean of the variables
#' @param sigma desired k x k covariance matrix between bivariate non-normal variables
#' @param skew a vector of k elements for the skewness of the variables
#' @param kurt a vector of k elements for the kurtosis of the variables
#' @param gam3 (optional) explicitly supply the gamma 3 value? Default computes this internally
#' @param gam4 (optional) explicitly supply the gamma 4 value? Default computes this internally
#' @param return_coefs logical; return the estimated coefficients only? See below regarding why this is useful.
#' @param coefs (optional) supply previously estimated coefficients? This is useful when there must be multiple
#'   data sets drawn and will avoid repetitive computations. Must be the object returned after passing
#'   \code{return_coefs = TRUE}
#' @param control a list of control parameters when locating the polynomial coefficients
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2016). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' Headrick, T. C. (2002). Fast fifth-order polynomial transforms for generating univariate and
#' multivariate nonnormal distributions. \emph{Computational Statistics & Data Analysis, 40}, 685-711.
#'
#' Olvera Astivia, O. L., & Zumbo, B. D. (2015). A Cautionary Note on the Use of the Vale and Maurelli
#' Method to Generate Multivariate, Nonnormal Data for Simulation Purposes.
#' \emph{Educational and Psychological Measurement, 75}, 541-567.
#'
#' @author Oscar L. Olvera Astivia and Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @aliases rHeadrick
#' @export
#' @examples
#'
#' \dontrun{
#' set.seed(1)
#'
#' N <- 200
#' mean <- c(rep(0,4))
#' Sigma <- matrix(.49, 4, 4)
#' diag(Sigma) <- 1
#' skewness <- c(rep(1,4))
#' kurtosis <- c(rep(2,4))
#'
#' nonnormal <- rHeadrick(N, mean, Sigma, skewness, kurtosis)
#' # cor(nonnormal)
#' # psych::describe(nonnormal)
#'
#' #-----------
#' # compute the coefficients, then supply them back to the function to avoid
#' # extra computations
#'
#' cfs <- rHeadrick(N, mean, Sigma, skewness, kurtosis, return_coefs = TRUE)
#' cfs
#'
#' # compare
#' system.time(nonnormal <- rHeadrick(N, mean, Sigma, skewness, kurtosis))
#' system.time(nonnormal <- rHeadrick(N, mean, Sigma, skewness, kurtosis,
#'                                    coefs=cfs))
#' }
#'
rHeadrick <- function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
                      skew = rep(0, nrow(sigma)), kurt = rep(0, nrow(sigma)),
                      gam3 = NaN, gam4=NaN, return_coefs = FALSE, coefs = NULL,
                      control = list(trace = FALSE, max.ntry = 15, obj.tol = 1e-10, n.valid.sol = 1)){

    sd <- diag(sigma)
    corr <- cov2cor(sigma)
    chol_corr <- t(chol(corr))
    skewness <- skew
    kurtosis <- kurt

    headrick02.poly.coeff <- function(skewness, kurtosis, gam3, gam4, control){

        gam1 <- skewness
        gam2 <- kurtosis

        gam <- c(gam1, gam2, gam3, gam4)

        obj.fun <- function(x, gam){

            if(length(x) != 6){
                stop("coefficients of fifth-order polynomial should be length-six")
            }

            c0 <- x[1]
            c1 <- x[2]
            c2 <- x[3]
            c3 <- x[4]
            c4 <- x[5]
            c5 <- x[6]

            gam1 <- gam[1]
            gam2 <- gam[2]
            gam3 <- gam[3]
            gam4 <- gam[4]

            eq.18 <- 0 + c0 + c2 + 3 * c4
            eq.22 <- -1 + c1^2 + 2 * c2^2 + 24 * c2 * c4 +
                6 * c1 * (c3 + 5 * c5) +
                3 * (5 * c3^2 + 32 * c4^2 + 70 * c3 * c5 + 315 * c5^2)

            eq.B1 <- -gam1 + 2 * (
                4 * c2^3 + 108 * c2^2 * c4 + 3 * c1^2 * (c2 + 6 * c4) +
                    18 * c1 * (2 * c2 * c3 + 16 * c3 * c4 + 15 * c2 * c5 + 150 * c4 * c5) +
                    9 * c2 * (15 * c3^2 + 128 * c4^2 + 280 * c3 * c5 + 1575 * c5^2) +
                    54 * c4 * (25 * c3^2 + 88 * c4^2 + 560 * c3 * c5 + 3675 * c5^2)
            )
            eq.B2 <- -gam2 + 24 * (
                2 * c2^4 + 96 * c2^3 * c4 + c1^3 * (c3 + 10 * c5) +
                    30 * c2^2 * (6 * c3^2 + 64 * c4^2 + 140 * c3 * c5 + 945 * c5^2) +
                    c1^2 * (2 * c2^2 + 18 * c3^2 + 36 * c2 * c4 + 192 * c4^2 + 375 * c3 * c5 + 2250 * c5^2) +
                    36 * c2 * c4 * (125 * c3^2 + 528 * c4^2 + 3360 * c3 * c5 + 25725 * c5^2) +
                    3 * c1 * (45 * c3^3 + 1584 * c3 * c4^2 + 1590 * c3^2 * c5 + 21360 * c4^2 * c5 + 21525 * c3 * c5^2 +
                                  110250 * c5^3 + 12 * c2^2 * (c3 + 10 * c5) + 8 * c2 * c4 * (32 * c3 + 375 * c5)) +
                    9 * (45 * c3^4 + 8704 * c4^4 + 2415 * c3^3 * c5 + 932400 * c4^2 * c5^2 + 3018750 * c5^4 +
                             20 * c3^2 * (178 * c4^2 + 2765 * c5^2) + 35 * c3 * (3104 * c4^2 * c5 + 18075 * c5^3))
            )
            eq.B3 <- -gam3 + 24 * (
                16 * c2^5 + 5 * c1^4 * c4 + 1200 * c2^4 * c4 + 10 * c1^3 * (3 * c2 * c3 + 42 * c3 * c4 + 40 * c2 * c5 + 570 * c4 * c5) +
                    300 * c2^3 * (10 * c3^2 + 128 * c4^2 + 280 * c3 * c5 + 2205 * c5^2) +
                    1080 * c2^2 * c4 * (125 * c3^2 + 3920 * c3 * c5 + 28 * (22 * c4^2 + 1225 * c5^2)) +
                    10 * c1^2 * (2 * c2^3 + 72 * c2^2 * c4 + 3 * c2 * (24 * c3^2 + 320 * c4^2 + 625 * c3 * c5 + 4500 * c5^2) +
                                     9 * c4 * (109 * c3^2 + 528 * c4^2 + 3130 * c3 * c5 + 24975 * c5^2)) +
                    30 * c1 * (8 * c2^3 * (2 * c3 + 25 * c5) + 40 * c2^2 * c4 * (16 * c3 + 225 * c5) +
                                   3 * c2 * (75 * c3^3 + 3168 * c3 * c4^2 + 3180 * c3^2 * c5 + 49840 * c4^2 * c5 + 50225 * c3 * c5^2 + 294000 * c5^3) +
                                   6 * c4 * (555 * c3^3 + 8704 * c3 * c4^2 + 26225 * c3^2 * c5 + 152160 * c4^2 * c5 + 459375 * c3 * c5^2 + 2963625 * c5^3)) +
                    90 * c2 * (270 * c3^4 + 16905 * c3^3 * c5 + 280 * c3^2 * (89 * c4^2 + 1580 * c5^2) +
                                   35 * c3 * (24832 * c4^2 * c5 + 162675 * c5^3) +
                                   4 * (17408 * c4^4 + 2097900 * c4^2 * c5^2 + 7546875 * c5^4)) +
                    27 * c4 * (14775 * c3^4 + 1028300 * c3^3 * c5 + 50 * c3^2 * (10144 * c4^2 + 594055 * c5^2) +
                                   700 * c3 * (27904 * c4^2 * c5 + 598575 * c5^3) +
                                   3 * (316928 * c4^4 + 68908000 * c4^2 * c5^2 + 806378125 * c5^4))
            )
            eq.B4 <- -gam4 + 120 * (
                32 * c2^6 + 3456 * c2^5 * c4 + 6 * c1^5 * c5 +
                    3 * c1^4 * (9 * c3^2 + 16 * c2 * c4 + 168 * c4^2 + 330 * c3 * c5 + 2850 * c5^2) +
                    720 * c2^4 * (15 * c3^2 + 224 * c4^2 + 490 * c3 * c5 + 4410 * c5^2) +
                    6048 * c2^3 * c4 * (125 * c3^2 + 704 * c4^2 + 4480 * c3 * c5 + 44100 * c5^2) +
                    12 * c1^3 * (4 * c2^2 * (3 * c3 + 50 * c5) + 60 * c2 * c4 * (7 * c3 + 114 * c5) +
                                     3 * (24 * c3^3 + 1192 * c3 * c4^2 + 1170 * c3^2 * c5 + 20440 * c4^2 * c5 +
                                              20150 * c3 * c5^2 + 124875 * c5^3)) +
                    216 * c2^2 * (945 * c3^4 + 67620 * c3^3 * c5 +
                                      560 * c3^2 * (178 * c4^2 + 3555 * c5^2) +
                                      315 * c3 * (12416 * c4^2 * c5 + 90375 * c5^3) +
                                      6 * (52224 * c4^4 + 6993000 * c4^2 * c5^2 + 27671875 * c5^4)) +
                    6 * c1^2 * (8 * c2^4 + 480 * c2^3 * c4 +
                                    180 * c2^2 * (4 * c3^2 + 64 * c4^2 + 125 * c3 * c5 + 1050 * c5^2) +
                                    72 * c2 * c4 * (327 * c3^2 + 1848 * c4^2 + 10955 * c3 * c5 + 99900 * c5^2) +
                                    9 * (225 * c3^4 + 22824 * c3^2 * c4^2 + 69632 * c4^4 + 15090 * c3^3 * c5 +
                                             830240 * c3 * c4^2 * c5 + 412925 * c3^2 * c5^2 +
                                             8239800 * c4^2 * c5^2 + 5475750 * c3 * c5^3 + 29636250 * c5^4)) +
                    1296 * c2 * c4 * (5910 * c3^4 + 462735 * c3^3 * c5 +
                                          c3^2 * (228240 * c4^2 + 14851375 * c5^2) +
                                          175 * c3 * (55808 * c4^2 * c5 + 1316865 * c5^3) +
                                          3 * (158464 * c4^4 + 37899400 * c4^2 * c5^2 + 483826875 * c5^4)) +
                    27 * (9945 * c3^6 + 92930048 * c4^6 + 1166130 * c3^5 * c5 +
                              35724729600 * c4^4 * c5^2 + 977816385000 * c4^2 * c5^4 +
                              1907724656250 * c5^6 + 180 * c3^4 * (16082 * c4^2 + 345905 * c5^2) +
                              140 * c3^3 * (1765608 * c4^2 * c5 + 13775375 * c5^3) +
                              15 * c3^2 * (4076032 * c4^4 + 574146160 * c4^2 * c5^2 +
                                               2424667875 * c5^4) +
                              210 * c3 * (13526272 * c4^4 * c5 + 687499200 * c4^2 * c5^3 +
                                              1876468125 * c5^5)) +
                    18 * c1 * (80 * c2^4 * (c3 + 15 * c5) + 160 * c2^3 * c4 * (32 * c3 + 525 * c5) +
                                   12 * c2^2 * (225 * c3^3 + 11088 * c3 * c4^2 + 11130 * c3^2 * c5 +
                                                    199360 * c4^2 * c5 + 200900 * c3 * c5^2 + 1323000 * c5^3) +
                                   24 * c2 * c4 * (3885 * c3^3 + 69632 * c3 * c4^2 + 209800 * c3^2 * c5 +
                                                       1369440 * c4^2 * c5 + 4134375 * c3 * c5^2 + 29636250 * c5^3) +
                                   9 * (540 * c3^5 + 48585 * c3^4 * c5 +
                                            20 * c3^3 * (4856 * c4^2 + 95655 * c5^2) +
                                            80 * c3^2 * (71597 * c4^2 * c5 + 513625 * c5^3) +
                                            4 * c3 * (237696 * c4^4 + 30726500 * c4^2 * c5^2 +
                                                          119844375 * c5^4) +
                                            5 * c5 * (4076032 * c4^4 + 191074800 * c4^2 * c5^2 +
                                                          483826875 * c5^4)))
            )

            eqs <- c(eq.18, eq.22, eq.B1, eq.B2, eq.B3, eq.B4)
            obj <- sum(eqs^2)
            obj
        }

        OPT <- list()
        ntry <- 0
        cnt <- 0
        start <- c(1, numeric(5))
        while(ntry+1 < control[["max.ntry"]]){

            ntry <- ntry + 1
            opt <- nlminb(start = start, objective = obj.fun, lower = -2, upper = 2,
                          control = list(abs.tol = 1e-10, rel.tol = 1e-10, eval.max = 1e6, iter.max = 1e6),
                          gam = gam)
            if(opt$convergence == 0 && opt$objective <= control[["obj.tol"]]){
                cnt <- cnt + 1
                OPT[[cnt]] <- opt
            }


            if(length(OPT) >= control[["n.valid.sol"]] ||
               (opt$objective <= control[["obj.tol"]] && opt$convergence == 0)){
                break
            }
            start <- sapply(c(.5, .25, .1, .01, .001, .0001), function(x) rnorm(1, sd = x))
        }

        if(length(OPT) == 0){
            stop(paste0("cannot find the coefficients of polynomial after ", control[["max.ntry"]], " attempts"))
        }

        min.obj <- 1e20
        idx <- -1
        for(i in 1:length(OPT)){
            if(OPT[[i]]$objective < min.obj){
                min.obj <- OPT[[i]]$objective
                idx <- i
            }
        }

        coeff <- OPT[[idx]]$par
        list(coeff = coeff, min.obj = min.obj)
    }

    headrick02.corr.match <- function(poly.coeff, corr){

        obj.fun2 <- function(x, c0, c1, c2, c3, c4, c5, i, j, rho.Y){

            eq <- -rho.Y +
                3*c0[j]*c4[i] + 3*c2[j]*c4[i] + 9*c4[i]*c4[j] + c0[i]*(c0[j] + c2[j] + 3*c4[j]) +
                c1[i]*c1[j]*x + 3*c1[j]*c3[i]*x + 3*c1[i]*c3[j]*x + 9*c3[i]*c3[j]*x +
                15*c1[j]*c5[i]*x + 45*c3[j]*c5[i]*x + 15*c1[i]*c5[j]*x +
                45*c3[i]*c5[j]*x + 225*c5[i]*c5[j]*x + 12*c2[j]*c4[i]*x^2 +
                72*c4[i]*c4[j]*x^2 + 6*c3[i]*c3[j]*x^3 + 60*c3[j]*c5[i]*x^3 +
                60*c3[i]*c5[j]*x^3 + 600*c5[i]*c5[j]*x^3 + 24*c4[i]*c4[j]*x^4 +
                120*c5[i]*c5[j]*x^5 +
                c2[i]*(c0[j] + c2[j] + 3*c4[j] + 2*c2[j]*x^2 + 12*c4[j]*x^2)
            eq
        }

        k <- ncol(poly.coeff)

        c0 <- as.vector(poly.coeff[1,], mode = "numeric")
        c1 <- as.vector(poly.coeff[2,], mode = "numeric")
        c2 <- as.vector(poly.coeff[3,], mode = "numeric")
        c3 <- as.vector(poly.coeff[4,], mode = "numeric")
        c4 <- as.vector(poly.coeff[5,], mode = "numeric")
        c5 <- as.vector(poly.coeff[6,], mode = "numeric")


        l <- 0
        inter.corr <- diag(1, k)
        obj <- matrix(NA, k , k)
        for(i in 1:(k-1)){
            for(j in (i+1):k){
                l <- l + 1
                rho.Y <- corr[i, j]
                opt <- uniroot(obj.fun2, interval=c(-1, 1), c0 = c0, c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5,
                                i = i, j = j, rho.Y = rho.Y, check.conv = TRUE)
                inter.corr[i, j] <- opt$root
                inter.corr[j, i] <- opt$root
                obj[i, j] <- opt$f.root
            }
        }

        list(inter.corr = inter.corr, obj = obj)

    }
    # -----------------------------------------------------------------------------

    ##setting up

    if(is.null(control[["trace"]])){
        control[["trace"]] <- FALSE
    }

    if(is.null(control[["max.ntry"]])){
        control[["max.ntry"]] <- 15
    }

    if(is.null(control[["obj.tol"]])){
        control[["obj.tol"]] <- 1e-10
    }

    if(is.null(control[["n.valid.sol"]])){
        control[["n.valid.sol"]] <- 1
    }

    start = Sys.time()

    k <- nrow(corr)

    if (is.nan(gam3[1]) && !is.nan(gam4[1])){
        stop("Error: Please provide both gam3 and gam4, or neither.")
    }

    if (is.nan(gam4[1]) && !is.nan(gam3[1])){
        stop("Error: Please provide both gam3 and gam4, or neither.")
    }

    default_gam3 = FALSE
    default_gam4 = FALSE

    if(is.nan(gam3[1])){
        gam3 = pmax(skewness, kurtosis)
        default_gam3 = TRUE
    }

    if(is.nan(gam4[1])){
        gam4 = pmax(skewness,kurtosis)^2
        default_gam4 = TRUE
    }

    len <- c(length(mean), length(sd), length(skewness), length(kurtosis), length(gam3), length(gam4))
    if(var(len) != 0){
        stop("Lengths of mean, std, skewness, kurtosis, gam3 and gam4 must be equal")
    }

    if(len[1] != 1 && len[1] != k){
        stop("Inconsistent length/dim of moments and correlation")
    }

    if(len[1] == 1){
        mean <- rep(mean, k)
        sd <- rep(sd, k)
        skewness <- rep(skewness, k)
        kurtosis <- rep(kurtosis, k)
        gam3 <- rep(gam3, k)
        gam4 <- rep(gam4, k)
    }

    for (i in 1:k){
        if (kurtosis[i] <= skewness[i]^2 - 2){
            stop("Error: the ", i," th component of kurtosis is not bigger than skewness squared minus 2.\n")
        }
    }

    ##Solve for coefficients c0-c5 using equation 18, 22, B1-B4

    coeff <- NULL
    obj.poly.coeff <- NULL
    poly.coeff <- NULL
    gam4_fit = rep(0,k)
    gam3_fit = rep(0,k)

    if(!is.null(coefs)){
        colnames(coefs) <- c('g1', 'g2', 'tol', 'g3', 'g4', paste0('c', 0:5))
        compiled <- coefs
    } else compiled <- matched <- matrix(0)[-1L,, drop=FALSE]

    for(i in 1:k){

        if(nrow(compiled) > 0L)
            matched = compiled[compiled["g1"]==skewness[i] & compiled["g2"]==kurtosis[i] &
                                   compiled["tol"]<=control[["obj.tol"]],]

        if (control[["trace"]]){
            cat("Time elapsed ", as.numeric(Sys.time()-start, units="secs") ,
                " seconds. Start fitting c0 - c5 for distribution ", i, ".\n", sep="")
        }

        if (default_gam3 && default_gam4){

            if (nrow(matched) > 0){
            	if (control[["trace"]]){
            		cat("Configuration found in compiled list. Compiled coefficients will be used. \n")
            	}
            	matched = matched[order(-matched$tol),]
            	curr.coeff = c(as.vector(matched[1,c("c0", "c1", "c2", "c3", "c4", "c5")]))
            	curr.obj = matched[1, "tol"]
            	gam3_fit[i] = matched[1, "g3"]
            	gam4_fit[i] = matched[1, "g4"]
            }
            else{
            j <- 1
            j3 <- 1
            poly.coeff <- NULL
            upper = 4
            # step_size = 4
            iterations = 0
            while(j<=15 && j3<=15 && is.null(poly.coeff)){
                iterations = iterations + 1
                tic = Sys.time()
                gam3_fit[i] = gam3[i]/2*2^j3
                gam4_fit[i] = gam4[i]/2*2^j
                poly.coeff <- headrick02.poly.coeff(skewness[i], kurtosis[i], gam3_fit[i], gam4_fit[i], control = control)
                if(is.null(poly.coeff)){
                    if (control[["trace"]]){
                        cat("Trial ",iterations," unsuccessful. Time spent: ", as.numeric(Sys.time()-tic, units="secs") , " seconds.\n", sep = "")
                    }
                    j3 <- j3+1
                    if(j3==upper+1 && j<upper){
                        j3 <- 1
                        j <- j+1
                    }
                    else if(j3==upper+1 && j==upper){
                        # input = "y"
                        stop("No solutions found after ", iterations," iterations")
                    }
                }
            }
            if(!is.null(poly.coeff)){
                if (control[["trace"]]){
                    cat("Trial ",iterations," successful. Time spent: ", as.numeric(Sys.time()-tic, units="secs") , " seconds.\n", sep = "")
                }
                curr.coeff = poly.coeff$coeff
                curr.obj = poly.coeff$min.obj
                to_append = matrix(c(skewness[i],kurtosis[i],curr.obj,gam3_fit[i], gam4_fit[i], curr.coeff), nrow=1)
            }

            if (is.null(poly.coeff)){
                cat("Error: no solution found for the combination of skewness: ", skewness[i], "; kurtosis: ",
                    kurtosis[i], ".\n", sep = "")
                return(NULL)
            }
            }
        }

        else{
            matched = compiled[compiled["g1"]==skewness[i] & compiled["g2"]==kurtosis[i]
            	& compiled["g3"] == gam3[i] & compiled["g4"] == gam4[i]
            	& compiled["tol"]<=control[["obj.tol"]],]
            if (nrow(matched)>0){
            	if (control[["trace"]]){
            		cat("Configuration found in compiled list. Compiled coefficients will be used. \n")
            	}
            	matched = matched[order(-matched$tol)]
            	curr.coeff = c(as.vector(matched[1,c("c0", "c1", "c2", "c3", "c4", "c5")]))
            	curr.obj = matched[1, "tol"]
            	gam3_fit[i] = matched[1, "g3"]
            	gam4_fit[i] = matched[1, "g4"]
            }
            else {
            gam3_fit[i]  = gam3[i]
            gam4_fit[i] = gam4[i]
            poly.coeff <- headrick02.poly.coeff(skewness[i], kurtosis[i], gam3_fit[i], gam4_fit[i], control = control)
            if(is.null(poly.coeff)){
                cat("Error: no solution found for the combination of skewness: ", skewness[i], "; kurtosis: ",
                    kurtosis[i], " gam3: ", gam3_fit[i], "; gam4: ", gam4_fit[i], ".\n", sep = "")
                return
            }
            else{
                curr.coeff = poly.coeff$coeff
                curr.obj = poly.coeff$min.obj
                to_append = matrix(c(skewness[i],kurtosis[i],curr.obj,gam3_fit[i], gam4_fit[i], curr.coeff), nrow=1)
            }
        }
        }

        coeff <- c(coeff, curr.coeff)
        obj.poly.coeff <- c(obj.poly.coeff, curr.obj)
    }

    coeff = matrix(coeff, nrow = 6)

    desired.moments <-data.frame(mean = mean, sd=sd, skewness = skewness, kurtosis = kurtosis,
                                 gam3 = gam3_fit, gam4=gam4_fit)
    rownames(desired.moments) <- paste0("Y", 1:nrow(desired.moments))

    if (control[["trace"]]){
        cat("Finished fitting c0 - c5. Time elapsed ",
            as.numeric(Sys.time()-start, units="secs") ," seconds. \n", sep="")
    }

    summary.poly.coeff <- rbind(obj.poly.coeff, coeff)
    colnames(summary.poly.coeff) <- paste0("Distribution ", 1:ncol(summary.poly.coeff))
    rownames(summary.poly.coeff) <- c("obj value @ convergence", paste0("c", 0:5))
    colnames(coeff) <- paste0("Distribution ", 1:ncol(summary.poly.coeff))
    rownames(coeff) <- paste0("c", 0:5)

    ##Solve for intermediate correlation using equation 26
    if(k>1){
        if (control[["trace"]]){
            cat("\nStart solving for intermediate correlation matrix...\n")
        }

        corr.match <- headrick02.corr.match(coeff, corr)
        inter.corr <- corr.match$inter.corr
        obj.corr.match <- corr.match$obj
        colnames(inter.corr) <- paste0("Z", 1:ncol(inter.corr))
        rownames(inter.corr) <- paste0("Z", 1:nrow(inter.corr))

        colnames(obj.corr.match) <- paste0("Z", 1:ncol(obj.corr.match))
        rownames(obj.corr.match) <- paste0("Z", 1:nrow(obj.corr.match))

        if (control[["trace"]]){
            cat("Finished solving for intermediate correlation matrix. Time elapsed ",
                as.numeric(Sys.time()-start, units="secs") ," seconds.\n", sep="")
        }
    }
    else{
        inter.corr <- corr
        colnames(inter.corr) <- paste0("Z", 1:ncol(inter.corr))
        rownames(inter.corr) <- paste0("Z", 1:nrow(inter.corr))

    }

    c0 <- as.vector(coeff[1,], mode = "numeric")
    c1 <- as.vector(coeff[2,], mode = "numeric")
    c2 <- as.vector(coeff[3,], mode = "numeric")
    c3 <- as.vector(coeff[4,], mode = "numeric")
    c4 <- as.vector(coeff[5,], mode = "numeric")
    c5 <- as.vector(coeff[6,], mode = "numeric")

    ## Generate intermediate normal distribution with desired intermediate correlation
    Z <- t(chol_corr %*% t(matrix(rnorm(n*k), ncol=k)))
    Z2 <- Z^2
    Z3 <- Z^3
    Z4 <- Z^4
    Z5 <- Z^5

    ## Generate multivariate distribution with desired property
    Y <- matrix(0, nrow = n, ncol = k)
    for(i in 1:k){
        Y[, i] <- mean[i] + sd[i]*(c0[i] + c1[i] * Z[, i] + c2[i] * Z2[, i] +
                                       c3[i] * Z3[, i] + c4[i] * Z4[, i] + c5[i] * Z5[, i])
    }

    if(return_coefs) Y <- data.frame(skew=skew, kurt=kurt, conv.tol=summary.poly.coeff[1L,],
                                       gam3=gam3, gam4=gam4, t(summary.poly.coeff)[,-1L])
    Y
}

#' Generate data with the multivariate normal (i.e., Gaussian) distribution
#'
#' Function generates data from the multivariate normal distribution given some mean vector and/or
#' covariance matrix.
#'
#' @param n number of observations to generate
#'
#' @param mean mean vector, default is \code{rep(0, length = ncol(sigma))}
#'
#' @param sigma positive definite covariance matrix, default is \code{diag(length(mean))}
#'
#' @return a numeric matrix with columns equal to \code{length(mean)}
#'
#' @seealso \code{\link{runSimulation}}
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2016). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
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
#' # random normal values with mean [5, 10] and variances [3,6], and covariance 2
#' sigma <- matrix(c(3,2,2,6), 2, 2)
#' mu <- c(5,10)
#' x <- rmvnorm(1000, mean = mu, sigma = sigma)
#' head(x)
#' summary(x)
#' plot(x[,1], x[,2])
#'
#'
rmvnorm <- function (n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)))
{
    # code borrowed and modified from mvtnorm package, October 21, 2017
    if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps),
                     check.attributes = FALSE))
        stop("sigma must be a symmetric matrix")
    if (length(mean) != nrow(sigma))
        stop("mean and sigma have non-conforming size")
    ev <- eigen(sigma, symmetric = TRUE)
    if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1])))
        warning("sigma is numerically not positive semi-definite")
    R <- t(ev$vectors %*% (t(ev$vectors) * sqrt(pmax(ev$values, 0))))
    retval <- matrix(rnorm(n * ncol(sigma)), nrow = n) %*% R
    retval <- sweep(retval, 2, mean, "+")
    colnames(retval) <- names(mean)
    retval
}

#' Generate data with the multivariate t distribution
#'
#' Function generates data from the multivariate t distribution given a covariance matrix,
#' non-centrality parameter (or mode), and degrees of freedom.
#'
#' @param n number of observations to generate
#'
#' @param sigma positive definite covariance matrix
#'
#' @param df degrees of freedom. \code{df = 0} and \code{df = Inf}
#'   corresponds to the multivariate normal distribution
#'
#' @param delta the vector of non-centrality parameters of length \code{n}
#'   which specifies the either the modes (default) or non-centrality parameters
#'
#' @param Kshirsagar logical; triggers whether to generate data with non-centrality parameters
#'   or to adjust the simulated data to the mode of the distribution. The default uses the mode
#'
#' @return a numeric matrix with columns equal to \code{ncol(sigma)}
#'
#' @seealso \code{\link{runSimulation}}
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2016). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
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
#' # random t values given variances [3,6], covariance 2, and df = 15
#' sigma <- matrix(c(3,2,2,6), 2, 2)
#' x <- rmvt(1000, sigma = sigma, df = 15)
#' head(x)
#' summary(x)
#' plot(x[,1], x[,2])
#'
#'
rmvt <- function (n, sigma, df, delta = rep(0, nrow(sigma)),
                  Kshirsagar = FALSE)
{
    # code borrowed and modified from mvtnorm package, October 21, 2017
    stopifnot(!missing(n))
    stopifnot(!missing(sigma))
    stopifnot(!missing(df))
    if (length(delta) != nrow(sigma))
        stop("delta and sigma have non-conforming size")
    stopifnot(df >= 0)
    if (df == 0 || !is.finite(df))
        return(rmvnorm(n, mean = delta, sigma = sigma))
    ret <- if(Kshirsagar){
        rmvnorm(n, mean = delta, sigma = sigma) / sqrt(rchisq(n, df)/df)
    } else {
        sims <- rmvnorm(n, sigma = sigma) / sqrt(rchisq(n, df)/df)
        sweep(sims, 2, delta, "+")
    }
    ret
}

#' Generate data with the inverse Wishart distribution
#'
#' Function generates data in the form of symmetric matrices from the inverse
#' Wishart distribution given a covariance matrix and degrees of freedom.
#'
#' @param n number of matrix observations to generate. By default \code{n = 1}, which returns a single
#'   symmetric matrix. If \code{n > 1} then a list of \code{n} symmetric matrices are returned instead
#'
#' @param sigma positive definite covariance matrix
#'
#' @param df degrees of freedom
#'
#' @return a numeric matrix with columns equal to \code{ncol(sigma)} when \code{n = 1}, or a list
#'   of \code{n} matrices with the same properties
#'
#' @seealso \code{\link{runSimulation}}
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2016). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 249-281.
#' \doi{10.20982/tqmp.16.4.p249}
#'
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
#' # random inverse Wishart matrix given variances [3,6], covariance 2, and df=15
#' sigma <- matrix(c(3,2,2,6), 2, 2)
#' x <- rinvWishart(sigma = sigma, df = 15)
#' x
#'
#' # list of matrices
#' x <- rinvWishart(20, sigma = sigma, df = 15)
#' x
#'
rinvWishart <- function(n = 1, df, sigma){
    ret <- rWishart(n, df=df, Sigma = solve(sigma))
    ret <- apply(ret, 3, solve)
    ret <- lapply(1L:ncol(ret), function(ind, mats)
        matrix(mats[,ind], ncol(sigma), ncol(sigma)), mats=ret)
    if(n == 1L) ret <- ret[[1L]]
    ret
}
