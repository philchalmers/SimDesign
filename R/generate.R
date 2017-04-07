#' Generate Non-normal Distributions with Vale & Maurelli (1983) method
#'
#' Generate multivariate non-normal distributions using the method described by Vale & Maurelli (1983). If only
#' a single variable is generated then this function is equivalent to the method described by Fleishman (1978).
#'
#' This function is primarily a wrapper for the code written by Cengiz Zopluoglu (last edited April 20, 2011)
#' with some slight modifications.
#'
#' @param n sample size
#' @param mean a vector of k elements for the mean of the variables
#' @param sigma desired k x k covariance matrix between bivariate non-normal variables
#' @param skew a vector of k elements for the skewness of the variables
#' @param kurt a vector of k elements for the kurtosis of the variables
#'
#' @references
#'
#' Fleishman, A. I. (1978). A method for simulating non-normal distributions.
#' \emph{Psychometrika, 43}, 521-532.
#'
#' Vale, C. & Maurelli, V. (1983). Simulating multivariate nonnormal distributions.
#' \emph{Psychometrika, 48}(3), 465-471.
#' @author Cengiz Zopluoglu and Phil Chalmers
#' @aliases rValeMaurelli
#' @export
#' @examples
#'
#' set.seed(1)
#'
#' # univariate with skew
#' nonnormal <- rValeMaurelli(10000, mean=10, sigma=5, skew=1)
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
	#Internal Function to compute the a,b,c,d for a variable given the skewness and
	#kurtosis. Use Newton-Raphson Iteration with a Jacobian matrix to solve the system of
	#non-linear equations. Equation 2,3, and 4 in Vale&Maurelli(1983)
    k <- ncol(cor)
	tol=.00001
	constant <- function(sk,ku,start){ #Start Internal Function 1
		#sk , desired skewness
		#ku , desired kurtosis
		#start, starting values for the iteratin, based on Fleishman(1978) using c(1,0,0)
		#is reasonable
		start=c(1,0,0)
		max.iter <- 500
		F <- function(x){
			F <- matrix(0,nrow=3)
			b=x[1]
			c=x[2]
			d=x[3]
			F[1]= b^2+6*b*d+2*c^2+15*d^2-1
			F[2]= 2*c*(b^2+24*b*d+105*d^2+2)-sk
			F[3]=24*(b*d+c^2*(1+b^2+28*b*d)+d^2*(12+48*b*d+141*c^2+225*d^2))-ku
			F
		}
		J <- function(x){
			b=x[1]
			c=x[2]
			d=x[3]
			j=matrix(0,ncol=3,nrow=3)
			j[1,1]= 2*b+6*d
			j[1,2]= 4*c
			j[1,3]= 6*b+30*d
			j[2,1]= 4*b*c+48*c*d
			j[2,2]= 2*b^2+48*b*d+210*d^2+4
			j[2,3]= 48*b*c+420*c*d
			j[3,1]=24*d+48*c^2*b
			j[3,2]=48*c+48*c*b^2+1344*c*b*d+6768*c*d^2
			j[3,3]=24*b+672*c^2*b+576*d+3456*b*d^2+6768*d*c^2+21600*d^3
			j
		}
		x0 <- start
		fx <- F(x0)
		jx <- J(x0)
		d <- solve(J(x0))%*%F(x0)
		iter <- 0
		d=det(J(x0))
		if (identical(all.equal(d,0),TRUE))
		{cat("Jacobian has no inverse. Try a different initial point.","\n")
			break}
		while((abs(d)> tol) && (iter < max.iter)) {
			x0 <- x0-solve(J(x0))%*%F(x0)
			d <- solve(J(x0))%*%F(x0)
			fx <- F(x0)
			jx <- J(x0)
			iter <- iter+1
		}
		x0
	} #End internal function 1
	#Compute the constants a,b,c, and d for each variable with a desired skewness and
	#kurtosis
	constants <- matrix(nrow=k,ncol=4)
	for(i in 1:k) {
		constants[i,2:4]=t(constant(skew[i],kurt[i],start=c(1,0,0)))
		constants[i,1]=-(constants[i,3])
	}
	#Internal Function to solve the polynomial function to find the intermediate
	#correlation between two normal variables for a given desired correlation between two
	#non-normal variables and constants a,b,c,d Use Newton-Raphson iteration to
	#approximate the root
	solve.p12 <- function(r12,a1,a2,b1,b2,c1,c2,d1,d2) { #Start Internal Function 2
		max.iter=500
		start=.5
		ftn <- function(p12) {
			a <-((b1*b2+3*b1*d2+3*d1*b2+9*d1*d2)*p12)+((2*c1*c2)*p12^2)+((6*d1*d2)*p12^3)-r12
			b <-(b1*b2+3*b1*d2+3*d1*b2+9*d1*d2)+((4*c1*c2)*p12)+((12*d1*d2)*p12^2)
			c(a,b)
		}
		p12 <- start
		fx <- ftn(p12)
		iter <- 0
		while((abs(fx[1]) > tol) && (iter < max.iter)) {
			p12 <- p12-fx[1]/fx[2]
			fx <- ftn(p12)
			iter <- iter+1
		}
		p12
	} #End Internal Function 2
	#Compute the intermediate intercorrelation matrix required for normal variables
	#These normal variables are used to construct non-normal variables
	inter <- matrix(0,k,k)
	for(i in 1:k) {
		for(j in 1:k) {
			inter[i,j]=solve.p12(cor[i,j],constants[i,1],constants[j,1],constants[i,2],constants[j,2],
			                     constants[i,3],constants[j,3],
								 constants[i,4],constants[j,4])
		}
	}
	diag(inter) <- 1
	#Compute the multivariate normal variables based on the intermediate intercorrelation
	#matrix
	#Eigen decomposition of correlation matrix
	U <- eigen(inter)$vectors
	L <- eigen(inter)$values
	b <- U%*%diag(sqrt(L))
	#Creating independent multivariate normal variables
	normal <- matrix(nrow=n,ncol=k)
	for(i in 1:k) { normal[,i]=rnorm(n,0,1) }
	#Creating correlated multivariate normal variables
	d <- as.data.frame(normal%*%t(b))
	#Creating correlated non-normal multivariate variables from correlated multivariate
	#normal variables using constants a,b,c, and d
	nonnormal <- as.data.frame(matrix(nrow=n,ncol=k))
	for(i in 1:k) {
		nonnormal[,i]=constants[i,1]+
			constants[i,2]*d[,i]+constants[i,3]*d[,i]^2+constants[i,4]*d[,i]^3
	}
	nonnormal <- t(t(nonnormal) * sds + mean)
	nonnormal
}
