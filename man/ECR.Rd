% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_functions.R
\name{ECR}
\alias{ECR}
\title{Compute empirical coverage rates}
\usage{
ECR(
  CIs,
  parameter,
  tails = FALSE,
  CI_width = FALSE,
  complement = FALSE,
  names = NULL,
  unname = FALSE
)
}
\arguments{
\item{CIs}{a \code{numeric} vector or \code{matrix} of confidence interval values for a
given parameter value, where the first element/column indicates the lower confidence interval
and the second element/column the upper confidence interval. If a
vector of length 2 is passed instead then the returned value will be either a 1 or 0 to indicate
whether the parameter value was or was not within the interval, respectively. Otherwise,
the input must be a matrix with an even number of columns}

\item{parameter}{a numeric scalar indicating the fixed parameter value. Alternative, a \code{numeric}
vector object with length equal to the number of rows as \code{CIs} (use to compare sets of parameters
at once)}

\item{tails}{logical; when TRUE returns a vector of length 2 to indicate the proportion of times
the parameter was lower or higher than the supplied interval, respectively. This is mainly only
useful when the coverage region is not expected to be symmetric, and therefore is generally not
required. Note that \code{1 - sum(ECR(CIs, parameter, tails=TRUE)) == ECR(CIs, parameter)}}

\item{CI_width}{logical; rather than returning the overall coverage rate, return the
average width of the CIs instead? Useful when comparing the efficiency of different CI
estimators}

\item{complement}{logical; rather than computing the proportion of
population parameters within the CI, return the proportion outside the
advertised CI (1 - ECR = alpha). In the case where only one value is provided,
which normally would return a 0 if outside the CI or 1 if inside, the values
will be switched (useful when using, for example, CI tests of for the significance
of parameters)}

\item{names}{an optional character vector used to name the returned object. Generally useful
when more than one CI estimate is investigated at once}

\item{unname}{logical; apply \code{\link{unname}} to the results to remove any variable
names?}
}
\description{
Computes the detection rate for determining empirical coverage rates given a set of estimated
confidence intervals. Note that using \code{1 - ECR(CIs, parameter)} will provide the empirical
detection rate. Also supports computing the average width of the CIs, which may be useful when comparing
the efficiency of CI estimators.
}
\examples{

CIs <- matrix(NA, 100, 2)
for(i in 1:100){
   dat <- rnorm(100)
   CIs[i,] <- t.test(dat)$conf.int
}

ECR(CIs, 0)
ECR(CIs, 0, tails = TRUE)
ECR(CIs, 0, complement = TRUE) # proportion outside interval

# single vector input
CI <- c(-1, 1)
ECR(CI, 0)
ECR(CI, 0, complement = TRUE)
ECR(CI, 2)
ECR(CI, 2, complement = TRUE)
ECR(CI, 2, tails = TRUE)

# parameters of the same size as CI
parameters <- 1:10
CIs <- cbind(parameters - runif(10), parameters + runif(10))
parameters <- parameters + rnorm(10)
ECR(CIs, parameters)

# average width of CIs
ECR(CIs, parameters, CI_width=TRUE)

# ECR() for multiple CI estimates in the same object
parameter <- 10
CIs <- data.frame(lowerCI_1=parameter - runif(10),
                  upperCI_1=parameter + runif(10),
                  lowerCI_2=parameter - 2*runif(10),
                  upperCI_2=parameter + 2*runif(10))
head(CIs)
ECR(CIs, parameter)
ECR(CIs, parameter, tails=TRUE)
ECR(CIs, parameter, CI_width=TRUE)

# often a good idea to provide names for the output
ECR(CIs, parameter, names = c('this', 'that'))
ECR(CIs, parameter, CI_width=TRUE, names = c('this', 'that'))
ECR(CIs, parameter, tails=TRUE, names = c('this', 'that'))

}
\references{
Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
\doi{10.20982/tqmp.16.4.p248}

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
\doi{10.1080/10691898.2016.1246953}
}
\seealso{
\code{\link{EDR}}
}
\author{
Phil Chalmers \email{rphilip.chalmers@gmail.com}
}
