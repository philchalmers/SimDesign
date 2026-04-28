# Compute empirical coverage rates

Computes the detection rate for determining empirical coverage rates
given a set of estimated confidence intervals. Note that using
`1 - ECR(CIs, parameter)` will provide the empirical detection rate.
Also supports computing the average width of the CIs, which may be
useful when comparing the efficiency of CI estimators.

## Usage

``` r
ECR(
  CIs,
  parameter,
  tails = FALSE,
  CI_width = FALSE,
  complement = FALSE,
  names = NULL,
  unname = FALSE
)
```

## Arguments

- CIs:

  a `numeric` vector or `matrix` of confidence interval values for a
  given parameter value, where the first element/column indicates the
  lower confidence interval and the second element/column the upper
  confidence interval. If a vector of length 2 is passed instead then
  the returned value will be either a 1 or 0 to indicate whether the
  parameter value was or was not within the interval, respectively.
  Otherwise, the input must be a matrix with an even number of columns

- parameter:

  a numeric scalar indicating the fixed parameter value. Alternative, a
  `numeric` vector object with length equal to the number of rows as
  `CIs` (use to compare sets of parameters at once)

- tails:

  logical; when TRUE returns a vector of length 2 to indicate the
  proportion of times the parameter was lower or higher than the
  supplied interval, respectively. This is mainly only useful when the
  coverage region is not expected to be symmetric, and therefore is
  generally not required. Note that
  `1 - sum(ECR(CIs, parameter, tails=TRUE)) == ECR(CIs, parameter)`

- CI_width:

  logical; rather than returning the overall coverage rate, return the
  average width of the CIs instead? Useful when comparing the efficiency
  of different CI estimators

- complement:

  logical; rather than computing the proportion of population parameters
  within the CI, return the proportion outside the advertised CI (1 -
  ECR = alpha). In the case where only one value is provided, which
  normally would return a 0 if outside the CI or 1 if inside, the values
  will be switched (useful when using, for example, CI tests of for the
  significance of parameters)

- names:

  an optional character vector used to name the returned object.
  Generally useful when more than one CI estimate is investigated at
  once

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## See also

[`EDR`](http://philchalmers.github.io/SimDesign/reference/EDR.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
CIs <- matrix(NA, 100, 2)
for(i in 1:100){
   dat <- rnorm(100)
   CIs[i,] <- t.test(dat)$conf.int
}

ECR(CIs, 0)
#> [1] 0.92
ECR(CIs, 0, tails = TRUE)
#> [1] 0.03 0.05
ECR(CIs, 0, complement = TRUE) # proportion outside interval
#> [1] 0.08

# single vector input
CI <- c(-1, 1)
ECR(CI, 0)
#> [1] 1
ECR(CI, 0, complement = TRUE)
#> [1] 0
ECR(CI, 2)
#> [1] 0
ECR(CI, 2, complement = TRUE)
#> [1] 1
ECR(CI, 2, tails = TRUE)
#> [1] 0 1

# parameters of the same size as CI
parameters <- 1:10
CIs <- cbind(parameters - runif(10), parameters + runif(10))
parameters <- parameters + rnorm(10)
ECR(CIs, parameters)
#> [1] 0.6

# average width of CIs
ECR(CIs, parameters, CI_width=TRUE)
#> [1] 1.107228

# ECR() for multiple CI estimates in the same object
parameter <- 10
CIs <- data.frame(lowerCI_1=parameter - runif(10),
                  upperCI_1=parameter + runif(10),
                  lowerCI_2=parameter - 2*runif(10),
                  upperCI_2=parameter + 2*runif(10))
head(CIs)
#>   lowerCI_1 upperCI_1 lowerCI_2 upperCI_2
#> 1  9.868010  10.95595  8.056701  11.70004
#> 2  9.540787  10.46340  9.198803  10.49114
#> 3  9.990340  10.39441  8.744864  11.23700
#> 4  9.095354  10.05217  9.778473  10.56988
#> 5  9.670788  10.71086  9.079359  11.79476
#> 6  9.392610  10.17076  8.889488  11.87073
ECR(CIs, parameter)
#> [1] 1 1
ECR(CIs, parameter, tails=TRUE)
#> [1] 0 0 0 0
ECR(CIs, parameter, CI_width=TRUE)
#> [1] 0.8840042 1.9404326

# often a good idea to provide names for the output
ECR(CIs, parameter, names = c('this', 'that'))
#> this that 
#>    1    1 
ECR(CIs, parameter, CI_width=TRUE, names = c('this', 'that'))
#>      this      that 
#> 0.8840042 1.9404326 
ECR(CIs, parameter, tails=TRUE, names = c('this', 'that'))
#> this_lower this_upper that_lower that_upper 
#>          0          0          0          0 
```
