# Generate non-normal data with Vale & Maurelli's (1983) method

Generate multivariate non-normal distributions using the third-order
polynomial method described by Vale & Maurelli (1983). If only a single
variable is generated then this function is equivalent to the method
described by Fleishman (1978).

## Usage

``` r
rValeMaurelli(
  n,
  mean = rep(0, nrow(sigma)),
  sigma = diag(length(mean)),
  skew = rep(0, nrow(sigma)),
  kurt = rep(0, nrow(sigma))
)
```

## Arguments

- n:

  number of samples to draw

- mean:

  a vector of k elements for the mean of the variables

- sigma:

  desired k x k covariance matrix between bivariate non-normal variables

- skew:

  a vector of k elements for the skewness of the variables

- kurt:

  a vector of k elements for the kurtosis of the variables

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

Fleishman, A. I. (1978). A method for simulating non-normal
distributions. *Psychometrika, 43*, 521-532.

Vale, C. & Maurelli, V. (1983). Simulating multivariate nonnormal
distributions. *Psychometrika, 48*(3), 465-471.

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r

set.seed(1)

# univariate with skew
nonnormal <- rValeMaurelli(10000, mean=10, sigma=5, skew=1, kurt=3)
descript(nonnormal)
#> # A tibble: 1 × 11
#>   VARIABLE      n  mean  trim median    sd   IQR  skew  kurt   min   max
#>   <fct>     <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 nonnormal 10000  9.99  9.83   9.68  2.25  2.58 0.908  2.42  1.44  26.9

# multivariate with skew and kurtosis
n <- 10000
r12 <- .4
r13 <- .9
r23 <- .1
cor <- matrix(c(1,r12,r13,r12,1,r23,r13,r23,1),3,3)
sk <- c(1.5,1.5,0.5)
ku <- c(3.75,3.5,0.5)

nonnormal <- rValeMaurelli(n, sigma=cor, skew=sk, kurt=ku)
cor(nonnormal) |> round(3)
#>       [,1]  [,2]  [,3]
#> [1,] 1.000 0.373 0.874
#> [2,] 0.373 1.000 0.086
#> [3,] 0.874 0.086 1.000
descript(nonnormal)
#> # A tibble: 3 × 11
#>   VARIABLE     n     mean    trim median    sd   IQR  skew  kurt   min   max
#>   <fct>    <dbl>    <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 V1       10000 -0.00636 -0.136  -0.237 1.00   1.17 1.57  3.95  -2.02  7.49
#> 2 V2       10000  0.00773 -0.125  -0.218 1.01   1.22 1.48  3.20  -1.31  6.70
#> 3 V3       10000 -0.00889 -0.0591 -0.103 0.993  1.30 0.546 0.500 -3.04  5.24
```
