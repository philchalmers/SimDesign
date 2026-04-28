# Generate data with the multivariate t distribution

Function generates data from the multivariate t distribution given a
covariance matrix, non-centrality parameter (or mode), and degrees of
freedom.

## Usage

``` r
rmvt(n, sigma, df, delta = rep(0, nrow(sigma)), Kshirsagar = FALSE)
```

## Arguments

- n:

  number of observations to generate

- sigma:

  positive definite covariance matrix

- df:

  degrees of freedom. `df = 0` and `df = Inf` corresponds to the
  multivariate normal distribution

- delta:

  the vector of non-centrality parameters of length `n` which specifies
  the either the modes (default) or non-centrality parameters

- Kshirsagar:

  logical; triggers whether to generate data with non-centrality
  parameters or to adjust the simulated data to the mode of the
  distribution. The default uses the mode

## Value

a numeric matrix with columns equal to `ncol(sigma)`

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

[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
# random t values given variances [3,6], covariance 2, and df = 15
sigma <- matrix(c(3,2,2,6), 2, 2)
x <- rmvt(1000, sigma = sigma, df = 15)
head(x)
#>            [,1]      [,2]
#> [1,]  1.1355823 -1.511125
#> [2,]  4.2293566  3.520368
#> [3,]  1.4742611 -2.296661
#> [4,]  0.4845494 -0.982761
#> [5,] -0.4792007 -3.010312
#> [6,] -1.8363903 -1.736213
summary(x)
#>        V1                 V2           
#>  Min.   :-6.15834   Min.   :-10.22541  
#>  1st Qu.:-1.27204   1st Qu.: -1.86951  
#>  Median :-0.06571   Median : -0.13726  
#>  Mean   :-0.05165   Mean   : -0.06365  
#>  3rd Qu.: 1.23916   3rd Qu.:  1.80328  
#>  Max.   : 8.84541   Max.   : 10.35493  
plot(x[,1], x[,2])


```
