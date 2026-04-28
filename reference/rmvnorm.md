# Generate data with the multivariate normal (i.e., Gaussian) distribution

Function generates data from the multivariate normal distribution given
some mean vector and/or covariance matrix.

## Usage

``` r
rmvnorm(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)))
```

## Arguments

- n:

  number of observations to generate

- mean:

  mean vector, default is `rep(0, length = ncol(sigma))`

- sigma:

  positive definite covariance matrix, default is `diag(length(mean))`

## Value

a numeric matrix with columns equal to `length(mean)`

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
# random normal values with mean [5, 10] and variances [3,6], and covariance 2
sigma <- matrix(c(3,2,2,6), 2, 2)
mu <- c(5,10)
x <- rmvnorm(1000, mean = mu, sigma = sigma)
head(x)
#>          [,1]      [,2]
#> [1,] 6.060771 11.172431
#> [2,] 4.222922  9.918127
#> [3,] 6.189192  7.146768
#> [4,] 6.328861  7.243104
#> [5,] 4.874275 12.614418
#> [6,] 2.573901  9.202745
summary(x)
#>        V1                  V2        
#>  Min.   : 0.008016   Min.   : 1.091  
#>  1st Qu.: 3.779530   1st Qu.: 8.364  
#>  Median : 5.018941   Median :10.009  
#>  Mean   : 4.927963   Mean   : 9.975  
#>  3rd Qu.: 6.062034   3rd Qu.:11.546  
#>  Max.   :10.018291   Max.   :17.717  
plot(x[,1], x[,2])


```
