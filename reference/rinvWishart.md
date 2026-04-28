# Generate data with the inverse Wishart distribution

Function generates data in the form of symmetric matrices from the
inverse Wishart distribution given a covariance matrix and degrees of
freedom.

## Usage

``` r
rinvWishart(n = 1, df, sigma)
```

## Arguments

- n:

  number of matrix observations to generate. By default `n = 1`, which
  returns a single symmetric matrix. If `n > 1` then a list of `n`
  symmetric matrices are returned instead

- df:

  degrees of freedom

- sigma:

  positive definite covariance matrix

## Value

a numeric matrix with columns equal to `ncol(sigma)` when `n = 1`, or a
list of `n` matrices with the same properties

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
# random inverse Wishart matrix given variances [3,6], covariance 2, and df=15
sigma <- matrix(c(3,2,2,6), 2, 2)
x <- rinvWishart(sigma = sigma, df = 15)
x
#>            [,1]       [,2]
#> [1,] 0.12592879 0.06824857
#> [2,] 0.06824857 0.31885404

# list of matrices
x <- rinvWishart(20, sigma = sigma, df = 15)
x
#> [[1]]
#>            [,1]       [,2]
#> [1,] 0.14125605 0.06475135
#> [2,] 0.06475135 0.38464374
#> 
#> [[2]]
#>             [,1]        [,2]
#> [1,]  0.30345161 -0.03541925
#> [2,] -0.03541925  0.26979829
#> 
#> [[3]]
#>           [,1]      [,2]
#> [1,] 0.2574038 0.1281387
#> [2,] 0.1281387 0.3872453
#> 
#> [[4]]
#>           [,1]      [,2]
#> [1,] 0.2364292 0.1184846
#> [2,] 0.1184846 0.2543226
#> 
#> [[5]]
#>            [,1]       [,2]
#> [1,] 0.20712354 0.08245683
#> [2,] 0.08245683 0.39818001
#> 
#> [[6]]
#>           [,1]      [,2]
#> [1,] 0.1253428 0.1237987
#> [2,] 0.1237987 0.4092931
#> 
#> [[7]]
#>            [,1]       [,2]
#> [1,] 0.17509635 0.03126645
#> [2,] 0.03126645 0.27069185
#> 
#> [[8]]
#>           [,1]      [,2]
#> [1,] 0.5432412 0.4282939
#> [2,] 0.4282939 0.7519928
#> 
#> [[9]]
#>            [,1]       [,2]
#> [1,] 0.11311930 0.05432718
#> [2,] 0.05432718 0.39363138
#> 
#> [[10]]
#>           [,1]      [,2]
#> [1,] 0.2130090 0.1274582
#> [2,] 0.1274582 0.3060374
#> 
#> [[11]]
#>           [,1]      [,2]
#> [1,] 0.3098325 0.2499661
#> [2,] 0.2499661 0.6363222
#> 
#> [[12]]
#>           [,1]      [,2]
#> [1,] 0.1242213 0.0631768
#> [2,] 0.0631768 0.3443811
#> 
#> [[13]]
#>           [,1]      [,2]
#> [1,] 0.4275060 0.2879198
#> [2,] 0.2879198 0.5902773
#> 
#> [[14]]
#>           [,1]      [,2]
#> [1,] 0.3093765 0.1630539
#> [2,] 0.1630539 0.2796718
#> 
#> [[15]]
#>            [,1]       [,2]
#> [1,] 0.11046516 0.02646219
#> [2,] 0.02646219 0.34872737
#> 
#> [[16]]
#>            [,1]       [,2]
#> [1,] 0.11632594 0.02074906
#> [2,] 0.02074906 0.30345385
#> 
#> [[17]]
#>           [,1]      [,2]
#> [1,] 0.2470951 0.1609995
#> [2,] 0.1609995 0.6436081
#> 
#> [[18]]
#>           [,1]      [,2]
#> [1,] 0.1766255 0.2190138
#> [2,] 0.2190138 0.6149394
#> 
#> [[19]]
#>           [,1]      [,2]
#> [1,] 0.1951444 0.0698632
#> [2,] 0.0698632 0.3893907
#> 
#> [[20]]
#>          [,1]      [,2]
#> [1,] 0.258277 0.1546960
#> [2,] 0.154696 0.4691242
#> 
```
