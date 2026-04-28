# Compute the relative difference

Computes the relative difference statistic of the form
`(est - pop)/ pop`, which is equivalent to the form `est/pop - 1`. If
matrices are supplied then an equivalent matrix variant will be used of
the form `(est - pop) * solve(pop)`. Values closer to 0 indicate better
relative parameter recovery. Note that for single variable inputs this
is equivalent to `bias(..., type = 'relative')`.

## Usage

``` r
RD(est, pop, as.vector = TRUE, unname = FALSE)
```

## Arguments

- est:

  a `numeric` vector, `matrix/data.frame`, or `list` containing the
  parameter estimates

- pop:

  a `numeric` vector or matrix containing the true parameter values.
  Must be of comparable dimension to `est`

- as.vector:

  logical; always wrap the result in a
  [`as.vector`](https://rdrr.io/r/base/vector.html) function before
  returning?

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns a `vector` or `matrix` depending on the inputs and whether
`as.vector` was used

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
# vector
pop <- seq(1, 100, length.out=9)
est1 <- pop + rnorm(9, 0, .2)
(rds <- RD(est1, pop))
#> [1]  9.161195e-02 -2.856299e-03 -4.743365e-03 -5.027428e-05  7.186801e-04
#> [6]  8.588291e-05  3.142036e-03  2.113068e-03  1.682020e-03
summary(rds)
#>       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#> -4.743e-03 -5.027e-05  7.187e-04  1.019e-02  2.113e-03  9.161e-02 

# matrix
pop <- matrix(c(1:8, 10), 3, 3)
est2 <- pop + rnorm(9, 0, .2)
RD(est2, pop, as.vector = FALSE)
#>             [,1]       [,2]        [,3]
#> [1,] -0.09783211  0.6298721 -0.43147808
#> [2,] -0.32422225  0.3607876 -0.07267172
#> [3,]  0.04717509 -0.2042294  0.12395488
(rds <- RD(est2, pop))
#> [1] -0.09783211 -0.32422225  0.04717509  0.62987208  0.36078763 -0.20422939
#> [7] -0.43147808 -0.07267172  0.12395488
summary(rds)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -0.431478 -0.204229 -0.072672  0.003484  0.123955  0.629872 

```
