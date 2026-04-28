# Compute congruence coefficient

Computes the congruence coefficient, also known as an "unadjusted"
correlation or Tucker's congruence coefficient.

## Usage

``` r
CC(x, y = NULL, unname = FALSE)
```

## Arguments

- x:

  a vector or `data.frame`/`matrix` containing the variables to use. If
  a vector then the input `y` is required, otherwise the congruence
  coefficient is computed for all bivariate combinations

- y:

  (optional) the second vector input to use if `x` is a vector

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

[`cor`](https://rdrr.io/r/stats/cor.html)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
vec1 <- runif(1000)
vec2 <- runif(1000)

CC(vec1, vec2)
#> [1] 0.7539623
# compare to cor()
cor(vec1, vec2)
#> [1] 0.009919872

# column input
df <- data.frame(vec1, vec2, vec3 = runif(1000))
CC(df)
#>           vec1      vec2      vec3
#> vec1 1.0000000 0.7539623 0.7528033
#> vec2 0.7539623 1.0000000 0.7574734
#> vec3 0.7528033 0.7574734 1.0000000
cor(df)
#>             vec1        vec2       vec3
#> vec1 1.000000000 0.009919872 0.02368507
#> vec2 0.009919872 1.000000000 0.02560829
#> vec3 0.023685075 0.025608293 1.00000000
```
