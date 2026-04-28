# Compute the relative absolute bias of multiple estimators

Computes the relative absolute bias given the bias estimates for
multiple estimators.

## Usage

``` r
RAB(x, percent = FALSE, unname = FALSE)
```

## Arguments

- x:

  a `numeric` vector of bias estimates (see
  [`bias`](http://philchalmers.github.io/SimDesign/reference/bias.md)),
  where the first element will be used as the reference

- percent:

  logical; change returned result to percentage by multiplying by 100?
  Default is FALSE

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns a `vector` of absolute bias ratios indicating the relative bias
effects compared to the first estimator. Values less than 1 indicate
better bias estimates than the first estimator, while values greater
than 1 indicate worse bias than the first estimator

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
pop <- 1
samp1 <- rnorm(5000, 1)
bias1 <- bias(samp1, pop)
samp2 <- rnorm(5000, 1)
bias2 <- bias(samp2, pop)

RAB(c(bias1, bias2))
#> [1]  1.00000 31.51306
RAB(c(bias1, bias2), percent = TRUE) # as a percentage
#> [1]  100.000 3151.306
```
