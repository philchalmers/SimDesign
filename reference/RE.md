# Compute the relative efficiency of multiple estimators

Computes the relative efficiency given the RMSE (default) or MSE values
for multiple estimators.

## Usage

``` r
RE(x, MSE = FALSE, percent = FALSE, unname = FALSE)
```

## Arguments

- x:

  a `numeric` vector of root mean square error values (see
  [`RMSE`](http://philchalmers.github.io/SimDesign/reference/RMSE.md)),
  where the first element will be used as the reference. Otherwise, the
  object could contain MSE values if the flag `MSE = TRUE` is also
  included

- MSE:

  logical; are the input value mean squared errors instead of root mean
  square errors?

- percent:

  logical; change returned result to percentage by multiplying by 100?
  Default is FALSE

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns a `vector` of variance ratios indicating the relative efficiency
compared to the first estimator. Values less than 1 indicate better
efficiency than the first estimator, while values greater than 1
indicate worse efficiency than the first estimator

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
samp1 <- rnorm(100, 1, sd = 0.5)
RMSE1 <- RMSE(samp1, pop)
samp2 <- rnorm(100, 1, sd = 1)
RMSE2 <- RMSE(samp2, pop)

RE(c(RMSE1, RMSE2))
#> [1] 1.000000 4.072193
RE(c(RMSE1, RMSE2), percent = TRUE) # as a percentage
#> [1] 100.0000 407.2193

# using MSE instead
mse <- c(RMSE1, RMSE2)^2
RE(mse, MSE = TRUE)
#> [1] 1.000000 4.072193
```
