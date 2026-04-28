# Compute the mean absolute error

Computes the average absolute deviation of a sample estimate from the
parameter value. Accepts estimate and parameter values, as well as
estimate values which are in deviation form.

## Usage

``` r
MAE(
  estimate,
  parameter = NULL,
  type = "MAE",
  center = mean,
  percent = FALSE,
  unname = FALSE
)
```

## Arguments

- estimate:

  a `numeric` vector, `matrix`/`data.frame`, or `list` of parameter
  estimates. If a vector, the length is equal to the number of
  replications. If a `matrix`/`data.frame` the number of rows must equal
  the number of replications. `list` objects will be looped over using
  the same rules after above after first translating the information
  into one-dimensional vectors and re-creating the structure upon return

- parameter:

  a `numeric` scalar/vector or `matrix` indicating the fixed parameter
  values. If a single value is supplied and `estimate` is a
  `matrix`/`data.frame` then the value will be recycled for each column;
  otherwise, each element will be associated with each respective column
  in the `estimate` input. If `NULL`, then it will be assumed that the
  `estimate` input is in a deviation form (therefore
  `mean(abs(estimate))` will be returned)

- type:

  type of deviation to compute. Can be `'MAE'` (default) for the mean
  absolute error, `'NMSE'` for the normalized MAE (MAE /
  (max(estimate) - min(estimate))), or `'SMSE'` for the standardized MAE
  (MAE / sd(estimate))

- center:

  function to compute the central tendency. Default uses
  [`mean`](https://rdrr.io/r/base/mean.html), reflecting the canonical
  \\mean((\hat{p}\_i - p)^2)\\ difference, but other options can be
  supplied to provide more robust central tendency estimates, such as
  [`median`](https://rdrr.io/r/stats/median.html) (i.e., median-squared
  error) or `\(x) mean(x, trim = 0.1)`

- percent:

  logical; change returned result to percentage by multiplying by 100?
  Default is FALSE

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns a numeric vector indicating the overall mean absolute error in
the estimates

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

RMSE

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
pop <- 1
samp <- rnorm(100, 1, sd = 0.5)
MAE(samp, pop)
#> [1] 0.3545376

dev <- samp - pop
MAE(dev)
#> [1] 0.3545376
MAE(samp, pop, type = 'NMAE')
#> [1] 0.1193529
MAE(samp, pop, type = 'SMAE')
#> [1] 0.7859817

# matrix input
mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
MAE(mat, parameter = 2)
#>        M1        M2 
#> 0.4552233 0.8681354 

# same, but with data.frame
df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
MAE(df, parameter = c(2,2))
#>        M1        M2 
#> 0.4040587 0.7386515 

# parameters of the same size
parameters <- 1:10
estimates <- parameters + rnorm(10)
MAE(estimates, parameters)
#> [1] 0.8352983
```
