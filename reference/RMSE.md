# Compute the (normalized) root mean square error

Computes the average deviation (root mean square error; also known as
the root mean square deviation) of a sample estimate from the parameter
value. Accepts estimate and parameter values, as well as estimate values
which are in deviation form.

## Usage

``` r
RMSE(
  estimate,
  parameter = NULL,
  type = "RMSE",
  center = mean,
  MSE = FALSE,
  percent = FALSE,
  unname = FALSE
)

RMSD(
  estimate,
  parameter = NULL,
  type = "RMSE",
  center = mean,
  MSE = FALSE,
  percent = FALSE,
  unname = FALSE
)
```

## Arguments

- estimate:

  a `numeric` vector, `matrix`/`data.frame`, or `list` of parameter
  estimates. If a vector, the length is equal to the number of
  replications. If a `matrix`/`data.frame`, the number of rows must
  equal the number of replications. `list` objects will be looped over
  using the same rules after above after first translating the
  information into one-dimensional vectors and re-creating the structure
  upon return

- parameter:

  a `numeric` scalar/vector indicating the fixed parameter values. If a
  single value is supplied and `estimate` is a `matrix`/`data.frame`
  then the value will be recycled for each column; otherwise, each
  element will be associated with each respective column in the
  `estimate` input. If `NULL` then it will be assumed that the
  `estimate` input is in a deviation form (therefore
  `sqrt(mean(estimate^2))` will be returned)

- type:

  type of deviation to compute. Can be `'RMSE'` (default) for the root
  mean square-error, `'NRMSE'` for the normalized RMSE (RMSE /
  (max(estimate) - min(estimate))), `'SRMSE'` for the standardized RMSE
  (RMSE / sd(estimate)), `'CV'` for the coefficient of variation, or
  `'RMSLE'` for the root mean-square log-error

- center:

  function to compute the central tendency. Default uses
  [`mean`](https://rdrr.io/r/base/mean.html), reflecting the canonical
  \\mean((\hat{p}\_i - p)^2)\\ difference (and subsequently
  square-rooted), but other options can be supplied to provide more
  robust central tendency estimates, such as
  [`median`](https://rdrr.io/r/stats/median.html) (i.e., root
  median-squared error) or `\(x) mean(x, trim = 0.1)`. Note that the
  centering function is also used in the denominator of `type = 'CV'`
  and `'RMSLE'`

- MSE:

  logical; return the mean square error equivalent of the results
  instead of the root mean-square error (in other words, the result is
  squared)? Default is `FALSE`

- percent:

  logical; change returned result to percentage by multiplying by 100?
  Default is FALSE

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns a `numeric` vector indicating the overall average deviation in
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

[`bias`](http://philchalmers.github.io/SimDesign/reference/bias.md)

MAE

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
pop <- 1
samp <- rnorm(100, 1, sd = 0.5)
RMSE(samp, pop)
#> [1] 0.4888468

dev <- samp - pop
RMSE(dev)
#> [1] 0.4888468

RMSE(samp, pop, type = 'NRMSE')
#> [1] 0.2246974
RMSE(dev, type = 'NRMSE')
#> [1] 0.2246974
RMSE(dev, pop, type = 'SRMSE')
#> [1] 2.255327
RMSE(samp, pop, type = 'CV')
#> [1] 0.486093
RMSE(samp, pop, type = 'RMSLE')
#> [1] 0.2590176

# percentage reported
RMSE(samp, pop, type = 'NRMSE')
#> [1] 0.2246974
RMSE(samp, pop, type = 'NRMSE', percent = TRUE)
#> [1] 22.46974

# matrix input
mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
RMSE(mat, parameter = 2)
#>        M1        M2 
#> 0.4933475 1.0596005 
RMSE(mat, parameter = c(2, 3))
#>        M1        M2 
#> 0.4933475 1.5255310 

# different parameter associated with each column
mat <- cbind(M1=rnorm(1000, 2, sd = 0.25), M2 = rnorm(1000, 3, sd = .25))
RMSE(mat, parameter = c(2,3))
#>        M1        M2 
#> 0.2513247 0.2531064 

# same, but with data.frame
df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
RMSE(df, parameter = c(2,2))
#>        M1        M2 
#> 0.4434231 1.0532604 

# parameters of the same size
parameters <- 1:10
estimates <- parameters + rnorm(10)
RMSE(estimates, parameters)
#> [1] 0.5072955
```
