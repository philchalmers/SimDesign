# Compute (relative/standardized) bias summary statistic

Computes the (relative) bias of a sample estimate from the parameter
value. Accepts estimate and parameter values, as well as estimate values
which are in deviation form. If relative bias is requested the
`estimate` and `parameter` inputs are both required.

## Usage

``` r
bias(
  estimate,
  parameter = NULL,
  type = "bias",
  center = mean,
  deviance = colSDs,
  abs = FALSE,
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

  a `numeric` scalar/vector indicating the fixed parameters. If a single
  value is supplied and `estimate` is a `matrix`/`data.frame` then the
  value will be recycled for each column; otherwise, each element will
  be associated with each respective column in the `estimate` input. If
  `NULL` then it will be assumed that the `estimate` input is in a
  deviation form (therefore `mean(estimate))` will be returned)

- type:

  type of bias statistic to return. Default (`'bias'`) computes the
  standard bias (average difference between sample and population),
  `'relative'` computes the relative bias statistic (i.e., divide the
  bias by the value in `parameter`; note that multiplying this by 100
  gives the "percent bias" measure, or if Type I error rates
  (\\\alpha\\) are supplied will result in the "percentage error"),
  `'abs_relative'` computes the relative bias for each replication
  independently, takes the absolute value of each term, then computes
  the mean estimate, and `'standardized'` computes the standardized bias
  estimate (standard bias divided by the standard deviation of the
  sample estimates)

- center:

  function to compute the central tendency. Default uses
  [`mean`](https://rdrr.io/r/base/mean.html), reflecting the canonical
  \\mean(\hat{p}\_i - p)\\ difference, but other options can be supplied
  to provide more robust central tendency estimates, such as
  [`median`](https://rdrr.io/r/stats/median.html) or
  `\(x) mean(x, trim = 0.1)`

- deviance:

  function to compute the deviance criterion, currently used when
  `type = 'standardize'`. Default uses
  [`colSDs`](http://philchalmers.github.io/SimDesign/reference/colVars.md),
  reflecting the canonical standard deviation of an incoming `matrix`
  object (required), but other options can be supplied to provide more
  robust deviation estimates, such as `\(x) apply(x, 2, IQR)` for the
  interquartile range. Note that if using an alternative `center` then,
  if relevant, this should be adjusted too

- abs:

  logical; find the absolute bias between the parameters and estimates?
  This effectively just applies the
  [`abs`](https://rdrr.io/r/base/MathFun.html) transformation to the
  returned result. Default is FALSE

- percent:

  logical; change returned result to percentage by multiplying by 100?
  Default is FALSE

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns a `numeric` vector indicating the overall
(relative/standardized) bias in the estimates

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

[`RMSE`](http://philchalmers.github.io/SimDesign/reference/RMSE.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
pop <- 2
samp <- rnorm(100, 2, sd = 0.5)
bias(samp, pop)
#> [1] 0.03794647
bias(samp, pop, type = 'relative')
#> [1] 0.01897324
bias(samp, pop, type = 'standardized')
#> [1] 0.08430616
bias(samp, pop, type = 'abs_relative')
#> [1] 0.1798972

dev <- samp - pop
bias(dev)
#> [1] 0.03794647

# equivalent here
bias(mean(samp), pop)
#> [1] 0.03794647

# different center function
bias(samp, pop, center=median)  # median instead of mean
#> [1] 0.07150887

# matrix input
mat <- cbind(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
bias(mat, parameter = 2)
#>          M1          M2 
#> -0.07654450  0.02579237 
bias(mat, parameter = 2, type = 'relative')
#>          M1          M2 
#> -0.03827225  0.01289619 
bias(mat, parameter = 2, type = 'standardized')
#>          M1          M2 
#> -0.17388356  0.02381093 

# different parameter associated with each column
mat <- cbind(M1=rnorm(1000, 2, sd = 0.25), M2 = rnorm(1000, 3, sd = .25))
bias(mat, parameter = c(2,3))
#>            M1            M2 
#>  0.0011274050 -0.0001033804 
bias(mat, parameter = c(2,3), type='relative')
#>            M1            M2 
#>  5.637025e-04 -3.446014e-05 
bias(mat, parameter = c(2,3), type='standardized')
#>            M1            M2 
#>  0.0045699530 -0.0004164888 

# same, but with data.frame
df <- data.frame(M1=rnorm(100, 2, sd = 0.5), M2 = rnorm(100, 2, sd = 1))
bias(df, parameter = c(2,2))
#>         M1         M2 
#> 0.02246154 0.09597741 

# parameters of the same size
parameters <- 1:10
estimates <- parameters + rnorm(10)
bias(estimates, parameters)
#> [1] -0.2471583


# relative difference dividing by the magnitude of parameters
bias(estimates, parameters, type = 'abs_relative')
#> [1] 0.2585918

# relative bias as a percentage
bias(estimates, parameters, type = 'abs_relative', percent = TRUE)
#> [1] 25.85918

# percentage error (PE) statistic given alpha (Type I error) and EDR() result
# edr <- EDR(results, alpha = .05)
edr <- c(.04, .05, .06, .08)
bias(matrix(edr, 1L), .05, type = 'relative', percent = TRUE)
#> [1] -20   0  20  60

```
