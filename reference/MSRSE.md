# Compute the relative performance behavior of collections of standard errors

The mean-square relative standard error (MSRSE) compares standard error
estimates to the standard deviation of the respective parameter
estimates. Values close to 1 indicate that the behavior of the standard
errors closely matched the sampling variability of the parameter
estimates.

## Usage

``` r
MSRSE(SE, SD, percent = FALSE, unname = FALSE)
```

## Arguments

- SE:

  a `numeric` scalar/vector indicating the average standard errors
  across the replications, or a `matrix` of collected standard error
  estimates themselves to be used to compute the average standard
  errors. Each column/element in this input corresponds to the
  column/element in `SD`

- SD:

  a `numeric` scalar/vector indicating the standard deviation across the
  replications, or a `matrix` of collected parameter estimates
  themselves to be used to compute the standard deviations. Each
  column/element in this input corresponds to the column/element in `SE`

- percent:

  logical; change returned result to percentage by multiplying by 100?
  Default is FALSE

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns a `vector` of ratios indicating the relative performance of the
standard error estimates to the observed parameter standard deviation.
Values less than 1 indicate that the standard errors were larger than
the standard deviation of the parameters (hence, the SEs are interpreted
as more conservative), while values greater than 1 were smaller than the
standard deviation of the parameters (i.e., more liberal SEs)

## Details

Mean-square relative standard error (MSRSE) is expressed as

\$\$MSRSE = \frac{E(SE(\psi)^2)}{SD(\psi)^2} = \frac{1/R \*
\sum\_{r=1}^R SE(\psi_r)^2}{SD(\psi)^2}\$\$

where \\SE(\psi_r)\\ represents the estimate of the standard error at
the \\r\\th simulation replication, and \\SD(\psi)\\ represents the
standard deviation estimate of the parameters across all \\R\\
replications. Note that \\SD(\psi)^2\\ is used, which corresponds to the
variance of \\\psi\\.

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
Generate <- function(condition, fixed_objects) {
   X <- rep(0:1, each = 50)
   y <- 10 + 5 * X + rnorm(100, 0, .2)
   data.frame(y, X)
}

Analyse <- function(condition, dat, fixed_objects) {
   mod <- lm(y ~ X, dat)
   so <- summary(mod)
   ret <- c(SE = so$coefficients[,"Std. Error"],
            est = so$coefficients[,"Estimate"])
   ret
}

Summarise <- function(condition, results, fixed_objects) {
   MSRSE(SE = results[,1:2], SD = results[,3:4])
}

results <- runSimulation(replications=500, generate=Generate,
                         analyse=Analyse, summarise=Summarise)
results
#> # A tibble: 1 × 6
#>   `SE.(Intercept)`   SE.X REPLICATIONS SIM_TIME      SEED COMPLETED             
#>              <dbl>  <dbl>        <dbl> <chr>        <int> <chr>                 
#> 1          0.98822 1.1781          500 0.45s    402147477 Tue Apr 28 13:55:27 2…

```
