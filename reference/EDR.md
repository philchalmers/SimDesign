# Compute the empirical detection/rejection rate for Type I errors and Power

Computes the detection/rejection rate for determining empirical Type I
error and power rates using information from p-values.

## Usage

``` r
EDR(p, alpha = 0.05, unname = FALSE)
```

## Arguments

- p:

  a `numeric` vector or `matrix`/`data.frame` of p-values from the
  desired statistical estimator. If a `matrix`, each statistic must be
  organized by column, where the number of rows is equal to the number
  of replications

- alpha:

  the detection threshold (typical values are .10, .05, and .01).
  Default is .05

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

[`ECR`](http://philchalmers.github.io/SimDesign/reference/ECR.md),
[`Bradley1978`](http://philchalmers.github.io/SimDesign/reference/Bradley1978.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
rates <- numeric(100)
for(i in 1:100){
   dat <- rnorm(100)
   rates[i] <- t.test(dat)$p.value
}

EDR(rates)
#> [1] 0.07
EDR(rates, alpha = .01)
#> [1] 0

# multiple rates at once
rates <- cbind(runif(1000), runif(1000))
EDR(rates)
#> [1] 0.034 0.043
```
