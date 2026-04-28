# Decompose the simulation into ANOVA-based effects

Given the results from a simulation with
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
form an ANOVA table (without p-values) with effect sizes based on the
eta-squared statistic. These results provide approximate indications of
observable simulation effects, therefore these ANOVA-based results are
generally useful as exploratory rather than inferential tools.

## Usage

``` r
SimAnova(formula, dat, subset = NULL, rates = TRUE)
```

## Arguments

- formula:

  an R formula generally of a form suitable for
  [`lm`](https://rdrr.io/r/stats/lm.html) or
  [`aov`](https://rdrr.io/r/stats/aov.html). However, if the dependent
  variable (left size of the equation) is omitted then all the dependent
  variables in the simulation will be used and the result will return a
  list of analyses

- dat:

  an object returned from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  of class `'SimDesign'`

- subset:

  an optional argument to be passed to
  [`subset`](https://rdrr.io/r/base/subset.html) with the same name.
  Used to subset the results object while preserving the associated
  attributes

- rates:

  logical; does the dependent variable consist of rates (e.g., returned
  from [`ECR`](http://philchalmers.github.io/SimDesign/reference/ECR.md)
  or [`EDR`](http://philchalmers.github.io/SimDesign/reference/EDR.md))?
  Default is TRUE, which will use the logit of the DV to help stabilize
  the proportion-based summary statistics when computing the parameters
  and effect sizes

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
data(BF_sim)

# all results (not usually good to mix Power and Type I results together)
SimAnova(alpha.05.F ~ (groups_equal + distribution)^2, BF_sim)
#>                               SS df    MS     F     p sig eta.sq eta.sq.part
#> groups_equal               0.080  1 0.080 0.022 0.885   .  0.001       0.001
#> distribution              17.790  3 5.930 1.590 0.223   .  0.192       0.193
#> groups_equal:distribution  0.006  3 0.002 0.001 1.000   .  0.000       0.000
#> Residuals                 74.598 20 3.730    NA    NA      0.806          NA

# only use anova for Type I error conditions
SimAnova(alpha.05.F ~ (groups_equal + distribution)^2, BF_sim, subset = var_ratio == 1)
#>                               SS df     MS       F     p sig eta.sq eta.sq.part
#> groups_equal               0.027  1  0.027   0.281 0.610   .  0.001       0.034
#> distribution              30.755  3 10.252 108.101 0.000 ***  0.975       0.976
#> groups_equal:distribution  0.002  3  0.001   0.006 0.999   .  0.000       0.002
#> Residuals                  0.759  8  0.095      NA    NA      0.024          NA

# run all DVs at once using the same formula
SimAnova(~ groups_equal * distribution, BF_sim, subset = var_ratio == 1)
#> $alpha.05.F
#>                               SS df     MS       F     p sig eta.sq eta.sq.part
#> groups_equal               0.027  1  0.027   0.281 0.610   .  0.001       0.034
#> distribution              30.755  3 10.252 108.101 0.000 ***  0.975       0.976
#> groups_equal:distribution  0.002  3  0.001   0.006 0.999   .  0.000       0.002
#> Residuals                  0.759  8  0.095      NA    NA      0.024          NA
#> 
#> $alpha.05.Jacknife
#>                              SS df    MS      F     p sig eta.sq eta.sq.part
#> groups_equal              0.077  1 0.077  3.644 0.093   .  0.016       0.313
#> distribution              4.462  3 1.487 70.265 0.000 ***  0.933       0.963
#> groups_equal:distribution 0.072  3 0.024  1.140 0.390   .  0.015       0.300
#> Residuals                 0.169  8 0.021     NA    NA      0.035          NA
#> 
#> $alpha.05.Layard
#>                               SS df    MS      F     p sig eta.sq eta.sq.part
#> groups_equal               0.004  1 0.004  0.056 0.818   .  0.000       0.007
#> distribution              10.111  3 3.370 46.579 0.000 ***  0.943       0.946
#> groups_equal:distribution  0.023  3 0.008  0.104 0.955   .  0.002       0.038
#> Residuals                  0.579  8 0.072     NA    NA      0.054          NA
#> 
#> $alpha.05.Levene
#>                              SS df    MS      F     p sig eta.sq eta.sq.part
#> groups_equal              0.024  1 0.024  1.615 0.239   .  0.006       0.168
#> distribution              4.263  3 1.421 93.943 0.000 ***  0.960       0.972
#> groups_equal:distribution 0.030  3 0.010  0.661 0.599   .  0.007       0.199
#> Residuals                 0.121  8 0.015     NA    NA      0.027          NA
#> 
#> $alpha.05.W10
#>                              SS df    MS     F     p sig eta.sq eta.sq.part
#> groups_equal              0.040  1 0.040 0.573 0.471   .  0.021       0.067
#> distribution              1.263  3 0.421 5.956 0.020   .  0.669       0.691
#> groups_equal:distribution 0.019  3 0.006 0.092 0.963   .  0.010       0.033
#> Residuals                 0.566  8 0.071    NA    NA      0.299          NA
#> 
#> $alpha.05.W50
#>                              SS df    MS      F     p sig eta.sq eta.sq.part
#> groups_equal              0.073  1 0.073  3.005 0.121   .  0.048       0.273
#> distribution              1.164  3 0.388 16.002 0.001  **  0.763       0.857
#> groups_equal:distribution 0.095  3 0.032  1.304 0.338   .  0.062       0.328
#> Residuals                 0.194  8 0.024     NA    NA      0.127          NA
#> 
```
