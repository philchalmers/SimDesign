# Bradley's (1978) empirical robustness interval

Robustness interval criteria for empirical detection rate estimates and
empirical coverage estimates defined by Bradley (1978). See
[`EDR`](http://philchalmers.github.io/SimDesign/reference/EDR.md) and
[`ECR`](http://philchalmers.github.io/SimDesign/reference/ECR.md) to
obtain such estimates.

## Usage

``` r
Bradley1978(
  rate,
  alpha = 0.05,
  type = "liberal",
  CI = FALSE,
  out.logical = FALSE,
  out.labels = c("conservative", "robust", "liberal"),
  unname = FALSE
)
```

## Arguments

- rate:

  (optional) numeric vector containing the empirical detection rate(s)
  or empirical confidence interval estimates. If supplied a character
  vector with elements defined in `out.labels` or a logical vector will
  be returned indicating whether the detection rate estimate is
  considered 'robust'.

  When the input is an empirical coverage rate the argument `CI` must be
  set to `TRUE`.

  If this input is missing, the interval criteria will be printed to the
  console

- alpha:

  Type I error rate to evaluated (default is .05)

- type:

  character vector indicating the type of interval classification to
  use. Default is 'liberal', however can be 'stringent' to use Bradley's
  more stringent robustness criteria

- CI:

  logical; should this robust interval be constructed on empirical
  detection rates (`FALSE`) or empirical coverage rates (`TRUE`)?

- out.logical:

  logical; should the output vector be TRUE/FALSE indicating whether the
  supplied empirical detection rate/CI should be considered "robust"?
  Default is FALSE, in which case the out.labels elements are used
  instead

- out.labels:

  character vector of length three indicating the classification labels
  according to the desired robustness interval

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## References

Bradley, J. V. (1978). Robustness? *British Journal of Mathematical and
Statistical Psychology, 31*, 144-152.

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## See also

[`EDR`](http://philchalmers.github.io/SimDesign/reference/EDR.md),
[`ECR`](http://philchalmers.github.io/SimDesign/reference/ECR.md),
[`Serlin2000`](http://philchalmers.github.io/SimDesign/reference/Serlin2000.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
# interval criteria used for empirical detection rates
Bradley1978()
#> liberal.lower liberal.upper 
#>         0.025         0.075 
Bradley1978(type = 'stringent')
#> stringent.lower stringent.upper 
#>           0.045           0.055 
Bradley1978(alpha = .01, type = 'stringent')
#> stringent.lower stringent.upper 
#>           0.009           0.011 

# intervals applied to empirical detection rate estimates
edr <- c(test1 = .05, test2 = .027, test3 = .051, test4 = .076, test5 = .024)

Bradley1978(edr)
#>          test1          test2          test3          test4          test5 
#>       "robust"       "robust"       "robust"      "liberal" "conservative" 
Bradley1978(edr, out.logical=TRUE) # is robust?
#> test1 test2 test3 test4 test5 
#>  TRUE  TRUE  TRUE FALSE FALSE 

#####
# interval criteria used for coverage estimates

Bradley1978(CI = TRUE)
#> liberal.lower liberal.upper 
#>         0.925         0.975 
Bradley1978(CI = TRUE, type = 'stringent')
#> stringent.lower stringent.upper 
#>           0.945           0.955 
Bradley1978(CI = TRUE, alpha = .01, type = 'stringent')
#> stringent.lower stringent.upper 
#>           0.989           0.991 

# intervals applied to empirical coverage rate estimates
ecr <- c(test1 = .950, test2 = .973, test3 = .949, test4 = .924, test5 = .976)

Bradley1978(ecr, CI=TRUE)
#>          test1          test2          test3          test4          test5 
#>       "robust"       "robust"       "robust"      "liberal" "conservative" 
Bradley1978(ecr, CI=TRUE, out.logical=TRUE) # is robust?
#> test1 test2 test3 test4 test5 
#>  TRUE  TRUE  TRUE FALSE FALSE 
```
