# Empirical detection robustness method suggested by Serlin (2000)

Hypothesis test to determine whether an observed empirical detection
rate, coupled with a given robustness interval, statistically differs
from the population value. Uses the methods described by Serlin (2000)
as well to generate critical values (similar to confidence intervals,
but define a fixed window of robustness). Critical values may be
computed without performing the simulation experiment (hence, can be
obtained a priori).

## Usage

``` r
Serlin2000(p, alpha, delta, R, CI = 0.95)
```

## Arguments

- p:

  (optional) a vector containing the empirical detection rate(s) to be
  tested. Omitting this input will compute only the CV1 and CV2 values,
  while including this input will perform a one-sided hypothesis test
  for robustness

- alpha:

  Type I error rate (e.g., often set to .05)

- delta:

  (optional) symmetric robustness interval around `alpha` (e.g., a value
  of .01 when `alpha = .05` would test the robustness window .04-.06)

- R:

  number of replications used in the simulation

- CI:

  confidence interval for `alpha` as a proportion. Default of 0.95
  indicates a 95% interval

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Serlin, R. C. (2000). Testing for Robustness in Monte Carlo Studies.
*Psychological Methods, 5*, 230-240.

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
# Cochran's criteria at alpha = .05 (i.e., 0.5 +- .01), assuming N = 2000
Serlin2000(p = .051, alpha = .05, delta = .01, R = 2000)
#>       p z(|p-a| - d))   Pr(>|z|) robust        CV1        CV2
#> 1 0.051     -1.846761 0.03239089    yes 0.04125991 0.05864068

# Bradley's liberal criteria given p = .06 and .076, assuming N = 1000
Serlin2000(p = .060, alpha = .05, delta = .025, R = 1000)
#>      p z(|p-a| - d))   Pr(>|z|) robust        CV1        CV2
#> 1 0.06     -2.176429 0.01476161    yes 0.03815878 0.06259781
Serlin2000(p = .076, alpha = .05, delta = .025, R = 1000)
#>       p z(|p-a| - d))  Pr(>|z|) robust        CV1        CV2
#> 1 0.076     0.1450953 0.4423178     no 0.03815878 0.06259781

# multiple p-values
Serlin2000(p = c(.05, .06, .07), alpha = .05, delta = .025, R = 1000)
#>      p z(|p-a| - d))     Pr(>|z|) robust        CV1        CV2
#> 1 0.05    -3.6273813 0.0001431552    yes 0.03815878 0.06259781
#> 2 0.06    -2.1764288 0.0147616080    yes 0.03815878 0.06259781
#> 3 0.07    -0.7254763 0.2340799549     no 0.03815878 0.06259781

# CV values computed before simulation performed
Serlin2000(alpha = .05, R = 2500)
#>          CV1        CV2
#> 1 0.04210768 0.05763959
```
