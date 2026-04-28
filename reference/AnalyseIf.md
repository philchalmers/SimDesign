# Perform a test that indicates whether a given `Analyse()` function should be executed

This function is designed to prevent specific analysis function
executions when the design conditions are not met. Primarily useful when
the `analyse` argument to
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
was input as a named list object, however some of the analysis functions
are not interesting/compatible with the generated data and should
therefore be skipped.

## Usage

``` r
AnalyseIf(x, condition = NULL)
```

## Arguments

- x:

  logical statement to evaluate. If the statement evaluates to `TRUE`
  then the remainder of the defined function will be evaluated

- condition:

  (optional) the current design condition. This does not need to be
  supplied if the expression in `x` evaluates to valid logical (e.g.,
  use `Attach(condition)` prior to using `AnalyseIf`, or use
  `with(condition, AnalyseIf(someLogicalTest))`)

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

[`Analyse`](http://philchalmers.github.io/SimDesign/reference/Analyse.md),
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

Design <- createDesign(N=c(10,20,30), var.equal = c(TRUE, FALSE))

Generate <- function(condition, fixed_objects) {
  Attach(condition)
  dat <- data.frame(DV = rnorm(N*2), IV = gl(2, N, labels=c('G1', 'G2')))
  dat
}

# always run this analysis for each row in Design
Analyse1 <- function(condition, dat, fixed_objects) {
  mod <- t.test(DV ~ IV, data=dat)
  mod$p.value
}

# Only perform analysis when variances are equal and N = 20 or 30
Analyse2 <- function(condition, dat, fixed_objects) {
  AnalyseIf(var.equal && N %in% c(20, 30), condition)
  mod <- t.test(DV ~ IV, data=dat, var.equal=TRUE)
  mod$p.value
}

Summarise <- function(condition, results, fixed_objects) {
  ret <- EDR(results, alpha=.05)
  ret
}

#-------------------------------------------------------------------

# append names 'Welch' and 'independent' to associated output
res <- runSimulation(design=Design, replications=100, generate=Generate,
                     analyse=list(Welch=Analyse1, independent=Analyse2),
                     summarise=Summarise)
res

# leave results unnamed
res <- runSimulation(design=Design, replications=100, generate=Generate,
                     analyse=list(Analyse1, Analyse2),
                     summarise=Summarise)


} # }
```
