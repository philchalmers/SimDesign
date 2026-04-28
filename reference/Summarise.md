# Summarise simulated data using various population comparison statistics

This collapses the simulation results within each condition to composite
estimates such as RMSE, bias, Type I error rates, coverage rates, etc.
See the `See Also` section below for useful functions to be used within
`Summarise`.

## Usage

``` r
Summarise(condition, results, fixed_objects)
```

## Arguments

- condition:

  a single row from the `design` input from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  (as a `data.frame`), indicating the simulation conditions

- results:

  a `tibble` data frame (if `Analyse` returned a named numeric vector of
  any length) or a `list` (if `Analyse` returned a `list` or multi-rowed
  `data.frame`) containing the analysis results from
  [`Analyse`](http://philchalmers.github.io/SimDesign/reference/Analyse.md),
  where each cell is stored in a unique row/list element

- fixed_objects:

  object passed down from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

## Value

for best results should return a named `numeric` vector or `data.frame`
with the desired meta-simulation results. Named `list` objects can also
be returned, however the subsequent results must be extracted via
[`SimExtract`](http://philchalmers.github.io/SimDesign/reference/SimExtract.md)

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

[`bias`](http://philchalmers.github.io/SimDesign/reference/bias.md),
[`RMSE`](http://philchalmers.github.io/SimDesign/reference/RMSE.md),
[`RE`](http://philchalmers.github.io/SimDesign/reference/RE.md),
[`EDR`](http://philchalmers.github.io/SimDesign/reference/EDR.md),
[`ECR`](http://philchalmers.github.io/SimDesign/reference/ECR.md),
[`MAE`](http://philchalmers.github.io/SimDesign/reference/MAE.md),
[`SimExtract`](http://philchalmers.github.io/SimDesign/reference/SimExtract.md)

## Examples

``` r
if (FALSE) { # \dontrun{

summarise <- function(condition, results, fixed_objects) {

    #find results of interest here (alpha < .1, .05, .01)
    lessthan.05 <- EDR(results, alpha = .05)

    # return the results that will be appended to the design input
    ret <- c(lessthan.05=lessthan.05)
    ret
}

} # }
```
