# Extract Simulation Errors

Extractor function in situations where
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
returned a simulation with detected `ERRORS`.

## Usage

``` r
SimErrors(obj, seeds = FALSE, subset = TRUE)
```

## Arguments

- obj:

  object returned from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  containing an `ERRORS` column

- seeds:

  logical; locate `.Random.seed` state that caused the error message?

- subset:

  logical; take a subset of the `design` object showing only conditions
  that returned errors?

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

[`SimWarnings`](http://philchalmers.github.io/SimDesign/reference/SimWarnings.md),
[`SimExtract`](http://philchalmers.github.io/SimDesign/reference/SimExtract.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples
