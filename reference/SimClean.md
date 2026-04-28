# Removes/cleans files and folders that have been saved

This function is mainly used in pilot studies where results and datasets
have been temporarily saved by
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
but should be removed before beginning the full Monte Carlo simulation
(e.g., remove files and folders which contained bugs/biased results).

## Usage

``` r
SimClean(
  ...,
  dirs = NULL,
  temp = TRUE,
  results = FALSE,
  seeds = FALSE,
  save_details = list()
)
```

## Arguments

- ...:

  one or more character objects indicating which files to remove. Used
  to remove temp files that were saved, or for removing specific file
  names

- dirs:

  a character vector indicating which directories to remove

- temp:

  logical; remove the temporary file saved when passing `save = TRUE`?

- results:

  logical; remove the `.rds` results files saved when passing
  `save_results = TRUE`?

- seeds:

  logical; remove the seed files saved when passing `save_seeds = TRUE`?

- save_details:

  a list pertaining to information about how and where files were saved
  (see the corresponding list in
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md))

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

[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

# remove file called 'results.rds'
SimClean('results.rds')

# remove default temp file
SimClean()

# remove customized saved-results directory called 'mydir'
SimClean(results = TRUE, save_details = list(save_results_dirname = 'mydir'))

} # }
```
