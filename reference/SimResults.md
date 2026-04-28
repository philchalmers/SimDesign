# Read in saved simulation results

If
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
was passed the flag `save_results = TRUE` then the row results
corresponding to the `design` object will be stored to a suitable
sub-directory as individual `.rds` or `qs2` files. While users could use
[`readRDS`](https://rdrr.io/r/base/readRDS.html) and `qs2` directly to
read these files in themselves, this convenience function will read the
desired rows in automatically given the returned object from the
simulation. Can be used to read in 1 or more files at once (if more than
1 file is read in then the result will be stored in a list).

## Usage

``` r
SimResults(obj, which, prefix = "results-row", wd = getwd(), rbind = FALSE)
```

## Arguments

- obj:

  object returned from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  where `save_results = TRUE` or `store_results` was used. If the former
  then the remaining function arguments can be useful for reading in
  specific files.

  Alternatively, the object can be from the `Spower` package, evaluated
  using either `Spower()` or `SpowerBatch()`

- which:

  a numeric vector indicating which rows should be read in. If missing,
  all rows will be read in

- prefix:

  character indicating prefix used for stored files

- wd:

  working directory; default is found with
  [`getwd`](https://rdrr.io/r/base/getwd.html).

- rbind:

  logical; should the results be combined by row or returned as a list?
  Only applicable when the supplied `obj` was obtained from the function
  `Spower::SpowerBatch()`

## Value

the returned result is either a nested list (when `length(which) > 1`)
or a single list (when `length(which) == 1`) containing the simulation
results. Each read-in result refers to a list of 4 elements:

- `condition`:

  the associate row (ID) and conditions from the respective `design`
  object

- `results`:

  the object with returned from the `analyse` function, potentially
  simplified into a matrix or data.frame

- `errors`:

  a table containing the message and number of errors that caused the
  generate-analyse steps to be rerun. These should be inspected
  carefully as they could indicate validity issues with the simulation
  that should be noted

- `warnings`:

  a table containing the message and number of non-fatal warnings which
  arose from the analyse step. These should be inspected carefully as
  they could indicate validity issues with the simulation that should be
  noted

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

[`descript`](http://philchalmers.github.io/SimDesign/reference/descript.md),
[`SimRead`](http://philchalmers.github.io/SimDesign/reference/SimRead.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

# store results (default behaviour)
sim <- runSimulation(..., store_results = TRUE)
SimResults(sim)

# store results to drive if RAM issues are present
obj <- runSimulation(..., save_results = TRUE)

# row 1 results
row1 <- SimResults(obj, 1)

# rows 1:5, stored in a named list
rows_1to5 <- SimResults(obj, 1:5)

# all results
rows_all <- SimResults(obj)

} # }
```
