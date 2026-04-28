# Extract extra information from SimDesign objects

Extracts any error or warnings messages, the seeds associated with any
error or warning messages, and any analysis results that were stored in
the final simulation object.

## Usage

``` r
SimExtract(object, what, fuzzy = TRUE, append = TRUE)
```

## Arguments

- object:

  object returned from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- what:

  character vector indicating what information to extract, written in
  agnostic casing (e.g., `'ERRORS'` and `'errors'` are equivalent).
  Possible inputs include

  `'errors'`

  :   to return a `tibble` object containing counts of any error
      messages

  `'warnings'`

  :   to return a `data.frame` object containing counts of any warning
      messages

  `'seeds'`

  :   for the specified random number generation seeds

  `'Random.seeds'`

  :   for the complete list of `.Random.seed` states across replications
      (only stored when
      `runSimulation(..., control = list(store_Random.seeds=TRUE))`)

  `'log_times'`

  :   for the per replication generate/analyse execution times (recorded
      in seconds)

  `'error_seeds'` and `'warning_seeds'`

  :   to extract the associated `.Random.seed` values associated with
      the ERROR/WARNING messages

  `'prepare_seeds'`

  :   to extract the `.Random.seed` states captured before `prepare()`
      was called for each condition

  `'prepare_error_seed'`

  :   to extract the `.Random.seed` state when `prepare()` encountered
      an error (useful for debugging with `load_seed_prepare`)

  `'results'`

  :   to extract the simulation results if the option `store_results`
      was passed to
      [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

  `'filename'` and `'save_results_dirname'`

  :   for extracting the saved file/directory name information (if
      used), `'functions'` to extract the defined functions used in the
      experiment

  `'design'`

  :   to extract the original design object

  Note that `'warning_seeds'` are not stored automatically in
  simulations and require passing `store_warning_seeds = TRUE` to
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md).

- fuzzy:

  logical; use fuzzy string matching to reduce effectively identical
  messages? For example, when attempting to invert a matrix the error
  message *"System is computationally singular: reciprocal condition
  number = 1.92747e-17"* and *"System is computationally singular:
  reciprocal condition number = 2.15321e-16"* are effectively the same,
  and likely should be reported in the same columns of the extracted
  output

- append:

  logical; append the design conditions when extracting error/warning
  messages?

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

[`SimErrors`](http://philchalmers.github.io/SimDesign/reference/SimErrors.md),
[`SimWarnings`](http://philchalmers.github.io/SimDesign/reference/SimWarnings.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

Generate <- function(condition, fixed_objects) {
    int <- sample(1:10, 1)
    if(int > 5) warning('GENERATE WARNING: int greater than 5')
    if(int == 1) stop('GENERATE ERROR: integer is 1')
    rnorm(5)
}

Analyse <- function(condition, dat, fixed_objects) {
    int <- sample(1:10, 1)
    if(int > 5) warning('ANALYSE WARNING: int greater than 5')
    if(int == 1) stop('ANALYSE ERROR: int is 1')
    c(ret = 1)
}

Summarise <- function(condition, results, fixed_objects) {
    mean(results)
}

res <- runSimulation(replications = 100, seed=1234,
                     generate=Generate, analyse=Analyse, summarise=Summarise)
res

SimExtract(res, what = 'errors')     # see also SimErrors()
SimExtract(res, what = 'warnings')   # see also SimWarnings()
seeds <- SimExtract(res, what = 'error_seeds')
seeds[,1:3]

# replicate a specific error for debugging (type Q to exit debugger)
res <- runSimulation(replications = 100, load_seed=seeds[,1], debug='analyse-1',
                     generate=Generate, analyse=Analyse, summarise=Summarise)



} # }
```
