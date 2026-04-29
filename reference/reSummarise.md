# Run a summarise step for results that have been saved to the hard drive

When
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
uses the option `save_results = TRUE` the R replication results from the
Generate-Analyse functions are stored to the hard drive. As such,
additional summarise components may be required at a later time, whereby
the respective `.rds` files must be read back into R to be summarised.
This function performs the reading of these files, application of a
provided summarise function, and final collection of the respective
results.

## Usage

``` r
reSummarise(
  summarise,
  dir = NULL,
  files = NULL,
  results = NULL,
  Design = NULL,
  fixed_objects = NULL,
  boot_method = "none",
  boot_draws = 1000L,
  CI = 0.95,
  prefix = "results-row"
)
```

## Arguments

- summarise:

  a summarise function to apply to the read-in files. See
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  for details.

  Note that if the simulation contained only one row then the new
  summarise function can be defined as either
  `summarise <- function(results, fixed_objects)`, if `fixed_objects` is
  required, or `summarise <- function(results)`,

- dir:

  directory pointing to the .rds files to be read-in that were saved
  from `runSimulation(..., save_results=TRUE)`. If `NULL`, it is assumed
  the current working directory contains the .rds files

- files:

  (optional) names of files to read-in. If `NULL` all files located
  within `dir` will be used

- results:

  (optional) the results of
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  when no `summarise` function was provided. Can be either a `tibble` or
  `matrix` (indicating that exactly one design condition was evaluated),
  or a `list` of `matrix`/`tibble` objects indicating that multiple
  conditions were performed with no summarise evaluation.

  Alternatively, if `store_results = TRUE` in the
  [`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  execution then the final SimDesign object may be passed, where the
  generate-analyse information will be extracted from the object instead

- Design:

  (optional) if `results` input used, and design condition information
  important in the summarise step, then the original `design` object
  from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  should be included

- fixed_objects:

  (optional) see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  for details

- boot_method:

  method for performing non-parametric bootstrap confidence intervals
  for the respective meta-statistics computed by the `Summarise`
  function. See
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  for details

- boot_draws:

  number of non-parametric bootstrap draws to sample for the `summarise`
  function after the generate-analyse replications are collected.
  Default is 1000

- CI:

  bootstrap confidence interval level (default is 95%)

- prefix:

  character indicating prefix used for stored files

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
Design <- createDesign(N = c(10, 20, 30))

Generate <- function(condition, fixed_objects) {
    dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
    dat
}

Analyse <- function(condition, dat, fixed_objects) {
    ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
    ret
}

Summarise <- function(condition, results, fixed_objects){
    colMeans(results)
}

if (FALSE) { # \dontrun{
# run the simulation
runSimulation(design=Design, replications=50,
              generate=Generate, analyse=Analyse,
              summarise=Summarise, save_results=TRUE,
              save_details = list(save_results_dirname='simresults'))


res <- reSummarise(Summarise, dir = 'simresults/')
res

Summarise2 <- function(condition, results, fixed_objects){
    ret <- c(mean_ests=colMeans(results), SE=colSDs(results))
    ret
}

res2 <- reSummarise(Summarise2, dir = 'simresults/')
res2

SimClean(dir='simresults/')

} # }

###
# Similar, but with results stored within the final object

res <- runSimulation(design=Design, replications=50, store_results = TRUE,
                     generate=Generate, analyse=Analyse, summarise=Summarise)
res
#> # A tibble: 3 × 7
#>       N   mean median REPLICATIONS SIM_TIME       SEED COMPLETED               
#>   <dbl>  <dbl>  <dbl>        <dbl> <chr>         <int> <chr>                   
#> 1    10 10.192 10.230           50 0.01s    1771457935 Wed Apr 29 19:53:26 2026
#> 2    20 10.146 10.084           50 0.01s     590590962 Wed Apr 29 19:53:26 2026
#> 3    30 10.257 10.328           50 0.01s     273992574 Wed Apr 29 19:53:26 2026

# same summarise but with bootstrapping
res2 <- reSummarise(Summarise, results = res, boot_method = 'basic')
res2
#> # A tibble: 3 × 7
#>       N  mean median BOOT_mean_2.5 BOOT_mean_97.5 BOOT_median_2.5
#>   <dbl> <dbl>  <dbl>         <dbl>          <dbl>           <dbl>
#> 1    10  10.2   10.2          9.76           10.6            9.69
#> 2    20  10.1   10.1          9.87           10.4            9.75
#> 3    30  10.3   10.3          9.99           10.5           10.1 
#> # ℹ 1 more variable: BOOT_median_97.5 <dbl>
```
