# Saving simulation results and state

Unsurprisingly, you may want to save your results to your hard disk in
case of power outages or random system crashes to allow restarting at
the interrupted location, save more complete versions of the analysis
results in case you want to inspect the complete simulation results at a
later time, store/restore the R seeds for debugging and replication
purposes, and so on. This document demonstrates various ways in which
`SimDesign` saves output to hard disks.

As usual, define the functions of interest.

``` r
library(SimDesign)
# SimFunctions()

Design <- createDesign(N = c(10,20,30))
```

``` r
Generate <- function(condition, fixed_objects) {
    dat <- rnorm(condition$N)    
    dat
}

Analyse <- function(condition, dat, fixed_objects) {
    ret <- c(p = t.test(dat)$p.value)
    ret
}

Summarise <- function(condition, results, fixed_objects) {
    ret <- EDR(results, alpha = .05)
    ret
}
```

This is a very simple simulation that takes very little time to
complete, however it will be used to show the basic saving concepts
supported in `SimDesign`. Note that more detailed information is located
in the `runSimulation` documentation.

## Option: `save = TRUE` (Default is `TRUE`)

The `save` flag triggers whether temporary results should be saved to
the hard-disk in case of power outages and crashes. When this flag is
used results can easily be restored automatically and the simulation can
continue where it left off after the hardware problems have been dealt
with. In fact, no modifications in the code required because
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
will automatically detect temporary files to resume from (so long as
they are resumed from the same computer node; otherwise, see the
`save_details` list).

As a simple example, say that in the $N = 30$ condition something went
terribly wrong and the simulation crashed. However, the first two design
conditions are perfectly fine. The `save` flag is very helpful here
because the state is not lost and the results are still useful. Finally,
supplying a `filename` argument will safely save the aggregate
simulation results to the hard-drive for future reference; however, this
won’t be called until the simulation is complete.

``` r
Analyse <- function(condition, dat, fixed_objects) {
    if(condition$N == 30) stop('Danger Will Robinson!')
    ret <- c(p = t.test(dat)$p.value)
    ret
}

res <- runSimulation(Design, replications = 1000, save=TRUE, filename='my-simple-sim',
                     generate=Generate, analyse=Analyse, summarise=Summarise,
                     control = list(stop_on_fatal = TRUE))
```

Check that temporary file still exists.

``` r
files <- dir()
files[grepl('SIMDESIGN', files)]
```

    ## [1] "SIMDESIGN-TEMPFILE_runnervmeorf1"

Notice here that the simulation stopped at 67% because the third design
condition threw too many consecutive errors (this is a built-in
fail-safe in `SimDesign`). To imitate a type of crash/power outage,
`control = list(stop_on_fatal = TRUE)` input; otherwise, the simulation
would continue normally over these terminal conditions though place `NA`
placeholders for the terminal condition.

After we fix this portion of the code the simulation can be restarted at
the previous state and continue on as normal. Therefore, in the event of
unforeseen program execution crashes no time is lost.

``` r
Analyse <- function(condition, dat, fixed_objects) {
    ret <- c(p = t.test(dat)$p.value)
    ret
}

res <- runSimulation(Design, replications = 1000, save=TRUE, filename='my-simple-sim',
                     generate=Generate, analyse=Analyse, summarise=Summarise)
```

Check which files exist.

``` r
files <- dir()
files[grepl('SIMDESIGN', files)]
```

    ## character(0)

``` r
files[grepl('my-simp', files)]
```

    ## [1] "my-simple-sim.rds"

Notice that when complete, the temporary file is removed from the
hard-drive.

Relatedly, the `.Random.seed` states for each successful replication can
be saved by passing `control = list(store_Random.seeds = TRUE))` to
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md),
though these are generally only useful under exceptional circumstances
(e.g., when the generate-analyse results are unusual but did not throw a
warning or error message, yet should be inspected interactively).

## `store_results` (`TRUE` by default)

Passing `store_results = TRUE` stores the `results` object information
that are passed to
[`Summarise()`](http://philchalmers.github.io/SimDesign/reference/Summarise.md)
in the returned object. This allows for further inspection of the
simulation results, and potential to use functions such as
[`reSummarise()`](http://philchalmers.github.io/SimDesign/reference/reSummarise.md)
to provide meta-summaries of the simulation at a later time. After the
simulation is complete, these results can be extracted using
`SimResults(res)` (or more generally with
`SimExtract(res, what = 'results')`). For example,

``` r
# store_results=TRUE by default
res <- runSimulation(Design, replications = 3, 
              generate=Generate, analyse=Analyse, summarise=Summarise)
results <- SimResults(res)
results
```

    ## # A tibble: 9 × 2
    ##       N     p
    ##   <dbl> <dbl>
    ## 1    10 0.363
    ## 2    10 0.816
    ## 3    10 0.555
    ## 4    20 0.674
    ## 5    20 0.481
    ## 6    20 0.120
    ## 7    30 0.664
    ## 8    30 0.527
    ## 9    30 0.848

Note that this should be used if the number of replications/`design`
conditions is small enough to warrant such storage; otherwise, the R
session may run out of memory (RAM) as the simulation progresses.
Otherwise, `save_results = TRUE` described below is the recommended
approach to resolve potential memory issues.

## Option: `save_results = TRUE` (`FALSE` by default; set to `TRUE` if RAM is an issue)

Finally, the `save_results` argument will output the `results` elements
that were passed to
[`Summarise()`](http://philchalmers.github.io/SimDesign/reference/Summarise.md)
to separate `.rds` files containing all the analysis results and
`condition` information. This option is supported primarily for
simulations that are anticipated to have memory storage issues, where
the results are written to the hard-drive and released from memory. Note
that when using `save_results` the `save` flag is automatically set to
`TRUE` to ensure that the simulation state is correctly tracked.

``` r
res <- runSimulation(Design, replications = 1000, save_results=TRUE,
                     generate=Generate, analyse=Analyse, summarise=Summarise)
dir <- dir()
directory <- dir[grepl('SimDesign-results', dir)]
dir(directory)
```

    ## [1] "results-row-1" "results-row-2" "results-row-3"

Here we can see that three files have been saved to the folder with the
computer node name and a prefixed `'SimDesign-results'` character
string. Each file contains the respective simulation results (including
errors and warnings), which can be read in directly with
[`SimRead()`](http://philchalmers.github.io/SimDesign/reference/SimRead.md):

``` r
row1 <- SimRead(paste0(directory, '/results-row-1'))
str(row1)
```

    ## List of 6
    ##  $ condition    : tibble [1 × 1] (S3: tbl_df/tbl/data.frame)
    ##   ..$ N: num 10
    ##  $ results      :'data.frame':   1000 obs. of  1 variable:
    ##   ..$ p: num [1:1000] 0.976 0.397 0.143 0.595 0.453 ...
    ##  $ errors       : 'table' int[0 (1d)] 
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ : NULL
    ##  $ error_seeds  : NULL
    ##  $ warnings     : 'table' int[0 (1d)] 
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ warnings: NULL
    ##  $ warning_seeds: NULL

``` r
row1$condition
```

    ## # A tibble: 1 × 1
    ##       N
    ##   <dbl>
    ## 1    10

``` r
head(row1$results)
```

    ##        p
    ## 1 0.9759
    ## 2 0.3974
    ## 3 0.1430
    ## 4 0.5947
    ## 5 0.4534
    ## 6 0.2007

or, equivalently, with the
[`SimResults()`](http://philchalmers.github.io/SimDesign/reference/SimResults.md)
function

``` r
# first row
row1 <- SimResults(res, which = 1)
str(row1)
```

    ## tibble [1,000 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ N: num [1:1000] 10 10 10 10 10 10 10 10 10 10 ...
    ##  $ p: num [1:1000] 0.976 0.397 0.143 0.595 0.453 ...

The
[`SimResults()`](http://philchalmers.github.io/SimDesign/reference/SimResults.md)
function has the added benefit that it can read-in all simulation
results at once (only recommended if RAM can hold all the information),
or simply hand pick which ones should be inspected. For example, here is
how all the saved results can be inspected:

``` r
input <- SimResults(res)
str(input)
```

    ## tibble [3,000 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ N: num [1:3000] 10 10 10 10 10 10 10 10 10 10 ...
    ##  $ p: num [1:3000] 0.976 0.397 0.143 0.595 0.453 ...

Should the need arise to remove the results directory then the
[`SimClean()`](http://philchalmers.github.io/SimDesign/reference/SimClean.md)
function is the easiest way to remove all unwanted files and
directories.

``` r
SimClean(results = TRUE)
```

## Recommendations

My general recommendation when running simulations is to supply a
`filename = 'some_simulation_name'` when your simulation is finally
ready for run time (particularly for simulations which take a long time
to finish), and to leave the default `save = TRUE` and
`store_results = TRUE` to track any temporary files in the event of
unexpected crashes and to store the `results` objects for future
inspection (should the need arise). As the aggregation of the simulation
results is often what you are interested in then this approach will
ensure that the results are stored in a succinct manner for later
analyses. However, if RAM is suspected to be an issue as the simulation
progresses then using `save_results = TRUE` is recommended to avoid
memory-based storage issues.

Note that for array simulations evaluated via
[`runArraySimulation()`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)
the results will be stored within each respective object using
`store_results = TRUE` as files are independently saved on the assigned
arrays, negating the implicit RAM issue that could arise when using
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md).
