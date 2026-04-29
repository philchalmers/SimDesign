# Managing warning and error messages

Error catching is an important area to consider when creating Monte
Carlo simulations. Sometimes, iterative algorithms will ‘fail to
converge’, or otherwise crash for other reasons (e.g., sparse data).
Moreover, errors may happen in unexpected combinations of the design
factors under investigation, which can lead to abrupt termination of a
simulation’s execution.

`SimDesign` makes managing errors much easier because the internal
functions are automatically wrapped within
[`try()`](https://rdrr.io/r/base/try.html) blocks, and therefore
simulations will not terminate unexpectedly. This type of information is
also collected in the final simulation object since it may be relevant
to the writer that something unknown is going wrong in the code-base.
Below we demonstrate what happens when errors are thrown and caught, and
how this information is tracked in the returned object.

#### (Potential Prerequisite) Converting warnings to errors explicitly via `manageWarnings()`

Before getting into the specifics of error handling, there are often
occasions where functions outside of the user’s control will return
`warning` message that either border on (or should be treated as) error
messages if they influence the veracity of the simulation results. Such
examples may include models that appear to ‘converge’ but do so with
non-nonsensical parameter estimates (e.g., negative variances,
non-positive definite correlation matrices, maximum iterations reached
in an numerical searching algorithm, etc). However, because such issues
are non-fatal third-party software (i.e., functions not written by the
developer of the simulation) may simply raise a `warning` message for
further inspection, whereas in a Monte Carlo simulation experiment such
issues should generally be treated as invalid (though their frequency of
occurrence should still be tracked, as is the default in `SimDesign`).

To resolve this issue, and to avoid using a more nuclear option such as
setting `option(warn=2)` to treat *all* warnings as errors in the
simulation, the function
[`manageWarnings()`](http://philchalmers.github.io/SimDesign/reference/manageWarnings.md)
can be used to explicit convert known warning message strings into
errors that disrupt the code execution while allowing other warning
messages to continue to be raised.

For example, if a function utilized in a simulation was

``` r
myfun <- function() {
    if(sample(c(TRUE, FALSE), 1, prob = c(.1, .9)))
        warning('This warning is serious')
    if(sample(c(TRUE, FALSE), 1, prob = c(.5, .5)))
        warning('This warning is no big deal')
    return(1)
}

set.seed(1)
out <- myfun()

set.seed(2)
out <- myfun()
```

    ## Warning in myfun(): This warning is no big deal

``` r
set.seed(7)
out <- myfun()
```

    ## Warning in myfun(): This warning is serious

then whenever the serious warning message is raised it could be
explicitly converted to an error using an internal
[`grepl()`](https://rdrr.io/r/base/grep.html) test.

``` r
set.seed(1)
out1 <- manageWarnings(myfun(), 
                        warning2error='This warning is serious')
out1
```

    ## [1] 1

``` r
set.seed(2)
out2 <- manageWarnings(myfun(), 
                        warning2error='This warning is serious')
```

    ## Warning in myfun(): This warning is no big deal

``` r
out2
```

    ## [1] 1

``` r
set.seed(7)
out3 <- manageWarnings(myfun(), 
                        warning2error='This warning is serious')
```

    ## Error:
    ## ! This warning is serious

which now converts the previous warning message into an error message,
thereby correctly disrupting the flow of the Monte Carlo simulation
experiment and prompting a new call to
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md).
Of course, all warning and error messages are tallied in the resulting
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
object, though now the serious warnings will be tallied as disruptive
errors instead of warnings that continued normally.

## Error Managing Workflow

### Define the functions

As usual, define the functions of interest.

``` r
library(SimDesign)
# SimFunctions(comments=FALSE)

Design <- createDesign(N = c(10,20,30))
```

``` r
Generate <- function(condition, fixed_objects) {
    ret <- with(condition, rnorm(N))
    ret
}

Analyse <- function(condition, dat, fixed_objects) {
    whc <- sample(c(0,1,2,3), 1, prob = c(.7, .20, .05, .05))
    if(whc == 0){
       ret <- mean(dat)
    } else if(whc == 1){
        ret <- t.test() # missing arguments
    } else if(whc == 2){
        ret <- t.test('invalid') # invalid arguments
    } else if(whc == 3){
        # throw error manually 
        stop('Manual error thrown') 
    }
    # manual warnings
    if(sample(c(TRUE, FALSE), 1, prob = c(.1, .9)))
        warning('This warning happens rarely')
    if(sample(c(TRUE, FALSE), 1, prob = c(.5, .5)))
        warning('This warning happens much more often')
    ret
}

Summarise <- function(condition, results, fixed_objects) {
    ret <- c(bias = bias(results, 0))
    ret
}
```

The above simulation is just an example of how errors are tracked in
`SimDesign`, as well as how to throw a manual error in case the data
should be re-drawn based on the user’s decision (e.g., when a model
converges, but fails to do so before some number of predefined
iterations).

### Run the simulation

``` r
result <- runSimulation(Design, replications = 100, 
                       generate=Generate, analyse=Analyse, summarise=Summarise)
```

``` r
print(result)
```

    ## # A tibble: 3 × 8
    ##       N     bias REPLICATIONS SIM_TIME       SEED COMPLETED      ERRORS WARNINGS
    ##   <dbl>    <dbl>        <dbl> <chr>         <int> <chr>           <int>    <int>
    ## 1    10 0.061138          100 0.05s    1140350788 Wed Apr 29 19…     53       59
    ## 2    20 0.014295          100 0.16s     312928385 Wed Apr 29 19…     52       60
    ## 3    30 0.017927          100 0.05s     866248189 Wed Apr 29 19…     42       56

What you’ll immediately notice from this output object is that counts of
the error and warning messages have been appended to the `result`
object. This is useful to determine just how problematic the errors and
warnings are based on their frequency alone. Furthermore, the specific
frequency in which the errors/warnings occurred are also included for
each design condition (here the `t.test.default()` error, where no
inputs were supplied, occurred more often than the manually thrown error
as well as the invalid-input error) after extracting and inspecting
`SimErrors(result)` and `SimWarnings(result)`.

``` r
SimErrors(result)
```

    ##    N ERROR:  Error in t.test.default("invalid") : not enough 'x' observations\n
    ## 1 10                                                                         12
    ## 2 20                                                                          9
    ## 3 30                                                                         10
    ##   ERROR:  Error in t.test.default() : argument "x" is missing, with no default\n
    ## 1                                                                             31
    ## 2                                                                             38
    ## 3                                                                             25
    ##   ERROR:  Manual error thrown\n
    ## 1                            10
    ## 2                             5
    ## 3                             7

``` r
SimWarnings(result)
```

    ##    N WARNING:  This warning happens much more often
    ## 1 10                                             49
    ## 2 20                                             53
    ## 3 30                                             44
    ##   WARNING:  This warning happens rarely
    ## 1                                    10
    ## 2                                     7
    ## 3                                    12

Finally, `SimDesign` has a built-in safety feature controlled by with
`max_errors` argument to avoid getting stuck in infinite redrawing
loops. By default, if more than 50 errors are consecutively returned
then the simulation condition will be halted, and a warning message will
be printed to the console indicating the last observed fatal error.
These safety features are built-in because too many consecutive
[`stop()`](https://rdrr.io/r/base/stop.html) calls generally indicates a
major problem in the simulation code which should be fixed before
continuing. However, when encountering fatal errors in a given
simulation condition the remainder of the simulation experiment will
still be executed as normal, where for the problematic conditions`NA`
placeholders will be assigned to these rows in the final output object.
This is so that the entire experiment does not unexpectedly terminate
due to one or more problematic row conditions in `Design`, and instead
these conditions can be inspected and debugged all at once at a later
time. Of course, if inspecting the code directly, the simulation could
be manually halted so that these terminal errors can be attended to
immediately (e.g., using `Ctrl + c`, or clicking the ‘Stop’ icon in
Rstudio).

## What to do?

When errors are raised the first step is to locate and potentially fix
them if the issue affects the quality of the simulation experimenter.
Note that not all ‘errors’ are problematic (e.g., some algorithms simply
cannot converge under select empirical conditions, and this is worth
noting), however in situations where the error was surprising, and
potentially addressable (e.g., is simply a coding mistake), then various
strategies can be taken moving forward.

### Explicit debugging

If errors occur too often (but not in a fatal way) then the respective
design conditions should either be extracted out of the simulation or
further inspected to determine if they can be fixed (e.g., providing
better starting values, increasing convergence criteria/number of
iterations, etc). For instance, say that the fourth row of the `design`
object raised a number of error messages that should be inspected
further. One useful approach then would be to debug the 4th row on the
instance that an error is raised, which can be achieved using the
following:

``` r
runSimulation(..., debug = 'error-4')
```

The `error` flag is used to enter R’s debugger on the first instance of
an error, while the `-4` indicates that only the 4th row of `design`
should be evaluated. This is also one instance where changing warning
messages into error messages (i.e.,
`runSimulation(..., extra_options = list(warnings_as_errors=TRUE))`) is
particularly useful so that the state that generated a warning can be
inspected directly. Note that similar arguments can be made for
explicitly debugging functions in the generate-analyse-summarise chain
(e.g., `debug = 'analyse-4'`), though these are less useful for
debugging (more useful for initial code design).

### Manual debugging via `try()`

Failing the above approach, manually wrapping the suspected problematic
function(s) in [`try()`](https://rdrr.io/r/base/try.html) calls can also
work, though this may require some waiting if the errors are less
frequent. Specifically, adding the line
`if(is(object, 'try-error')) browser()` will jump into the
location/replication where the object unexpectedly witnessed, though
admittedly this is more clunky approach than using `debug`.
Nevertheless, jumping into the exact instant when the error occurred,
particularly in the case where an `analyse()` function is throwing
multiple error messages, can help diagnose what exactly went wrong in
the simulation state, thereby allowing you to quickly fix the issue.

### Extracting error seeds for hard-to-find bugs

Finally, failing both approaches stored information can be used to
locating and repeating the observed errors, which is particularly useful
if the error raised is rare. By default, all `.Random.seed` states
associated with errors are stored within the final object, and these can
be extracted using the `SimErrors(..., seeds=TRUE)` option. This
function returns a `tibble` object with each seed stored column-wise and
as `list` elements, where the associated error message is contained in
the column name itself (and allowed to be coerced into a valid column
name to make it easier to use the `$` operator). For example,

``` r
eseeds <- SimErrors(result, seeds = TRUE)
eseeds
```

    ## # A tibble: 3 × 2
    ##       N SEEDS           
    ##   <dbl> <list>          
    ## 1    10 <named list [3]>
    ## 2    20 <named list [3]>
    ## 3    30 <named list [3]>

``` r
# list elements are named based on the error observed
names(eseeds$SEEDS[[1]])
```

    ## [1] "Error in t test default     argument  x  is missing  with no default "
    ## [2] "Manual error thrown "                                                 
    ## [3] "Error in t test default  invalid     not enough  x  observations "

``` r
# one of the seeds seed associated with a condition error (third unique error raised)
eseeds$SEEDS[[1]][[3]][,1]
```

    ## # A tibble: 626 × 1
    ##    Design_row_1.11..Error.in.t.test.default..invalid.....not.enough..x..observ…¹
    ##                                                                            <int>
    ##  1                                                                         10403
    ##  2                                                                           115
    ##  3                                                                    2043326239
    ##  4                                                                    -971754976
    ##  5                                                                    -386998729
    ##  6                                                                    1721252608
    ##  7                                                                     230191164
    ##  8                                                                     546351527
    ##  9                                                                     910264940
    ## 10                                                                   -1637379845
    ## # ℹ 616 more rows
    ## # ℹ abbreviated name:
    ## #   ¹​Design_row_1.11..Error.in.t.test.default..invalid.....not.enough..x..observations.

Given the specific seed of interest, as well as the specific row
condition that it is associated with, replicating an exact error can be
achieved by passing this vector to the `load_seed` input. For example,
replicating the first error message can be achieved as follows, where it
makes the most sense to immediately go into the debugging mode via the
`debug` inputs.

**Note**: It is important to manually select the correct `Design` row
using this error extraction approach; otherwise, the seed will clearly
not replicate the exact problem state.

``` r
picked_seed <- eseeds$SEEDS[[1]][[3]][,1]

# debug analyse() for first row of Design object via debug='analyse-1'
runSimulation(Design, replications = 100, load_seed=picked_seed, debug='analyse-1',
              generate=Generate, analyse=Analyse, summarise=Summarise)

# this is equivalent (not evaluated)
# runSimulation(Design[1, ], replications = 100, load_seed=picked_seed, debug='analyse',
#              generate=Generate, analyse=Analyse, summarise=Summarise)
```

The `.Random.seed` state will be loaded at this exact state, and will
always be repeated at this state as well (in case `c` is typed in the
debugger, or somehow the error is harder to find while walking through
the debug mode). Hence, users must type `Q` to exit the debugger after
they have better understood the nature of the error message first-hand,
after which the `load_seed` input should be removed.
