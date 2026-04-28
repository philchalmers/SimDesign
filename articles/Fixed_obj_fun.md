# Exporting objects and functions from the workspace

## Including fixed objects

R is fun language for computer programming and statistics, but it’s not
without it’s quirks. For instance, R generally has a recursive strategy
when attempting to find objects within functions. If an object can’t be
found, R will start to look outside the function’s environment to see if
the object can be located there, and if not, look within even
higher-level environments… This recursive search continues until it
searches for the object in the user workspace/Global environment, and
only when the object can’t be found here will an error be thrown. This
is a strange feature to most programmers who come from other languages,
and when writing simulations may cause some severely unwanted masking
issues. This tutorial demonstrates how to make sure all required
user-defined objects are visible to `SimDesign`.

## Scoping

To demonstrate the issue, let’s define two objects and a function which
uses these objects.

``` r
obj1 <- 10
obj2 <- 20
```

When evaluated, these objects are visible to the user, and can be seen
by typing in the R console by typing
[`ls()`](https://rdrr.io/r/base/ls.html). Functions which do not define
objects with the same name will also be able to locate these values.

``` r
myfun <- function(x) obj1 + obj2
myfun(1)
```

    ## [1] 30

This behavior is indeed a bit strange, but it’s one of R’s quirks.
Unfortunately, when running code in parallel across different cores
these objects *will not be visible*, and therefore must be exported
using other methods (e.g., in the `parallel` package this is done with
[`clusterExport()`](https://rdrr.io/r/parallel/clusterApply.html)).

``` r
library(parallel)
cl <- makeCluster(2)
res <- try(parSapply(cl=cl, 1:4, myfun))
res
```

    ## Error in checkForRemoteErrors(val) : 
    ##   2 nodes produced errors; first error: object 'obj1' not found

Exporting the objects to the cluster fixes the issue.

``` r
clusterExport(cl=cl, c('obj1', 'obj2'))
parSapply(cl=cl, 1:4, myfun)
```

    ## [1] 30 30 30 30

The same reasoning above applies to functions defined in the R
work-space as well, including functions defined within third-party R
packages. Hence, in order to use functions from other packages they must
either be explicitly loaded with
[`require()`](https://rdrr.io/r/base/library.html) or
[`library()`](https://rdrr.io/r/base/library.html) within the
distributed code, or referenced via their Namespace with the `::`
operator (e.g., `mvtnorm::rmvtnorm()`).

## Exporting functions for parallel computing

### Exporting third-party libraries

For simulations that require third-party packages to work correctly
(e.g., `rgumbel()` from the `extraDistr` package) there needs to be
explicit instructions to indicate which namespace these functions
originate from. For parallel computing applications there are three ways
to indicate the namespace of a third-party function:

1.  Explicitly attach the package prior to executing the simulation via
    [`library()`](https://rdrr.io/r/base/library.html) or
    [`require()`](https://rdrr.io/r/base/library.html). This must be
    done prior to using
    [`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
2.  Explicitly pass a character vector containing the packages to be
    attached to `runSimulation(..., packages)` (e.g.,
    `runSimulation(..., packages = c('extraDistr', 'copula'))`)
3.  Point to the function using the `::` operator (e.g.,
    [`extraDistr::rgumbel()`](https://rdrr.io/pkg/extraDistr/man/Gumbel.html))

For performance and masking related issues it is recommended to only
export functions that are actually used in the simulation (e.g., attach
`dplyr` and `tidy` explicitly rather than attaching the complete
`tidyverse`). Note that 2) is technically not required if 1) or 3) is
used.

As a reminder, attaching packages can result in function masking in the
R session, and therefore in situations were ambiguity exists it is
recommended to use the `::` operator explicitly even if these were
attached. Doing so avoids makes it clear where the function originates
regardless of the order with which the packages were attached to the
session.

### Exporting user-defined functions

Exporting user-defined functions explicitly is not required in
`SimDesign`. Any previously written function that has been defined in
the R work space prior to executing
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
will automatically be exported (again, be cognizant of any potential
function masking). Other R objects, on the other hand, are *not*
explicitly exported. See the next section for further details.

## Exporting objects in `SimDesign`

In order to make objects safely visible in `SimDesign` the strategy is
very simple: wrap all desired objects into a named list (or other
object), and pass this to the `fixed_objects` argument. From here,
elements can be indexed using the `$` operator or
[`with()`](https://rdrr.io/r/base/with.html) function, or whatever other
method may be convenient. Note, however, this is only required for
defined *objects* not *functions*.

As an aside, an alternative approach is simply to define/source the
objects within the respective `SimDesign` functions; that way they will
clearly be visible at run-time. The following `fixed_objects` approach
is really only useful when the defined objects contain a large amount of
code or information.

``` r
library(SimDesign)
#SimFunctions(comments = FALSE)

### Define design conditions and number of replications
Design <- createDesign(N = c(10, 20, 30))
replications <- 1000

# define custom functions and objects (or use source() to read these in from an external file)
SD <- 2
my_gen_fun <- function(n, sd) rnorm(n, sd = sd)
my_analyse_fun <- function(x) c(p = t.test(x)$p.value)
fixed_objects <- list(SD=SD)

#---------------------------------------------------------------------------

Generate <- function(condition, fixed_objects) {
    Attach(condition) # make condition names available (e.g., N)
    
    # further, can use with() to use 'SD' directly instead of 'fixed_objects$SD'
    ret <- with(fixed_objects, my_gen_fun(N, sd=SD))
    ret
}

Analyse <- function(condition, dat, fixed_objects) {
    ret <- my_analyse_fun(dat)
    ret
}

Summarise <- function(condition, results, fixed_objects) {
    ret <- EDR(results, alpha = .05)
    ret
}

#---------------------------------------------------------------------------

### Run the simulation
res <- runSimulation(Design, replications, verbose=FALSE, fixed_objects=fixed_objects,
                     generate=Generate, analyse=Analyse, summarise=Summarise, debug='none')
res
```

    ## # A tibble: 3 × 6
    ##       N     p REPLICATIONS SIM_TIME       SEED COMPLETED               
    ##   <dbl> <dbl>        <dbl> <chr>         <int> <chr>                   
    ## 1    10 0.044         1000 0.22s     488349520 Tue Apr 28 13:56:00 2026
    ## 2    20 0.06          1000 0.33s     469344150 Tue Apr 28 13:56:01 2026
    ## 3    30 0.054         1000 0.21s    1550099436 Tue Apr 28 13:56:01 2026

By placing objects in a list and passing this to `fixed_objects`, the
objects are safely exported to all relevant functions. Furthermore,
running this code in parallel will also be valid as a consequence (see
below) because all objects are properly exported to each core.

``` r
res <- runSimulation(Design, replications, verbose=FALSE, fixed_objects=fixed_objects,
                     generate=Generate, analyse=Analyse, summarise=Summarise, debug='none',
                     parallel = TRUE)
```

Again, remember that this is only required for **R objects that are NOT
user-defined functions or third-party functions**!
