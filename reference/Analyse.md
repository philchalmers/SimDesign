# Compute estimates and statistics

Compute all relevant test statistics, parameter estimates, detection
rates, and so on. This is the computational heavy lifting portion of the
Monte Carlo simulation. Users may define a single Analysis function to
perform all the analyses in the same function environment, or may define
a `list` of named functions to
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
to allow for a more modularized approach to performing the analyses in
independent blocks (but that share the same generated data). Note that
if a suitable
[`Generate`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
function was not supplied then this function can be used to be generate
and analyse the Monte Carlo data (though in general this setup is not
recommended for larger simulations).

## Usage

``` r
Analyse(condition, dat, fixed_objects)
```

## Arguments

- condition:

  a single row from the design input (as a `data.frame`), indicating the
  simulation conditions

- dat:

  the `dat` object returned from the
  [`Generate`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
  function (usually a `data.frame`, `matrix`, `vector`, or `list`)

- fixed_objects:

  object passed down from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

## Value

returns a named `numeric` vector or `data.frame` with the values of
interest (e.g., p-values, effects sizes, etc), or a `list` containing
values of interest (e.g., separate matrix and vector of parameter
estimates corresponding to elements in `parameters`). If a `data.frame`
is returned with more than 1 row then these objects will be wrapped into
suitable `list` objects

## Details

In some cases, it may be easier to change the output to a named `list`
containing different parameter configurations (e.g., when determining
RMSE values for a large set of population parameters).

The use of [`try`](https://rdrr.io/r/base/try.html) functions is
generally not required in this function because `Analyse` is internally
wrapped in a [`try`](https://rdrr.io/r/base/try.html) call. Therefore,
if a function stops early then this will cause the function to halt
internally, the message which triggered the
[`stop`](https://rdrr.io/r/base/stop.html) will be recorded, and
[`Generate`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
will be called again to obtain a different dataset. That said, it may be
useful for users to throw their own
[`stop`](https://rdrr.io/r/base/stop.html) commands if the data should
be re-drawn for other reasons (e.g., an estimated model terminated
correctly but the maximum number of iterations were reached).

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

[`stop`](https://rdrr.io/r/base/stop.html),
[`AnalyseIf`](http://philchalmers.github.io/SimDesign/reference/AnalyseIf.md),
[`manageWarnings`](http://philchalmers.github.io/SimDesign/reference/manageWarnings.md)

## Examples

``` r
if (FALSE) { # \dontrun{

analyse <- function(condition, dat, fixed_objects) {

    # require packages/define functions if needed, or better yet index with the :: operator
    require(stats)
    mygreatfunction <- function(x) print('Do some stuff')

    #wrap computational statistics in try() statements to control estimation problems
    welch <- t.test(DV ~ group, dat)
    ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

    # In this function the p values for the t-tests are returned,
    #  and make sure to name each element, for future reference
    ret <- c(welch = welch$p.value,
             independent = ind$p.value)

    return(ret)
}

# A more modularized example approach

analysis_welch <- function(condition, dat, fixed_objects) {
    welch <- t.test(DV ~ group, dat)
    ret <- c(p=welch$p.value)
    ret
}

analysis_ind <- function(condition, dat, fixed_objects) {
    ind <- t.test(DV ~ group, dat, var.equal=TRUE)
    ret <- c(p=ind$p.value)
    ret
}

# pass functions as a named list
# runSimulation(..., analyse=list(welch=analyse_welch, independent=analysis_ind))

} # }
```
