# Attach objects for easier reference

The behaviour of this function is very similar to
[`attach`](https://rdrr.io/r/base/attach.html), however it is
environment specific, and therefore only remains defined in a given
function rather than in the Global Environment. Hence, this function is
much safer to use than the
[`attach`](https://rdrr.io/r/base/attach.html), which incidentally
should never be used in your code. This is useful primarily as a
convenience function when you prefer to call the variable names in
`condition` directly rather than indexing with `condition$sample_size`
or `with(condition, sample_size)`, for example.

## Usage

``` r
Attach(
  ...,
  omit = NULL,
  check = TRUE,
  attach_listone = TRUE,
  RStudio_flags = FALSE,
  clip = interactive()
)
```

## Arguments

- ...:

  a comma separated list of `data.frame`, `tibble`, `list`, or `matrix`
  objects containing (column) elements that should be placed in the
  current working environment

- omit:

  an optional character vector containing the names of objects that
  should not be attached to the current environment. For instance, if
  the objects named 'a' and 'b' should not be attached then use
  `omit = c('a', 'b')`. When NULL (default) all objects are attached

- check:

  logical; check to see if the function will accidentally replace
  previously defined variables with the same names as in `condition`?
  Default is `TRUE`, which will avoid this error

- attach_listone:

  logical; if the element to be assign is a list of length one then
  assign the first element of this list with the associated name. This
  generally avoids adding an often unnecessary list 1 index, such as
  `name <- list[[1L]]`

- RStudio_flags:

  logical; print R script output comments that disable flagged missing
  variables in RStudio? Requires the form
  `Attach(Design, RStudio_flags=TRUE)` or in an interactive debugging
  session `Attach(condition, RStudio_flags=TRUE)`

- clip:

  when `Rstudio_flags = TRUE` should the output be copied to the
  clipboard using `clipr`? Makes it easier to paste into existing code.
  Only clipped in interactive mode

## Details

Note that if you are using RStudio with the *"Warn if variable used has
no definition in scope"* diagnostic flag then using `Attach()` will
raise suspensions. To suppress such issues, you can either disable such
flags (the atomic solution) or evaluate the following output in the R
console and place the output in your working simulation file.

`Attach(Design, RStudio_flags = TRUE)`

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

[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md),
[`Generate`](http://philchalmers.github.io/SimDesign/reference/Generate.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
Design <- createDesign(N1=c(10,20),
                       N2=c(10,20),
                       sd=c(1,2))
Design
#> # A tibble: 8 × 3
#>      N1    N2    sd
#>   <dbl> <dbl> <dbl>
#> 1    10    10     1
#> 2    20    10     1
#> 3    10    20     1
#> 4    20    20     1
#> 5    10    10     2
#> 6    20    10     2
#> 7    10    20     2
#> 8    20    20     2

# does not use Attach()
Generate <- function(condition, fixed_objects ) {
    # condition = single row of Design input (e.g., condition <- Design[1,])
    N1 <- condition$N1
    N2 <- condition$N2
    sd <- condition$sd

    group1 <- rnorm(N1)
    group2 <- rnorm(N2, sd=sd)
    dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
                      DV = c(group1, group2))
    dat
}

# similar to above, but using the Attach() function instead of indexing
Generate <- function(condition, fixed_objects ) {
    Attach(condition) # N1, N2, and sd are now 'attached' and visible

    group1 <- rnorm(N1)
    group2 <- rnorm(N2, sd=sd)
    dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
                      DV = c(group1, group2))
    dat
}

#####################
# NOTE: if you're using RStudio with code diagnostics on then evaluate + add the
# following output to your source file to manually support the flagged variables

Attach(Design, RStudio_flags=TRUE)
#> # !diagnostics suppress=N1,N2,sd

# Below is the same example, however with false positive missing variables suppressed
# when # !diagnostics ... is added added to the source file(s)

# !diagnostics suppress=N1,N2,sd
Generate <- function(condition, fixed_objects ) {
    Attach(condition) # N1, N2, and sd are now 'attached' and visible

    group1 <- rnorm(N1)
    group2 <- rnorm(N2, sd=sd)
    dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
                      DV = c(group1, group2))
    dat
}

```
