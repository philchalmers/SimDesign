# Create the simulation design object

Create a partially or fully-crossed data object reflecting the unique
simulation design conditions. Each row of the returned object represents
a unique simulation condition, and each column represents the named
factor variables under study.

## Usage

``` r
createDesign(
  ...,
  subset,
  fractional = NULL,
  tibble = TRUE,
  stringsAsFactors = FALSE,
  fully.crossed = TRUE
)

# S3 method for class 'Design'
print(x, list2char = TRUE, pillar.sigfig = 5, show.IDs = FALSE, ...)

# S3 method for class 'Design'
x[i, j, ..., drop = FALSE]

rbindDesign(..., keep.IDs = FALSE)
```

## Arguments

- ...:

  comma separated list of named input objects representing the
  simulation factors to completely cross. Note that these arguments are
  passed to [`expand.grid`](https://rdrr.io/r/base/expand.grid.html) to
  perform the complete crossings

- subset:

  (optional) a logical vector indicating elements or rows to keep to
  create a partially crossed simulation design

- fractional:

  a fractional design matrix returned from the `FrF2` package. Note that
  the order of the factor names/labels are associated with the
  respective `...` inputs

- tibble:

  logical; return a `tibble` object instead of a `data.frame`? Default
  is TRUE

- stringsAsFactors:

  logical; should character variable inputs be coerced to factors when
  building a `data.frame`? Default is FALSE

- fully.crossed:

  logical; create a fully-crossed design object? Setting to `FALSE` will
  attempt to combine the design elements column-wise via
  `data.frame(...)` instead of `expand.grid(...)`

- x:

  object of class `'Design'`

- list2char:

  logical; for `tibble` object re-evaluate list elements as character
  vectors for better printing of the levels? Note that this does not
  change the original classes of the object, just how they are printed.
  Default is TRUE

- pillar.sigfig:

  number of significant digits to print. Default is 5

- show.IDs:

  logical; print the internally stored Design ID indicators?

- i:

  row index

- j:

  column index

- drop:

  logical; drop to lower dimension class?

- keep.IDs:

  logical; keep the internal ID variables in the `Design` objects? Use
  this when row-binding conditions that are matched with previous
  conditions (e.g., when using
  [`expandDesign`](http://philchalmers.github.io/SimDesign/reference/expandDesign.md))

## Value

a `tibble` or `data.frame` containing the simulation experiment
conditions to be evaluated in
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

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

[`expandDesign`](http://philchalmers.github.io/SimDesign/reference/expandDesign.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

# modified example from runSimulation()

Design <- createDesign(N = c(10, 20),
                       SD = c(1, 2))
Design

# remove N=10, SD=2 row from initial definition
Design <- createDesign(N = c(10, 20),
                       SD = c(1, 2),
                       subset = !(N == 10 & SD == 2))
Design

# example with list inputs
Design <- createDesign(N = c(10, 20),
                       SD = c(1, 2),
                       combo = list(c(0,0), c(0,0,1)))
Design   # notice levels printed (not typical for tibble)
print(Design, list2char = FALSE)   # standard tibble output

Design <- createDesign(N = c(10, 20),
                       SD = c(1, 2),
                       combo = list(c(0,0), c(0,0,1)),
                       combo2 = list(c(5,10,5), c(6,7)))
Design
print(Design, list2char = FALSE)   # standard tibble output

# design without crossing (inputs taken-as is)
Design <- createDesign(N = c(10, 20),
                       SD = c(1, 2), cross=FALSE)
Design   # only 2 rows

##########

## fractional factorial example

library(FrF2)
# help(FrF2)

# 7 factors in 32 runs
fr <- FrF2(32,7)
dim(fr)
fr[1:6,]

# Create working simulation design given -1/1 combinations
fDesign <- createDesign(sample_size=c(100,200),
                        mean_diff=c(.25, 1, 2),
                        variance.ratio=c(1,4, 8),
                        equal_size=c(TRUE, FALSE),
                        dists=c('norm', 'skew'),
                        same_dists=c(TRUE, FALSE),
                        symmetric=c(TRUE, FALSE),
                        # remove same-normal combo
                        subset = !(symmetric & dists == 'norm'),
                        fractional=fr)
fDesign

} # }
```
