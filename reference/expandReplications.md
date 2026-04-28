# Expand the replications to match `expandDesign`

Expands the replication budget to match the
[`expandDesign`](http://philchalmers.github.io/SimDesign/reference/expandDesign.md)
structure.

## Usage

``` r
expandReplications(replications, repeat_conditions)
```

## Arguments

- replications:

  number of replications. Can be a scalar to reflect the same
  replications overall, or a vector of unequal replication budgets.

- repeat_conditions:

  integer vector used to repeat each design row the specified number of
  times. Can either be a single integer, which repeats each row this
  many times, or an integer vector equal to the number of total rows in
  the created object.

## Value

an integer vector of the replication budget matching the expanded
structure in
[`expandDesign`](http://philchalmers.github.io/SimDesign/reference/expandDesign.md)

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

# repeat each row 4 times (for cluster computing)
Design <- createDesign(N = c(10, 20),
                       SD.equal = c(TRUE, FALSE))
Design4 <- expandDesign(Design, 4)
Design4

# match the replication budget. Target is 1000 replications
(replications4 <- expandReplications(1000, 4))

# hence, evaluate each row in Design4 250 times
cbind(Design4, replications4)

####
# Unequal Design intensities

Design24 <- createDesign(SD.equal = c(TRUE, FALSE),
                       N = c(10, 100, 1000))
# split first two conditions into half rows, next two conditions into quarters,
#  while N=1000 condition into tenths
expand <- c(2,2,4,4,10,10)
eDesign <- expandDesign(Design, expand)
eDesign

# target replications is R=1000 per condition
(replications24 <- expandReplications(1000, expand))
cbind(eDesign, replications24)

} # }
```
