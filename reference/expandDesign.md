# Expand the simulation design object for array computing

Repeat each design row the specified number of times. This is primarily
used for cluster computing where jobs are distributed with batches of
replications and later aggregated into a complete simulation object (see
[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)
and
[`SimCollect`](http://philchalmers.github.io/SimDesign/reference/SimCollect.md)).

## Usage

``` r
expandDesign(Design, repeat_conditions)
```

## Arguments

- Design:

  object created by
  [`createDesign`](http://philchalmers.github.io/SimDesign/reference/createDesign.md)
  which should have its rows repeated for optimal HPC schedulers

- repeat_conditions:

  integer vector used to repeat each design row the specified number of
  times. Can either be a single integer, which repeats each row this
  many times, or an integer vector equal to the number of total rows in
  the created object.

  This argument is useful when distributing independent row conditions
  to cluster computing environments, particularly with different
  `replication` information. For example, if 1000 replications in total
  are the target but the condition is repeated over 4 rows then only 250
  replications per row would be required across the repeated conditions.
  See
  [`SimCollect`](http://philchalmers.github.io/SimDesign/reference/SimCollect.md)
  for combining the simulation objects once complete

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

[`expandReplications`](http://philchalmers.github.io/SimDesign/reference/expandReplications.md),
[`createDesign`](http://philchalmers.github.io/SimDesign/reference/createDesign.md),
[`SimCollect`](http://philchalmers.github.io/SimDesign/reference/SimCollect.md),
[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)

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

# repeat first two rows 2x and the rest 4 times (for cluster computing
#   where first two conditions are faster to execute)
Design <- createDesign(SD.equal = c(TRUE, FALSE),
                       N = c(10, 100, 1000))
Design24 <- expandDesign(Design, c(2,2,rep(4, 4)))
Design24

} # }
```
