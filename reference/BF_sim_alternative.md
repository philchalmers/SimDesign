# (Alternative) Example simulation from Brown and Forsythe (1974)

Example results from the Brown and Forsythe (1974) article on robust
estimators for variance ratio tests. Statistical tests and distributions
are organized by columns and the unique design conditions are organized
by rows. See
[`BF_sim`](http://philchalmers.github.io/SimDesign/reference/BF_sim.md)
for an alternative form of the same simulation where distributions are
also included in the rows. Code for this simulation is available on the
wiki (<https://github.com/philchalmers/SimDesign/wiki>).

## References

Brown, M. B. and Forsythe, A. B. (1974). Robust tests for the equality
of variances. *Journal of the American Statistical Association,
69*(346), 364–367.

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
if (FALSE) { # \dontrun{
data(BF_sim_alternative)
head(BF_sim_alternative)

#' #Type I errors
subset(BF_sim_alternative, var_ratio == 1)
} # }
```
