# Compute the relative standard error ratio

Computes the relative standard error ratio given the set of estimated
standard errors (SE) and the deviation across the R simulation
replications (SD). The ratio is formed by finding the expectation of the
SE terms, and compares this expectation to the general variability of
their respective parameter estimates across the R replications (ratio
should equal 1). This is used to roughly evaluate whether the SEs being
advertised by a given estimation method matches the sampling variability
of the respective estimates across samples.

## Usage

``` r
RSE(SE, ests, unname = FALSE)
```

## Arguments

- SE:

  a `numeric` matrix of SE estimates across the replications (extracted
  from the `results` object in the Summarise step). Alternatively, can
  be a vector containing the mean of the SE estimates across the R
  simulation replications

- ests:

  a `numeric` matrix object containing the parameter estimates under
  investigation found within the
  [`Summarise`](http://philchalmers.github.io/SimDesign/reference/Summarise.md)
  function. This input is used to compute the standard
  deviation/variance estimates for each column to evaluate how well the
  expected SE matches the standard deviation

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## Value

returns vector of variance ratios, (RSV = SE^2/SD^2)

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
R <- 10000
par_ests <- cbind(rnorm(R), rnorm(R, sd=1/10),
                  rnorm(R, sd=1/15))
colnames(par_ests) <- paste0("par", 1:3)
(SDs <- colSDs(par_ests))
#>       par1       par2       par3 
#> 1.00319636 0.09950705 0.06681960 

SEs <- cbind(1 + rnorm(R, sd=.01),
             1/10 + + rnorm(R, sd=.01),
             1/15 + rnorm(R, sd=.01))
(E_SEs <- colMeans(SEs))
#> [1] 1.00018127 0.10001882 0.06673368
RSE(SEs, par_ests)
#>      par1      par2      par3 
#> 0.9969945 1.0051431 0.9987141 

# equivalent to the form
colMeans(SEs) / SDs
#>      par1      par2      par3 
#> 0.9969945 1.0051431 0.9987141 

```
