# Compute prediction estimates for the replication size using bootstrap MSE estimates

This function computes bootstrap mean-square error estimates to
approximate the sampling behavior of the meta-statistics in SimDesign's
`summarise` functions. A single design condition is supplied, and a
simulation with `max(Rstar)` replications is performed whereby the
generate-analyse results are collected. After obtaining these
replication values, the replications are further drawn from (with
replacement) using the differing sizes in `Rstar` to approximate the
bootstrap MSE behavior given different replication sizes. Finally, given
these bootstrap estimates linear regression models are fitted using the
predictor term `one_sqrtR = 1 / sqrt(Rstar)` to allow extrapolation to
replication sizes not observed in `Rstar`. For more information about
the method and subsequent bootstrap MSE plots, refer to Koehler, Brown,
and Haneuse (2009).

## Usage

``` r
bootPredict(
  condition,
  generate,
  analyse,
  summarise,
  fixed_objects = NULL,
  ...,
  Rstar = seq(100, 500, by = 100),
  boot_draws = 1000
)

boot_predict(...)
```

## Arguments

- condition:

  a `data.frame` consisting of one row from the original `design` input
  object used within
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- generate:

  see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- analyse:

  see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- summarise:

  see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- fixed_objects:

  see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- ...:

  additional arguments to be passed to
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- Rstar:

  a vector containing the size of the bootstrap subsets to obtain.
  Default investigates the vector \[100, 200, 300, 400, 500\] to compute
  the respective MSE terms

- boot_draws:

  number of bootstrap replications to draw. Default is 1000

## Value

returns a list of linear model objects (via
[`lm`](https://rdrr.io/r/stats/lm.html)) for each meta-statistics
returned by the `summarise()` function

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Koehler, E., Brown, E., & Haneuse, S. J.-P. A. (2009). On the Assessment
of Monte Carlo Error in Simulation-Based Statistical Analyses. *The
American Statistician, 63*, 155-162.

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
set.seed(4321)
Design <- createDesign(sigma = c(1, 2))

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects) {
    dat <- rnorm(100, 0, condition$sigma)
    dat
}

Analyse <- function(condition, dat, fixed_objects) {
    CIs <- t.test(dat)$conf.int
    names(CIs) <- c('lower', 'upper')
    ret <- c(mean = mean(dat), CIs)
    ret
}

Summarise <- function(condition, results, fixed_objects) {
    ret <- c(mu_bias = bias(results[,"mean"], 0),
             mu_coverage = ECR(results[,c("lower", "upper")], parameter = 0))
    ret
}

if (FALSE) { # \dontrun{
# boot_predict supports only one condition at a time
out <- bootPredict(condition=Design[1L, , drop=FALSE],
    generate=Generate, analyse=Analyse, summarise=Summarise)
out # list of fitted linear model(s)

# extract first meta-statistic
mu_bias <- out$mu_bias

dat <- model.frame(mu_bias)
print(dat)

# original R metric plot
R <- 1 / dat$one_sqrtR^2
plot(R, dat$MSE, type = 'b', ylab = 'MSE', main = "Replications by MSE")

plot(MSE ~ one_sqrtR, dat, main = "Bootstrap prediction plot", xlim = c(0, max(one_sqrtR)),
     ylim = c(0, max(MSE)), ylab = 'MSE', xlab = expression(1/sqrt(R)))
beta <- coef(mu_bias)
abline(a = 0, b = beta, lty = 2, col='red')

# what is the replication value when x-axis = .02? What's its associated expected MSE?
1 / .02^2 # number of replications
predict(mu_bias, data.frame(one_sqrtR = .02)) # y-axis value

# approximately how many replications to obtain MSE = .001?
(beta / .001)^2
} # }
```
