# Rejection sampling (i.e., accept-reject method)

This function supports the rejection sampling (i.e., accept-reject)
approach to drawing values from seemingly difficult (probability)
density functions by sampling values from more manageable proxy
distributions.

## Usage

``` r
rejectionSampling(
  n,
  df,
  dg,
  rg,
  M,
  method = "optimize",
  interval = NULL,
  logfuns = FALSE,
  maxM = 1e+05,
  parstart = rg(1L),
  ESRS_Mstart = 1.0001
)
```

## Arguments

- n:

  number of samples to draw

- df:

  the desired (potentially un-normed) density function to draw
  independent samples from. Must be in the form of a `function` with a
  single input corresponding to the values sampled from `rg`. Function
  is assumed to be vectorized (if not, see
  [`Vectorize`](https://rdrr.io/r/base/Vectorize.html))

- dg:

  the proxy (potentially un-normed) density function to draw samples
  from in lieu of drawing samples from `df`. The support for this
  density function should be the same as `df` (i.e., when `df(x) > 0`
  then `dg(x) > 0`). Must be in the form of a `function` with a single
  input corresponding to the values sampled from `rg`. Function is
  assumed to be vectorized (if not, see
  [`Vectorize`](https://rdrr.io/r/base/Vectorize.html))

- rg:

  the proxy random number generation function, associated with `dg`,
  used to draw proposal samples from. Must be in the form of a
  `function` with a single input corresponding to the number of values
  to draw, while the output can either be a vector or a matrix (if a
  matrix, each independent observation must be stored in a unique row).
  Function is assumed to be vectorized (if not, see
  [`Vectorize`](https://rdrr.io/r/base/Vectorize.html))

- M:

  the upper-bound of the ratio of probability density functions to help
  minimize the number of discarded draws and define the corresponding
  rescaled proposal envelope. When missing, `M` is computed internally
  by finding a reasonable maximum of `log(df(x)) - log(dg(x))`, and this
  value is returned to the console. When both `df` and `dg` are true
  probability density functions (i.e., integrate to 1) the acceptance
  probability is equal to 1/M

- method:

  when M is missing, the optimization of M is done either by finding the
  mode of the log-density values (`"optimize"`) or by using the
  "Empirical Supremum Rejection Sampling" method (`"ESRS"`)

- interval:

  when M is missing, for univariate density function draws, the interval
  to search within via
  [`optimize`](https://rdrr.io/r/stats/optimize.html). If not specified,
  a sample of 5000 values from the `rg` function definition will be
  collected, and the min/max will be obtained via this random sample

- logfuns:

  logical; have the `df` and `dg` function been written so as to return
  log-densities instead of the original densities? The FALSE default
  assumes the original densities are returned (use TRUE when higher
  accuracy is required when generating each density definition)

- maxM:

  logical; if when optimizing M the value is greater than this cut-off
  then stop; ampler would likelihood be too efficient, or optimization
  is failing

- parstart:

  starting value vector for optimization of M in multidimensional
  distributions

- ESRS_Mstart:

  starting M value for the ESRS algorithm

## Value

returns a vector or matrix of draws (corresponding to the output class
from `rg`) from the desired `df`

## Details

The accept-reject algorithm is a flexible approach to obtaining i.i.d.'s
from a difficult to sample from (probability) density function where
either the transformation method fails or inverse transform method is
difficult to manage. The algorithm does so by sampling from a more
"well-behaved" proxy distribution (with identical support, up to some
proportionality constant `M` that reshapes the proposal density to
envelope the target density), and accepts the draws if they are likely
within the target density. Hence, the closer the shape of `dg(x)` is to
the desired `df(x)`, the more likely the draws are to be accepted;
otherwise, many iterations of the accept-reject algorithm may be
required, which decreases the computational efficiency.

## References

Caffo, B. S., Booth, J. G., and Davison, A. C. (2002). Empirical
supremum rejection sampling. `Biometrika`, 89, 745–754.

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

# Generate X ~ beta(a,b), where a and b are a = 2.7 and b = 6.3,
# and the support is Y ~ Unif(0,1)
dfn <- function(x) dbeta(x, shape1 = 2.7, shape2 = 6.3)
dgn <- function(x) dunif(x, min = 0, max = 1)
rgn <- function(n) runif(n, min = 0, max = 1)

# when df and dg both integrate to 1, acceptance probability = 1/M
M <- rejectionSampling(df=dfn, dg=dgn, rg=rgn)
M
dat <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, M=M)
hist(dat, 100)
hist(rbeta(10000, 2.7, 6.3), 100) # compare

# obtain empirical estimate of M via ESRS method
M <- rejectionSampling(1000, df=dfn, dg=dgn, rg=rgn, method='ESRS')
M

# generate using better support function (here, Y ~ beta(2,6)),
#   and use log setup in initial calls (more numerically accurate)
dfn <- function(x) dbeta(x, shape1 = 2.7, shape2 = 6.3, log = TRUE)
dgn <- function(x) dbeta(x, shape1 = 2, shape2 = 6, log = TRUE)
rgn <- function(n) rbeta(n, shape1 = 2, shape2 = 6)
M <- rejectionSampling(df=dfn, dg=dgn, rg=rgn, logfuns=TRUE) # better M
M

## Alternative estimation of M
## M <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, logfuns=TRUE,
##                        method='ESRS')
dat <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, M=M, logfuns=TRUE)
hist(dat, 100)

#------------------------------------------------------
# sample from wonky (and non-normalized) density function, like below
dfn <- function(x){
    ret <- numeric(length(x))
    ret[x <= .5] <- dnorm(x[x <= .5])
    ret[x > .5] <-  dnorm(x[x > .5]) + dchisq(x[x > .5], df = 2)
    ret
}
y <- seq(-5,5, length.out = 1000)
plot(y, dfn(y), type = 'l', main = "Function to sample from")

# choose dg/rg functions that have support within the range [-inf, inf]
rgn <- function(n) rnorm(n, sd=4)
dgn <- function(x) dnorm(x, sd=4)

## example M height from above graphic
##  (M selected using ESRS to help stochastically avoid local mins)
M <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, method='ESRS')
M
lines(y, dgn(y)*M, lty = 2)
dat <- rejectionSampling(10000, df=dfn, dg=dgn, rg=rgn, M=M)
hist(dat, 100, prob=TRUE)

# true density (normalized)
C <- integrate(dfn, -Inf, Inf)$value
ndfn <- function(x) dfn(x) / C
curve(ndfn, col='red', lwd=2, add=TRUE)


#-----------------------------------------------------
# multivariate distribution
dfn <- function(x) sum(log(c(dnorm(x[1]) + dchisq(x[1], df = 5),
                   dnorm(x[2], -1, 2))))
rgn <- function(n) c(rnorm(n, sd=3), rnorm(n, sd=3))
dgn <- function(x) sum(log(c(dnorm(x[1], sd=3), dnorm(x[1], sd=3))))

# M <- rejectionSampling(df=dfn, dg=dgn, rg=rgn, logfuns=TRUE)
dat <- rejectionSampling(5000, df=dfn, dg=dgn, rg=rgn, M=4.6, logfuns=TRUE)
hist(dat[,1], 30)
hist(dat[,2], 30)
plot(dat)


} # }
```
