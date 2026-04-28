# Generate non-normal data with Headrick's (2002) method

Generate multivariate non-normal distributions using the fifth-order
polynomial method described by Headrick (2002).

## Usage

``` r
rHeadrick(
  n,
  mean = rep(0, nrow(sigma)),
  sigma = diag(length(mean)),
  skew = rep(0, nrow(sigma)),
  kurt = rep(0, nrow(sigma)),
  gam3 = NaN,
  gam4 = NaN,
  return_coefs = FALSE,
  coefs = NULL,
  control = list(trace = FALSE, max.ntry = 15, obj.tol = 1e-10, n.valid.sol = 1)
)
```

## Arguments

- n:

  number of samples to draw

- mean:

  a vector of k elements for the mean of the variables

- sigma:

  desired k x k covariance matrix between bivariate non-normal variables

- skew:

  a vector of k elements for the skewness of the variables

- kurt:

  a vector of k elements for the kurtosis of the variables

- gam3:

  (optional) explicitly supply the gamma 3 value? Default computes this
  internally

- gam4:

  (optional) explicitly supply the gamma 4 value? Default computes this
  internally

- return_coefs:

  logical; return the estimated coefficients only? See below regarding
  why this is useful.

- coefs:

  (optional) supply previously estimated coefficients? This is useful
  when there must be multiple data sets drawn and will avoid repetitive
  computations. Must be the object returned after passing
  `return_coefs = TRUE`

- control:

  a list of control parameters when locating the polynomial coefficients

## Details

This function is primarily a wrapper for the code written by Oscar L.
Olvera Astivia (last edited Feb 26, 2015) with some modifications (e.g.,
better starting values for the Newton optimizer, passing previously
saved coefs, etc).

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

Headrick, T. C. (2002). Fast fifth-order polynomial transforms for
generating univariate and multivariate nonnormal distributions.
*Computational Statistics & Data Analysis, 40*, 685-711.

Olvera Astivia, O. L., & Zumbo, B. D. (2015). A Cautionary Note on the
Use of the Vale and Maurelli Method to Generate Multivariate, Nonnormal
Data for Simulation Purposes. *Educational and Psychological
Measurement, 75*, 541-567.

## Author

Oscar L. Olvera Astivia and Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)

N <- 200
mean <- c(rep(0,4))
Sigma <- matrix(.49, 4, 4)
diag(Sigma) <- 1
skewness <- c(rep(1,4))
kurtosis <- c(rep(2,4))

nonnormal <- rHeadrick(N, mean, Sigma, skewness, kurtosis)
cor(nonnormal) |> round(3)
descript(nonnormal)

#-----------
# compute the coefficients, then supply them back to the function to avoid
# extra computations

cfs <- rHeadrick(N, mean, Sigma, skewness, kurtosis, return_coefs = TRUE)
cfs

# compare
system.time(nonnormal <- rHeadrick(N, mean, Sigma, skewness, kurtosis))
system.time(nonnormal <- rHeadrick(N, mean, Sigma, skewness, kurtosis,
                                   coefs=cfs))
} # }
```
