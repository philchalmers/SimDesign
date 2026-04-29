# Generate integer values within specified range

Efficiently generate positive and negative integer values with (default)
or without replacement. This function is mainly a wrapper to the
[`sample.int`](https://rdrr.io/r/base/sample.html) function (which
itself is much more efficient integer sampler than the more general
[`sample`](https://rdrr.io/r/base/sample.html)), however is intended to
work with both positive and negative integer ranges since `sample.int`
only returns positive integer values that must begin at `1L`.

## Usage

``` r
rint(n, min, max, replace = TRUE, prob = NULL)
```

## Arguments

- n:

  number of samples to draw

- min:

  lower limit of the distribution. Must be finite

- max:

  upper limit of the distribution. Must be finite

- replace:

  should sampling be with replacement?

- prob:

  a vector of probability weights for obtaining the elements of the
  vector being sampled

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
set.seed(1)

# sample 1000 integer values within 20 to 100
x <- rint(1000, min = 20, max = 100)
summary(x)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   20.00   40.00   59.00   59.55   79.25  100.00 

# sample 1000 integer values within 100 to 10 billion
x <- rint(1000, min = 100, max = 1e8)
summary(x)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>     2499 25538779 48425070 49782329 75851586 99994040 

# compare speed to sample()
system.time(x <- rint(1000, min = 100, max = 1e8))
#>    user  system elapsed 
#>       0       0       0 
system.time(x2 <- sample(100:1e8, 1000, replace = TRUE))
#>    user  system elapsed 
#>       0       0       0 

# sample 1000 integer values within -20 to 20
x <- rint(1000, min = -20, max = 20)
summary(x)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -20.000 -10.250   0.000   0.067  11.000  20.000 
```
