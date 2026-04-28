# Generate a random set of values within a truncated range

Generates data given a supplied random number generating function that
are constructed to fall within a particular range. Sampled values
outside this range are discarded and re-sampled until the desired
criteria has been met.

## Usage

``` r
rtruncate(n, rfun, range, ..., redraws = 100L)
```

## Arguments

- n:

  number of observations to generate. This should be the first argument
  passed to `rfun`

- rfun:

  a function to generate random values. Function can return a
  numeric/integer vector or matrix, and additional arguments requred for
  this function are passed through the argument `...`

- range:

  a numeric vector of length two, where the first element indicates the
  lower bound and the second the upper bound. When values are generated
  outside these two bounds then data are redrawn until the bounded
  criteria is met. When the output of `rfun` is a matrix then this input
  can be specified as a matrix with two rows, where each the first row
  corresponds to the lower bound and the second row the upper bound for
  each generated column in the output

- ...:

  additional arguments to be passed to `rfun`

- redraws:

  the maximum number of redraws to take before terminating the iterative
  sequence. This is in place as a safety in case the `range` is too
  small given the random number generator, causing too many consecutive
  rejections. Default is 100

## Value

either a numeric vector or matrix, where all values are within the
desired `range`

## Details

In simulations it is often useful to draw numbers from truncated
distributions rather than across the full theoretical range. For
instance, sampling parameters within the range \[-4,4\] from a normal
distribution. The `rtruncate` function has been designed to accept any
sampling function, where the first argument is the number of values to
sample, and will draw values iteratively until the number of values
within the specified bound are obtained. In situations where it is
unlikely for the bounds to be located (e.g., sampling from a standard
normal distribution where all values are within \[-10,-6\]) then the
sampling scheme will throw an error if too many re-sampling executions
are required (default will stop if more that 100 calls to `rfun` are
required).

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

[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
# n = 1000 truncated normal vector between [-2,3]
vec <- rtruncate(1000, rnorm, c(-2,3))
summary(vec)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -1.96441 -0.59106  0.06295  0.05083  0.64803  2.94607 

# truncated correlated multivariate normal between [-1,4]
mat <- rtruncate(1000, rmvnorm, c(-1,4),
   sigma = matrix(c(2,1,1,1),2))
summary(mat)
#>        V1                V2         
#>  Min.   :-0.9951   Min.   :-0.9982  
#>  1st Qu.:-0.1587   1st Qu.:-0.1852  
#>  Median : 0.5094   Median : 0.3215  
#>  Mean   : 0.6414   Mean   : 0.4022  
#>  3rd Qu.: 1.3206   3rd Qu.: 0.9156  
#>  Max.   : 3.9608   Max.   : 3.0498  

# truncated correlated multivariate normal between [-1,4] for the
#  first column and [0,3] for the second column
mat <- rtruncate(1000, rmvnorm, cbind(c(-1,4), c(0,3)),
   sigma = matrix(c(2,1,1,1),2))
summary(mat)
#>        V1                 V2          
#>  Min.   :-0.99785   Min.   :0.002002  
#>  1st Qu.: 0.07791   1st Qu.:0.316850  
#>  Median : 0.84390   Median :0.707062  
#>  Mean   : 0.88020   Mean   :0.792454  
#>  3rd Qu.: 1.58988   3rd Qu.:1.147544  
#>  Max.   : 3.89832   Max.   :2.964880  

# truncated chi-square with df = 4 between [2,6]
vec <- rtruncate(1000, rchisq, c(2,6), df = 4)
summary(vec)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.000   2.688   3.500   3.637   4.437   5.999 
```
