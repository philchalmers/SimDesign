# Form Column Standard Deviation and Variances

Form column standard deviation and variances for numeric arrays (or data
frames).

## Usage

``` r
colVars(x, na.rm = FALSE, unname = FALSE)

colSDs(x, na.rm = FALSE, unname = FALSE)
```

## Arguments

- x:

  an array of two dimensions containing numeric, complex, integer or
  logical values, or a numeric data frame

- na.rm:

  logical; remove missing values in each respective column?

- unname:

  logical; apply [`unname`](https://rdrr.io/r/base/unname.html) to the
  results to remove any variable names?

## See also

[`colMeans`](https://rdrr.io/r/base/colSums.html)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
results <- matrix(rnorm(100), ncol=4)
colnames(results) <- paste0('stat', 1:4)

colVars(results)
#>     stat1     stat2     stat3     stat4 
#> 0.7607727 0.4891335 0.7170886 1.3992792 
colSDs(results)
#>     stat1     stat2     stat3     stat4 
#> 0.8722229 0.6993808 0.8468108 1.1829113 

results[1,1] <- NA
colSDs(results)
#>     stat1     stat2     stat3     stat4 
#>        NA 0.6993808 0.8468108 1.1829113 
colSDs(results, na.rm=TRUE)
#>     stat1     stat2     stat3     stat4 
#> 0.8855740 0.6993808 0.8468108 1.1829113 
colSDs(results, na.rm=TRUE, unname=TRUE)
#> [1] 0.8855740 0.6993808 0.8468108 1.1829113
```
