# Check for missing files in array simulations

Given the saved files from a
[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)
remote evaluation check whether all `.rds` files have been saved. If
missing the missing row condition numbers will be returned.

## Usage

``` r
SimCheck(dir = NULL, files = NULL, min = 1L, max = NULL)
```

## Arguments

- dir:

  character vector input indicating the directory containing the `.rds`
  files (see `files`)

- files:

  vector of file names referring to the saved simulation files. E.g.
  `c('mysim-1.rds', 'mysim-2.rds', ...)`

- min:

  minimum number after the `'-'` deliminator. Default is 1

- max:

  maximum number after the `'-'` deliminator. If not specified is
  extracted from the attributes in the first file

## Value

returns an invisible list of indices of empty, missing and
empty-and-missing row conditions. If no missing then an empty list is
returned

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

## See also

[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md),
[`SimCollect`](http://philchalmers.github.io/SimDesign/reference/SimCollect.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

# if files are in mysimfiles/ directory
SimCheck('mysimfiles')

# specifying files explicility
setwd('mysimfiles/')
SimCheck(files=dir())

} # }
```
