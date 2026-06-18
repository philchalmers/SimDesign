# Check whether package versions are as expected

Check if a specific version of a package is installed, and throw an
error if not what was anticipated. Particularly useful when submitting
jobs to cluster compute nodes where package versions may be been updated
inadvertently.

## Usage

``` r
CheckPackages(...)
```

## Arguments

- ...:

  character vectors indicating package and version to check, of the form
  `"package OPERATOR version"`, where `"OPERATOR"` is one of R's logical
  expressions (e.g., `"dplyr == 1.2.1"`). Can contain one or more
  expressions to evaluate

## Value

invisible return of the package names as a character vector

## Examples

``` r

if (FALSE) { # \dontrun{

  CheckPackages('dplyr == 1.2.1') # fails if not exact version
  CheckPackages('dplyr <= 1.2.1') # fails if not equal to or less than
  CheckPackages('dplyr >= 1.2.1') # allows specific version or higher

  CheckPackages('dplyr <= 1.2.1', 'mirt >= 1.45.1')  # multiple checks

} # }
```
