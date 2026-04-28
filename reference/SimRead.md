# Read simulation files

Convenience function that switches between
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) and
[`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html) for
functions saved with `SimDesign`. By convention, objects saved with the
extension `.rds` are read as R binary files and typically reflect the
final object from
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
or
[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md),
while files without an extension are read using `qs2` (most often
temporary files or results written to an associated sub-directory).

## Usage

``` r
SimRead(filename)
```

## Arguments

- filename:

  name of the file to read in

## Value

the R binary object
