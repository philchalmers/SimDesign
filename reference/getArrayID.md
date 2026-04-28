# Get job array ID (e.g., from SLURM or other HPC array distributions)

Get the array ID from an HPC array distribution job (e.g., from SLURM or
from optional command line arguments). The array ID is used to index the
rows in the design object in
[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md).
For instance, a SLURM array with 10 independent jobs might have the
following shell instructions.

## Usage

``` r
getArrayID(type = "slurm", trailingOnly = TRUE, ID.shift = 0L)
```

## Arguments

- type:

  an integer indicating the element from the result of
  [`commandArgs`](https://rdrr.io/r/base/commandArgs.html) to extract,
  or a `character` specifying the the type of. Default is `'slurm'`

- trailingOnly:

  logical value passed to
  [`commandArgs`](https://rdrr.io/r/base/commandArgs.html). Only used
  when `type` is an integer

- ID.shift:

  single integer value used to shift the array ID by a constant. Useful
  when there are array range limitation that must be specified in the
  shell files (e.g., array can only be 10000 but there are more rows in
  the `design` object). For example, if the array ID should be 10000
  through 12000, but the cluster computer enviroment does not allow
  these indices, then including the arrange range as 1-2000 in the shell
  file with `shift=9999` would add this constant to the detected
  arrayID, thereby indexing the remaining row elements in the `design`
  object

## Details

- `#!/bin/bash -l`:
- `#SBATCH --time=00:01:00`:
- `#SBATCH --array=1-10`:

which names the associated jobs with the numbers 1 through 10.
`getArrayID()` then extracts this information per array, which is used
as the `runArraySimulation(design, ..., arrayID = getArrayID())` to pass
specific rows for the `design` object.

## See also

[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)

## Examples
