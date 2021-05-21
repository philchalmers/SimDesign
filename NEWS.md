# NEWS file for SimDesign

## Changes in SimDesign 2.4

- Removed `plyr::rbind.fill` in favour of `dplyr::bind_row()`, which removed `plyr` as a dependency

- `Attach()` now accepts multiple list-like objects as inputs

- Added `SimCheck()` for checking the state of a long-running simulation via inspecting the main temp file

- `sessioninfo` package used in placed of the traditional `sessionInfo()`

- Print number of cores in use when parallel processing is in use

- A number of arguments from `runSimulation()` moved into `extra_options` list argument to simplify documentation

- Parallel processing now uses FORK instead of PSOCK when on Unix machines by default

- More natural use of `RPushbullet` by changing the `notification` input into one that accepts a character vector ("none", "condition", "complete") to send `pbPost()` call. Also more informative in the default messages sent
