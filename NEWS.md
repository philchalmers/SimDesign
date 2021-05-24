# NEWS file for SimDesign

## Changes in SimDesign 2.4

- Prevent `aggregate_simulations()` from overwriting files and directories accidentally

- Removed `plyr::rbind.fill` in favour of `dplyr::bind_row()`, which removed `plyr` as a dependency

- `Attach()` now accepts multiple list-like objects as inputs

- Added `SimCheck()` for checking the state of a long-running simulation via inspecting the main temp file

- `sessioninfo` package used in placed of the traditional `sessionInfo()`

- Print number of cores when parallel processing is in use

- A number of arguments from `runSimulation()` moved into `extra_options` list argument to simplify documentation

- Parallel processing now uses FORK instead of PSOCK when on Unix machines by default

- More natural use of `RPushbullet` by changing the `notification` input into one that accepts a character vector ("none", "condition", "complete") to send `pbPost()` call. Also more informative in the default messages sent

## Changes in SimDesign 2.0+

- Added "Empirical Supremum Rejection Sampling" method to `rejectionSampling()` to find better constant **M** (useful when there are local minimums in the `f(x)/g(x)` ratio)

- `rejectionSampling()` made more general, with additional examples provided in the help files

- Bootstrap CI estimates moved into `runSimulation()`, deprecating the less optimal `SimBoot()`

- `runSimulation(..., save=TRUE)` now default to always store meta-information about the simulation state

- Added `renv` to the suggests lists since it's useful to hard-store package versions used in simulations

- `data.frame` objects largely replaced with `tibble` data frames instead as they render better for larger simulations

- Support for `rbind()` and `cbind()` on final simulation results to add additional condition/meta-summary information

- Use `createDesign()` instead of `expand.grid()` in code, which provides more structured information and flexibility

- Added `SimExtract()` to extract important but silent information

- Added `stop_on_fatal` logical argument to more aggressively terminate the simulation rather than do things more gracefully
