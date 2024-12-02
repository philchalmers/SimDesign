# NEWS file for SimDesign

## Changes in SimDesign 2.18

- `SimResults()` now gives the same output behavior when `store_results` or 
  `save_results` are used (see issue #45)

- Use of `SimSolve(..., wait.time)` now automatically sets the `maxiter` to 
  3000 to avoid early terminations

## Changes in SimDesign 2.17.1

- `runArraySimulation()` now correctly searches in `.GlobalEnv` for user
  defined functions

- `manageWarnings(... suppress)` argument now allows for partial matching 
  and other regex inputs

- `SimCollect()` now automatically checks whether all files are expected to 
  be present via `SimCheck()`

- `runArraySimulation()` gains a `array2row` function to allow array jobs
  to index multiple conditions in the `design` object (default uses one 
  `arrayID` per row, the original behaviour)

- `runArraySimulation()` gains `parallel` flag and friends to use multi-core
  processing within array distributions. RNG numbers within the L'Ecuyer-CMRG
  algorithm are incremented using `parallel::nextRNGSubStream()` within each 
  defined core

- Better name checking when using the supported `list` inputs in `runSimulation()`
  and `runArraySimulation()`
  
- `SimCollect()` more efficient when combining a large number of files (e.g.,
  greater than 5000 `.rds` files stored via `runArraySimulation()`). Gains a 
  `dir` argument for this purpose as well so that a full directory can be specified
  
- `SimCheck()` repurposed to check for missing files for `runArraySimulation()` 

## Changes in SimDesign 2.16

- Fix for `SimCollect()` when `runArraySimulation()` result contains 
  mixed warning outputs  (reported by Michael Troung)

- `manageMessages()` added in a similar spirit to `manageWarnigns()`, though
  to change messages into either errors or warnings (default behavior is the 
  same as `quiet()`)

- `manageWarnings()` gains an `suppress` argument to specify explicit 
  warnings strings that can be suppressed (i.e., are known to be innocuous).
  This provides better coding practice than the nuclear alternative
  `base::suppressWarnings()`
  
- `convertWarnings()` name changed to `manageWarning()` given its 
  increased functionality. 

- `timeFormater()` function added to isolate logic of SBATCH time specification
  utility. Now used in several places of the package (e.g., 
  `runArraySimulation()`, `PBA()`, `SimSolve()`)

- Switch to camel casing format in all functions (e.g., 
  `add_missing() -> addMissing()`, `gen_seeds() -> genSeeds()`, etc). Exception
  is that `aggregate_simulations()` has changed to `SimCollect()`
  
- `SimSolve()` gains a `predCI.tol` argument to allow algorithm termination based
  advertised precision of the estimates

## Changes in SimDesign 2.15.1

- `runSimulation(..., control = list(store_Random.seeds))` logical added to 
  store all `.Random.seed` replication states. Generally not recommended due 
  to the size of these stored elements in larger simulations, however can be
  useful for debugging purposes where errors or warnings are not thrown

- `runArraySimulation()` added to better support distributing array's of jobs
  on HPC clusters. Works best when combined with new `expandDesign()` 
  function (see next point) and the improved `aggregate_simulations()` behaviour 
  for more evenly distribution replication budgets across independent jobs. 
  An associated vignette file has been added to the package to provide 
  context and tutorial information for Slurm clusters
  
- `expandDesign()` added to repeat the row conditions a number of 
  times instead of just once. This is useful when
  exporting each condition independently to computing clusters, where each cluster
  contains only a fraction of the target `replications` (see issue #33)
  
- `getArrayID()` added to detect the array job ID (used 
  with `runArraySimulation(..., arrayID)`)

- `aggregate_simulations()` now requires explicit `filename` 
  argument to save the collapsed simulation information
  
- `aggregate_simulations()` generalized to detect whether the `Design` conditions
  have repeated row definitions and therefore should be conditionally averaged
  over (see new `expandDesign()` function)
  
- `runArraySimulation()` and `runSimulation()`'s `control` list gain 
  new `max_time` and `max_RAM` arguments to evaluate simulation 
  replications up until this time 
  or RAM storage constraint is reached. In the event that the target 
  replications are not reached the simulations up to this point, or the max 
  RAM storage has been reached, then on the partial results will be returned 
  (with a warning). This is mainly useful for HPC cluster jobs that require time 
  and RAM constraints (e.g., 4 days per job; 4GB of RAM), 
  where some jobs or simulation conditions may be more time/RAM consuming than 
  others (requested by Mikko Rönkkö)
  
- Expose seed generation control per simulation condition via the 
  function `gen_seeds()`, which also automatically constructs proper 
  L'Ecuyer-CMRG seeds to be distributed across the `runArraySimulation()` jobs 

## Changes in SimDesign 2.14

- `SimSolve()` function added to perform (stochastic) root-solving to estimate 
  specific criteria from simulation studies. Currently supports uni-root type 
  problems for continuous or discrete variables via the probabilistic 
  bisection algorithm with bolstering and interpolations (ProBABLI), 
  Brent's method, and the classical bisection approach, the latter two of which
  can be problematic if the number of replications per iteration are too low
  
- `SFA()` function added for fitting surrogate functional forms to 
  simulation results and subsequently solving specific roots. Supports single
  root or multi-root applications, where by default the modelling is performed 
  via generalized linear models

- `runSimulation(..., store_results = TRUE)` is now the default to automatically
  store the results from `Analyse()` in the returned simulation object. 
  If RAM issues are suspected then `save_results = TRUE` is still 
  the recommended approach

- `convertWarnings()` wrapper/post-hoc function added to convert specific 
  warning messages to errors during simulations. Useful when only a subset
  of warnings are known to be problematic, while other warning messages
  (whether known or not) are treated as provisionally innocuous

- `control` gains a `print_RAM` logical argument to suppress printing the RAM
  when `verbose = TRUE`. Disabling this can reduce execution time
  as garbage collector (`gc()`) calls are avoided, which is required extract
  the current RAM state. Setting `verbose = FALSE` will also automatically 
  disable the RAM and `gc()` calls and their overhead
  
- `Attach()` now accepts `matrix` input objects, and gains a `RStudio_flags` 
  argument to generate syntax that suppresses false positives about variables 
  outside of the function's scope


## Changes in SimDesign 2.13

- Fix Github issue #26 related to extremely long warning/error messages 

- `save_results_filename` added to `runSimulation()` saving details to allow
  asynchronous (though unchecked) file storage to the same results 
  directory (suggested by Jan Göttmann)

- `ECR()` gain a `complement` logical to indicate whether parameter was 
  outside advertised interval (complement of coverage). Useful when CIs are
  used as formal hypothesis tests (e.g., bootstrap CI tests for power)

- `runSimulation(..., extra_options)` changed to `control` instead to
  control less commonly used flags

## Changes in SimDesign 2.12

- `createDesign()` gains a `fractional` argument to support design 
  input structures from the `FrF2` package for fractional factorial designs. 
  Useful when detecting main/low-dimensional interaction effects 
  across a large number of factor variables (suggested by Achim Zeileis). 
  Example added to the wiki to demonstrate its use
  
- When `summarise()` function not supplied the `Design` input 
  is now appended to the `results` object when 
  using `SimExtract(res, what = 'results'`). Only supported when the `results`
  object is a `matrix`-like structure
  
- `RAM` element added to resulting objects to indicate the amount of RAM used
  during each evaluation. This is particularly useful when using
  `runSimulation(..., store_results = TRUE)` to inspect how much RAM is being 
  being consumed (otherwise, `runSimulation(..., save_results = TRUE)` should be
  used if RAM storage is suspected to be an issue)
  
- `resummarise()` and `aggregate_simulation()` now better support 
  the internally stored results terms when using `store_results = TRUE`
  
- `runSimulation(..., save = TRUE)` changed to `save = replications > 10` 
  to only write temporary files when the replications are larger (less 
  hard-drive strain when initially testing simulation experiment with very 
  small replications)

- hexsticker added to make `SimDesign` part of the cool-kids club

- `filename` and `save_results_dirname` extractors added to `SimExtract()`

## Changes in SimDesign 2.11

- `PBA()` function added for probabilistic bisection algorithm, with associated
  `print()` and `plot()` S3 methods

- `debug` gains `'-'` structure to allow debugging on specific
  rows of the `design` input. For instance, if the simulation 
  ran successfully until row 10, and unknown errors terminated the simulation, 
  then using `runSimulation(..., debug = 'error-10')` 
  will initiate the debugger on the first instance for the 10th row 
  conditions in the supplied `design` object
  
- Progress reporting now includes abbreviated condition names and values
  in the console per condition
  
- New function `nc()` to be used in situation where uniquely naming a vector or list
  according to the object names is useful (cf. `x <- c(A,B,C)`, which typically
  returns an unnamed vector, to `x <- nc(A,B,C)`, in which `names(x)` is `"A" "B" "C"`).
  This is mainly useful in the `Analyse()` step where objects must be named uniquely
  in order to track the results in `Summarise()`
  
- Added `Bradley1978()` for test of Bradley's (1978) robustness interval for 
  empirical detection/coverage rate statistics
  
- `runSimulation(..., Generate)` can now be specified as a named list of functions
  similar to `Analyse()`, however only the first valid data generation function
  will be used as the constructor of the simulated data (see the new `GenerateIf()` 
  function to control the flow of these generation steps). This list input should
  really only be used when the population generation functions are differ widely
  depending on the `condition` under investigation
  
- `SimFunctions()` adds a few new inputs for saving one or more files (`save_structure`),
  defining one or more generate function (`nGenerate`), whether to include an extra
  file for user-defined objects and functions (`extra_file`), and whether a basic
  `knitr::spin()` header should be included when saving the files (`spin_header`)

## Changes in SimDesign 2.10.1

- Support the `future` package by using `runSimulation(..., parallel = 'future')` 
  to replace the built-in parallel processing inputs. Using the `future` package 
  approach makes several arguments to `runSimulation()` unnecessary as these 
  can be specified when defining `future::plan()` (e.g., `cl`, `MPI`, etc)
  
- When using the `future` approach the `progressr` package is used. 
  Allows the progress bar to be started via `progressr::with_progress()` and 
  modified by the front-end user (see `?runSimulatino` for an example 
  using `progressr::handler()`)

## Changes in SimDesign 2.9.1

- `extra_options` gains support for `.options.mpi` to control the MPI properties
  documented in `doMPI`
- `quite()` now removes the sunk connection temp file to save storage
  issues (e.g., when distributing on Slurm)
- `Attach()` gains an `omit` argument to omit specific elements from being attached
  to the working environment (default still attaches all objects supplied)

## Changes in SimDesign 2.8

- Using a list definition for `Analyse` input now executes all functions by default
  regardless of errors thrown. Error messages and seeds remain captured in the output,
  however are labelled according to the number of errors that were observed (e.g.,
  `SimExtra(result, what = 'errors')` may return column with 
  `"ERROR:  2 INDEPENDENT ERRORS THROWN: ..."`). Previous early termination default 
  can be reset by passing `extra_options = list(try_all_analyse = FALSE)` to `runSimulation()`.
  Special thanks to Mark Lai for bringing this to my attention on Issue #20

- Added `beep` argument to `runSimulation()` to play a beep message via the `beepr`

## Changes in SimDesign 2.7.1

- Added `RSE()` function to compute the relative behaviour of the average standard error to the 
  standard deviation of a set of parameter estimates across the replications 
  (`RSE = E(par_SEs) / SD(par_ests)`)

- Bugfix for new list input for analysis functions when error raised (reported by Mark Lai)

## Changes in SimDesign 2.7

- `SimExtract()` gains a `fuzzy` argument to allow fuzzy matching of error and warning messages.
  This helps collapse very similar errors messages in the recorded tables,
  thereby improving how to discern any pattern in the errors/warnings (e.g., Messages such as 
  *"ERROR: system is computationally singular: reciprocal condition number = 9.63735e-18"* and 
  *"ERROR: system is computationally singular: reciprocal condition number = 6.74615e-17"* are 
  effectively the same, and so their number of recorded occurrences should be collapsed)

- Added `AnalyseIf()` function to allow specific analysis function to be included explicitly. Useful
  when the defined analysis function is not compatible with a row-condition in the `Design` object. 
  Only relevant when the `analyse` argument was defined as a named list of functions

- The `analyse` argument to `runSimulation()` now accepts a named `list` of functions rather than a 
  single analysis function. This allows the user to separate the independent analyses into distinct
  functional blocks rather than having all analyses within the same function, and potentially allows
  for better modularity. The `debug` argument now also accepts the names of these respective
  list elements to debug these function definitions quickly
  
- `SimFunctions()` gains an `nAnalyses` argument to specify how many analysis functions should 
  be templated (default is 1, retaining the previous package defaults)

## Changes in SimDesign 2.6

- Various performance improvements to reduce execution overhead (e.g., `REPLICATION` ID now moved
  to an `extra_option` as this was identified as a bottleneck)

- Meta-statistical functions now support a `fun(list, matrix)` input form to compute element-wise
  summaries that return a `matrix` structure
  
- `Summarise()` can now return `list` arguments that can later be extracted 
  via `SimExtract(sim, what = 'summarise')`. Consequently, because list outputs are now 
  viable the `purrr` package has been added to the `suggests` list

## Changes in SimDesign 2.5

- Prevent `aggregate_simulations()` from overwriting files and directories accidentally. As well,
  the auto-detection of suitable .rds files has been removed as explicitly stating the files/directories
  to be aggregated is less error prone

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
