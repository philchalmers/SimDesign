# One Dimensional Root (Zero) Finding in Simulation Experiments

Provides a stochastic root-finding approach to solve specific quantities
in simulation experiments (e.g., solving for a specific sample size to
meet a target power rate) using the Probablistic Bisection Algorithm
with Bolstering and Interpolations (ProBABLI; Chalmers, 2024). The
structure follows the three functional steps outlined in
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md),
however portions of the `design` input are taken as variables to be
estimated rather than fixed, where an additional constant `b` is
required in order to solve the root equation `f(x) - b = 0`.

## Usage

``` r
SimSolve(
  design,
  interval,
  b,
  generate,
  analyse,
  summarise,
  replications = list(burnin.iter = 15L, burnin.reps = 50L, max.reps = 500L,
    min.total.reps = 9000L, increase.by = 10L),
  integer = TRUE,
  formula = y ~ poly(x, 2),
  family = "binomial",
  parallel = FALSE,
  cl = NULL,
  save = TRUE,
  resume = TRUE,
  method = "ProBABLI",
  wait.time = NULL,
  ncores = parallelly::availableCores(omit = 1L),
  type = ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK"),
  maxiter = 100L,
  check.interval = TRUE,
  predCI = 0.95,
  predCI.tol = NULL,
  lastSolve = NULL,
  verbose = interactive(),
  control = list(),
  ...
)

# S3 method for class 'SimSolve'
summary(object, tab.only = FALSE, reps.cutoff = 300, ...)

# S3 method for class 'SimSolve'
plot(x, y, ...)
```

## Arguments

- design:

  a `tibble` or `data.frame` object containing the Monte Carlo
  simulation conditions to be studied, where each row represents a
  unique condition and each column a factor to be varied (see
  [`createDesign`](http://philchalmers.github.io/SimDesign/reference/createDesign.md)).
  However, exactly one column of this object in each row must be
  specified with `NA` placeholders to indicate that the missing value
  should be estimated via the select stochastic optimizer

- interval:

  a `vector` of length two, or `matrix` with `nrow(design)` rows and two
  columns, containing the end-points of the interval to be searched per
  row condition. If a vector then the interval will be used for all rows
  in the supplied `design` object when passed to the
  [`PBA`](http://philchalmers.github.io/SimDesign/reference/PBA.md)
  engine

- b:

  a single constant used to solve the root equation `f(x) - b = 0`

- generate:

  generate function. See
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- analyse:

  analysis function. See
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- summarise:

  summary function that returns a single number corresponding to the
  function evaluation `f(x)` in the equation `f(x) = b` to be solved as
  a root `f(x) - b = 0`. Unlike in the standard
  [`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  definitions this input is required. For further information on this
  function specification, see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- replications:

  a named `list` or `vector` indicating the number of replication to use
  for each design condition per PBA iteration. By default the input is a
  `list` with the arguments `burnin.iter = 15L`, specifying the number
  of burn-in iterations to used, `burnin.reps = 50L` to indicate how
  many replications to use in each burn-in iteration, `max.reps = 500L`
  to prevent the replications from increasing higher than this number,
  `min.total.reps = 9000L` to avoid termination when very few
  replications have been explored (lower bound of the replication
  budget), and `increase.by = 10L` to indicate how many replications to
  increase per iteration after the burn-in stage. Default can
  overwritten by explicit definition (e.g.,
  `replications = list(increase.by = 25L)`).

  Vector inputs can specify the exact replications for each respective
  iteration. As a general rule, early iterations should be relatively
  low for initial searches to avoid unnecessary computations when
  locating the approximate location of the root, while the number of
  replications should gradually increase after this burn-in to reduce
  the sampling variability.

- integer:

  logical; should the values of the root be considered integer or
  numeric? If `TRUE` then bolstered directional decisions will be made
  in the
  [`PBA`](http://philchalmers.github.io/SimDesign/reference/PBA.md)
  function based on the collected sampling history

- formula:

  regression formula to use when `interpolate = TRUE`. Default fits an
  orthogonal polynomial of degree 2

- family:

  `family` argument passed to [`glm`](https://rdrr.io/r/stats/glm.html).
  By default the `'binomial'` family is used, as this function defaults
  to power analysis setups where isolated results passed to `summarise`
  will return 0/1s, however other families should be used if `summarise`
  returns something else (e.g., if solving for a particular standard
  error then a `'gaussian'` family would be more appropriate).

  Note that if individual results from the `analyse` steps should not be
  used (i.e., only the aggregate from `summarise` is meaningful) then
  set `control = list(summarise.reg_data = TRUE)` to override the
  default behaviour, thereby using only the aggregate information and
  weights

- parallel:

  for parallel computing for slower simulation experiments (see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  for details)

- cl:

  see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- save:

  logical; store temporary file in case of crashes. If detected in the
  working directory will automatically be loaded to resume (see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  for similar behaviour)

- resume:

  logical; if a temporary `SimDesign` file is detected should the
  simulation resume from this location? Keeping this `TRUE` is generally
  recommended, however this should be disabled if using `SimSolve`
  within
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
  to avoid reading improper save states

- method:

  optimizer method to use. Default is the stochastic root-finder
  `'ProBABLI'`, but can also be the deterministic options `'Brent'`
  (which uses the function
  [`uniroot`](https://rdrr.io/r/stats/uniroot.html)) or `'bisection'`
  for the classical bisection method. If using deterministic
  root-finders then `replications` must either equal a single constant
  to reflect the number of replication to use per deterministic
  iteration or be a vector of length `maxiter` to indicate the
  replications to use per iteration

- wait.time:

  (optional) argument passed to
  [`PBA`](http://philchalmers.github.io/SimDesign/reference/PBA.md) to
  indicate the time to wait (specified in minutes if a numeric vector is
  passed) per row in the `Design` object rather than using
  pre-determined termination criteria based on the estimates. For
  example, if three three conditions were defined in `Design`, and
  `wait.time="5"`, then the total search time till terminate after 15
  minutes regardless of independently specified termination criteria in
  `control`. See
  [`timeFormater`](http://philchalmers.github.io/SimDesign/reference/timeFormater.md)
  for alternative specifications

- ncores:

  see
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- type:

  type of cluster (see
  [`makeCluster`](https://rdrr.io/r/parallel/makeCluster.html)) or
  plotting type to use.

  If `type` used in `plot` then can be `'density'` to plot the density
  of the iteration history after the burn-in stage, `'iterations'` for a
  bubble plot with inverse replication weights. If not specified then
  the default PBA plots are provided (see
  [`PBA`](http://philchalmers.github.io/SimDesign/reference/PBA.md))

- maxiter:

  the maximum number of iterations (default 100) except when `wait.time`
  is specified (automatically increased to 3000 to avoid early
  termination)

- check.interval:

  logical; should an initial check be made to determine whether
  `f(interval[1L])` and `f(interval[2L])` have opposite signs? If
  `FALSE`, the specified `interval` is assumed to contain a root, where
  `f(interval[1]) < 0` and `f(interval[2] > 0`. Default is `TRUE`

- predCI:

  advertised confidence interval probability for final model-based
  prediction of target `b` given the root input estimate. Returned as an
  element in the [`summary()`](https://rdrr.io/r/base/summary.html) list
  output

- predCI.tol:

  (optional) rather than relying on the changes between successive
  estimates (default), if the predicting CI is consistently within this
  supplied tolerance range then the search will be terminated. This
  provides termination behaviour based on the predicted precision of the
  root solutions rather than their stability history, and therefore can
  be used to obtain estimates with a particular level of advertised
  accuracy. For example, when solving for a sample size value (`N`) if
  the solution associated with `b = .80` requires that the advertised 95
  is consistently between \[.795, .805\] then `predCI.tol = .01` should
  be used to reflect this tolerance range

- lastSolve:

  stub for `Spower` package; not to be used by front-end users

- verbose:

  logical; print information to the console?

- control:

  a `list` of the algorithm control parameters. If not specified, the
  defaults described below are used.

  `tol`

  :   tolerance criteria for early termination (.1 for `integer = TRUE`
      searches; .00025 for non-integer searches

  `rel.tol`

  :   relative tolerance criteria for early termination (default .0001)

  `k.success`

  :   number of consecutive tolerance successes given `rel.tol` and
      `tol` criteria (default is 3)

  `bolster`

  :   logical; should the PBA evaluations use bolstering based on
      previous evaluations? Default is `TRUE`, though only applicable
      when `integer = TRUE`

  `interpolate.R`

  :   number of replications to collect prior to performing the
      interpolation step (default is 3000 after accounting for data
      exclusion from `burnin.iter`). Setting this to 0 will disable any
      interpolation computations

  `include_reps`

  :   logical; include a column in the `condition` elements to indicate
      how many replications are currently being evaluated? Mainly useful
      when further tuning within each ProBABLI iteration is desirable
      (e.g., for increasing/decreasing bootstrap draws as the search
      progresses). Default is `FALSE`

  `summarise.reg_data`

  :   logical; should the aggregate results from `Summarise` (along with
      its associated weights) be used for the interpolation steps, or
      the raw data from the `Analyse` step? Set this to `TRUE` when the
      individual results from `Analyse` give less meaningful information

- ...:

  additional arguments to be pasted to
  [`PBA`](http://philchalmers.github.io/SimDesign/reference/PBA.md)

- object:

  object of class `'SimSolve'`

- tab.only:

  logical; print only the (reduced) table of estimates?

- reps.cutoff:

  integer indicating the rows to omit from the output if the number of
  replications are less than this value

- x:

  object of class `'SimSolve'`

- y:

  design row to plot. If omitted defaults to 1

## Value

the filled-in `design` object containing the associated lower and upper
interval estimates from the stochastic optimization

## Details

Root finding is performed using a progressively bolstered version of the
probabilistic bisection algorithm
([`PBA`](http://philchalmers.github.io/SimDesign/reference/PBA.md)) to
find the associated root given the noisy simulation objective function
evaluations. Information is collected throughout the search to make more
accurate predictions about the associated root via interpolation. If
interpolations fail, then the last iteration of the PBA search is
returned as the best guess.

For greater advertised accuracy with ProBABLI, termination criteria can
be based on the width of the advertised predicting interval (via
`predCI.tol`) or by specifying how long the investigator is willing to
wait for the final estimates (via `wait.time`, where longer wait times
lead to progressively better accuracy in the final estimates).

## References

Chalmers, R. P. (2024). Solving Variables with Monte Carlo Simulation
Experiments: A Stochastic Root-Solving Approach.
`Psychological Methods`. DOI: 10.1037/met0000689

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

## See also

[`SFA`](http://philchalmers.github.io/SimDesign/reference/SFA.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

##########################
## A Priori Power Analysis
##########################

# GOAL: Find specific sample size in each group for independent t-test
# corresponding to a power rate of .8
#
# For ease of the setup, assume the groups are the same size, and the mean
# difference corresponds to Cohen's d values of .2, .5, and .8
# This example can be solved numerically using the pwr package (see below),
# though the following simulation setup is far more general and can be
# used for any generate-analyse combination of interest

# SimFunctions(SimSolve=TRUE)

#### Step 1 --- Define your conditions under study and create design data.frame.
#### However, use NA placeholder for sample size as it must be solved,
#### and add desired power rate to object

Design <- createDesign(N = NA,
                       d = c(.2, .5, .8),
                       sig.level = .05)
Design    # solve for NA's

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summarise functions

Generate <- function(condition, fixed_objects) {
    Attach(condition)
    group1 <- rnorm(N)
    group2 <- rnorm(N, mean=d)
    dat <- data.frame(group = gl(2, N, labels=c('G1', 'G2')),
                      DV = c(group1, group2))
    dat
}

Analyse <- function(condition, dat, fixed_objects) {
    p <- t.test(DV ~ group, dat, var.equal=TRUE)$p.value
    p
}

Summarise <- function(condition, results, fixed_objects) {
    # Must return a single number corresponding to f(x) in the
    # root equation f(x) = b

    ret <- c(power = EDR(results, alpha = condition$sig.level))
    ret
}

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Optimize N over the rows in design

### (For debugging) may want to see if simulation code works as intended first
### for some given set of inputs
# runSimulation(design=createDesign(N=100, d=.8, sig.level=.05),
#              replications=10, generate=Generate, analyse=Analyse,
#              summarise=Summarise)

# Initial search between N = [10,500] for each row using the default
   # integer solver (integer = TRUE). In this example, b = target power
solved <- SimSolve(design=Design, b=.8, interval=c(10, 500),
                generate=Generate, analyse=Analyse,
                summarise=Summarise)
solved
summary(solved)
plot(solved, 1)
plot(solved, 2)
plot(solved, 3)

# also can plot median history and estimate precision
plot(solved, 1, type = 'history')
plot(solved, 1, type = 'density')
plot(solved, 1, type = 'iterations')

# verify with true power from pwr package
library(pwr)
pwr.t.test(d=.2, power = .8) # sig.level/alpha = .05 by default
pwr.t.test(d=.5, power = .8)
pwr.t.test(d=.8, power = .8)

# use estimated N results to see how close power was
N <- solved$N
pwr.t.test(d=.2, n=N[1])
pwr.t.test(d=.5, n=N[2])
pwr.t.test(d=.8, n=N[3])

# with rounding
N <- ceiling(solved$N)
pwr.t.test(d=.2, n=N[1])
pwr.t.test(d=.5, n=N[2])
pwr.t.test(d=.8, n=N[3])

### failing analytic formula, confirm results with more precise
###  simulation via runSimulation()
###  (not required, if accuracy is important then ProBABLI should be run longer)
# csolved <- solved
# csolved$N <- ceiling(solved$N)
# confirm <- runSimulation(design=csolved, replications=10000, parallel=TRUE,
#                         generate=Generate, analyse=Analyse,
#                         summarise=Summarise)
# confirm

# Similarly, terminate if the prediction interval is consistently predicted
#   to be between [.795, .805]. Note that maxiter increased as well
solved_predCI <- SimSolve(design=Design, b=.8, interval=c(10, 500),
                     generate=Generate, analyse=Analyse, summarise=Summarise,
                     maxiter=200, predCI.tol=.01)
solved_predCI
summary(solved_predCI) # note that predCI.b are all within [.795, .805]

N <- solved_predCI$N
pwr.t.test(d=.2, n=N[1])
pwr.t.test(d=.5, n=N[2])
pwr.t.test(d=.8, n=N[3])

# Alternatively, and often more realistically, wait.time can be used
# to specify how long the user is willing to wait for a final estimate.
# Solutions involving more iterations will be more accurate,
# and therefore it is recommended to run the ProBABLI root-solver as long
# the analyst can tolerate if the most accurate estimates are desired.
# Below executes the simulation for 2 minutes per condition

solved_2min <- SimSolve(design=Design[1, ], b=.8, interval=c(10, 500),
                generate=Generate, analyse=Analyse, summarise=Summarise,
                wait.time="2")
solved_2min
summary(solved_2min)

# use estimated N results to see how close power was
N <- solved_2min$N
pwr.t.test(d=.2, n=N[1])


#------------------------------------------------

#######################
## Sensitivity Analysis
#######################

# GOAL: solve effect size d given sample size and power inputs (inputs
# for root no longer required to be an integer)

# Generate-Analyse-Summarise functions identical to above, however
# Design input includes NA for d element
Design <- createDesign(N = c(100, 50, 25),
                       d = NA,
                       sig.level = .05)
Design    # solve for NA's

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summarise functions (same as above)

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Optimize d over the rows in design
# search between d = [.1, 2] for each row

# In this example, b = target power
# note that integer = FALSE to allow smooth updates of d
solved <- SimSolve(design=Design, b = .8, interval=c(.1, 2),
                   generate=Generate, analyse=Analyse,
                   summarise=Summarise, integer=FALSE)
solved
summary(solved)
plot(solved, 1)
plot(solved, 2)
plot(solved, 3)

# plot median history and estimate precision
plot(solved, 1, type = 'history')
plot(solved, 1, type = 'density')
plot(solved, 1, type = 'iterations')

# verify with true power from pwr package
library(pwr)
pwr.t.test(n=100, power = .8)
pwr.t.test(n=50, power = .8)
pwr.t.test(n=25, power = .8)

# use estimated d results to see how close power was
pwr.t.test(n=100, d = solved$d[1])
pwr.t.test(n=50, d = solved$d[2])
pwr.t.test(n=25, d = solved$d[3])

### failing analytic formula, confirm results with more precise
###  simulation via runSimulation() (not required; if accuracy is important
###  PROBABLI should just be run longer)
# confirm <- runSimulation(design=solved, replications=10000, parallel=TRUE,
#                         generate=Generate, analyse=Analyse,
#                         summarise=Summarise)
# confirm


#------------------------------------------------

#####################
## Criterion Analysis
#####################

# GOAL: solve Type I error rate (alpha) given sample size, effect size, and
# power inputs (inputs for root no longer required to be an integer). Only useful
# when Type I error is less important than achieving the desired 1-beta (power)

Design <- createDesign(N = 50,
                        d = c(.2, .5, .8),
                        sig.level = NA)
Design    # solve for NA's

# all other function definitions same as above

# search for alpha within [.0001, .8]
solved <- SimSolve(design=Design, b = .8, interval=c(.0001, .8),
                   generate=Generate, analyse=Analyse,
                   summarise=Summarise, integer=FALSE)
solved
summary(solved)
plot(solved, 1)
plot(solved, 2)
plot(solved, 3)

# plot median history and estimate precision
plot(solved, 1, type = 'history')
plot(solved, 1, type = 'density')
plot(solved, 1, type = 'iterations')

# verify with true power from pwr package
library(pwr)
pwr.t.test(n=50, power = .8, d = .2, sig.level=NULL)
pwr.t.test(n=50, power = .8, d = .5, sig.level=NULL)
pwr.t.test(n=50, power = .8, d = .8, sig.level=NULL)

# use estimated alpha results to see how close power was
pwr.t.test(n=50, d = .2, sig.level=solved$sig.level[1])
pwr.t.test(n=50, d = .5, sig.level=solved$sig.level[2])
pwr.t.test(n=50, d = .8, sig.level=solved$sig.level[3])

### failing analytic formula, confirm results with more precise
###  simulation via runSimulation() (not required; if accuracy is important
###  PROBABLI should just be run longer)
# confirm <- runSimulation(design=solved, replications=10000, parallel=TRUE,
#                         generate=Generate, analyse=Analyse,
#                         summarise=Summarise)
# confirm

} # }
```
