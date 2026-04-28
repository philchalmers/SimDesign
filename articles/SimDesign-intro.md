# Introduction to the SimDesign package

> Seek computer programs that allow you to do the thinking. – George E.
> P. Box

Whether you are interested in evaluating the performance of a new
optimizer or estimation criteria, re-evaluating previous research claims
(e.g., ANOVA is ‘robust’ to violations of normality), want to determine
power rates for an upcoming research proposal (cf. the `Spower`
package), or simply wish to appease a strange thought in your head about
a new statistical idea you heard about, designing Monte Carlo
simulations can be incredibly rewarding and are extremely important to
those who are statistically oriented.

However, organizing simulations can be a challenge, particularly to
those new to the topic, where all too often investigators resort to the
inefficient and error prone strategies (e.g., the dreaded “for-loop”
strategy, *for*-ever resulting in confusing, error prone, and simulation
specific code). The package `SimDesign` is one attempt to fix these and
other issues that often arise when designing Monte Carlo simulation
experiments, while also providing a templated setup that is designed to
support many useful features that can be useful when evaluating
simulation research for novice and advanced users.

Generally speaking, Monte Carlo simulations can be broken into three
major components:

- **generate** your data from some model/probability density function
  given various **design** conditions to be studied (e.g., sample size,
  distributions, group sizes, etc),
- **analyse** the generated data using whatever statistical analyses you
  are interested in (e.g., $t$-test, ANOVA, SEMs, IRT, etc), and collect
  the statistics/CIs/$p$-values/parameter estimates you are interested
  in, and
- **summarise** the results after repeating the simulations $R$ number
  of times to obtain empirical estimates of the population’s behavior.

Each operation above represents the essential components of the
`SimDesign` package. The **design** component is represented by a
`tibble`-like object containing the simulation conditions to be
investigated, while **generate**, **analyse**, and **summarise**
represent user-defined functions which comprise the three steps in the
simulation. Each of these components are constructed and passed to the
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
function where the simulation steps are evaluated, ultimately returning
a `tibble`-like object containing the simulation results.

## A general overview

After loading the `SimDesign` package, we begin by defining the required
user-constructed functions. To expedite this process, a call to
[`SimFunctions()`](http://philchalmers.github.io/SimDesign/reference/SimFunctions.md)
can be used to create a suitable template, where all the necessary
functional arguments have been pre-assigned and only the body of the
functions need to be modified. The documentation of each argument can be
found in the respective R help files, however the organization is
conceptually simple.

To begin, the following code should be copied and saved to an external
source (i.e., text) file.

``` r
library(SimDesign)
SimFunctions()
```

    #-------------------------------------------------------------------

    library(SimDesign)

    Design <- createDesign(factor1 = NA,
                           factor2 = NA)

    #-------------------------------------------------------------------

    Generate <- function(condition, fixed_objects) {
        dat <- data.frame()
        dat
    }

    Analyse <- function(condition, dat, fixed_objects) {
        ret <- nc(stat1 = NaN, stat2 = NaN)
        ret
    }

    Summarise <- function(condition, results, fixed_objects) {
        ret <- c(bias = NaN, RMSE = NaN)
        ret
    }

    #-------------------------------------------------------------------

    res <- runSimulation(design=Design, replications=2, generate=Generate, 
                         analyse=Analyse, summarise=Summarise)
    res

Alternatively, if you are lazy (read: efficient) or just don’t like
copy-and-pasting,
[`SimFunctions()`](http://philchalmers.github.io/SimDesign/reference/SimFunctions.md)
can write the output to a file by providing a `filename` argument. The
following creates a file (`mysim.R`) containing the simulation
design/execution and required user-defined functions. For Rstudio users,
this will also automatically open up the file in a new coding window.

``` r
SimDesign::SimFunctions('mysim')
```

For larger simulations, you may want to use two files, and if you’d
prefer to have helpful comments included then these can be achieved with
the `singlefile` and `comments` arguments, respectively.

``` r
SimFunctions('mysim', singlefile = FALSE, comments = TRUE)
```

The choice of using a single file or not is entirely a matter of
preference, and will not influence the overall simulation
implementation. However, should you wish to include separate analysis or
generate functions the arguments `nGenerate` and `nAnalyse` can be
useful to compartmentalize generally distinct portions of the code
(e.g., one analyse function for fitting and extracting components of a
structural equation model, and one analyse function for fitting and
extracting information form an item response theory model).

## Simulation: Determine estimator efficiency

As a toy example, let’s consider how the following investigation using
`SimDesign`:

*Question*: How does trimming affect recovering the mean of a
distribution? Investigate this using different sample sizes with
Gaussian and $\chi^{2}$ distributions. Also, demonstrate the effect of
using the median to recover the mean.

### Define the conditions

First, define the condition combinations that should be investigated. In
this case we wish to study 4 different sample sizes, and use a symmetric
and skewed distribution. The use of
[`createDesign()`](http://philchalmers.github.io/SimDesign/reference/createDesign.md)
is required to create a completely crossed-design for each combination
(there are 8 in total).

``` r
Design <- createDesign(sample_size = c(30, 60, 120, 240), 
                       distribution = c('norm', 'chi'))
Design
```

    ## # A tibble: 8 × 2
    ##   sample_size distribution
    ##         <dbl> <chr>       
    ## 1          30 norm        
    ## 2          60 norm        
    ## 3         120 norm        
    ## 4         240 norm        
    ## 5          30 chi         
    ## 6          60 chi         
    ## 7         120 chi         
    ## 8         240 chi

Each row in `Design` represents a unique condition to be studied in the
simulation. In this case, the first condition to be studied comes from
row 1, where $N = 30$ and the distribution is from the Gaussian/normal
family.

### Define the functions

We first start by defining the data generation functional component. The
only argument accepted by this function is `condition`, which will
always be a *single row from the Design data.frame object*. Conditions
are run sequentially from row 1 to the last row in `Design`. It is also
possible to pass a `fixed_objects` object to the function for including
fixed sets of population parameters and other conditions, however for
this simple simulation this input is not required.

``` r
Generate <- function(condition, fixed_objects) {
    N <- condition$sample_size
    dist <- condition$distribution
    if(dist == 'norm'){
        dat <- rnorm(N, mean = 3)
    } else if(dist == 'chi'){
        dat <- rchisq(N, df = 3)
    }
    dat
}
```

As we can see above,
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
will return a numeric vector of length $N$ containing the data to be
analysed, each with a population mean of 3 (because a $\chi^{2}$
distribution has a mean equal to its df). Next, we define the `analyse`
component to analyse said data:

``` r
Analyse <- function(condition, dat, fixed_objects) {
    M0 <- mean(dat)
    M1 <- mean(dat, trim = .1)
    M2 <- mean(dat, trim = .2)
    med <- median(dat)
    
    ret <- c(mean_no_trim=M0, mean_trim.1=M1, mean_trim.2=M2, median=med)
    ret
}
```

This function accepts the data previously returned from
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
(`dat`), the condition vector previously mentioned, and returns 4 named
elements. Note that the element names do not have to be constant across
the row-conditions, however it will often make conceptual sense to do
so.

At this point, we may conceptually think of the first two functions as
being evaluated independently $R$ times to obtain $R$ sets of results.
In other words, if we wanted the number of replications to be 100, the
first two functions would be independently run (at least) 100 times, the
results from
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md)
would be stored, and we would then need to summarise these 100 elements
into meaningful meta statistics to describe their empirical properties.
This is where computing meta-statistics such as bias, root mean-square
error, detection rates, and so on are of primary importance.
Unsurprisingly, then, this is the purpose of the `summarise` component:

``` r
Summarise <- function(condition, results, fixed_objects) {
    obs_bias <- bias(results, parameter = 3)
    obs_RMSE <- RMSE(results, parameter = 3)
    ret <- c(bias=obs_bias, RMSE=obs_RMSE, RE=RE(obs_RMSE))
    ret
}
```

Again, `condition` is the same as was defined before, while `results` is
a `matrix` containing all the results from
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md),
where each row represents the result returned from each respective
replication, and the number of columns is equal to the length of a
single vector returned by
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md).

That sounds much more complicated than it is — all you really need to
know for this simulation is that an $R$ x 4 matrix called `results` is
available to build a suitable summary from. Because the results is a
matrix, [`apply()`](https://rdrr.io/r/base/apply.html) is useful to
apply a function over each respective row. The bias and RMSE are
obtained for each respective statistic, and the overall result is
returned as a vector.

Stopping for a moment and thinking carefully, we know that each
`condition` will be paired with a unique vector returned from
[`Summarise()`](http://philchalmers.github.io/SimDesign/reference/Summarise.md).
Therefore, you might be thinking that the result returned from the
simulation will be in a rectangular form, such as in a `matrix`,
`data.frame`, or `tibble`. Well, you’d be right!

### Putting it all together

The last stage of the `SimDesign` work-flow is to pass the four defined
elements to the
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
function which, unsurprisingly given its name, runs the simulation.

There are numerous options available in the function, and these should
be investigated by reading the
[`help(runSimulation)`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
HTML file. Options for performing simulations in parallel,
storing/resuming temporary results, debugging functions, and so on are
available. Below we simply request that each condition be run 1000 times
on a single processor, and finally store the results to an object called
`res`.

``` r
res <- runSimulation(Design, replications = 1000, generate=Generate, 
                         analyse=Analyse, summarise=Summarise)

# Final simulation object
res
```

    ## # A tibble: 8 × 18
    ##   sample_size distribution bias.mean_no_trim bias.mean_trim.1 bias.mean_trim.2
    ##         <dbl> <chr>                    <dbl>            <dbl>            <dbl>
    ## 1          30 norm              -0.0048080        -0.0046834       -0.0029265 
    ## 2          60 norm               0.0030546         0.0039219        0.0031080 
    ## 3         120 norm               0.0011275         0.00078027       0.00057227
    ## 4         240 norm               0.00095261        0.0011309        0.00070515
    ## 5          30 chi                0.022222         -0.29863         -0.44355   
    ## 6          60 chi               -0.0042835        -0.34631         -0.49384   
    ## 7         120 chi               -0.0050244        -0.34566         -0.48818   
    ## 8         240 chi               -0.000058300      -0.34650         -0.48943   
    ## # ℹ 13 more variables: bias.median <dbl>, RMSE.mean_no_trim <dbl>,
    ## #   RMSE.mean_trim.1 <dbl>, RMSE.mean_trim.2 <dbl>, RMSE.median <dbl>,
    ## #   RE.mean_no_trim <dbl>, RE.mean_trim.1 <dbl>, RE.mean_trim.2 <dbl>,
    ## #   RE.median <dbl>, REPLICATIONS <dbl>, SIM_TIME <chr>, SEED <int>,
    ## #   COMPLETED <chr>

If `runSimulation(..., store_results = TRUE)` were used, which is the
current default in the package if RAM is not an issue, then the complete
stored results can be viewed using

``` r
# Extract complete set of stored results
results <- SimResults(res)
results
```

    ## # A tibble: 8,000 × 6
    ##    sample_size distribution mean_no_trim mean_trim.1 mean_trim.2 median
    ##          <dbl> <chr>               <dbl>       <dbl>       <dbl>  <dbl>
    ##  1          30 norm                 3.18        3.14        3.15   3.17
    ##  2          30 norm                 2.84        2.80        2.78   2.86
    ##  3          30 norm                 3.29        3.21        3.14   2.95
    ##  4          30 norm                 3.34        3.37        3.34   3.28
    ##  5          30 norm                 2.78        2.77        2.77   2.76
    ##  6          30 norm                 3.15        3.08        3.06   3.03
    ##  7          30 norm                 3.03        3.00        3.00   2.99
    ##  8          30 norm                 3.50        3.51        3.55   3.55
    ##  9          30 norm                 2.95        2.91        2.90   2.94
    ## 10          30 norm                 2.98        2.99        3.01   2.95
    ## # ℹ 7,990 more rows

As can be seen from the printed results from the `res` object, each
result from the
[`Summarise()`](http://philchalmers.github.io/SimDesign/reference/Summarise.md)
function has been paired with their respective condition,
meta-statistics have been properly named, and three additional columns
have been appended to the results: `REPLICATIONS`, which indicates how
many time the conditions were performed, `SIM_TIME`, indicating the time
(in seconds) it took to completely finish the respective conditions, and
`SEED`, which indicates the random seeds used by `SimDesign` for each
condition (for reproducibility). A call to
[`View()`](https://rdrr.io/r/utils/View.html) in the R console may also
be a nice way to sift through the `res` object, while for the `results`
object one can use functions like
[`descript()`](http://philchalmers.github.io/SimDesign/reference/descript.md)
and verbs from the `dplyr` package to get a better understanding of the
distributions.

``` r
# summary statistics for complete results
descript(results)
```

    ## # A tibble: 5 × 12
    ##   VARS      n   mean   trim     sd   skew   kurt   min   P25   P50    P75    max
    ##   <fct> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
    ## 1 samp…  8000 112.   107.   80.4    0.657 -1.10  30    52.5  90    150    240   
    ## 2 mean…  8000   3.00   3.00  0.234  0.340  3.75   1.86  2.89  3.00   3.10   4.63
    ## 3 mean…  8000   2.83   2.85  0.279 -0.555  0.701  1.50  2.65  2.90   3.02   4.25
    ## 4 mean…  8000   2.76   2.78  0.329 -0.523 -0.169  1.38  2.50  2.85   3.01   4.23
    ## 5 medi…  8000   2.69   2.71  0.402 -0.440 -0.498  1.05  2.36  2.81   3.01   4.16

``` r
# conditional summary statistics using dplyr verbs
results |> group_by(sample_size, distribution) |> 
    descript()
```

    ## sample_size: 30
    ## distribution: chi
    ## # A tibble: 4 × 12
    ##   VARS           n  mean  trim    sd  skew    kurt   min   P25   P50   P75   max
    ##   <fct>      <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_no_t…  1000  3.02  3.02 0.447 0.149 -0.117   1.86  2.71  3.02  3.31  4.63
    ## 2 mean_trim…  1000  2.70  2.69 0.419 0.170 -0.102   1.50  2.42  2.69  2.96  4.25
    ## 3 mean_trim…  1000  2.56  2.55 0.423 0.215 -0.0328  1.38  2.27  2.54  2.83  4.23
    ## 4 median      1000  2.42  2.41 0.476 0.375  0.313   1.05  2.10  2.38  2.74  4.16
    ## 
    ## ------------------------------------------------------------
    ##  
    ## sample_size: 60
    ## distribution: chi
    ## # A tibble: 4 × 12
    ##   VARS             n  mean  trim    sd  skew  kurt   min   P25   P50   P75   max
    ##   <fct>        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_no_trim  1000  3.00  2.99 0.319 0.296 0.108  2.19  2.78  2.98  3.20  4.14
    ## 2 mean_trim.1   1000  2.65  2.64 0.295 0.368 0.363  1.92  2.45  2.64  2.84  3.96
    ## 3 mean_trim.2   1000  2.51  2.50 0.293 0.378 0.437  1.78  2.30  2.50  2.69  3.81
    ## 4 median        1000  2.36  2.35 0.335 0.409 0.484  1.32  2.13  2.34  2.56  3.77
    ## 
    ## ------------------------------------------------------------
    ##  
    ## sample_size: 120
    ## distribution: chi
    ## # A tibble: 4 × 12
    ##   VARS           n  mean  trim    sd  skew    kurt   min   P25   P50   P75   max
    ##   <fct>      <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_no_t…  1000  2.99  2.99 0.224 0.213 -0.0319  2.34  2.84  2.99  3.14  3.69
    ## 2 mean_trim…  1000  2.65  2.65 0.214 0.159 -0.130   2.07  2.51  2.64  2.80  3.30
    ## 3 mean_trim…  1000  2.51  2.51 0.215 0.149 -0.170   1.93  2.37  2.50  2.65  3.15
    ## 4 median      1000  2.36  2.36 0.240 0.243 -0.209   1.71  2.20  2.35  2.52  3.04
    ## 
    ## ------------------------------------------------------------
    ##  
    ## sample_size: 240
    ## distribution: chi
    ## # A tibble: 4 × 12
    ##   VARS           n  mean  trim    sd   skew   kurt   min   P25   P50   P75   max
    ##   <fct>      <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_no_t…  1000  3.00  3.00 0.158 0.0822 0.0561  2.47  2.89  3.00  3.10  3.53
    ## 2 mean_trim…  1000  2.65  2.65 0.148 0.0650 0.0595  2.10  2.55  2.65  2.75  3.11
    ## 3 mean_trim…  1000  2.51  2.51 0.148 0.0699 0.0749  1.94  2.41  2.50  2.61  2.96
    ## 4 median      1000  2.37  2.37 0.164 0.0263 0.0812  1.79  2.26  2.37  2.48  2.88
    ## 
    ## ------------------------------------------------------------
    ##  
    ## sample_size: 30
    ## distribution: norm
    ## # A tibble: 4 × 12
    ##   VARS           n  mean  trim    sd   skew   kurt   min   P25   P50   P75   max
    ##   <fct>      <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_no_t…  1000  3.00  2.99 0.185 0.0458 0.0342  2.36  2.87  2.99  3.13  3.64
    ## 2 mean_trim…  1000  3.00  2.99 0.190 0.0408 0.0225  2.42  2.86  2.99  3.12  3.66
    ## 3 mean_trim…  1000  3.00  3.00 0.196 0.0277 0.0462  2.36  2.86  2.99  3.13  3.67
    ## 4 median      1000  3.00  3.00 0.222 0.0755 0.173   2.26  2.85  3.00  3.15  3.79
    ## 
    ## ------------------------------------------------------------
    ##  
    ## sample_size: 60
    ## distribution: norm
    ## # A tibble: 4 × 12
    ##   VARS           n  mean  trim    sd   skew   kurt   min   P25   P50   P75   max
    ##   <fct>      <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_no_t…  1000  3.00  3.00 0.125 0.118  0.326   2.59  2.92  3.00  3.08  3.48
    ## 2 mean_trim…  1000  3.00  3.00 0.129 0.130  0.261   2.61  2.92  3.00  3.09  3.49
    ## 3 mean_trim…  1000  3.00  3.00 0.134 0.127  0.230   2.57  2.91  3.00  3.09  3.49
    ## 4 median      1000  3.01  3.01 0.157 0.0518 0.0579  2.46  2.89  3.00  3.12  3.53
    ## 
    ## ------------------------------------------------------------
    ##  
    ## sample_size: 120
    ## distribution: norm
    ## # A tibble: 4 × 12
    ##   VARS        n  mean  trim     sd   skew     kurt   min   P25   P50   P75   max
    ##   <fct>   <dbl> <dbl> <dbl>  <dbl>  <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_n…  1000  3.00  3.00 0.0908 0.0590  3.22e-2  2.72  2.94  3.00  3.06  3.29
    ## 2 mean_t…  1000  3.00  3.00 0.0940 0.0884  1.61e-2  2.69  2.94  3.00  3.06  3.31
    ## 3 mean_t…  1000  3.00  3.00 0.0977 0.0878 -5.57e-4  2.69  2.93  3.00  3.06  3.32
    ## 4 median   1000  3.00  3.00 0.113  0.0609 -9.05e-2  2.64  2.92  3.00  3.07  3.36
    ## 
    ## ------------------------------------------------------------
    ##  
    ## sample_size: 240
    ## distribution: norm
    ## # A tibble: 4 × 12
    ##   VARS        n  mean  trim     sd     skew   kurt   min   P25   P50   P75   max
    ##   <fct>   <dbl> <dbl> <dbl>  <dbl>    <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_n…  1000  3.00  3.00 0.0643  7.45e-2 -0.189  2.81  2.96  3.00  3.04  3.25
    ## 2 mean_t…  1000  3.00  3.00 0.0659  8.92e-2 -0.161  2.80  2.95  3.00  3.05  3.24
    ## 3 mean_t…  1000  3.00  3.00 0.0684  9.35e-2 -0.146  2.79  2.95  3.00  3.05  3.23
    ## 4 median   1000  3.00  3.00 0.0817 -9.91e-4 -0.172  2.71  2.94  3.00  3.06  3.26

### Interpreting the results

In this case, visually inspecting the simulation table is enough to
understand what is occurring, though for other Monte Carlo simulations
use of ANOVAs, marginalized tables, and graphics should be used to
capture the essentially phenomenon in the results. Monte Carlo
simulations are just like collecting and analysing data for experiments,
so my advice would be to put on your analysis hats and present your data
as though it were data collected from the real world.

In this particular simulation, it is readily apparent that using the
un-adjusted mean will adequately recover the population mean with little
bias. The precision also seems to increase as sample sizes increase,
which is indicated by the decreasing RMSE statistics. Generally,
trimming causes less efficiency in the estimates, where greater amounts
of trimming results in even less efficiency, and using the median as a
proxy to estimate the mean is the least effective method. This is
witnessed rather clearly in the following table, which prints the
relative efficiency of the estimators:

``` r
REs <- res[,grepl('RE\\.', colnames(res))]
data.frame(Design, REs)
```

    ##   sample_size distribution RE.mean_no_trim RE.mean_trim.1 RE.mean_trim.2
    ## 1          30         norm               1            1.0            1.1
    ## 2          60         norm               1            1.1            1.1
    ## 3         120         norm               1            1.1            1.2
    ## 4         240         norm               1            1.1            1.1
    ## 5          30          chi               1            1.3            1.9
    ## 6          60          chi               1            2.0            3.2
    ## 7         120          chi               1            3.3            5.7
    ## 8         240          chi               1            5.7           10.5
    ##   RE.median
    ## 1       1.4
    ## 2       1.6
    ## 3       1.5
    ## 4       1.6
    ## 5       2.8
    ## 6       5.1
    ## 7       9.2
    ## 8      17.1

Finally, when the $\chi^{2}$ distribution was investigated only the
un-adjusted mean accurately portrayed the population mean. This isn’t
surprising, because the trimmed mean is, after all, making inferences
about the population trimmed mean, and the median is making inferences
about, well, the median. Only when the distributions under investigation
are symmetric are the statistics able to draw inferences about the same
inferences about the mean of the population.

## Conceptual walk-through of what runSimulation() is doing

The following is a conceptual breakdown of what
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
is actually doing behind the scenes. Here we demonstrate the results
from the first condition (row 1 of `Design`) to show what each function
returns.

A single replication in a Monte Carlo simulation results in the
following objects:

``` r
(condition <- Design[1, ])
```

    ## # A tibble: 1 × 2
    ##   sample_size distribution
    ##         <dbl> <chr>       
    ## 1          30 norm

``` r
dat <- Generate(condition)
dat
```

    ##  [1] 2.37 3.18 2.16 4.60 3.33 2.18 3.49 3.74 3.58 2.69 4.51 3.39 2.38 0.79 4.12
    ## [16] 2.96 2.98 3.94 3.82 3.59 3.92 3.78 3.07 1.01 3.62 2.94 2.84 1.53 2.52 3.42

``` r
res <- Analyse(condition, dat)
res
```

    ## mean_no_trim  mean_trim.1  mean_trim.2       median 
    ##          3.1          3.2          3.2          3.3

We can see that
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
returns a `numeric` vector which is accepted by
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md).
The
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md)
function then completes the analysis portion using the generated data,
and returns a named vector with the observed parameter estimates. Of
course, this is only a single replication, and therefore is not really
meaningful in the grand scheme of things; so, it must be repeated a
number of times.

``` r
# repeat 1000x
results <- matrix(0, 1000, 4)
colnames(results) <- names(res)
for(i in 1:1000){
    dat <- Generate(condition)
    res <- Analyse(condition, dat)
    results[i, ] <- res
}
head(results)
```

    ##      mean_no_trim mean_trim.1 mean_trim.2 median
    ## [1,]          3.1         3.1         3.1    2.9
    ## [2,]          3.1         3.1         3.1    3.1
    ## [3,]          3.1         3.1         3.0    2.8
    ## [4,]          2.7         2.6         2.6    2.7
    ## [5,]          3.2         3.2         3.2    3.0
    ## [6,]          3.1         3.1         3.0    3.1

``` r
descript(results) # common descriptive statistics
```

    ## # A tibble: 4 × 12
    ##   VARS           n  mean  trim    sd    skew  kurt   min   P25   P50   P75   max
    ##   <fct>      <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 mean_no_t…  1000  3.00  3.00 0.174  0.0821 0.135  2.45  2.89  3.00  3.11  3.67
    ## 2 mean_trim…  1000  3.00  3.00 0.178  0.0656 0.145  2.40  2.88  3.00  3.11  3.67
    ## 3 mean_trim…  1000  3.00  3.00 0.186  0.0297 0.142  2.39  2.88  3.00  3.12  3.68
    ## 4 median      1000  3.00  3.00 0.215 -0.0399 0.137  2.24  2.86  3.00  3.14  3.78

The matrix stored in `results` contains 1000 parameter estimates
returned from each statistic. After this is obtained, we can move on to
summarising the output through the
[`Summarise()`](http://philchalmers.github.io/SimDesign/reference/Summarise.md)
function to obtain average estimates, their associated sampling error,
their efficiency, and so on.

``` r
Summarise(condition, results) 
```

    ## bias.mean_no_trim  bias.mean_trim.1  bias.mean_trim.2       bias.median 
    ##           -0.0011           -0.0031           -0.0035           -0.0037 
    ## RMSE.mean_no_trim  RMSE.mean_trim.1  RMSE.mean_trim.2       RMSE.median 
    ##            0.1739            0.1777            0.1859            0.2146 
    ##   RE.mean_no_trim    RE.mean_trim.1    RE.mean_trim.2         RE.median 
    ##            1.0000            1.0442            1.1425            1.5225

This process is then repeated for each row `condition` in the `Design`
object until the entire simulation study is complete.

Of course,
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
does much more than this conceptual outline, which is why it exists.
Namely, errors and warnings are controlled and tracked, data is re-drawn
when needed, parallel processing is supported, debugging is easier with
the `debug` input (or by inserting
[`browser()`](https://rdrr.io/r/base/browser.html) directly), temporary
and full results can be saved to external files, the simulation state
can be saved/restored, build-in safety features are included, and more.
The point, however, is that you as the user *should not be bogged down
with the nitty-gritty details of setting up the simulation
work-flow/features*; instead, you should be focusing your time on the
important generate-analyse-summarise steps, organized in the body of the
above functions, that are required to obtain your interesting simulation
results. After all, the point designing a computer simulation experiment
is to understand the resulting output, not to become a master of all
aspects of your select computing language pertaining to object storage,
parallel processing, RAM storage, defensive coding, progress reporting,
reproducibility, post-processing, …, ad nauseam.

To access further examples and instructions feel free to visit the
[package wiki on Github](https://github.com/philchalmers/SimDesign/wiki)
