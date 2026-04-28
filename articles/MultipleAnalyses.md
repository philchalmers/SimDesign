# Multiple analysis functions

This vignette demonstrates one of the newer features in the `SimDesign`
package pertaining to multiple analysis function definitions that can be
selected for each `Design` condition or whenever they are applicable.
The purpose of providing multiple analysis functions is to

1.  Remove the less readable if-then-else combinations that can appear
    when writing simulation code, where specific analysis functions are
    not intended to be used on a given generated dataset,
2.  Provide better automatic naming of the analysis results across
    independent subroutines,
3.  Construct more readable code when the
    [`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md)
    function contains too much code to easily track, and to
4.  Create a more modular approach to isolating the analysis functions
    for the purpose of redistribution or reusing in related projects

Functionality speaking, this type of organization does not change how
`SimDesign` generally operates. For that reason, the coding style
presented in this vignette can be considered optional. However, if any
of the above points resonate well with you then following the details of
this coding organization style may prove useful.

## Description of structure

The usual work-flow with `SimDesign` requires first calling
[`SimFunctions()`](http://philchalmers.github.io/SimDesign/reference/SimFunctions.md)
to generate a working template, such as the following.

``` r
SimDesign::SimFunctions()
```

    ## #-------------------------------------------------------------------
    ## 
    ## library(SimDesign)
    ## 
    ## Design <- createDesign(factor1 = NA,
    ##                        factor2 = NA)
    ## 
    ## #-------------------------------------------------------------------
    ## 
    ## Generate <- function(condition, fixed_objects) {
    ##     dat <- data.frame()
    ##     dat
    ## }
    ## 
    ## Analyse <- function(condition, dat, fixed_objects) {
    ##     ret <- nc(stat1 = NaN, stat2 = NaN)
    ##     ret
    ## }
    ## 
    ## Summarise <- function(condition, results, fixed_objects) {
    ##     ret <- c(bias = NaN, RMSE = NaN)
    ##     ret
    ## }
    ## 
    ## #-------------------------------------------------------------------
    ## 
    ## res <- runSimulation(design=Design, replications=2, generate=Generate, 
    ##                      analyse=Analyse, summarise=Summarise)
    ## res

which uses the default `nAnalyses=1` to generate only a single
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md)
function. In the context of multiple analysis functions, however, users
may be more interested in passing the number of analysis functions they
believe they will need in their simulation (e.g., if analyzing a
$t$-test setup to compare the Welch versus independent samples t-test,
then two analysis functions should be used). Passing `nAnalyses=2` to
[`SimFunctions()`](http://philchalmers.github.io/SimDesign/reference/SimFunctions.md)
creates the following template:

``` r
SimDesign::SimFunctions(nAnalyses = 2)
```

    ## #-------------------------------------------------------------------
    ## 
    ## library(SimDesign)
    ## 
    ## Design <- createDesign(factor1 = NA,
    ##                        factor2 = NA)
    ## 
    ## #-------------------------------------------------------------------
    ## 
    ## Generate <- function(condition, fixed_objects) {
    ##     dat <- data.frame()
    ##     dat
    ## }
    ## 
    ## Analyse.A1 <- function(condition, dat, fixed_objects) {
    ##     ret <- nc(stat1 = NaN, stat2 = NaN)
    ##     ret
    ## }
    ## 
    ## Analyse.A2 <- function(condition, dat, fixed_objects) {
    ##     ret <- nc(stat1 = NaN, stat2 = NaN)
    ##     ret
    ## }
    ## 
    ## #-------------------------------------------------------------------
    ## 
    ## Summarise <- function(condition, results, fixed_objects) {
    ##     ret <- c(bias = NaN, RMSE = NaN)
    ##     ret
    ## }
    ## 
    ## #-------------------------------------------------------------------
    ## 
    ## res <- runSimulation(design=Design, replications=2,generate=Generate, 
    ##                      analyse=list(A1=Analyse.A1, A2=Analyse.A2), 
    ##                      summarise=Summarise)
    ## res

Notice in this case that there are two `Analyse.#()` definitions
constructed, and when passed to
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
these are organized as a named `list`. The names of the list will
ultimately be attached to the names of the analysis objects so that
there is no ambiguity in the outputted information. However, the inputs
to the
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md)
functions will always be the same, as the `dat` object formed by the
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
call will be passed to each of these Analyse definitions (hence, the
generate data is held constant across the respective analyses).

The above template should of course be modified to replace the less
useful names of the `Analyse.#()` components. By default users will want
to change these to something like `Analyse.some_statistic`,
`Analyse.some_other_statistic`, …, `Analyse.some_other_other_statistic`,
and so on, where the number of `Analyse.#` function definitions will
ultimately end up in the `runSimulation(..., Analyse=list())` input.
Supplying better names to the named `list` component is also recommended
as these will be used to name the associated results in the
[`Summarise()`](http://philchalmers.github.io/SimDesign/reference/Summarise.md)
step.

Finally, note that all the rules about objects and object naming from
the typical single Analyse function still apply and are properly checked
internally for suitable names and consistency. The independently defined
Analyse functions are also *interchangable* and
*removable*/*replaceable*, which makes the structure of the
Generate-Analyse-Summarise setup more modular with respect to the
analysis components.

### An example

The following code is [adopted from the
Wiki](http://philchalmers.github.io/SimDesign/html/03-Parameter_recovery_simulation.md),
and so details about the simulation should be obtained from that source.

``` r
# Note that all attached packages are automatically exported in SimDesign
library(SimDesign)
library(mirt)
library(lavaan)
# SimFunctions(nAnalyses = 2)

sample_sizes <- c(250, 500, 1000)
nitems <- c(10, 20)
Design <- createDesign(sample_size = sample_sizes, 
                       nitems = nitems)

# create list of additional parameters which are fixed across conditions
set.seed(1)
pars_10 <- rbind(a = round(rlnorm(10, .3, .5)/1.702, 2),
                 d = round(rnorm(10, 0, .5)/1.702, 2))
pars_20 <- rbind(a = round(rlnorm(20, .3, .5)/1.702, 2),
                 d = round(rnorm(20, 0, .5)/1.702, 2))
pars <- list(ten=pars_10, twenty=pars_20)

P_logit <- function(a, d, Theta) exp(a * Theta + d) / (1 + exp(a * Theta + d))
P_ogive <- function(a, d, Theta) pnorm(a * Theta + d)
```

``` r
Generate <- function(condition, fixed_objects) {

    N <- condition$sample_size
    nitems <- condition$nitems
    nitems_name <- ifelse(nitems == 10, 'ten', 'twenty')

    #extract objects from fixed_objects
    a <- fixed_objects[[nitems_name]]['a', ]
    d <- fixed_objects[[nitems_name]]['d', ]

    dat <- matrix(NA, N, nitems)
    colnames(dat) <- paste0('item_', 1:nitems)
    Theta <- rnorm(N)
    for(j in 1:nitems){
        p <- P_ogive(a[j], d[j], Theta)
        for(i in 1:N)
            dat[i,j] <- sample(c(1,0), 1, prob = c(p[i], 1 - p[i]))
    }
    as.data.frame(dat) #data.frame works nicer with lavaan
}

Analyse.FIML <- function(condition, dat, fixed_objects) {
    mod <- mirt(dat, 1L, verbose=FALSE)
    if(!extract.mirt(mod, 'converged')) stop('mirt did not converge')
    cfs <- mirt::coef(mod, simplify = TRUE, digits = Inf)
    FIML_as <- cfs$items[,1L] / 1.702
    
    ret <- c(as=unname(FIML_as))
    ret
}

Analyse.DWLS <- function(condition, dat, fixed_objects) {
    nitems <- condition$nitems
    lavmod <- paste0('F =~ ', paste0('NA*', colnames(dat)[1L], ' + '),
                     paste0(colnames(dat)[-1L], collapse = ' + '),
                     '\nF ~~ 1*F')
    lmod <- sem(lavmod, dat, ordered = colnames(dat))
    if(!lavInspect(lmod, 'converged')) stop('lavaan did not converge')
    cfs2 <- lavaan::coef(lmod)
    DWLS_alpha <- cfs2[1L:nitems]
    const <- sqrt(1 - DWLS_alpha^2)
    DWLS_as <- DWLS_alpha / const

    ret <- c(as=unname(DWLS_as))
    ret
}

Summarise <- function(condition, results, fixed_objects) {
    nitems <- condition$nitems
    nitems_name <- ifelse(nitems == 10, 'ten', 'twenty')

    #extract objects from fixed_objects
    a <- fixed_objects[[nitems_name]]['a', ]
    pop <- c(a, a)

    obt_bias <- bias(results, pop)
    obt_RMSE <- RMSE(results, pop)
    ret <- c(bias=obt_bias, RMSE=obt_RMSE)
    ret
}
```

``` r
res <- runSimulation(Design, replications=100, verbose=FALSE, parallel=TRUE,
                     generate=Generate, 
                     analyse=list(FIML=Analyse.FIML, DWLS=Analyse.DWLS), 
                     summarise=Summarise, filename = 'mirt_lavaan',
                     fixed_objects=pars)
```

``` r
res
```

    ## # A tibble: 6 × 86
    ##   sample_size nitems bias.FIML.as1 bias.FIML.as2 bias.FIML.as3 bias.FIML.as4
    ##         <dbl>  <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
    ## 1         250     10     0.0099887     0.013457     -0.020161      0.18616  
    ## 2         500     10    -0.021136      0.018720     -0.018173      0.10424  
    ## 3        1000     10    -0.0039473    -0.0014778    -0.013393      0.077890 
    ## 4         250     20     0.060647      0.011787      0.064688     -0.026314 
    ## 5         500     20     0.019202      0.045376      0.021837     -0.0063587
    ## 6        1000     20     0.019976      0.018608      0.0079817    -0.011498 
    ## # ℹ 80 more variables: bias.FIML.as5 <dbl>, bias.FIML.as6 <dbl>,
    ## #   bias.FIML.as7 <dbl>, bias.FIML.as8 <dbl>, bias.FIML.as9 <dbl>,
    ## #   bias.FIML.as10 <dbl>, bias.DWLS.as1 <dbl>, bias.DWLS.as2 <dbl>,
    ## #   bias.DWLS.as3 <dbl>, bias.DWLS.as4 <dbl>, bias.DWLS.as5 <dbl>,
    ## #   bias.DWLS.as6 <dbl>, bias.DWLS.as7 <dbl>, bias.DWLS.as8 <dbl>,
    ## #   bias.DWLS.as9 <dbl>, bias.DWLS.as10 <dbl>, RMSE.FIML.as1 <dbl>,
    ## #   RMSE.FIML.as2 <dbl>, RMSE.FIML.as3 <dbl>, RMSE.FIML.as4 <dbl>, …

In this particular formulation the `mirt` and `lavaan` package analyses
have been completely isolated into their own respective functions, and
in principle could therefore be analyzed independently in future
simulation studies. This adds a nicer layer of potential modularity to
the Analyse portion of the `SimDesign` framework, where re-using or
modifying previous `SimDesign` code should be less painful.

## `AnalyseIf()`

In situations where analysis functions defined in the `analyse` list
should only be applied in certain design conditions, users can include
an
[`AnalyseIf()`](http://philchalmers.github.io/SimDesign/reference/AnalyseIf.md)
definition at the beginning of their respective functions to ensure the
analyses are only executed when the provided logical is `TRUE`. This
logical ensures the data generation conditions are suitable for the
analysis function to be investigated; otherwise, it is skipped over in
the generate-analyse-summarise work-flow.

As a continuation from above, say that an investigator was also
interested in recovering the slope parameters of a factor analysis model
where the observed indicator variable were continuous as well as
discrete. The `Design` definition may therefore look like the following.

``` r
Design <- createDesign(sample_size = sample_sizes, 
                       nitems = nitems, 
                       indicators = c('discrete', 'continuous'))
Design
```

    ## # A tibble: 12 × 3
    ##    sample_size nitems indicators
    ##          <dbl>  <dbl> <chr>     
    ##  1         250     10 discrete  
    ##  2         500     10 discrete  
    ##  3        1000     10 discrete  
    ##  4         250     20 discrete  
    ##  5         500     20 discrete  
    ##  6        1000     20 discrete  
    ##  7         250     10 continuous
    ##  8         500     10 continuous
    ##  9        1000     10 continuous
    ## 10         250     20 continuous
    ## 11         500     20 continuous
    ## 12        1000     20 continuous

Provided that the
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
step utilized this `indicators` character vector, this would imply that
the `dat` object returned from
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
could consist of discrete or continuous data. In the case of continuous
indicator variables, `lavaan` could be used as it supports such
indicator types; however, `mirt` cannot. So, to ensure that only the
analysis function pertaining to `lavaan` is used one could include the
following replacement definition that used `mirt`, but now includes an
[`AnalyseIf()`](http://philchalmers.github.io/SimDesign/reference/AnalyseIf.md)
logical given the `indicators` variable’s state.

``` r
Analyse.FIML <- function(condition, dat, fixed_objects) {
    AnalyseIf(condition$indicators == 'discrete')
    # equivalently: 
    #   AnalyseIf(indicators == 'discrete', condition)
    #   with(condition, AnalyseIf(indicators == 'discrete'))
    mod <- mirt(dat, 1L, verbose=FALSE)
    if(!extract.mirt(mod, 'converged')) stop('mirt did not converge')
    cfs <- coef(mod, simplify = TRUE, digits = Inf)
    FIML_as <- cfs$items[,1L] / 1.702
    
    ret <- c(as=unname(FIML_as))
    ret
}
```

Using this definition the final object returned by
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)
will provide suitable `NA` placeholders (where appropriate). For
continuous indicators the results will be presented as though `mirt` was
never used for the continuous indicator conditions controlled by the
`Design` object.

Note that a similar logic can be made for multiple
[`Generate()`](http://philchalmers.github.io/SimDesign/reference/Generate.md)
functions, templated via `SimDesign::SimFunctions(nGenerate = 2)`, and
requiring specific
[`GenerateIf()`](http://philchalmers.github.io/SimDesign/reference/GenerateIf.md)
tests to indicate which generation function should be used. For those
interested in this type of structure, the following could be used to
separate the discrete/continuous data generation per `Design` row:

``` r
Generate.G1 <- function(condition, fixed_objects) {
    GenerateIf(condition$indicators == 'discrete')
    ...
    dat
}

Generate.G2 <- function(condition, fixed_objects) {
    GenerateIf(condition$indicators == 'continuous')
    ...
    dat
}

res <- runSimulation(design=Design, replications=1000,
                     generate=list(G1=Generate.G1, G2=Generate.G2), 
                     analyse=list(DWLS=Analyse.DWLS, FIML=Analyse.FIML), 
                     summarise=Summarise)
```

### Applying one analyse function per-condition

Interestingly,
[`AnalyseIf()`](http://philchalmers.github.io/SimDesign/reference/AnalyseIf.md)
could also be used to select only one analysis function at a time given
the components in the `Design` object. For instance, if the `Design`
definition were constructed using

``` r
Design <- createDesign(sample_size = sample_sizes, 
                       nitems = nitems, 
                       method = c('FIML', 'DWLS'))
Design
```

    ## # A tibble: 12 × 3
    ##    sample_size nitems method
    ##          <dbl>  <dbl> <chr> 
    ##  1         250     10 FIML  
    ##  2         500     10 FIML  
    ##  3        1000     10 FIML  
    ##  4         250     20 FIML  
    ##  5         500     20 FIML  
    ##  6        1000     20 FIML  
    ##  7         250     10 DWLS  
    ##  8         500     10 DWLS  
    ##  9        1000     10 DWLS  
    ## 10         250     20 DWLS  
    ## 11         500     20 DWLS  
    ## 12        1000     20 DWLS

and the analysis functions above were supplied defined as

``` r
Analyse.FIML <- function(condition, dat, fixed_objects) {
    AnalyseIf(method == 'FIML', condition)
    #...
}

Analyse.DWLS <- function(condition, dat, fixed_objects) {
    AnalyseIf(method == 'DWLS', condition)
    # ...
}

# ...
res <- runSimulation(Design, replications=100, verbose=FALSE, parallel=TRUE,
                     generate=Generate, 
                     analyse=list(Analyse.FIML, Analyse.DWLS), 
                     summarise=Summarise, filename = 'mirt_lavaan',
                     fixed_objects=pars)
```

then only one analysis function will be applied at a time in the
simulation experiment. Note that in this case there is no need to append
‘MML’ or ‘DWLS’ to the `results` objects as this becomes redundant with
the `method` column in the `Design` object, and so the `analyse` list
input is specified as an unnamed list (cf. earlier when the input was
named, which appended `MML.` and `DWLS.` to the `results` output in
[`Summarise()`](http://philchalmers.github.io/SimDesign/reference/Summarise.md)).

Users may find this a more natural setup than having to merge all
analysis information into a single
[`Analyse()`](http://philchalmers.github.io/SimDesign/reference/Analyse.md)
definition. The downside, however, is that the analysis function will be
applied to different generated datasets, which while theoretically
unbiased could have ramifications should the analysis functions throw
errors at different rates (even when explicitly supplying a `seed`
vector input to
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)).
