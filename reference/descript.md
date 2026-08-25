# Compute univariate descriptive statistics

Function returns univariate data summaries for each variable supplied.
For presentation purposes, discrete and continuous variables are treated
separately, the former of which reflects count/proportion information
while the ladder are supplied to a (customizable) list of univariate
summary functions. As such, quantitative/continuous variable information
is kept distinct in the output, while discrete variables (e.g.,
`factors` and `character` vectors) are returned by using the `discrete`
argument. When applicable a `"VARIABLE"` column will be included in the
output to indicate which variable is being summarised on the respective
row.

## Usage

``` r
descript(
  df,
  funs = get_descriptFuns(),
  by_group = FALSE,
  discrete = FALSE,
  collapse = FALSE
)

get_descriptFuns()
```

## Arguments

- df:

  typically a `data.frame` or `tibble`-like structure containing the
  variables of interest

  Note that `factor` and `character` vectors will be treated as discrete
  observations, and by default are omitted from the computation of the
  quantitative descriptive statistics specified in `funs`. However,
  setting `discrete = TRUE` will provide count-type information for
  these discrete variables, in which case arguments to `funs` are
  ignored

- funs:

  functions to apply when `discrete = FALSE`. Can be modified by the
  user to include or exclude further functions, however each supplied
  function must return a scalar. Use `get_discreteFuns()` to return the
  full list of functions, which may then be augmented or subsetted based
  on the user's requirements. Default descriptive statistic returned
  are:

  `n`

  :   number of non-missing observations

  `mean`

  :   mean

  `trim`

  :   trimmed mean (10%)

  `sd`

  :   standard deviation

  `skew`

  :   skewness (from `e1701`)

  `kurt`

  :   kurtosis (from `e1071`)

  `min`

  :   minimum

  `P25`

  :   25th percentile (a.k.a., 1st/lower quartile, Q1), returned from
      [`quantile`](https://rdrr.io/r/stats/quantile.html))

  `P50`

  :   median (50th percentile)

  `P75`

  :   75th percentile (a.k.a, 3rd/upper quartile, Q3), returned from
      [`quantile`](https://rdrr.io/r/stats/quantile.html))

  `max`

  :   maximum

  Note that by default the `na.rm` behavior is set to `TRUE` in each
  function call

- by_group:

  logical; when
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  were used to define the conditioning levels, should the output from
  [`by()`](https://rdrr.io/r/base/by.html) be organized by these group
  levels or by variable names? Only applicable when more than one
  variable is being described

- discrete:

  logical; include summary statistics for `discrete` variables only? If
  `TRUE` then only count and proportion information for the discrete
  variables will be returned, and `by_group` will automatically be set
  to `TRUE`. For greater flexibility in creating cross-tabulated
  count/proportion information see
  [`xtabs`](https://rdrr.io/r/stats/xtabs.html)

- collapse:

  logical; should the result be returned as a list output structured
  using [`by`](https://rdrr.io/r/base/by.html) or as a `tibble`? Default
  is `FALSE`

## Details

The purpose of this function is to provide a more pipe-friendly API for
selecting and subsetting variables using the `dplyr` syntax, where
conditional statistics are evaluated internally using the
[`by`](https://rdrr.io/r/base/by.html) function (when multiple variables
are to be summarised). As a special case, if only a single variable is
being summarised then the canonical output from
[`dplyr::summarise`](https://dplyr.tidyverse.org/reference/summarise.html)
will be returned.

*Conditioning*: As the function is intended to support pipe-friendly
code specifications, conditioning/group subset specifications are
declared using
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html) and
subsequently passed to `descript`.

## See also

[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html),
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html),
[`select`](https://dplyr.tidyverse.org/reference/select.html),
[`xtabs`](https://rdrr.io/r/stats/xtabs.html)

## Examples

``` r

data(mtcars)

if(FALSE){
  # run the following to see behavior with NA values in dataset
  mtcars[sample(1:nrow(mtcars), 3), 'cyl'] <- NA
  mtcars[sample(1:nrow(mtcars), 5), 'mpg'] <- NA
}

fmtcars <- within(mtcars, {
  cyl <- factor(cyl)
  am <- factor(am, labels=c('automatic', 'manual'))
  vs <- factor(vs)
})

# with and without factor variables
mtcars |> descript()
#> # A tibble: 11 × 12
#>    VARIABLE     n    mean    trim      sd   skew    kurt   min    P25    P50
#>    <fct>    <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl> <dbl>  <dbl>  <dbl>
#>  1 mpg         32  20.1    19.7     6.03   0.611 -0.373  10.4   15.4   19.2 
#>  2 cyl         32   6.19    6.23    1.79  -0.175 -1.76    4      4      6   
#>  3 disp        32 231.    223.    124.     0.382 -1.21   71.1  121.   196.  
#>  4 hp          32 147.    141.     68.6    0.726 -0.136  52     96.5  123   
#>  5 drat        32   3.60    3.58    0.535  0.266 -0.715   2.76   3.08   3.70
#>  6 wt          32   3.22    3.15    0.978  0.423 -0.0227  1.51   2.58   3.32
#>  7 qsec        32  17.8    17.8     1.79   0.369  0.335  14.5   16.9   17.7 
#>  8 vs          32   0.438   0.423   0.504  0.240 -2.00    0      0      0   
#>  9 am          32   0.406   0.385   0.499  0.364 -1.92    0      0      0   
#> 10 gear        32   3.69    3.62    0.738  0.529 -1.07    3      3      4   
#> 11 carb        32   2.81    2.65    1.62   1.05   1.26    1      2      2   
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
fmtcars |> descript()               # factors/discrete vars omitted
#> # A tibble: 8 × 12
#>   VARIABLE     n   mean   trim      sd  skew    kurt   min    P25    P50    P75
#>   <fct>    <dbl>  <dbl>  <dbl>   <dbl> <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg         32  20.1   19.7    6.03  0.611 -0.373  10.4   15.4   19.2   22.8 
#> 2 disp        32 231.   223.   124.    0.382 -1.21   71.1  121.   196.   326   
#> 3 hp          32 147.   141.    68.6   0.726 -0.136  52     96.5  123    180   
#> 4 drat        32   3.60   3.58   0.535 0.266 -0.715   2.76   3.08   3.70   3.92
#> 5 wt          32   3.22   3.15   0.978 0.423 -0.0227  1.51   2.58   3.32   3.61
#> 6 qsec        32  17.8   17.8    1.79  0.369  0.335  14.5   16.9   17.7   18.9 
#> 7 gear        32   3.69   3.62   0.738 0.529 -1.07    3      3      4      4   
#> 8 carb        32   2.81   2.65   1.62  1.05   1.26    1      2      2      4   
#> # ℹ 1 more variable: max <dbl>
fmtcars |> descript(discrete=TRUE)  # discrete variables only
#> VARIABLE: cyl
#> 
#>   count proportion
#> 4    11    0.34375
#> 6     7    0.21875
#> 8    14    0.43750
#> 
#> ------------------------------------------------------------
#>  
#> VARIABLE: vs
#> 
#>   count proportion
#> 0    18     0.5625
#> 1    14     0.4375
#> 
#> ------------------------------------------------------------
#>  
#> VARIABLE: am
#> 
#>           count proportion
#> automatic    19    0.59375
#> manual       13    0.40625

# usual pipe chaining
fmtcars |> select(mpg, wt) |> descript()
#> # A tibble: 2 × 12
#>   VARIABLE     n  mean  trim    sd  skew    kurt   min   P25   P50   P75   max
#>   <fct>    <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg         32 20.1  19.7  6.03  0.611 -0.373  10.4  15.4  19.2  22.8  33.9 
#> 2 wt          32  3.22  3.15 0.978 0.423 -0.0227  1.51  2.58  3.32  3.61  5.42
fmtcars |> subset(mpg > 20) |> select(mpg, wt) |> descript()
#> # A tibble: 2 × 12
#>   VARIABLE     n  mean  trim    sd    skew  kurt   min   P25   P50   P75   max
#>   <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg         14 25.5  25.2  4.60   0.553  -1.38 21    21.4  23.6  29.6  33.9 
#> 2 wt          14  2.42  2.43 0.577 -0.0349 -1.47  1.51  1.99  2.39  2.85  3.22

# conditioning with group_by(), printing across each variable
fmtcars |> group_by(cyl) |> descript()
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd   skew   kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     mpg         11  26.7  26.4  4.51  0.259 -1.65   21.4  22.8  26    30.4
#> 2 6     mpg          7  19.7  19.7  1.45 -0.158 -1.91   17.8  18.6  19.7  21  
#> 3 8     mpg         14  15.1  15.2  2.56 -0.363 -0.566  10.4  14.4  15.2  16.2
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd  skew  kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     disp        11  105.  104.  26.9 0.121 -1.64  71.1  78.8  108   121.
#> 2 6     disp         7  183.  183.  41.6 0.795 -1.23 145   160    168.  196.
#> 3 8     disp        14  353.  350.  67.8 0.453 -1.26 276.  302.   350.  390 
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd    skew    kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     hp          11  82.6  82.7  20.9 0.00626 -1.71      52  65.5   91    96 
#> 2 6     hp           7 122.  122.   24.3 1.36     0.249    105 110    110   123 
#> 3 8     hp          14 209.  204.   51.0 0.909    0.0921   150 176.   192.  241.
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd   skew   kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     drat        11  4.07  4.02 0.365  0.998  0.123  3.69  3.81  4.08  4.16
#> 2 6     drat         7  3.59  3.59 0.476 -0.736 -1.40   2.76  3.35  3.9   3.91
#> 3 8     drat        14  3.23  3.19 0.372  1.34   1.08   2.76  3.07  3.12  3.22
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd   skew   kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     wt          11  2.29  2.27 0.570  0.300 -1.36   1.51  1.88  2.2   2.62
#> 2 6     wt           7  3.12  3.12 0.356 -0.222 -1.98   2.62  2.82  3.22  3.44
#> 3 8     wt          14  4.00  3.95 0.759  0.988 -0.713  3.17  3.53  3.76  4.01
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd   skew    kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     qsec        11  19.1  19.0  1.68  0.550 -0.0207  16.7  18.6  18.9  20.0
#> 2 6     qsec         7  18.0  18.0  1.71 -0.125 -1.75    15.5  16.7  18.3  19.2
#> 3 8     qsec        14  16.8  16.9  1.20 -0.805 -0.919   14.5  16.1  17.2  17.6
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd  skew    kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     gear        11  4.09  4.11 0.539 0.115 -0.0106     3   4       4     4
#> 2 6     gear         7  3.86  3.86 0.690 0.106 -1.24       3   3.5     4     4
#> 3 8     gear        14  3.29  3.17 0.726 1.83   1.45       3   3       3     3
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd   skew  kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     carb        11  1.55  1.56 0.522 -0.158 -2.15     1  1      2       2
#> 2 6     carb         7  3.43  3.43 1.81  -0.261 -1.50     1  2.5    4       4
#> 3 8     carb        14  3.5   3.25 1.56   1.48   2.24     2  2.25   3.5     4
#> # ℹ 1 more variable: max <dbl>
fmtcars |> group_by(cyl, am) |> descript()
#> # A tibble: 6 × 14
#>   cyl   am     VARIABLE     n  mean  trim    sd    skew   kurt   min   P25   P50
#>   <fct> <fct>  <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 4     autom… mpg          3  22.9  22.9 1.45   0.0685 -2.33   21.5  22.2  22.8
#> 2 4     manual mpg          4  19.1  19.1 1.63   0.482  -1.91   17.8  18.0  18.6
#> 3 6     autom… mpg         12  15.0  15.1 2.77  -0.284  -0.964  10.4  14.0  15.2
#> 4 6     manual mpg          8  28.1  28.1 4.48  -0.208  -1.66   21.4  25.2  28.8
#> 5 8     autom… mpg          3  20.6  20.6 0.751 -0.385  -2.33   19.7  20.4  21  
#> 6 8     manual mpg          2  15.4  15.4 0.566  0      -2.75   15    15.2  15.4
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am       VARIABLE     n  mean  trim    sd   skew  kurt   min   P25   P50
#>   <fct> <fct>    <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     automat… disp         3 136.  136.  14.0  -0.309 -2.33 120.  130.  141. 
#> 2 4     manual   disp         4 205.  205.  44.7   0.168 -2.25 168.  168.  196. 
#> 3 6     automat… disp        12 358.  354.  71.8   0.303 -1.51 276.  297.  355  
#> 4 6     manual   disp         8  93.6  93.6 20.5   0.276 -1.89  71.1  78.0  87.0
#> 5 8     automat… disp         3 155   155    8.66 -0.385 -2.33 145   152.  160  
#> 6 8     manual   disp         2 326   326   35.4   0     -2.75 301   314.  326  
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am      VARIABLE     n  mean  trim    sd    skew  kurt   min   P25   P50
#>   <fct> <fct>   <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     automa… hp           3  84.7  84.7 19.7  -0.380  -2.33    62  78.5  95  
#> 2 4     manual  hp           4 115.  115.   9.18 -0.0940 -2.33   105 109.  116. 
#> 3 6     automa… hp          12 194.  194.  33.4   0.279  -1.44   150 175   180  
#> 4 6     manual  hp           8  81.9  81.9 22.7   0.137  -1.81    52  65.8  78.5
#> 5 8     automa… hp           3 132.  132.  37.5   0.385  -2.33   110 110   110  
#> 6 8     manual  hp           2 300.  300.  50.2   0      -2.75   264 282.  300. 
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am     VARIABLE     n  mean  trim    sd    skew   kurt   min   P25   P50
#>   <fct> <fct>  <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 4     autom… drat         3  3.77  3.77 0.13   0.382  -2.33   3.69  3.70  3.7 
#> 2 4     manual drat         4  3.42  3.42 0.592 -0.0926 -2.33   2.76  3     3.5 
#> 3 6     autom… drat        12  3.12  3.10 0.230  1.17    1.64   2.76  3.05  3.08
#> 4 6     manual drat         8  4.18  4.18 0.364  0.828  -0.472  3.77  4.02  4.10
#> 5 8     autom… drat         3  3.81  3.81 0.162 -0.385  -2.33   3.62  3.76  3.9 
#> 6 8     manual drat         2  3.88  3.88 0.481  0      -2.75   3.54  3.71  3.88
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am    VARIABLE     n  mean  trim    sd      skew  kurt   min   P25   P50
#>   <fct> <fct> <fct>    <dbl> <dbl> <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     auto… wt           3  2.94  2.94 0.408 -3.81e- 1 -2.33  2.46  2.81  3.15
#> 2 4     manu… wt           4  3.39  3.39 0.116 -7.35e- 1 -1.70  3.22  3.38  3.44
#> 3 6     auto… wt          12  4.10  4.04 0.768  8.54e- 1 -1.14  3.44  3.56  3.81
#> 4 6     manu… wt           8  2.04  2.04 0.409  3.49e- 1 -1.15  1.51  1.78  2.04
#> 5 8     auto… wt           3  2.76  2.76 0.128 -1.15e- 1 -2.33  2.62  2.70  2.77
#> 6 8     manu… wt           2  3.37  3.37 0.283 -1.15e-15 -2.75  3.17  3.27  3.37
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am        VARIABLE     n  mean  trim     sd      skew   kurt   min   P25
#>   <fct> <fct>     <fct>    <dbl> <dbl> <dbl>  <dbl>     <dbl>  <dbl> <dbl> <dbl>
#> 1 4     automatic qsec         3  21.0  21.0 1.67    3.85e- 1 -2.33   20    20.0
#> 2 4     manual    qsec         4  19.2  19.2 0.816   1.05e- 1 -2.02   18.3  18.8
#> 3 6     automatic qsec        12  17.1  17.2 0.802  -9.33e- 1 -0.338  15.4  17.0
#> 4 6     manual    qsec         8  18.4  18.4 1.13   -4.28e- 1 -1.39   16.7  18.1
#> 5 8     automatic qsec         3  16.3  16.3 0.769  -1.68e- 1 -2.33   15.5  16.0
#> 6 8     manual    qsec         2  14.6  14.6 0.0707 -1.89e-14 -2.75   14.5  14.5
#> # ℹ 3 more variables: P50 <dbl>, P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am     VARIABLE     n  mean  trim    sd    skew   kurt   min   P25   P50
#>   <fct> <fct>  <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 4     autom… gear         3  3.67  3.67 0.577  -0.385  -2.33     3   3.5   4  
#> 2 4     manual gear         4  3.5   3.5  0.577   0      -2.44     3   3     3.5
#> 3 6     autom… gear        12  3     3    0     NaN     NaN        3   3     3  
#> 4 6     manual gear         8  4.25  4.25 0.463   0.945  -1.21     4   4     4  
#> 5 8     autom… gear         3  4.33  4.33 0.577   0.385  -2.33     4   4     4  
#> 6 8     manual gear         2  5     5    0     NaN     NaN        5   5     5  
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am       VARIABLE     n  mean  trim    sd   skew  kurt   min   P25   P50
#>   <fct> <fct>    <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     automat… carb         3  1.67  1.67 0.577 -0.385 -2.33     1   1.5   2  
#> 2 4     manual   carb         4  2.5   2.5  1.73   0     -2.44     1   1     2.5
#> 3 6     automat… carb        12  3.08  3.1  0.900 -0.141 -1.85     2   2     3  
#> 4 6     manual   carb         8  1.5   1.5  0.535  0     -2.23     1   1     1.5
#> 5 8     automat… carb         3  4.67  4.67 1.15   0.385 -2.33     4   4     4  
#> 6 8     manual   carb         2  6     6    2.83   0     -2.75     4   5     6  
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
fmtcars |> group_by(cyl, am) |> select(mpg, wt) |> descript()
#> # A tibble: 6 × 14
#>   cyl   am     VARIABLE     n  mean  trim    sd    skew   kurt   min   P25   P50
#>   <fct> <fct>  <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 4     autom… mpg          3  22.9  22.9 1.45   0.0685 -2.33   21.5  22.2  22.8
#> 2 4     manual mpg          4  19.1  19.1 1.63   0.482  -1.91   17.8  18.0  18.6
#> 3 6     autom… mpg         12  15.0  15.1 2.77  -0.284  -0.964  10.4  14.0  15.2
#> 4 6     manual mpg          8  28.1  28.1 4.48  -0.208  -1.66   21.4  25.2  28.8
#> 5 8     autom… mpg          3  20.6  20.6 0.751 -0.385  -2.33   19.7  20.4  21  
#> 6 8     manual mpg          2  15.4  15.4 0.566  0      -2.75   15    15.2  15.4
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 14
#>   cyl   am    VARIABLE     n  mean  trim    sd      skew  kurt   min   P25   P50
#>   <fct> <fct> <fct>    <dbl> <dbl> <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     auto… wt           3  2.94  2.94 0.408 -3.81e- 1 -2.33  2.46  2.81  3.15
#> 2 4     manu… wt           4  3.39  3.39 0.116 -7.35e- 1 -1.70  3.22  3.38  3.44
#> 3 6     auto… wt          12  4.10  4.04 0.768  8.54e- 1 -1.14  3.44  3.56  3.81
#> 4 6     manu… wt           8  2.04  2.04 0.409  3.49e- 1 -1.15  1.51  1.78  2.04
#> 5 8     auto… wt           3  2.76  2.76 0.128 -1.15e- 1 -2.33  2.62  2.70  2.77
#> 6 8     manu… wt           2  3.37  3.37 0.283 -1.15e-15 -2.75  3.17  3.27  3.37
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>

# same, but formatting output by group instead of VARIABLE
fmtcars |> group_by(cyl) |> descript(by_group=TRUE)
#> cyl: 4
#> 
#> # A tibble: 8 × 12
#>   VARIABLE     n   mean   trim     sd     skew    kurt   min   P25    P50    P75
#>   <fct>    <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl> <dbl> <dbl>  <dbl>  <dbl>
#> 1 mpg         11  26.7   26.4   4.51   0.259   -1.65   21.4  22.8   26     30.4 
#> 2 disp        11 105.   104.   26.9    0.121   -1.64   71.1  78.8  108    121.  
#> 3 hp          11  82.6   82.7  20.9    0.00626 -1.71   52    65.5   91     96   
#> 4 drat        11   4.07   4.02  0.365  0.998    0.123   3.69  3.81   4.08   4.16
#> 5 wt          11   2.29   2.27  0.570  0.300   -1.36    1.51  1.88   2.2    2.62
#> 6 qsec        11  19.1   19.0   1.68   0.550   -0.0207 16.7  18.6   18.9   20.0 
#> 7 gear        11   4.09   4.11  0.539  0.115   -0.0106  3     4      4      4   
#> 8 carb        11   1.55   1.56  0.522 -0.158   -2.15    1     1      2      2   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> 
#> # A tibble: 8 × 12
#>   VARIABLE     n   mean   trim     sd   skew   kurt    min    P25    P50    P75
#>   <fct>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg          7  19.7   19.7   1.45  -0.158 -1.91   17.8   18.6   19.7   21   
#> 2 disp         7 183.   183.   41.6    0.795 -1.23  145    160    168.   196.  
#> 3 hp           7 122.   122.   24.3    1.36   0.249 105    110    110    123   
#> 4 drat         7   3.59   3.59  0.476 -0.736 -1.40    2.76   3.35   3.9    3.91
#> 5 wt           7   3.12   3.12  0.356 -0.222 -1.98    2.62   2.82   3.22   3.44
#> 6 qsec         7  18.0   18.0   1.71  -0.125 -1.75   15.5   16.7   18.3   19.2 
#> 7 gear         7   3.86   3.86  0.690  0.106 -1.24    3      3.5    4      4   
#> 8 carb         7   3.43   3.43  1.81  -0.261 -1.50    1      2.5    4      4   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> 
#> # A tibble: 8 × 12
#>   VARIABLE     n   mean   trim     sd   skew    kurt    min    P25    P50    P75
#>   <fct>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg         14  15.1   15.2   2.56  -0.363 -0.566   10.4   14.4   15.2   16.2 
#> 2 disp        14 353.   350.   67.8    0.453 -1.26   276.   302.   350.   390   
#> 3 hp          14 209.   204.   51.0    0.909  0.0921 150    176.   192.   241.  
#> 4 drat        14   3.23   3.19  0.372  1.34   1.08     2.76   3.07   3.12   3.22
#> 5 wt          14   4.00   3.95  0.759  0.988 -0.713    3.17   3.53   3.76   4.01
#> 6 qsec        14  16.8   16.9   1.20  -0.805 -0.919   14.5   16.1   17.2   17.6 
#> 7 gear        14   3.29   3.17  0.726  1.83   1.45     3      3      3      3   
#> 8 carb        14   3.5    3.25  1.56   1.48   2.24     2      2.25   3.5    4   
#> # ℹ 1 more variable: max <dbl>

# discrete variables also work with group_by()
fmtcars |> descript(discrete=TRUE)
#> VARIABLE: cyl
#> 
#>   count proportion
#> 4    11    0.34375
#> 6     7    0.21875
#> 8    14    0.43750
#> 
#> ------------------------------------------------------------
#>  
#> VARIABLE: vs
#> 
#>   count proportion
#> 0    18     0.5625
#> 1    14     0.4375
#> 
#> ------------------------------------------------------------
#>  
#> VARIABLE: am
#> 
#>           count proportion
#> automatic    19    0.59375
#> manual       13    0.40625
fmtcars |> group_by(cyl) |> descript(discrete=TRUE)
#> $COUNTS
#>    cyl
#> vs   4  6  8
#>   0  1  3 14
#>   1 10  4  0
#> 
#> $PROPORTIONS
#>    cyl
#> vs      4     6     8
#>   0 0.031 0.094 0.438
#>   1 0.312 0.125 0.000
#> 
#> 
#> ------------------------------------------------------------
#>  
#> $COUNTS
#>            cyl
#> am           4  6  8
#>   automatic  3  4 12
#>   manual     8  3  2
#> 
#> $PROPORTIONS
#>            cyl
#> am              4     6     8
#>   automatic 0.094 0.125 0.375
#>   manual    0.250 0.094 0.062
#> 
fmtcars |> group_by(am) |> descript(discrete=TRUE)
#> $COUNTS
#>    am
#> cyl automatic manual
#>   4         3      8
#>   6         4      3
#>   8        12      2
#> 
#> $PROPORTIONS
#>    am
#> cyl automatic manual
#>   4     0.094  0.250
#>   6     0.125  0.094
#>   8     0.375  0.062
#> 
#> 
#> ------------------------------------------------------------
#>  
#> $COUNTS
#>    am
#> vs  automatic manual
#>   0        12      6
#>   1         7      7
#> 
#> $PROPORTIONS
#>    am
#> vs  automatic manual
#>   0     0.375  0.188
#>   1     0.219  0.219
#> 
fmtcars |> group_by(cyl, am) |> descript(discrete=TRUE)
#> $COUNTS
#> , , am = automatic
#> 
#>    cyl
#> vs   4  6  8
#>   0  0  0 12
#>   1  3  4  0
#> 
#> , , am = manual
#> 
#>    cyl
#> vs   4  6  8
#>   0  1  3  2
#>   1  7  0  0
#> 
#> 
#> $PROPORTIONS
#> , , am = automatic
#> 
#>    cyl
#> vs      4     6     8
#>   0 0.000 0.000 0.375
#>   1 0.094 0.125 0.000
#> 
#> , , am = manual
#> 
#>    cyl
#> vs      4     6     8
#>   0 0.031 0.094 0.062
#>   1 0.219 0.000 0.000
#> 
#> 

# with single variables, typical dplyr::summarise() output returned
fmtcars |> select(mpg) |> descript()
#> # A tibble: 1 × 12
#>   VARIABLE     n  mean  trim    sd  skew   kurt   min   P25   P50   P75   max
#>   <fct>    <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg         32  20.1  19.7  6.03 0.611 -0.373  10.4  15.4  19.2  22.8  33.9
fmtcars |> group_by(cyl) |> select(mpg) |> descript()
#> # A tibble: 3 × 13
#>   cyl   VARIABLE     n  mean  trim    sd   skew   kurt   min   P25   P50   P75
#> * <fct> <fct>    <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     mpg         11  26.7  26.4  4.51  0.259 -1.65   21.4  22.8  26    30.4
#> 2 6     mpg          7  19.7  19.7  1.45 -0.158 -1.91   17.8  18.6  19.7  21  
#> 3 8     mpg         14  15.1  15.2  2.56 -0.363 -0.566  10.4  14.4  15.2  16.2
#> # ℹ 1 more variable: max <dbl>
fmtcars |> group_by(cyl, am) |> select(mpg) |> descript()
#> # A tibble: 6 × 14
#>   cyl   am     VARIABLE     n  mean  trim    sd    skew   kurt   min   P25   P50
#>   <fct> <fct>  <fct>    <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 4     autom… mpg          3  22.9  22.9 1.45   0.0685 -2.33   21.5  22.2  22.8
#> 2 4     manual mpg          4  19.1  19.1 1.63   0.482  -1.91   17.8  18.0  18.6
#> 3 6     autom… mpg         12  15.0  15.1 2.77  -0.284  -0.964  10.4  14.0  15.2
#> 4 6     manual mpg          8  28.1  28.1 4.48  -0.208  -1.66   21.4  25.2  28.8
#> 5 8     autom… mpg          3  20.6  20.6 0.751 -0.385  -2.33   19.7  20.4  21  
#> 6 8     manual mpg          2  15.4  15.4 0.566  0      -2.75   15    15.2  15.4
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>

# if you want a tibble from the list of information instead
fmtcars |> group_by(cyl) |> descript(collapse=TRUE)
#> # A tibble: 24 × 13
#>    cyl   VARIABLE     n   mean   trim     sd     skew    kurt    min    P25
#>    <fct> <fct>    <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl>  <dbl>  <dbl>
#>  1 4     mpg         11  26.7   26.4   4.51   0.259   -1.65    21.4   22.8 
#>  2 4     disp        11 105.   104.   26.9    0.121   -1.64    71.1   78.8 
#>  3 4     hp          11  82.6   82.7  20.9    0.00626 -1.71    52     65.5 
#>  4 4     drat        11   4.07   4.02  0.365  0.998    0.123    3.69   3.81
#>  5 4     wt          11   2.29   2.27  0.570  0.300   -1.36     1.51   1.88
#>  6 4     qsec        11  19.1   19.0   1.68   0.550   -0.0207  16.7   18.6 
#>  7 4     gear        11   4.09   4.11  0.539  0.115   -0.0106   3      4   
#>  8 4     carb        11   1.55   1.56  0.522 -0.158   -2.15     1      1   
#>  9 6     mpg          7  19.7   19.7   1.45  -0.158   -1.91    17.8   18.6 
#> 10 6     disp         7 183.   183.   41.6    0.795   -1.23   145    160   
#> # ℹ 14 more rows
#> # ℹ 3 more variables: P50 <dbl>, P75 <dbl>, max <dbl>
fmtcars |> group_by(cyl) |> descript(collapse=TRUE) |> arrange(VARIABLE)
#> # A tibble: 24 × 13
#>    cyl   VARIABLE     n   mean   trim     sd   skew    kurt    min    P25    P50
#>    <fct> <fct>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
#>  1 4     carb        11   1.55   1.56  0.522 -0.158 -2.15     1      1      2   
#>  2 6     carb         7   3.43   3.43  1.81  -0.261 -1.50     1      2.5    4   
#>  3 8     carb        14   3.5    3.25  1.56   1.48   2.24     2      2.25   3.5 
#>  4 4     disp        11 105.   104.   26.9    0.121 -1.64    71.1   78.8  108   
#>  5 6     disp         7 183.   183.   41.6    0.795 -1.23   145    160    168.  
#>  6 8     disp        14 353.   350.   67.8    0.453 -1.26   276.   302.   350.  
#>  7 4     drat        11   4.07   4.02  0.365  0.998  0.123    3.69   3.81   4.08
#>  8 6     drat         7   3.59   3.59  0.476 -0.736 -1.40     2.76   3.35   3.9 
#>  9 8     drat        14   3.23   3.19  0.372  1.34   1.08     2.76   3.07   3.12
#> 10 4     gear        11   4.09   4.11  0.539  0.115 -0.0106   3      4      4   
#> # ℹ 14 more rows
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
fmtcars |> group_by(am, cyl) |> select(mpg, wt) |> descript(collapse=TRUE)
#> # A tibble: 12 × 14
#>    am        cyl   VARIABLE     n  mean  trim    sd      skew   kurt   min   P25
#>    <fct>     <fct> <fct>    <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl> <dbl> <dbl>
#>  1 automatic 4     mpg          3 22.9  22.9  1.45   6.85e- 2 -2.33  21.5  22.2 
#>  2 automatic 4     wt           3  2.94  2.94 0.408 -3.81e- 1 -2.33   2.46  2.81
#>  3 manual    4     mpg          8 28.1  28.1  4.48  -2.08e- 1 -1.66  21.4  25.2 
#>  4 manual    4     wt           8  2.04  2.04 0.409  3.49e- 1 -1.15   1.51  1.78
#>  5 automatic 6     mpg          4 19.1  19.1  1.63   4.82e- 1 -1.91  17.8  18.0 
#>  6 automatic 6     wt           4  3.39  3.39 0.116 -7.35e- 1 -1.70   3.22  3.38
#>  7 manual    6     mpg          3 20.6  20.6  0.751 -3.85e- 1 -2.33  19.7  20.4 
#>  8 manual    6     wt           3  2.76  2.76 0.128 -1.15e- 1 -2.33   2.62  2.70
#>  9 automatic 8     mpg         12 15.0  15.1  2.77  -2.84e- 1 -0.964 10.4  14.0 
#> 10 automatic 8     wt          12  4.10  4.04 0.768  8.54e- 1 -1.14   3.44  3.56
#> 11 manual    8     mpg          2 15.4  15.4  0.566  0        -2.75  15    15.2 
#> 12 manual    8     wt           2  3.37  3.37 0.283 -1.15e-15 -2.75   3.17  3.27
#> # ℹ 3 more variables: P50 <dbl>, P75 <dbl>, max <dbl>
fmtcars |> group_by(am, cyl) |> select(mpg, wt) |>
  descript(collapse=TRUE) |> arrange(VARIABLE)
#> # A tibble: 12 × 14
#>    am        cyl   VARIABLE     n  mean  trim    sd      skew   kurt   min   P25
#>    <fct>     <fct> <fct>    <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl> <dbl> <dbl>
#>  1 automatic 4     mpg          3 22.9  22.9  1.45   6.85e- 2 -2.33  21.5  22.2 
#>  2 manual    4     mpg          8 28.1  28.1  4.48  -2.08e- 1 -1.66  21.4  25.2 
#>  3 automatic 6     mpg          4 19.1  19.1  1.63   4.82e- 1 -1.91  17.8  18.0 
#>  4 manual    6     mpg          3 20.6  20.6  0.751 -3.85e- 1 -2.33  19.7  20.4 
#>  5 automatic 8     mpg         12 15.0  15.1  2.77  -2.84e- 1 -0.964 10.4  14.0 
#>  6 manual    8     mpg          2 15.4  15.4  0.566  0        -2.75  15    15.2 
#>  7 automatic 4     wt           3  2.94  2.94 0.408 -3.81e- 1 -2.33   2.46  2.81
#>  8 manual    4     wt           8  2.04  2.04 0.409  3.49e- 1 -1.15   1.51  1.78
#>  9 automatic 6     wt           4  3.39  3.39 0.116 -7.35e- 1 -1.70   3.22  3.38
#> 10 manual    6     wt           3  2.76  2.76 0.128 -1.15e- 1 -2.33   2.62  2.70
#> 11 automatic 8     wt          12  4.10  4.04 0.768  8.54e- 1 -1.14   3.44  3.56
#> 12 manual    8     wt           2  3.37  3.37 0.283 -1.15e-15 -2.75   3.17  3.27
#> # ℹ 3 more variables: P50 <dbl>, P75 <dbl>, max <dbl>

# post-extraction (if you don't mind doing the extra computations
#   and extracting afterword)
fmtcars |> descript() |> select(VARIABLE, n, mean)
#> # A tibble: 8 × 3
#>   VARIABLE     n   mean
#>   <fct>    <dbl>  <dbl>
#> 1 mpg         32  20.1 
#> 2 disp        32 231.  
#> 3 hp          32 147.  
#> 4 drat        32   3.60
#> 5 wt          32   3.22
#> 6 qsec        32  17.8 
#> 7 gear        32   3.69
#> 8 carb        32   2.81
fmtcars |> select(mpg) |> descript() |> select(VARIABLE, n, mean)
#> # A tibble: 1 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 mpg         32  20.1
fmtcars |> group_by(cyl) |> select(mpg) |> descript() |>
  select(VARIABLE, n, mean)
#> # A tibble: 3 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 mpg         11  26.7
#> 2 mpg          7  19.7
#> 3 mpg         14  15.1
fmtcars |> group_by(cyl, am) |> descript() |> select(VARIABLE, n, mean)
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 mpg          3  22.9
#> 2 mpg          4  19.1
#> 3 mpg         12  15.0
#> 4 mpg          8  28.1
#> 5 mpg          3  20.6
#> 6 mpg          2  15.4
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 disp         3 136. 
#> 2 disp         4 205. 
#> 3 disp        12 358. 
#> 4 disp         8  93.6
#> 5 disp         3 155  
#> 6 disp         2 326  
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 hp           3  84.7
#> 2 hp           4 115. 
#> 3 hp          12 194. 
#> 4 hp           8  81.9
#> 5 hp           3 132. 
#> 6 hp           2 300. 
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 drat         3  3.77
#> 2 drat         4  3.42
#> 3 drat        12  3.12
#> 4 drat         8  4.18
#> 5 drat         3  3.81
#> 6 drat         2  3.88
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 wt           3  2.94
#> 2 wt           4  3.39
#> 3 wt          12  4.10
#> 4 wt           8  2.04
#> 5 wt           3  2.76
#> 6 wt           2  3.37
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 qsec         3  21.0
#> 2 qsec         4  19.2
#> 3 qsec        12  17.1
#> 4 qsec         8  18.4
#> 5 qsec         3  16.3
#> 6 qsec         2  14.6
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 gear         3  3.67
#> 2 gear         4  3.5 
#> 3 gear        12  3   
#> 4 gear         8  4.25
#> 5 gear         3  4.33
#> 6 gear         2  5   
#> 
#> ------------------------------------------------------------
#>  
#> # A tibble: 6 × 3
#>   VARIABLE     n  mean
#>   <fct>    <dbl> <dbl>
#> 1 carb         3  1.67
#> 2 carb         4  2.5 
#> 3 carb        12  3.08
#> 4 carb         8  1.5 
#> 5 carb         3  4.67
#> 6 carb         2  6   
fmtcars |> group_by(cyl) |> descript(collapse=TRUE) |>
  select(cyl, VARIABLE, n, mean)
#> # A tibble: 24 × 4
#>    cyl   VARIABLE     n   mean
#>    <fct> <fct>    <dbl>  <dbl>
#>  1 4     mpg         11  26.7 
#>  2 4     disp        11 105.  
#>  3 4     hp          11  82.6 
#>  4 4     drat        11   4.07
#>  5 4     wt          11   2.29
#>  6 4     qsec        11  19.1 
#>  7 4     gear        11   4.09
#>  8 4     carb        11   1.55
#>  9 6     mpg          7  19.7 
#> 10 6     disp         7 183.  
#> # ℹ 14 more rows

# only compute a subset of summary statistics
funs <- get_descriptFuns()
sfuns <- funs[c('n', 'mean', 'sd')] # subset
fmtcars |> descript(funs=sfuns) # only n, miss, mean, and sd
#> # A tibble: 8 × 4
#>   VARIABLE     n   mean      sd
#>   <fct>    <dbl>  <dbl>   <dbl>
#> 1 mpg         32  20.1    6.03 
#> 2 disp        32 231.   124.   
#> 3 hp          32 147.    68.6  
#> 4 drat        32   3.60   0.535
#> 5 wt          32   3.22   0.978
#> 6 qsec        32  17.8    1.79 
#> 7 gear        32   3.69   0.738
#> 8 carb        32   2.81   1.62 

# add a new functions
funs2 <- c(sfuns,
           trim_20 = \(x) mean(x, trim=.2, na.rm=TRUE),
           median= \(x) median(x, na.rm=TRUE))
fmtcars |> descript(funs=funs2)
#> # A tibble: 8 × 6
#>   VARIABLE     n   mean      sd trim_20 median
#>   <fct>    <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
#> 1 mpg         32  20.1    6.03    19.2   19.2 
#> 2 disp        32 231.   124.     219.   196.  
#> 3 hp          32 147.    68.6    138.   123   
#> 4 drat        32   3.60   0.535    3.58   3.70
#> 5 wt          32   3.22   0.978    3.20   3.32
#> 6 qsec        32  17.8    1.79    17.8   17.7 
#> 7 gear        32   3.69   0.738    3.55   4   
#> 8 carb        32   2.81   1.62     2.7    2   
```
