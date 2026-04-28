# Compute univariate descriptive statistics

Function returns univariate data summaries for each variable supplied.
For presentation purposes, discrete and continuous variables are treated
separately, the former of which reflects count/proportion information
while the ladder are supplied to a (customizable) list of univariate
summary functions. As such, quantitative/continuous variable information
is kept distinct in the output, while discrete variables (e.g.,
`factors` and `character` vectors) are returned by using the `discrete`
argument. When applicable a `"VARS"` column will be included in the
output to indicate which variable is being summarised on the respective
row.

## Usage

``` r
descript(df, funs = get_descriptFuns(), discrete = FALSE, collapse = FALSE)

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

- discrete:

  logical; include summary statistics for `discrete` variables only? If
  `TRUE` then only count and proportion information for the discrete
  variables will be returned. For greater flexibility in creating
  cross-tabulated count/proportion information see
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
#>    VARS      n    mean    trim      sd   skew    kurt   min    P25    P50    P75
#>    <fct> <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#>  1 mpg      32  20.1    19.7     6.03   0.611 -0.373  10.4   15.4   19.2   22.8 
#>  2 cyl      32   6.19    6.23    1.79  -0.175 -1.76    4      4      6      8   
#>  3 disp     32 231.    223.    124.     0.382 -1.21   71.1  121.   196.   326   
#>  4 hp       32 147.    141.     68.6    0.726 -0.136  52     96.5  123    180   
#>  5 drat     32   3.60    3.58    0.535  0.266 -0.715   2.76   3.08   3.70   3.92
#>  6 wt       32   3.22    3.15    0.978  0.423 -0.0227  1.51   2.58   3.32   3.61
#>  7 qsec     32  17.8    17.8     1.79   0.369  0.335  14.5   16.9   17.7   18.9 
#>  8 vs       32   0.438   0.423   0.504  0.240 -2.00    0      0      0      1   
#>  9 am       32   0.406   0.385   0.499  0.364 -1.92    0      0      0      1   
#> 10 gear     32   3.69    3.62    0.738  0.529 -1.07    3      3      4      4   
#> 11 carb     32   2.81    2.65    1.62   1.05   1.26    1      2      2      4   
#> # ℹ 1 more variable: max <dbl>
fmtcars |> descript()               # factors/discrete vars omitted
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim      sd  skew    kurt   min    P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>   <dbl> <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg      32  20.1   19.7    6.03  0.611 -0.373  10.4   15.4   19.2   22.8 
#> 2 disp     32 231.   223.   124.    0.382 -1.21   71.1  121.   196.   326   
#> 3 hp       32 147.   141.    68.6   0.726 -0.136  52     96.5  123    180   
#> 4 drat     32   3.60   3.58   0.535 0.266 -0.715   2.76   3.08   3.70   3.92
#> 5 wt       32   3.22   3.15   0.978 0.423 -0.0227  1.51   2.58   3.32   3.61
#> 6 qsec     32  17.8   17.8    1.79  0.369  0.335  14.5   16.9   17.7   18.9 
#> 7 gear     32   3.69   3.62   0.738 0.529 -1.07    3      3      4      4   
#> 8 carb     32   2.81   2.65   1.62  1.05   1.26    1      2      2      4   
#> # ℹ 1 more variable: max <dbl>
fmtcars |> descript(discrete=TRUE)  # discrete variables only
#> $cyl
#> # A tibble: 3 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 4         11      0.344
#> 2 6          7      0.219
#> 3 8         14      0.438
#> 
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0         18      0.562
#> 2 1         14      0.438
#> 
#> $am
#> # A tibble: 2 × 3
#>   values    count proportion
#>   <fct>     <int>      <dbl>
#> 1 automatic    19      0.594
#> 2 manual       13      0.406
#> 

# for discrete variables, xtabs() is generally nicer as cross-tabs can
# be specified explicitly (though can be cumbersome)
xtabs(~ am, fmtcars)
#> am
#> automatic    manual 
#>        19        13 
xtabs(~ am, fmtcars) |> prop.table()
#> am
#> automatic    manual 
#>   0.59375   0.40625 
xtabs(~ am + cyl + vs, fmtcars)
#> , , vs = 0
#> 
#>            cyl
#> am           4  6  8
#>   automatic  0  0 12
#>   manual     1  3  2
#> 
#> , , vs = 1
#> 
#>            cyl
#> am           4  6  8
#>   automatic  3  4  0
#>   manual     7  0  0
#> 
xtabs(~ am + cyl + vs, fmtcars) |> prop.table()
#> , , vs = 0
#> 
#>            cyl
#> am                4       6       8
#>   automatic 0.00000 0.00000 0.37500
#>   manual    0.03125 0.09375 0.06250
#> 
#> , , vs = 1
#> 
#>            cyl
#> am                4       6       8
#>   automatic 0.09375 0.12500 0.00000
#>   manual    0.21875 0.00000 0.00000
#> 

# usual pipe chaining
fmtcars |> select(mpg, wt) |> descript()
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd  skew    kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg      32 20.1  19.7  6.03  0.611 -0.373  10.4  15.4  19.2  22.8  33.9 
#> 2 wt       32  3.22  3.15 0.978 0.423 -0.0227  1.51  2.58  3.32  3.61  5.42
fmtcars |> subset(mpg > 20) |> select(mpg, wt) |> descript()
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd    skew  kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg      14 25.5  25.2  4.60   0.553  -1.38 21    21.4  23.6  29.6  33.9 
#> 2 wt       14  2.42  2.43 0.577 -0.0349 -1.47  1.51  1.99  2.39  2.85  3.22

# conditioning with group_by()
fmtcars |> group_by(cyl) |> descript()
#> cyl: 4
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim     sd     skew    kurt   min   P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl> <dbl> <dbl>  <dbl>  <dbl>
#> 1 mpg      11  26.7   26.4   4.51   0.259   -1.65   21.4  22.8   26     30.4 
#> 2 disp     11 105.   104.   26.9    0.121   -1.64   71.1  78.8  108    121.  
#> 3 hp       11  82.6   82.7  20.9    0.00626 -1.71   52    65.5   91     96   
#> 4 drat     11   4.07   4.02  0.365  0.998    0.123   3.69  3.81   4.08   4.16
#> 5 wt       11   2.29   2.27  0.570  0.300   -1.36    1.51  1.88   2.2    2.62
#> 6 qsec     11  19.1   19.0   1.68   0.550   -0.0207 16.7  18.6   18.9   20.0 
#> 7 gear     11   4.09   4.11  0.539  0.115   -0.0106  3     4      4      4   
#> 8 carb     11   1.55   1.56  0.522 -0.158   -2.15    1     1      2      2   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim     sd   skew   kurt    min    P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg       7  19.7   19.7   1.45  -0.158 -1.91   17.8   18.6   19.7   21   
#> 2 disp      7 183.   183.   41.6    0.795 -1.23  145    160    168.   196.  
#> 3 hp        7 122.   122.   24.3    1.36   0.249 105    110    110    123   
#> 4 drat      7   3.59   3.59  0.476 -0.736 -1.40    2.76   3.35   3.9    3.91
#> 5 wt        7   3.12   3.12  0.356 -0.222 -1.98    2.62   2.82   3.22   3.44
#> 6 qsec      7  18.0   18.0   1.71  -0.125 -1.75   15.5   16.7   18.3   19.2 
#> 7 gear      7   3.86   3.86  0.690  0.106 -1.24    3      3.5    4      4   
#> 8 carb      7   3.43   3.43  1.81  -0.261 -1.50    1      2.5    4      4   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim     sd   skew    kurt    min    P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg      14  15.1   15.2   2.56  -0.363 -0.566   10.4   14.4   15.2   16.2 
#> 2 disp     14 353.   350.   67.8    0.453 -1.26   276.   302.   350.   390   
#> 3 hp       14 209.   204.   51.0    0.909  0.0921 150    176.   192.   241.  
#> 4 drat     14   3.23   3.19  0.372  1.34   1.08     2.76   3.07   3.12   3.22
#> 5 wt       14   4.00   3.95  0.759  0.988 -0.713    3.17   3.53   3.76   4.01
#> 6 qsec     14  16.8   16.9   1.20  -0.805 -0.919   14.5   16.1   17.2   17.6 
#> 7 gear     14   3.29   3.17  0.726  1.83   1.45     3      3      3      3   
#> 8 carb     14   3.5    3.25  1.56   1.48   2.24     2      2.25   3.5    4   
#> # ℹ 1 more variable: max <dbl>
fmtcars |> group_by(cyl, am) |> descript()
#> cyl: 4
#> am: automatic
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim     sd    skew  kurt    min    P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg       3  22.9   22.9   1.45   0.0685 -2.33  21.5   22.2   22.8   23.6 
#> 2 disp      3 136.   136.   14.0   -0.309  -2.33 120.   130.   141.   144.  
#> 3 hp        3  84.7   84.7  19.7   -0.380  -2.33  62     78.5   95     96   
#> 4 drat      3   3.77   3.77  0.13   0.382  -2.33   3.69   3.70   3.7    3.81
#> 5 wt        3   2.94   2.94  0.408 -0.381  -2.33   2.46   2.81   3.15   3.17
#> 6 qsec      3  21.0   21.0   1.67   0.385  -2.33  20     20.0   20.0   21.5 
#> 7 gear      3   3.67   3.67  0.577 -0.385  -2.33   3      3.5    4      4   
#> 8 carb      3   1.67   1.67  0.577 -0.385  -2.33   1      1.5    2      2   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: automatic
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim     sd    skew  kurt    min    P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg       4  19.1   19.1   1.63   0.482  -1.91  17.8   18.0   18.6   19.8 
#> 2 disp      4 205.   205.   44.7    0.168  -2.25 168.   168.   196.   233.  
#> 3 hp        4 115.   115.    9.18  -0.0940 -2.33 105    109.   116.   123   
#> 4 drat      4   3.42   3.42  0.592 -0.0926 -2.33   2.76   3      3.5    3.92
#> 5 wt        4   3.39   3.39  0.116 -0.735  -1.70   3.22   3.38   3.44   3.44
#> 6 qsec      4  19.2   19.2   0.816  0.105  -2.02  18.3   18.8   19.2   19.6 
#> 7 gear      4   3.5    3.5   0.577  0      -2.44   3      3      3.5    4   
#> 8 carb      4   2.5    2.5   1.73   0      -2.44   1      1      2.5    4   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: automatic
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim     sd    skew    kurt    min    P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg      12  15.0   15.1   2.77   -0.284  -0.964  10.4   14.0   15.2   16.6 
#> 2 disp     12 358.   354.   71.8     0.303  -1.51  276.   297.   355    410   
#> 3 hp       12 194.   194.   33.4     0.279  -1.44  150    175    180    219.  
#> 4 drat     12   3.12   3.10  0.230   1.17    1.64    2.76   3.05   3.08   3.16
#> 5 wt       12   4.10   4.04  0.768   0.854  -1.14    3.44   3.56   3.81   4.36
#> 6 qsec     12  17.1   17.2   0.802  -0.933  -0.338  15.4   17.0   17.4   17.7 
#> 7 gear     12   3      3     0     NaN     NaN       3      3      3      3   
#> 8 carb     12   3.08   3.1   0.900  -0.141  -1.85    2      2      3      4   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 4
#> am: manual
#> # A tibble: 8 × 12
#>   VARS      n  mean  trim     sd   skew   kurt   min   P25   P50    P75    max
#>   <fct> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
#> 1 mpg       8 28.1  28.1   4.48  -0.208 -1.66  21.4  25.2  28.8   30.9   33.9 
#> 2 disp      8 93.6  93.6  20.5    0.276 -1.89  71.1  78.0  87.0  111.   121   
#> 3 hp        8 81.9  81.9  22.7    0.137 -1.81  52    65.8  78.5   97    113   
#> 4 drat      8  4.18  4.18  0.364  0.828 -0.472  3.77  4.02  4.10   4.27   4.93
#> 5 wt        8  2.04  2.04  0.409  0.349 -1.15   1.51  1.78  2.04   2.23   2.78
#> 6 qsec      8 18.4  18.4   1.13  -0.428 -1.39  16.7  18.1  18.6   19.0   19.9 
#> 7 gear      8  4.25  4.25  0.463  0.945 -1.21   4     4     4      4.25   5   
#> 8 carb      8  1.5   1.5   0.535  0     -2.23   1     1     1.5    2      2   
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: manual
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim     sd   skew  kurt    min    P25    P50    P75
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg       3  20.6   20.6   0.751 -0.385 -2.33  19.7   20.4   21     21   
#> 2 disp      3 155    155     8.66  -0.385 -2.33 145    152.   160    160   
#> 3 hp        3 132.   132.   37.5    0.385 -2.33 110    110    110    142.  
#> 4 drat      3   3.81   3.81  0.162 -0.385 -2.33   3.62   3.76   3.9    3.9 
#> 5 wt        3   2.76   2.76  0.128 -0.115 -2.33   2.62   2.70   2.77   2.82
#> 6 qsec      3  16.3   16.3   0.769 -0.168 -2.33  15.5   16.0   16.5   16.7 
#> 7 gear      3   4.33   4.33  0.577  0.385 -2.33   4      4      4      4.5 
#> 8 carb      3   4.67   4.67  1.15   0.385 -2.33   4      4      4      5   
#> # ℹ 1 more variable: max <dbl>
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: manual
#> # A tibble: 8 × 12
#>   VARS      n   mean   trim      sd       skew   kurt    min    P25    P50
#>   <fct> <dbl>  <dbl>  <dbl>   <dbl>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 mpg       2  15.4   15.4   0.566    0         -2.75  15     15.2   15.4 
#> 2 disp      2 326    326    35.4      0         -2.75 301    314.   326   
#> 3 hp        2 300.   300.   50.2      0         -2.75 264    282.   300.  
#> 4 drat      2   3.88   3.88  0.481    0         -2.75   3.54   3.71   3.88
#> 5 wt        2   3.37   3.37  0.283   -1.15e-15  -2.75   3.17   3.27   3.37
#> 6 qsec      2  14.6   14.6   0.0707  -1.89e-14  -2.75  14.5   14.5   14.6 
#> 7 gear      2   5      5     0      NaN        NaN      5      5      5   
#> 8 carb      2   6      6     2.83     0         -2.75   4      5      6   
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
fmtcars |> group_by(cyl, am) |> select(mpg, wt) |> descript()
#> cyl: 4
#> am: automatic
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd    skew  kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg       3 22.9  22.9  1.45   0.0685 -2.33 21.5  22.2  22.8  23.6  24.4 
#> 2 wt        3  2.94  2.94 0.408 -0.381  -2.33  2.46  2.81  3.15  3.17  3.19
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: automatic
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd   skew  kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg       4 19.1  19.1  1.63   0.482 -1.91 17.8  18.0  18.6  19.8  21.4 
#> 2 wt        4  3.39  3.39 0.116 -0.735 -1.70  3.22  3.38  3.44  3.44  3.46
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: automatic
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd   skew   kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg      12 15.0  15.1  2.77  -0.284 -0.964 10.4  14.0  15.2  16.6  19.2 
#> 2 wt       12  4.10  4.04 0.768  0.854 -1.14   3.44  3.56  3.81  4.36  5.42
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 4
#> am: manual
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd   skew  kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg       8 28.1  28.1  4.48  -0.208 -1.66 21.4  25.2  28.8  30.9  33.9 
#> 2 wt        8  2.04  2.04 0.409  0.349 -1.15  1.51  1.78  2.04  2.23  2.78
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: manual
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd   skew  kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg       3 20.6  20.6  0.751 -0.385 -2.33 19.7  20.4  21    21    21   
#> 2 wt        3  2.76  2.76 0.128 -0.115 -2.33  2.62  2.70  2.77  2.82  2.88
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: manual
#> # A tibble: 2 × 12
#>   VARS      n  mean  trim    sd      skew  kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 mpg       2 15.4  15.4  0.566  0        -2.75 15    15.2  15.4  15.6  15.8 
#> 2 wt        2  3.37  3.37 0.283 -1.15e-15 -2.75  3.17  3.27  3.37  3.47  3.57

# with single variables, typical dplyr::summarise() output returned
fmtcars |> select(mpg) |> descript()
#> # A tibble: 1 × 11
#>       n  mean  trim    sd  skew   kurt   min   P25   P50   P75   max
#>   <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    32  20.1  19.7  6.03 0.611 -0.373  10.4  15.4  19.2  22.8  33.9
fmtcars |> group_by(cyl) |> select(mpg) |> descript()
#> # A tibble: 3 × 12
#>   cyl       n  mean  trim    sd   skew   kurt   min   P25   P50   P75   max
#>   <fct> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4        11  26.7  26.4  4.51  0.259 -1.65   21.4  22.8  26    30.4  33.9
#> 2 6         7  19.7  19.7  1.45 -0.158 -1.91   17.8  18.6  19.7  21    21.4
#> 3 8        14  15.1  15.2  2.56 -0.363 -0.566  10.4  14.4  15.2  16.2  19.2
fmtcars |> group_by(cyl, am) |> select(mpg) |> descript()
#> # A tibble: 6 × 13
#>   cyl   am            n  mean  trim    sd    skew   kurt   min   P25   P50   P75
#>   <fct> <fct>     <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 4     automatic     3  22.9  22.9 1.45   0.0685 -2.33   21.5  22.2  22.8  23.6
#> 2 4     manual        4  19.1  19.1 1.63   0.482  -1.91   17.8  18.0  18.6  19.8
#> 3 6     automatic    12  15.0  15.1 2.77  -0.284  -0.964  10.4  14.0  15.2  16.6
#> 4 6     manual        8  28.1  28.1 4.48  -0.208  -1.66   21.4  25.2  28.8  30.9
#> 5 8     automatic     3  20.6  20.6 0.751 -0.385  -2.33   19.7  20.4  21    21  
#> 6 8     manual        2  15.4  15.4 0.566  0      -2.75   15    15.2  15.4  15.6
#> # ℹ 1 more variable: max <dbl>

# if you want a tibble from the list of information instead
fmtcars |> group_by(cyl) |> descript(collapse=TRUE)
#> # A tibble: 24 × 13
#>    cyl   VARS      n   mean   trim     sd     skew    kurt    min    P25    P50
#>    <fct> <fct> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
#>  1 4     mpg      11  26.7   26.4   4.51   0.259   -1.65    21.4   22.8   26   
#>  2 4     disp     11 105.   104.   26.9    0.121   -1.64    71.1   78.8  108   
#>  3 4     hp       11  82.6   82.7  20.9    0.00626 -1.71    52     65.5   91   
#>  4 4     drat     11   4.07   4.02  0.365  0.998    0.123    3.69   3.81   4.08
#>  5 4     wt       11   2.29   2.27  0.570  0.300   -1.36     1.51   1.88   2.2 
#>  6 4     qsec     11  19.1   19.0   1.68   0.550   -0.0207  16.7   18.6   18.9 
#>  7 4     gear     11   4.09   4.11  0.539  0.115   -0.0106   3      4      4   
#>  8 4     carb     11   1.55   1.56  0.522 -0.158   -2.15     1      1      2   
#>  9 6     mpg       7  19.7   19.7   1.45  -0.158   -1.91    17.8   18.6   19.7 
#> 10 6     disp      7 183.   183.   41.6    0.795   -1.23   145    160    168.  
#> # ℹ 14 more rows
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>
fmtcars |> group_by(am, cyl) |> select(mpg, wt) |> descript(collapse=TRUE)
#> # A tibble: 12 × 14
#>    am     cyl   VARS      n  mean  trim    sd      skew   kurt   min   P25   P50
#>    <fct>  <fct> <fct> <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl> <dbl> <dbl> <dbl>
#>  1 autom… 4     mpg       3 22.9  22.9  1.45   6.85e- 2 -2.33  21.5  22.2  22.8 
#>  2 autom… 4     wt        3  2.94  2.94 0.408 -3.81e- 1 -2.33   2.46  2.81  3.15
#>  3 manual 4     mpg       8 28.1  28.1  4.48  -2.08e- 1 -1.66  21.4  25.2  28.8 
#>  4 manual 4     wt        8  2.04  2.04 0.409  3.49e- 1 -1.15   1.51  1.78  2.04
#>  5 autom… 6     mpg       4 19.1  19.1  1.63   4.82e- 1 -1.91  17.8  18.0  18.6 
#>  6 autom… 6     wt        4  3.39  3.39 0.116 -7.35e- 1 -1.70   3.22  3.38  3.44
#>  7 manual 6     mpg       3 20.6  20.6  0.751 -3.85e- 1 -2.33  19.7  20.4  21   
#>  8 manual 6     wt        3  2.76  2.76 0.128 -1.15e- 1 -2.33   2.62  2.70  2.77
#>  9 autom… 8     mpg      12 15.0  15.1  2.77  -2.84e- 1 -0.964 10.4  14.0  15.2 
#> 10 autom… 8     wt       12  4.10  4.04 0.768  8.54e- 1 -1.14   3.44  3.56  3.81
#> 11 manual 8     mpg       2 15.4  15.4  0.566  0        -2.75  15    15.2  15.4 
#> 12 manual 8     wt        2  3.37  3.37 0.283 -1.15e-15 -2.75   3.17  3.27  3.37
#> # ℹ 2 more variables: P75 <dbl>, max <dbl>

# post-extraction (if you don't mind doing the extra computations
#   and extracting afterword)
fmtcars |> descript() |> select(n, mean)
#> # A tibble: 8 × 2
#>       n   mean
#>   <dbl>  <dbl>
#> 1    32  20.1 
#> 2    32 231.  
#> 3    32 147.  
#> 4    32   3.60
#> 5    32   3.22
#> 6    32  17.8 
#> 7    32   3.69
#> 8    32   2.81
fmtcars |> select(mpg) |> descript() |> select(n, mean)
#> # A tibble: 1 × 2
#>       n  mean
#>   <dbl> <dbl>
#> 1    32  20.1
fmtcars |> group_by(cyl) |> select(mpg) |> descript() |> select(n, mean)
#> # A tibble: 3 × 2
#>       n  mean
#>   <dbl> <dbl>
#> 1    11  26.7
#> 2     7  19.7
#> 3    14  15.1
fmtcars |> group_by(cyl, am) |> descript() |> select(n, mean)
#> cyl: 4
#> am: automatic
#> # A tibble: 8 × 2
#>       n   mean
#>   <dbl>  <dbl>
#> 1     3  22.9 
#> 2     3 136.  
#> 3     3  84.7 
#> 4     3   3.77
#> 5     3   2.94
#> 6     3  21.0 
#> 7     3   3.67
#> 8     3   1.67
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: automatic
#> # A tibble: 8 × 2
#>       n   mean
#>   <dbl>  <dbl>
#> 1     4  19.1 
#> 2     4 205.  
#> 3     4 115.  
#> 4     4   3.42
#> 5     4   3.39
#> 6     4  19.2 
#> 7     4   3.5 
#> 8     4   2.5 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: automatic
#> # A tibble: 8 × 2
#>       n   mean
#>   <dbl>  <dbl>
#> 1    12  15.0 
#> 2    12 358.  
#> 3    12 194.  
#> 4    12   3.12
#> 5    12   4.10
#> 6    12  17.1 
#> 7    12   3   
#> 8    12   3.08
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 4
#> am: manual
#> # A tibble: 8 × 2
#>       n  mean
#>   <dbl> <dbl>
#> 1     8 28.1 
#> 2     8 93.6 
#> 3     8 81.9 
#> 4     8  4.18
#> 5     8  2.04
#> 6     8 18.4 
#> 7     8  4.25
#> 8     8  1.5 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: manual
#> # A tibble: 8 × 2
#>       n   mean
#>   <dbl>  <dbl>
#> 1     3  20.6 
#> 2     3 155   
#> 3     3 132.  
#> 4     3   3.81
#> 5     3   2.76
#> 6     3  16.3 
#> 7     3   4.33
#> 8     3   4.67
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: manual
#> # A tibble: 8 × 2
#>       n   mean
#>   <dbl>  <dbl>
#> 1     2  15.4 
#> 2     2 326   
#> 3     2 300.  
#> 4     2   3.88
#> 5     2   3.37
#> 6     2  14.6 
#> 7     2   5   
#> 8     2   6   
fmtcars |> group_by(cyl) |> descript(collapse=TRUE) |>
  select(cyl, VARS, n, mean)
#> # A tibble: 24 × 4
#>    cyl   VARS      n   mean
#>    <fct> <fct> <dbl>  <dbl>
#>  1 4     mpg      11  26.7 
#>  2 4     disp     11 105.  
#>  3 4     hp       11  82.6 
#>  4 4     drat     11   4.07
#>  5 4     wt       11   2.29
#>  6 4     qsec     11  19.1 
#>  7 4     gear     11   4.09
#>  8 4     carb     11   1.55
#>  9 6     mpg       7  19.7 
#> 10 6     disp      7 183.  
#> # ℹ 14 more rows

# discrete variables also work with group_by(), though again
#  xtabs() is generally more flexible
fmtcars |> group_by(cyl) |> descript(discrete=TRUE)
#> cyl: 4
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          1     0.0909
#> 2 1         10     0.909 
#> 
#> $am
#> # A tibble: 2 × 3
#>   values    count proportion
#>   <fct>     <int>      <dbl>
#> 1 automatic     3      0.273
#> 2 manual        8      0.727
#> 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          3      0.429
#> 2 1          4      0.571
#> 
#> $am
#> # A tibble: 2 × 3
#>   values    count proportion
#>   <fct>     <int>      <dbl>
#> 1 automatic     4      0.571
#> 2 manual        3      0.429
#> 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0         14          1
#> 2 1          0          0
#> 
#> $am
#> # A tibble: 2 × 3
#>   values    count proportion
#>   <fct>     <int>      <dbl>
#> 1 automatic    12      0.857
#> 2 manual        2      0.143
#> 
fmtcars |> group_by(am) |> descript(discrete=TRUE)
#> am: automatic
#> $cyl
#> # A tibble: 3 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 4          3      0.158
#> 2 6          4      0.211
#> 3 8         12      0.632
#> 
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0         12      0.632
#> 2 1          7      0.368
#> 
#> 
#> ------------------------------------------------------------
#>  
#> am: manual
#> $cyl
#> # A tibble: 3 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 4          8      0.615
#> 2 6          3      0.231
#> 3 8          2      0.154
#> 
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          6      0.462
#> 2 1          7      0.538
#> 
fmtcars |> group_by(cyl, am) |> descript(discrete=TRUE)
#> cyl: 4
#> am: automatic
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          0          0
#> 2 1          3          1
#> 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: automatic
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          0          0
#> 2 1          4          1
#> 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: automatic
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0         12          1
#> 2 1          0          0
#> 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 4
#> am: manual
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          1      0.125
#> 2 1          7      0.875
#> 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 6
#> am: manual
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          3          1
#> 2 1          0          0
#> 
#> 
#> ------------------------------------------------------------
#>  
#> cyl: 8
#> am: manual
#> $vs
#> # A tibble: 2 × 3
#>   values count proportion
#>   <fct>  <int>      <dbl>
#> 1 0          2          1
#> 2 1          0          0
#> 

# only compute a subset of summary statistics
funs <- get_descriptFuns()
sfuns <- funs[c('n', 'mean', 'sd')] # subset
fmtcars |> descript(funs=sfuns) # only n, miss, mean, and sd
#> # A tibble: 8 × 4
#>   VARS      n   mean      sd
#>   <fct> <dbl>  <dbl>   <dbl>
#> 1 mpg      32  20.1    6.03 
#> 2 disp     32 231.   124.   
#> 3 hp       32 147.    68.6  
#> 4 drat     32   3.60   0.535
#> 5 wt       32   3.22   0.978
#> 6 qsec     32  17.8    1.79 
#> 7 gear     32   3.69   0.738
#> 8 carb     32   2.81   1.62 

# add a new functions
funs2 <- c(sfuns,
           trim_20 = \(x) mean(x, trim=.2, na.rm=TRUE),
           median= \(x) median(x, na.rm=TRUE))
fmtcars |> descript(funs=funs2)
#> # A tibble: 8 × 6
#>   VARS      n   mean      sd trim_20 median
#>   <fct> <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
#> 1 mpg      32  20.1    6.03    19.2   19.2 
#> 2 disp     32 231.   124.     219.   196.  
#> 3 hp       32 147.    68.6    138.   123   
#> 4 drat     32   3.60   0.535    3.58   3.70
#> 5 wt       32   3.22   0.978    3.20   3.32
#> 6 qsec     32  17.8    1.79    17.8   17.7 
#> 7 gear     32   3.69   0.738    3.55   4   
#> 8 carb     32   2.81   1.62     2.7    2   
```
