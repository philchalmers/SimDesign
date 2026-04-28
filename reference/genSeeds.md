# Generate random seeds

Generate seeds to be passed to `runSimulation`'s `seed` input. Values
are sampled from 1 to 2147483647, or are generated using L'Ecuyer-CMRG's
(2002) method (returning either a list if `arrayID` is omitted, or the
specific row value from this list if `arrayID` is included).

## Usage

``` r
genSeeds(design = 1L, iseed = NULL, arrayID = NULL, old.seeds = NULL)

gen_seeds(...)
```

## Arguments

- design:

  design matrix that requires a unique seed per condition, or a number
  indicating the number of seeds to generate. Default generates one
  number

- iseed:

  the initial `set.seed` number used to generate a sequence of
  independent seeds according to the L'Ecuyer-CMRG (2002) method. This
  is recommended whenever quality random number generation is required
  across similar (if not identical) simulation jobs (e.g., see
  [`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)).
  If `arrayID` is not specified then this will return a list of the
  associated seed for the full `design`

- arrayID:

  (optional) single integer input corresponding to the specific row in
  the `design` object when using the `iseed` input. This is used in
  functions such as
  [`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)
  to pull out the specific seed rather than manage a complete list, and
  is therefore more memory efficient

- old.seeds:

  (optional) vector or matrix of last seeds used in previous simulations
  to avoid repeating the same seed on a subsequent run. Note that this
  approach should be used sparingly as seeds set more frequently are
  more likely to correlate, and therefore provide less optimal random
  number behaviour (e.g., if performing a simulation on two runs to
  achieve 5000 \* 2 = 10,000 replications this is likely reasonable, but
  for simulations with 100 \* 2 = 200 replications this is more likely
  to be sub-optimal). Length must be equal to the number of rows in
  `design`

- ...:

  does nothing

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
# generate 1 seed (default)
genSeeds()
#> [1] 1243352586

# generate 5 unique seeds
genSeeds(5)
#> [1]  886678571  865383938 1718793689 1802720933  413067025

# generate from nrow(design)
design <- createDesign(factorA=c(1,2,3),
                       factorB=letters[1:3])
seeds <- genSeeds(design)
seeds
#> [1]  595688410 1809449992 1887503261 1000739687 1129367556  717434384 1123050453
#> [8]  474408704 1310164663

# construct new seeds that are independent from original (use this sparingly)
newseeds <- genSeeds(design, old.seeds=seeds)
newseeds
#> [1] 1607262655  390300427  731677967  298354824   27984649  225437218  746949893
#> [8] 1602927401  937942413

# can be done in batches too
newseeds2 <- genSeeds(design, old.seeds=cbind(seeds, newseeds))
cbind(seeds, newseeds, newseeds2) # all unique
#>            seeds   newseeds  newseeds2
#>  [1,]  595688410 1607262655  992053320
#>  [2,] 1809449992  390300427 1416086442
#>  [3,] 1887503261  731677967  823141978
#>  [4,] 1000739687  298354824 1246508874
#>  [5,] 1129367556   27984649 1619914930
#>  [6,]  717434384  225437218 1610796999
#>  [7,] 1123050453  746949893  187037883
#>  [8,]  474408704 1602927401 1156242821
#>  [9,] 1310164663  937942413 2079951069

############
# generate seeds for runArraySimulation()
(iseed <- genSeeds())  # initial seed
#> [1] 65720644
seed_list <- genSeeds(design, iseed=iseed)
seed_list
#> [[1]]
#> [1]       10407   379889883   659117664 -2055402783  1234182958  1566802391
#> [7]  1478353964
#> 
#> [[2]]
#> [1]       10407 -1996363988   993937887  1454248844 -1855571605  1976941944
#> [7] -1338551852
#> 
#> [[3]]
#> [1]       10407  -711630543  1740484843  1339480259  -153618978 -1565624872
#> [7]  1457706395
#> 
#> [[4]]
#> [1]       10407  -103215903 -1613204698   379013845 -1130009889   244099792
#> [7]   120529920
#> 
#> [[5]]
#> [1]       10407    67254529  1072668082  1044634035  -767520122 -2092261128
#> [7] -2098310082
#> 
#> [[6]]
#> [1]       10407   520068102 -1461006084 -1895507394  1038491043   168622900
#> [7] -1147596130
#> 
#> [[7]]
#> [1]       10407  1582176084  -312633288 -1779753696 -1577380533 -1192542975
#> [7]   626958622
#> 
#> [[8]]
#> [1]       10407  1481948650 -1459016231 -1192828505   957288152   959321237
#> [7]   791951982
#> 
#> [[9]]
#> [1]       10407  -249485550 -1214357330 -2043404912  1753729054  -154685538
#> [7]  1051389303
#> 
#> attr(,"iseed")
#> [1] 65720644

# expand number of unique seeds given iseed (e.g., in case more replications
# are required at a later date)
seed_list_tmp <- genSeeds(nrow(design)*2, iseed=iseed)
str(seed_list_tmp) # first 9 seeds identical to seed_list
#> List of 18
#>  $ : int [1:7] 10407 379889883 659117664 -2055402783 1234182958 1566802391 1478353964
#>  $ : int [1:7] 10407 -1996363988 993937887 1454248844 -1855571605 1976941944 -1338551852
#>  $ : int [1:7] 10407 -711630543 1740484843 1339480259 -153618978 -1565624872 1457706395
#>  $ : int [1:7] 10407 -103215903 -1613204698 379013845 -1130009889 244099792 120529920
#>  $ : int [1:7] 10407 67254529 1072668082 1044634035 -767520122 -2092261128 -2098310082
#>  $ : int [1:7] 10407 520068102 -1461006084 -1895507394 1038491043 168622900 -1147596130
#>  $ : int [1:7] 10407 1582176084 -312633288 -1779753696 -1577380533 -1192542975 626958622
#>  $ : int [1:7] 10407 1481948650 -1459016231 -1192828505 957288152 959321237 791951982
#>  $ : int [1:7] 10407 -249485550 -1214357330 -2043404912 1753729054 -154685538 1051389303
#>  $ : int [1:7] 10407 835808458 196552927 2117642140 149314186 338790750 1112575824
#>  $ : int [1:7] 10407 -1450304404 -2045409317 -179263388 -889219308 1191986309 -566625213
#>  $ : int [1:7] 10407 -688910850 1160691062 956749691 -422978313 2137821656 -29065073
#>  $ : int [1:7] 10407 1786743736 -1039513504 224186150 1629560031 503129039 1663133997
#>  $ : int [1:7] 10407 790977206 -2137758289 -566189168 229908201 -1823406113 -2016953102
#>  $ : int [1:7] 10407 -842368572 -658180239 -1250210600 1910856618 1975438294 2111869219
#>  $ : int [1:7] 10407 -1304052592 -41233767 800361080 -1382067640 -1349055971 2091568457
#>  $ : int [1:7] 10407 -450434814 307478060 1866031563 874517892 -825396283 2097801285
#>  $ : int [1:7] 10407 109698022 -156901408 144592825 2009040097 -1489056783 -200703622
#>  - attr(*, "iseed")= int 65720644

# more usefully for HPC, extract only the seed associated with an arrayID
arraySeed.15 <- genSeeds(nrow(design)*2, iseed=iseed, arrayID=15)
arraySeed.15
#> [[1]]
#> [1]       10407  -842368572  -658180239 -1250210600  1910856618  1975438294
#> [7]  2111869219
#> 
#> attr(,"arrayID")
#> [1] 15
#> attr(,"iseed")
#> [1] 65720644
```
