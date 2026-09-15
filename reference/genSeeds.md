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
#> [1] 1662459869

# generate 5 unique seeds
genSeeds(5)
#> [1] 1114065245 1912884359  315667258   63294833 2016034125

# generate from nrow(design)
design <- createDesign(factorA=c(1,2,3),
                       factorB=letters[1:3])
seeds <- genSeeds(design)
seeds
#> [1] 1432773678  902250067  327553800 2001139248 1563961203  605052329  162384174
#> [8]  154295365   59480506

# construct new seeds that are independent from original (use this sparingly)
newseeds <- genSeeds(design, old.seeds=seeds)
newseeds
#> [1] 1164878856  565750889 1481628660 1622107676  370609804 1359484302 2015035486
#> [8] 1892714227 1077988445

# can be done in batches too
newseeds2 <- genSeeds(design, old.seeds=cbind(seeds, newseeds))
cbind(seeds, newseeds, newseeds2) # all unique
#>            seeds   newseeds  newseeds2
#>  [1,] 1432773678 1164878856  690973054
#>  [2,]  902250067  565750889 1202609637
#>  [3,]  327553800 1481628660  290466644
#>  [4,] 2001139248 1622107676 1056690307
#>  [5,] 1563961203  370609804  984753781
#>  [6,]  605052329 1359484302 1587556041
#>  [7,]  162384174 2015035486 1310488415
#>  [8,]  154295365 1892714227  713725858
#>  [9,]   59480506 1077988445 1310472238

############
# generate seeds for runArraySimulation()
(iseed <- genSeeds())  # initial seed
#> [1] 1686542805
seed_list <- genSeeds(design, iseed=iseed)
seed_list
#> [[1]]
#> [1]       10407  2049157184  1180236609  -716931058 -1033289417  1204814860
#> [7]   366205341
#> 
#> [[2]]
#> [1]       10407  -508976853 -1076629573  -984224672   914252381 -1943912042
#> [7] -1140907484
#> 
#> [[3]]
#> [1]       10407 -2100630608  1225764234 -1020158595  -240468293  1610323385
#> [7]   300012134
#> 
#> [[4]]
#> [1]      10407  469457161 -879925944 -560424266 -719149884  308605257  409787536
#> 
#> [[5]]
#> [1]      10407 1242417686   68302964 1716537776 -792542308 -904536116  -96157985
#> 
#> [[6]]
#> [1]       10407 -1094368526   605222020    94249243  1797900323  1976353746
#> [7]  1874406403
#> 
#> [[7]]
#> [1]       10407  2128484416  -193024737  1084373940   592053008  1497180319
#> [7] -1956221037
#> 
#> [[8]]
#> [1]       10407 -1267613802 -1652712232    21151792 -1362062596   709437476
#> [7] -1342040891
#> 
#> [[9]]
#> [1]       10407   640749281 -1498833341   492290921   361806758 -1527993395
#> [7]   979995451
#> 
#> attr(,"iseed")
#> [1] 1686542805

# expand number of unique seeds given iseed (e.g., in case more replications
# are required at a later date)
seed_list_tmp <- genSeeds(nrow(design)*2, iseed=iseed)
str(seed_list_tmp) # first 9 seeds identical to seed_list
#> List of 18
#>  $ : int [1:7] 10407 2049157184 1180236609 -716931058 -1033289417 1204814860 366205341
#>  $ : int [1:7] 10407 -508976853 -1076629573 -984224672 914252381 -1943912042 -1140907484
#>  $ : int [1:7] 10407 -2100630608 1225764234 -1020158595 -240468293 1610323385 300012134
#>  $ : int [1:7] 10407 469457161 -879925944 -560424266 -719149884 308605257 409787536
#>  $ : int [1:7] 10407 1242417686 68302964 1716537776 -792542308 -904536116 -96157985
#>  $ : int [1:7] 10407 -1094368526 605222020 94249243 1797900323 1976353746 1874406403
#>  $ : int [1:7] 10407 2128484416 -193024737 1084373940 592053008 1497180319 -1956221037
#>  $ : int [1:7] 10407 -1267613802 -1652712232 21151792 -1362062596 709437476 -1342040891
#>  $ : int [1:7] 10407 640749281 -1498833341 492290921 361806758 -1527993395 979995451
#>  $ : int [1:7] 10407 1641482172 -1876790812 1538583098 -2020925973 698287475 -729584957
#>  $ : int [1:7] 10407 -1127431163 -238811372 578263651 -2056464543 66239038 -639057935
#>  $ : int [1:7] 10407 -2084384580 -2061704252 -1982166570 -1926427387 765709079 1240826300
#>  $ : int [1:7] 10407 817939500 -1070252420 610829131 146610572 2055911211 2146884578
#>  $ : int [1:7] 10407 -1747767347 1933804850 174086894 1746617083 968564595 420612480
#>  $ : int [1:7] 10407 1549693448 780962451 -1034409666 -469972278 -985334608 131694361
#>  $ : int [1:7] 10407 411194930 -562323519 -1435905957 1874601169 2026478697 660333181
#>  $ : int [1:7] 10407 1311942179 -1446305600 335501177 -2050336993 1984954862 -414370379
#>  $ : int [1:7] 10407 -79764087 155154665 -469182132 2003821367 -1384318929 -1813323225
#>  - attr(*, "iseed")= int 1686542805

# more usefully for HPC, extract only the seed associated with an arrayID
arraySeed.15 <- genSeeds(nrow(design)*2, iseed=iseed, arrayID=15)
arraySeed.15
#> [[1]]
#> [1]       10407  1549693448   780962451 -1034409666  -469972278  -985334608
#> [7]   131694361
#> 
#> attr(,"arrayID")
#> [1] 15
#> attr(,"iseed")
#> [1] 1686542805
```
