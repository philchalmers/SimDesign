# Auto-named Concatenation of Vector or List

This is a wrapper to the function [`c`](https://rdrr.io/r/base/c.html),
however names the respective elements according to their input object
name. For this reason, nesting `nc()` calls is not recommended (joining
independent `nc()` calls via [`c()`](https://rdrr.io/r/base/c.html) is
however reasonable).

## Usage

``` r
nc(..., use.names = FALSE, error.on.duplicate = TRUE)
```

## Arguments

- ...:

  objects to be concatenated

- use.names:

  logical indicating if `names` should be preserved (unlike
  [`c`](https://rdrr.io/r/base/c.html), default is `FALSE`)

- error.on.duplicate:

  logical; if the same object name appears in the returning object
  should an error be thrown? Default is `TRUE`

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## Examples

``` r
A <- 1
B <- 2
C <- 3

names(C) <- 'LetterC'

# compare the following
c(A, B, C) # unnamed
#>                 LetterC 
#>       1       2       3 

nc(A, B, C) # named
#> A B C 
#> 1 2 3 
nc(this=A, B, C) # respects override named (same as c() )
#> this    B    C 
#>    1    2    3 
nc(this=A, B, C, use.names = TRUE) # preserve original name
#>    this       B LetterC 
#>       1       2       3 

if (FALSE) { # \dontrun{
# throws errors if names not unique
nc(this=A, this=B, C)
nc(LetterC=A, B, C, use.names=TRUE)
} # }

# poor input choice names
nc(t.test(c(1:2))$p.value, t.test(c(3:4))$p.value)
#> t.test(c(1:2))_p.value t.test(c(3:4))_p.value 
#>             0.20483276             0.09033447 

# better to explicitly provide name
nc(T1 = t.test(c(1:2))$p.value,
   T2 = t.test(c(3:4))$p.value)
#>         T1         T2 
#> 0.20483276 0.09033447 

# vector of unnamed inputs
A <- c(5,4,3,2,1)
B <- c(100, 200)

nc(A, B, C) # A's and B's numbered uniquely
#>  A1  A2  A3  A4  A5  B1  B2   C 
#>   5   4   3   2   1 100 200   3 
c(A, B, C)  # compare
#>                                                         LetterC 
#>       5       4       3       2       1     100     200       3 
nc(beta=A, B, C) # replacement of object name
#> beta1 beta2 beta3 beta4 beta5    B1    B2     C 
#>     5     4     3     2     1   100   200     3 

# retain names attributes (but append object name, when appropriate)
names(A) <- letters[1:5]
nc(A, B, C)
#> A.a A.b A.c A.d A.e  B1  B2   C 
#>   5   4   3   2   1 100 200   3 
nc(beta=A, B, C)
#> beta.a beta.b beta.c beta.d beta.e     B1     B2      C 
#>      5      4      3      2      1    100    200      3 
nc(A, B, C, use.names=TRUE)
#>       a       b       c       d       e      B1      B2 LetterC 
#>       5       4       3       2       1     100     200       3 

# mix and match if some named elements work while others do not
c( nc(A, B, use.names=TRUE), nc(C))
#>   a   b   c   d   e  B1  B2   C 
#>   5   4   3   2   1 100 200   3 

if (FALSE) { # \dontrun{
# error, 'b' appears twice
names(B) <- c('b', 'b2')
nc(A, B, C, use.names=TRUE)
} # }

# List input
A <- list(1)
B <- list(2:3)
C <- list('C')

names(C) <- 'LetterC'

# compare the following
c(A, B, C) # unnamed
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2 3
#> 
#> $LetterC
#> [1] "C"
#> 

nc(A, B, C) # named
#> $A
#> [1] 1
#> 
#> $B
#> [1] 2 3
#> 
#> $C
#> [1] "C"
#> 
nc(this=A, B, C) # respects override named (same as c() and list() )
#> $this
#> [1] 1
#> 
#> $B
#> [1] 2 3
#> 
#> $C
#> [1] "C"
#> 
nc(this=A, B, C, use.names = TRUE) # preserve original name
#> $this
#> [1] 1
#> 
#> $B
#> [1] 2 3
#> 
#> $LetterC
#> [1] "C"
#> 

```
