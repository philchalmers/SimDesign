# Suppress verbose function messages

This function is used to suppress information printed from external
functions that make internal use of
[`message`](https://rdrr.io/r/base/message.html) and
[`cat`](https://rdrr.io/r/base/cat.html), which provide information in
interactive R sessions. For simulations, the session is not interactive,
and therefore this type of output should be suppressed. For similar
behaviour for suppressing warning messages, see
[`manageWarnings`](http://philchalmers.github.io/SimDesign/reference/manageWarnings.md).

## Usage

``` r
quiet(..., cat = TRUE, keep = FALSE, attr.name = "quiet.messages")
```

## Arguments

- ...:

  the functional expression to be evaluated

- cat:

  logical; also capture calls from
  [`cat`](https://rdrr.io/r/base/cat.html)? If `FALSE` only
  [`message`](https://rdrr.io/r/base/message.html) will be suppressed

- keep:

  logical; return a character vector of the messages/concatenate and
  print strings as an attribute to the resulting object from
  `expr(...)`?

- attr.name:

  attribute name to use when `keep = TRUE`

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## See also

[`manageWarnings`](http://philchalmers.github.io/SimDesign/reference/manageWarnings.md)

## Examples

``` r
myfun <- function(x, warn=FALSE){
   message('This function is rather chatty')
   cat("It even prints in different output forms!\n")
   message('And even at different....')
   cat("...times!\n")
   if(warn)
     warning('It may even throw warnings!')
   x
}

out <- myfun(1)
#> This function is rather chatty
#> It even prints in different output forms!
#> And even at different....
#> ...times!
out
#> [1] 1

# tell the function to shhhh
out <- quiet(myfun(1))
out
#> [1] 1

# which messages are suppressed? Extract stored attribute
out <- quiet(myfun(1), keep = TRUE)
attr(out, 'quiet.messages')
#>                                   message.1 
#>          "This function is rather chatty\n" 
#>                                   message.2 
#>               "And even at different....\n" 
#>                                       cat.1 
#> "It even prints in different output forms!" 
#>                                       cat.2 
#>                                 "...times!" 

# Warning messages still get through (see manageWarnings(suppress)
#  for better alternative than using suppressWarnings())
out2 <- myfun(2, warn=TRUE) |> quiet() # warning gets through
#> Warning: It may even throw warnings!
out2
#> [1] 2

# suppress warning message explicitly, allowing others to be raised if present
myfun(2, warn=TRUE) |> quiet() |>
   manageWarnings(suppress='It may even throw warnings!')
#> [1] 2
```
