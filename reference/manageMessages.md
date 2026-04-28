# Increase the intensity or suppress the output of an observed message

Provides more nuanced management of known message outputs messages that
appear in function calls outside the front-end users control (e.g.,
functions written in third-party packages). Specifically, this function
provides a less nuclear approach than
[`quiet`](http://philchalmers.github.io/SimDesign/reference/quiet.md)
and friends, which suppresses all `cat` and `message`s raised, and
instead allows for specific messages to be raised either to warnings or,
even more extremely, to errors. Note that for messages that are not
suppressed the order with which the output and message calls appear in
the original function is not retained.

## Usage

``` r
manageMessages(
  expr,
  allow = NULL,
  message2warning = NULL,
  message2error = NULL,
  ...
)
```

## Arguments

- expr:

  expression to be evaluated (e.g., ret \<- `myfun(args)`). Function
  should either be used as a wrapper, such as
  `manageMassages(ret <- myfun(args), ...)` or
  `ret <- manageMassages(myfun(args), ...)`, or more readably as a pipe,
  `ret <- myfun(args) |> manageMassages(...)`

- allow:

  (optional) a `character` vector indicating messages that should still
  appear, while all other messages should remain suppressed. Each
  supplied message is matched using a
  [`grepl`](https://rdrr.io/r/base/grep.html) expression, so partial
  matching is supported (though more specific messages are less likely
  to throw false positives). If `NULL`, all messages will be suppressed
  unless they appear in `message2error` or `message2warning`

- message2warning:

  (optional) Input can be a `character` vector containing messages that
  should probably be considered warning messages for the current
  application instead. Each supplied `character` vector element is
  matched using a [`grepl`](https://rdrr.io/r/base/grep.html)
  expression, so partial matching is supported (though more specific
  messages are less likely to throw false positives).

- message2error:

  (optional) Input can be a `character` vector containing
  known-to-be-severe messages that should be converted to errors for the
  current application. See `message2warning` for details.

- ...:

  additional arguments passed to
  [`grepl`](https://rdrr.io/r/base/grep.html)

## Value

returns the original result of `eval(expr)`, with warning messages
either left the same, increased to errors, or suppressed (depending on
the input specifications)

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

## See also

[`manageWarnings`](http://philchalmers.github.io/SimDesign/reference/manageWarnings.md),
[`quiet`](http://philchalmers.github.io/SimDesign/reference/quiet.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

myfun <- function(x, warn=FALSE){
   message('This function is rather chatty')
   cat("It even prints in different output forms!\n")
   message('And even at different ')
   cat(" many times!\n")
   cat("Too many messages can be annoying \n")
   if(warn)
     warning('It may even throw warnings ')
   x
}

out <- myfun(1)
out

# tell the function to shhhh
out <- quiet(myfun(1))
out

# same default behaviour as quiet(), but potential for nuance
out2 <- manageMessages(myfun(1))
identical(out, out2)

# allow some messages to still get printed
out2 <- manageMessages(myfun(1), allow = "many times!")
out2 <- manageMessages(myfun(1), allow = "This function is rather chatty")

# note: . matches single character (regex)
out2 <- manageMessages(myfun(1), allow = c("many times.",
                                           "This function is rather chatty"))

# convert specific message to warning
out3 <- manageMessages(myfun(1), message2warning = "many times!")
identical(out, out3)

# other warnings also get through
out3 <- manageMessages(myfun(1, warn=TRUE), message2warning = "times!")
identical(out, out3)

# convert message to error
manageMessages(myfun(1), message2error = "m... times!")

# multiple message intensity changes
manageMessages(myfun(1),
  message2warning = "It even prints in different output forms",
  message2error = "many times!")

manageMessages(myfun(1),
  allow = c("This function is rather chatty",
            "Too many messages can be annoying"),
  message2warning = "It even prints in different output forms",
  message2error = "many times!")

} # }
```
