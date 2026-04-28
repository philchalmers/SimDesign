# Generate a basic Monte Carlo simulation GUI template

This function generates suitable stand-alone code from the `shiny`
package to create simple web-interfaces for performing single condition
Monte Carlo simulations. The template generated is relatively
minimalistic, but allows the user to quickly and easily edit the saved
files to customize the associated shiny elements as they see fit.

## Usage

``` r
SimShiny(filename = NULL, dir = getwd(), design, ...)
```

## Arguments

- filename:

  an optional name of a text file to save the server and UI components
  (e.g., 'mysimGUI.R'). If omitted, the code will be printed to the R
  console instead

- dir:

  the directory to write the files to. Default is the working directory

- design:

  `design` object from
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

- ...:

  arguments to be passed to
  [`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md).
  Note that the `design` object is not used directly, and instead
  provides options to be selected in the GUI

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

[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

Design <- createDesign(sample_size = c(30, 60, 90, 120),
                       group_size_ratio = c(1, 4, 8),
                       standard_deviation_ratio = c(.5, 1, 2))

Generate <- function(condition, fixed_objects) {
    N <- condition$sample_size
    grs <- condition$group_size_ratio
    sd <- condition$standard_deviation_ratio
    if(grs < 1){
        N2 <- N / (1/grs + 1)
        N1 <- N - N2
    } else {
        N1 <- N / (grs + 1)
        N2 <- N - N1
    }
    group1 <- rnorm(N1)
    group2 <- rnorm(N2, sd=sd)
    dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
    dat
}

Analyse <- function(condition, dat, fixed_objects) {
    welch <- t.test(DV ~ group, dat)
    ind <- t.test(DV ~ group, dat, var.equal=TRUE)

    # In this function the p values for the t-tests are returned,
    #  and make sure to name each element, for future reference
    ret <- c(welch = welch$p.value, independent = ind$p.value)
    ret
}

Summarise <- function(condition, results, fixed_objects) {
    #find results of interest here (e.g., alpha < .1, .05, .01)
    ret <- EDR(results, alpha = .05)
    ret
}

# test that it works
# Final <- runSimulation(design=Design, replications=5,
#                       generate=Generate, analyse=Analyse, summarise=Summarise)

# print code to console
SimShiny(design=Design, generate=Generate, analyse=Analyse,
         summarise=Summarise)

# save shiny code to file
SimShiny('app.R', design=Design, generate=Generate, analyse=Analyse,
         summarise=Summarise)

# run the application
shiny::runApp()
shiny::runApp(launch.browser = TRUE) # in web-browser

} # }
```
