% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Attach.R
\name{Attach}
\alias{Attach}
\title{Attach the simulation conditions for easier reference}
\usage{
Attach(condition, check = TRUE)
}
\arguments{
\item{condition}{a \code{data.frame} containing the \code{condition} names}

\item{check}{logical; check to see if the function will accidentally replace previously defined
variables with the same names as in \code{condition}? Default is \code{TRUE}, which will avoid
this error}
}
\description{
This function accepts the \code{condition} object used to indicate the design conditions
and makes the variable names available in the environment from which it is called. This
is useful primarily as a convenience function when you prefer to call the variable names
in \code{condition} directly rather than indexing with \code{condition$sample_size} or
\code{with(condition, sample_size)}, for example.
}
\details{
The behavior of this function is very similar to \code{\link{attach}},
however it is environment specific, and
therefore only remains defined in a given function rather than in the Global Environment.
Hence, this function is much safer to use than the \code{\link{attach}}, which
incidentally should never be used in your code.
}
\examples{
\dontrun{

# does not use Attach()
mygenerate <- function(condition, fixed_objects = NULL){
    N1 <- condition$sample_sizes_group1
    N2 <- condition$sample_sizes_group2
    sd <- condition$standard_deviations

    group1 <- rnorm(N1)
    group2 <- rnorm(N2, sd=sd)
    dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
                      DV = c(group1, group2))
    dat
}

# similar to above, but using the Attach() function instead of indexing
mygenerate <- function(condition, fixed_objects = NULL){
    Attach(condition)
    N1 <- sample_sizes_group1
    N2 <- sample_sizes_group2
    sd <- standard_deviations

    group1 <- rnorm(N1)
    group2 <- rnorm(N2, sd=sd)
    dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
                      DV = c(group1, group2))
    dat
}
}
}
\seealso{
\code{\link{runSimulation}}, \code{\link{Generate}}
}
