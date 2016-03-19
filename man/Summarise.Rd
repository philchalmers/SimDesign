% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{Summarise}
\alias{Summarise}
\title{Summarise simulated data using various population comparison statistics}
\usage{
Summarise(condition, results, fixed_objects = NULL, parameters_list = NULL)
}
\arguments{
\item{condition}{a single row from the \code{design} input from \code{\link{runSimulation}}
(as a \code{data.frame}), indicating the simulation conditions}

\item{results}{a \code{data.frame} (if \code{Analyse} returned a numeric vector) or a \code{list}
(if \code{Analyse} returned a list) containing the simulation results from \code{\link{Analyse}},
where each cell is stored in a unique row/list element}

\item{fixed_objects}{object passed down from \code{\link{runSimulation}}}

\item{parameters_list}{an (optional) list containing all the 'parameters' elements generated
from \code{\link{Generate}}, where each repetition is stored in a unique element. If a \code{list}
was not returned from \code{\link{Generate}} then this will be \code{NULL}}
}
\value{
must return a named \code{numeric} vector with the desired meta-simulation results
}
\description{
This collapses the simulation results within each condition to composite
estimates such as RMSE, bias, Type I error rates, coverage rates, etc.
}
\examples{
\dontrun{

mysummarise <- function(condition, results, fixed_objects = NULL, parameters_list = NULL){

    #convert to matrix for convenience (if helpful)
    cell_results <- do.call(rbind, results)

    # silly test for bias and RMSE of a random number from 0
    pop_value <- 0
    bias.random_number <- bias(sapply(parameters_list, function(x) x$random_number), pop_value)
    RMSE.random_number <- RMSE(sapply(parameters_list, function(x) x$random_number), pop_value)

    #find results of interest here (alpha < .1, .05, .01)
    nms <- c('welch', 'independent')
    lessthan.05 <- EDR(results[,nms], alpha = .05)

    # return the results that will be appended to the design input
    ret <- c(bias.random_number=bias.random_number,
             RMSE.random_number=RMSE.random_number,
             lessthan.05=lessthan.05)
    return(ret)
}

}

}
\seealso{
\code{\link{bias}}, \code{\link{RMSE}}, \code{\link{RE}}, \code{\link{EDR}},
  \code{\link{ECR}}, \code{\link{MAE}}
}
