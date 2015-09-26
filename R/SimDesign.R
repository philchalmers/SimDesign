#' Simulation Design
#'
#' Functions to organize Monte Carlo simulations in R. Tries to manage the structure
#' and back-end Monte Carlo simulations, managing issues like resimulating
#' non-convergent results, organizing the functional workflow through a
#' generate -> analyse -> summerise stragegy, handling parallel back-end information,
#' saving and restoring temporary files, and so on. The primary function for
#' organizing the simulations is \code{\link{runSimulation}}.
#'
#' @name SimDesign
#' @docType package
#' @title Simulation Design
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @import foreach methods parallel
#' @keywords package
NULL
