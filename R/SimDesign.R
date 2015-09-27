#' Simulation Design
#'
#' This package provides tools to help organize Monte Carlo simulations in R.
#' The tools provided control the structure and back-end of the Monte Carlo simulations
#' by utilizing a generate-analyse-summerise strategy. The package
#' attempts to handle issues such as re-simulating non-convergent results,
#' supports parallel back-end computations, saves and restores temporary files,
#' aggregates results across independent nodes, and provides native support for debugging.
#' The primary function for
#' organizing the simulations is \code{\link{runSimulation}}.
#'
#' @name SimDesign
#' @docType package
#' @title Simulation Design
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @import foreach methods parallel
#' @keywords package
NULL
