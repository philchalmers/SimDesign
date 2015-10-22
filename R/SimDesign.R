#' Structure for Organizing Monte Carlo Simulation Designs
#'
#' Provides tools to help organize Monte Carlo simulations in R.
#' The tools provided control the structure and back-end of the Monte Carlo simulations
#' by utilizing a generate-analyse-summarise strategy. The functions
#' control common simulation issues such as re-simulating non-convergent results,
#' support parallel back-end computations, save and restore temporary files,
#' aggregate results across independent nodes, and provide native support for debugging..
#' The primary function for
#' organizing the simulations is \code{\link{runSimulation}}.
#'
#' @name SimDesign
#' @docType package
#' @title Structure for Organizing Monte Carlo Simulation Designs
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @import foreach methods parallel
#' @importFrom plyr rbind.fill
#' @keywords package
NULL