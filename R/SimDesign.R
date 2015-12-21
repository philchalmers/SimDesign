#' Structure for Organizing Monte Carlo Simulation Designs
#'
#' Provides tools to help organize Monte Carlo simulations in R. The package
#' controls the structure and back-end of Monte Carlo simulations
#' by utilizing a general generate-analyse-summarise strategy. The functions provided control common
#' simulation issues such as re-simulating non-convergent results, support parallel
#' back-end and MPI distributed computations, save and restore temporary files,
#' aggregate results across independent nodes, and provide native support for debugging.
#' The primary function for organizing the simulations is \code{\link{runSimulation}}.
#'
#' @name SimDesign
#' @docType package
#' @title Structure for Organizing Monte Carlo Simulation Designs
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @import foreach methods parallel
#' @importFrom plyr rbind.fill
#' @importFrom stats na.omit sd
#' @importFrom utils recover
#' @keywords package
NULL
