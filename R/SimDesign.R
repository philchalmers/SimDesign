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
#' @import foreach methods parallel stats
#' @importFrom plyr rbind.fill
#' @importFrom utils recover packageVersion sessionInfo head tail
#' @keywords package
NULL


#' Example simulation from Brown and Forsythe (1974)
#'
#' Example results from the Brown and Forsythe (1974) article on robust estimators for
#' variance ratio tests. Statistical tests are organized by columns and the unique design conditoins
#' are organized by rows. See \code{\link{BF_sim_alternative}} for an alternative form of the same
#' simulation. Code for this simulation is available of the wiki
#' (\url{https://github.com/philchalmers/SimDesign/wiki}).
#'
#' @name BF_sim
#' @docType data
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#' Brown, M. B. and Forsythe, A. B. (1974). Robust tests for the equality of variances.
#'   \emph{Journal of the American Statistical Association, 69}(346), 364--367.
#' @keywords data
#' @examples
#'
#' \dontrun{
#' data(BF_sim)
#' head(BF_sim)
#'
#' #Type I errors
#' subset(BF_sim, var_ratio == 1)
#' }
NULL

#' (Alternative) Example simulation from Brown and Forsythe (1974)
#'
#' Example results from the Brown and Forsythe (1974) article on robust estimators for
#' variance ratio tests. Statistical tests and distributions are organized by columns
#' and the unique design conditoins are organized by rows. See \code{\link{BF_sim}} for an alternative
#' form of the same simulation where distributions are also included in the rows.
#' Code for this simulation is available on the wiki (\url{https://github.com/philchalmers/SimDesign/wiki}).
#'
#' @name BF_sim_alternative
#' @docType data
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#' Brown, M. B. and Forsythe, A. B. (1974). Robust tests for the equality of variances.
#'   \emph{Journal of the American Statistical Association, 69}(346), 364--367.
#' @keywords data
#' @examples
#'
#' \dontrun{
#' data(BF_sim_alternative)
#' head(BF_sim_alternative)
#'
#' #' #Type I errors
#' subset(BF_sim_alternative, var_ratio == 1)
#' }
NULL
