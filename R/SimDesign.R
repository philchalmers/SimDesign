#' Structure for Organizing Monte Carlo Simulation Designs
#'
#' Provides tools to help organize Monte Carlo simulations in R. The package
#' controls the structure and back-end of Monte Carlo simulations
#' by utilizing a general generate-analyse-summarise strategy. The functions provided control common
#' simulation issues such as re-simulating non-convergent results, support parallel
#' back-end and MPI distributed computations, save and restore temporary files,
#' aggregate results across independent nodes, and provide native support for debugging.
#' The primary function for organizing the simulations is \code{\link{runSimulation}}.
#' For an in-depth tutorial of the package please refer to Chalmers and Adkins (2020; \doi{10.20982/tqmp.16.4.p248}).
#' For an earlier didactic presentation of the package users can refer to Sigal and Chalmers
#' (2016; \doi{10.1080/10691898.2016.1246953}). Finally, see the associated
#' wiki on Github (\url{https://github.com/philchalmers/SimDesign/wiki})
#' for other tutorial material, examples, and applications of \code{SimDesign} to real-world simulations.
#'
#' @name SimDesign
#' @docType package
#' @title Structure for Organizing Monte Carlo Simulation Designs
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @import foreach methods parallel stats
#' @importFrom plyr rbind.fill
#' @importFrom pbapply pblapply
#' @importFrom dplyr as_tibble
#' @importFrom utils recover packageVersion sessionInfo head tail capture.output
#' @keywords package
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
NULL


#' Example simulation from Brown and Forsythe (1974)
#'
#' Example results from the Brown and Forsythe (1974) article on robust estimators for
#' variance ratio tests. Statistical tests are organized by columns and the unique design conditions
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
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
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
#' and the unique design conditions are organized by rows. See \code{\link{BF_sim}} for an alternative
#' form of the same simulation where distributions are also included in the rows.
#' Code for this simulation is available on the wiki (\url{https://github.com/philchalmers/SimDesign/wiki}).
#'
#' @name BF_sim_alternative
#' @docType data
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references
#' Brown, M. B. and Forsythe, A. B. (1974). Robust tests for the equality of variances.
#'   \emph{Journal of the American Statistical Association, 69}(346), 364--367.
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
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
