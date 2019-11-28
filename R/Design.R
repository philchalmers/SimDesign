#' Create the simulation Design object
#'
#' Create a partially or fully-crossed data object reflecting the unique
#' simulation design conditions. Each row of the returned object represents
#' a unique simulation condition, and each column represents the named factor
#' variables unders study.
#'
#' @param ... comma separated list of named input objects representing the simulation
#'   factors to completely cross. Note that these arguments are passed to
#'   \code{\link{expand.grid}} to perform the complete crossings
#'
#' @param subset (optional) a logical vector indicating elements or rows to keep
#'   to create a partially crossed simulation design
#'
#' @param tibble logical; return a \code{tibble} object instead of a
#'   \code{data.frame}? Default is TRUE
#'
#' @param stringsAsFactors logical; should character variable inputs be coerced
#'   to factors? Default is FALSE
#'
#'
#' @return a \code{tibble} or \code{data.frame} containing the simulation experiment
#'   conditions to be evaluated in \code{\link{runSimulation}}
#'
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' # modified example from runSimulation()
#'
#' Design <- createDesign(N = c(10, 20),
#'                        SD = c(1, 2))
#' Design
#'
#' # remove N=10, SD=2 row from initial defintion
#' Design <- createDesign(N = c(10, 20),
#'                        SD = c(1, 2),
#'                        subset = !(N == 10 & SD == 2))
#' Design
#'
#' }
createDesign <- function(..., subset, tibble = TRUE, stringsAsFactors = FALSE){
    ret <- expand.grid(..., stringsAsFactors = stringsAsFactors)
    if (!missing(subset)){
        e <- substitute(subset)
        r <- eval(e, ret, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        ret <- ret[r & !is.na(r), , drop=FALSE]
    }
    if(tibble) ret <- dplyr::as_tibble(ret)
    ret
}
