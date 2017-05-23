#' Subset method for SimDesign objects
#'
#' \code{subset.SimDesign} is a default method for subsetting a
#' \code{data.frame} of class \code{SimDesign}. This is a modification
#' of the base R subset command to maintain the extra attributes
#' produced during a simulation.
#'
#' @param x A \code{data.frame} object, of class \code{SimDesign}.
#'
#' @return A \code{data.frame} object.
#'
#' @seealso \code{\link{SimDesign}}
#' @export
#'
#' @examples
#' \dontrun{
#' data("BF_sim")
#' x <- subset(BF_sim, select = 1:6)
#' attributes(x)
#' }
#'
subset.SimDesign <- function(x, subset, select, drop = FALSE, ...){
    design_names <- attributes(x)$design_names
    extra_info <- attributes(x)$extra_info

    x <- subset.data.frame(x, subset = subset,
                           select = select,
                           drop = drop, ...)

    # Assign previous attributes
    attributes(x)$design_names <- design_names
    attributes(x)$extra_info <- extra_info

    # Determine proper design and simulation attributes
    is.fact <- sapply(x, is.factor)
    attributes(x)$design_names$design <- names(is.fact[is.fact == TRUE])

    is.sim <- sapply(x, is.numeric)
    attributes(x)$design_names$sim <- names(is.sim[is.sim == TRUE])
    x
}
