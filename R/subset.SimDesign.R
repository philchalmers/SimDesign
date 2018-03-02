#' Subset method for SimDesign objects
#'
#' \code{subset.SimDesign} is a default method for subsetting a
#' \code{data.frame} of class \code{SimDesign}. This is a modification
#' of the base R subset command to maintain the extra attributes
#' produced during a simulation.
#'
#' @param x A \code{data.frame} object, of class \code{SimDesign}
#' @param ... Further arguments to be passed to \code{\link{subset}}
#'
#' @return A \code{data.frame}/\code{SimDesign} class object.
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @seealso \code{\link{SimDesign}}
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#' data("BF_sim")
#' x <- subset(BF_sim, select = 1:6)
#' attributes(x)
#' head(x)
#'
#' x1 <- subset(BF_sim, select = c(1,2,4,5,10))
#' attributes(x1)
#'
#' x2 <- subset(BF_sim, select = var_ratio:alpha.05.Jacknife)
#' attributes(x2)
#'
#' x3 <- subset(BF_sim, var_ratio == 1)
#' dim(BF_sim)
#' dim(x3)
#' }
#'
subset.SimDesign <- function(x, ...){
    design_names <- attributes(x)$design_names
    extra_info <- attributes(x)$extra_info
    x <- as.data.frame(x)
    ret <- subset(x, ...)
    # Assign proper class and attributes
    class(ret) <- c('SimDesign', 'data.frame')
    design_names$design <- intersect(design_names$design, names(ret))
    design_names$sim <- intersect(design_names$sim, names(ret))
    attributes(ret)$design_names <- design_names
    attributes(ret)$extra_info <- extra_info
    ret
}

