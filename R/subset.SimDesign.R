#' Subset method for SimDesign objects
#'
#' \code{subset.SimDesign} is a default method for subsetting a
#' \code{data.frame} of class \code{SimDesign}. This is a modification
#' of the base R subset command to maintain the extra attributes
#' produced during a simulation.
#'
#' @param x A \code{data.frame} object, of class \code{SimDesign}.
#' @param subset A logical expression indicating elements or rows to keep.
#' @param select An expression, indicating columns to select from a dataframe.
#' @param drop Additional arguments passed on to [ indexing operator.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return A \code{data.frame} object.
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
subset.SimDesign <- function(x, subset, select, drop = FALSE, ...){
    design_names <- attributes(x)$design_names
    extra_info <- attributes(x)$extra_info

    xd <- as.data.frame(x)
    r <- if (missing(subset))
        rep_len(TRUE, nrow(xd))
    else {
        e <- substitute(subset)
        r <- eval(e, xd, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        r & !is.na(r)
    }
    vars <- if (missing(select))
        TRUE
    else {
        nl <- as.list(seq_along(xd))
        names(nl) <- names(xd)
        eval(substitute(select), nl, parent.frame())
    }
    ret <- xd[r, vars, drop = drop]

    # Assign proper class and attributes
    class(ret) <- c('SimDesign', 'data.frame')
    attributes(ret)$design_names <- design_names
    attributes(ret)$extra_info <- extra_info

    # Reassign design and sim attributes, depending on selected columns
    full_mod <- c(attributes(ret)$design_names$design, attributes(ret)$design_names$sim)
    match <- intersect(names(ret), full_mod)

    is.fact <- sapply(ret[, match], is.factor)
    attributes(ret)$design_names$design <- match[is.fact]

    is.sim <- sapply(ret[, match], is.numeric)
    attributes(ret)$design_names$sim <- match[is.sim]
    return(ret)
}

