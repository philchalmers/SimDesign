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
#'
#' @seealso \code{\link{SimDesign}}
#' @export
#'
#' @examples
#' \dontrun{
#' data("BF_sim")
#' x <- subset(BF_sim, select = 1:6)
#' attributes(x)
#'
#' x1 <- subset(BF_sim, select = var_ratio:alpha.05.Jacknife)
#' attributes(x1)
#'
#' x2 <- subset(BF_sim, var_ratio == 1)
#' dim(BF_sim)
#' dim(x2)
#' }
#'
subset.SimDesign <- function(x, subset, select, drop = FALSE, ...){
    design_names <- attributes(x)$design_names
    extra_info <- attributes(x)$extra_info

    x <- as.data.frame(x)
    r <- if (missing(subset))
        rep_len(TRUE, nrow(x))
    else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        r & !is.na(r)
    }
    vars <- if (missing(select))
        TRUE
    else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        eval(substitute(select), nl, parent.frame())
    }
    ret <- x[r, vars, drop = drop]

    # Assign proper class and attributes
    class(ret) <- c('SimDesign', 'data.frame')
    attributes(ret)$design_names <- design_names
    attributes(ret)$extra_info <- extra_info

    # Determine proper design and simulation attributes
    is.fact <- sapply(ret, is.factor)
    attributes(ret)$design_names$design <- names(is.fact[is.fact == TRUE])

    is.sim <- sapply(ret, is.numeric)
    attributes(ret)$design_names$sim <- names(is.sim[is.sim == TRUE])
    return(ret)
}
