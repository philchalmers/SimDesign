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
#' x <- subset(BF_sim, select = 1:6)
#' attributes(x)
#' }
#'
subset.SimDesign <- function(x, subset, select, drop = FALSE, ...){
    design_names <- attributes(x)$design_names
    extra_info <- attributes(x)$extra_info

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
    x <- x[r, vars, drop = drop]

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
