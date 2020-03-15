#' Create the simulation Design object
#'
#' Create a partially or fully-crossed data object reflecting the unique
#' simulation design conditions. Each row of the returned object represents
#' a unique simulation condition, and each column represents the named factor
#' variables under study.
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
#'   to factors when building a \code{data.frame}? Default is FALSE
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
#' # remove N=10, SD=2 row from initial definition
#' Design <- createDesign(N = c(10, 20),
#'                        SD = c(1, 2),
#'                        subset = !(N == 10 & SD == 2))
#' Design
#'
#' # example with list inputs
#' Design <- createDesign(N = c(10, 20),
#'                        SD = c(1, 2),
#'                        combo = list(c(0,0), c(0,0,1)))
#' Design   # notice levels printed (not typical for tibble)
#' print(Design, list2char = FALSE)   # standard tibble output
#'
#' Design <- createDesign(N = c(10, 20),
#'                        SD = c(1, 2),
#'                        combo = list(c(0,0), c(0,0,1)),
#'                        combo2 = list(c(5,10,5), c(6,7)))
#' Design
#' print(Design, list2char = FALSE)   # standard tibble output
#'
#' }
createDesign <- function(..., subset, tibble = TRUE, stringsAsFactors = FALSE){
    dots <- list(...)
    if(is.null(names(dots)) || any(names(dots) == ""))
        stop("Please provide meaningful names for each supplied simulation factor")
    ret <- expand.grid(..., stringsAsFactors = stringsAsFactors)
    if (!missing(subset)){
        e <- substitute(subset)
        r <- eval(e, ret, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        ret <- ret[r & !is.na(r), , drop=FALSE]
    }
    if(tibble) ret <- dplyr::as_tibble(ret)
    class(ret) <- c('Design', class(ret))
    ret
}

#' @rdname createDesign
#' @param x object returned by \code{\link{createDesign}}
#' @param list2char logical; for \code{tibble} object re-evaluate list elements
#'   as character vectors for better printing of the levels? Note that this
#'   does not change the original classes of the object, just how they are printed.
#'   Default is TRUE
#' @export
print.Design <- function(x, list2char = TRUE, ...){
    classes <- sapply(x, class)
    if(list2char && any(classes == 'list') && is(x, 'tbl_df'))
        x <- list2char(x)
    classes2 <- sapply(x, class)
    class(x) <- class(x)[!(class(x) %in% 'Design')]
    printDesign(x, whichlist=  which(classes != classes2), ...)
}

list2char <- function(x){
    classes <- sapply(x, class)
    pick <- x[ , classes == 'list']
    nms <- names(pick)
    for(nm in nms){
        tmp <- try(as.vector(apply(pick[,nm], 2L, as.character)), TRUE)
        if(is(tmp, 'try-error')) next
        tmp <- sub("c(", "[", tmp, fixed=TRUE)
        tmp <- sub(")", "]", tmp, fixed=TRUE)
        x[ , nm] <- tmp
    }
    x
}

cat_line <- function(...) {
    cat(paste0(..., "\n"), sep = "")
}

printDesign <- function(x, whichlist, ..., n = NULL, width = NULL, n_extra = NULL) {
    ff <- format(x, ..., n = n, width = width, n_extra = n_extra)
    if(length(whichlist)){
        if(grepl("\\[23m", ff[3])){ # for Rstudio formatting
            ff3 <- strsplit(ff[3], "\\[23m")[[1]]
            for(w in whichlist)
                ff3[w] <- gsub('chr', 'lst', ff3[w])
            ff3 <- paste0(ff3, '[23m', collapse='')
            ff[3] <- ff3
        } else { # in LaTeX, HTML, Word
            ff3 <- strsplit(ff[3], ">")[[1]]
            for(w in whichlist)
                ff3[w] <- gsub('chr', 'lst', ff3[w])
            ff3 <- paste0(ff3, collapse='>')
            ff[3] <- ff3
        }
    }
    cat_line(ff)
    invisible(x)
}
