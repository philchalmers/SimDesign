#' Create the simulation design object
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
#' @param fractional a fractional design matrix returned from the
#'   \code{FrF2} package.
#'   Note that the order of the factor names/labels are associated with the
#'   respective \code{...} inputs
#'
#' @param tibble logical; return a \code{tibble} object instead of a
#'   \code{data.frame}? Default is TRUE
#'
#' @param stringsAsFactors logical; should character variable inputs be coerced
#'   to factors when building a \code{data.frame}? Default is FALSE
#'
#' @return a \code{tibble} or \code{data.frame} containing the simulation experiment
#'   conditions to be evaluated in \code{\link{runSimulation}}
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @seealso \code{\link{expandDesign}}
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
#' ##########
#'
#' ## fractional factorial example
#'
#' library(FrF2)
#' # help(FrF2)
#'
#' # 7 factors in 32 runs
#' fr <- FrF2(32,7)
#' dim(fr)
#' fr[1:6,]
#'
#' # Create working simulation design given -1/1 combinations
#' fDesign <- createDesign(sample_size=c(100,200),
#'                         mean_diff=c(.25, 1, 2),
#'                         variance.ratio=c(1,4, 8),
#'                         equal_size=c(TRUE, FALSE),
#'                         dists=c('norm', 'skew'),
#'                         same_dists=c(TRUE, FALSE),
#'                         symmetric=c(TRUE, FALSE),
#'                         # remove same-normal combo
#'                         subset = !(symmetric & dists == 'norm'),
#'                         fractional=fr)
#' fDesign
#'
#' }
createDesign <- function(..., subset, fractional = NULL,
                         tibble = TRUE, stringsAsFactors = FALSE){
    dots <- list(...)
    if(any(sapply(dots, is, class2='data.frame') | sapply(dots, is, class2='tibble')))
        stop('data.frame/tibble design elements not supported; please use a list input instead',
             call. = FALSE)
    if(is.null(names(dots)) || any(names(dots) == ""))
        stop("Please provide meaningful names for each supplied simulation factor",
             call.=FALSE)
    if(!is.null(fractional)){
        dots <- list(dots)
        ret <- as.data.frame(fractional)
        for(i in 1:length(dots[[1L]])){
            vals <- unique(dots[[1]][[i]])
            uniq <- ret[[i]]
            ret[[i]] <- ifelse(ret[[i]] == uniq[1L], vals[1L], vals[2L])
        }
        colnames(ret) <- names(dots[[1L]])
    } else {
        ret <- expand.grid(..., stringsAsFactors = stringsAsFactors)
    }
    if (!missing(subset)){
        e <- substitute(subset)
        r <- eval(e, ret, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        ret <- ret[r & !is.na(r), , drop=FALSE]
    }
    pick <- apply(ret, 2, function(x) all(is.na(x)))
    if(any(pick))
        ret[,pick] <- as.numeric(ret[,pick])
    if(tibble) ret <- dplyr::as_tibble(ret)
    class(ret) <- c('Design', class(ret))
    ret
}

#' Create the simulation design object
#'
#' Repeat each design row the specified number of times. This is primarily used
#' for cluster computing where jobs are distributed with batches of replications
#' and later aggregated into a complete simulation object
#' (see also \code{\link{aggregate_simulations}}). The keyword 'CONDITION' is
#' included for tracking which row is associated with which original simulation
#' design condition.
#'
#' @param Design object created by \code{\link{createDesign}}
#'
#' @param repeat_conditions integer vector used to repeat each design row
#'   the specified number of times. Can either be a single integer, which repeats
#'   each row this many times, or an integer vector equal to the number of total
#'   rows in the created object.
#'
#'   This argument is useful when distributing independent row conditions to
#'   cluster computing environments, particularly with different \code{replication}
#'   information. For example, if 1000 replications in total are the target but
#'   the condition is repeated over 4 rows then only 250 replications per row
#'   would be required across the repeated conditions. See
#'   \code{\link{aggregate_simulations}} for combining the simulation objects
#'   once complete
#'
#' @return a \code{tibble} or \code{data.frame} containing the simulation experiment
#'   conditions to be evaluated in \code{\link{runSimulation}}
#'
#' @seealso \code{\link{createDesign}}, \code{\link{aggregate_simulations}},
#'    \code{\link{runArraySimulation}}
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
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
#' # repeat each row 4 times (for cluster computing)
#' Design <- createDesign(N = c(10, 20),
#'                        SD.equal = c(TRUE, FALSE))
#' Design4 <- expandDesign(Design, 4)
#' Design4
#'
#' # repeat first two rows 2x and the rest 4 times (for cluster computing
#' #   where first two conditions are faster to execute)
#' Design <- createDesign(SD.equal = c(TRUE, FALSE),
#'                        N = c(10, 100, 1000))
#' Design24 <- expandDesign(Design, c(2,2,rep(4, 4)))
#' Design24
#'
#' }
expandDesign <- function(Design, repeat_conditions){
    stopifnot(!missing(Design))
    stopifnot(!missing(repeat_conditions))
    if(any(colnames(Design) == 'CONDITION'))
        stop('Design column names cannot include CONDITION. Please remove')
    if(length(repeat_conditions) == 1L)
        repeat_conditions <- rep(repeat_conditions, nrow(Design))
    stopifnot("length of repeat_rows must equal number of rows in final object"=
                  length(repeat_conditions) == nrow(Design))
    rep_vec <- rep(1L:nrow(Design), times=repeat_conditions)
    ret <- Design[sort(rep_vec), ]
    ret <- dplyr::mutate(ret, CONDITION=rep_vec, .before = 1L)
    rownames(ret) <- NULL
    ret
}

#' @rdname createDesign
#' @param x object returned by \code{\link{createDesign}}
#' @param list2char logical; for \code{tibble} object re-evaluate list elements
#'   as character vectors for better printing of the levels? Note that this
#'   does not change the original classes of the object, just how they are printed.
#'   Default is TRUE
#' @param pillar.sigfig number of significant digits to print. Default is 5
#' @export
print.Design <- function(x, list2char = TRUE, pillar.sigfig = 5, ...){
    classes <- sapply(x, class)
    if(list2char && any(classes == 'list') && is(x, 'tbl_df'))
        x <- list2char(x)
    classes2 <- sapply(x, class)
    class(x) <- class(x)[!(class(x) %in% 'Design')]
    old <- options(pillar.sigfig=pillar.sigfig)
    printDesign(x, whichlist=  which(classes != classes2), ...)
    options(old)
    invisible(NULL)
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
            ff3 <- ff3[2L:length(ff3) - 1L]
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
