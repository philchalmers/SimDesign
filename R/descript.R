#' Compute univariate descriptive statistics
#'
#' Function returns univariate data summaries for each variable supplied. For presentation
#' purposes, discrete and continuous variables are treated separately, the former of which
#' reflects count/proportion information while the ladder are supplied to a (customizable) list
#' of univariate summary functions. As such, quantitative/continuous variable
#' information is kept distinct in the output, while discrete variables (e.g.,
#' \code{factors} and \code{character} vectors) are returned by using the
#' \code{discrete} argument. When applicable a \code{"VARS"} column will be included in the
#' output to indicate which variable is being summarised on the respective row.
#'
#' The purpose of this function is to provide
#' a more pipe-friendly API for selecting and subsetting variables using the
#' \code{dplyr} syntax, where conditional statistics are evaluated
#' internally using the \code{\link{by}} function (when multiple variables are
#' to be summarised). As a special case,
#' if only a single variable is being summarised then the canonical output
#' from \code{dplyr::summarise} will be returned.
#'
#' \emph{Conditioning}: As the function is intended to support
#' pipe-friendly code specifications, conditioning/group subset
#' specifications are declared using \code{\link[dplyr]{group_by}}
#' and subsequently passed to \code{descript}.
#'
#' @param df typically a \code{data.frame} or \code{tibble}-like structure
#'  containing the variables of interest
#'
#'  Note that \code{factor} and \code{character} vectors will be treated as
#'  discrete observations, and by default are omitted from the computation
#'  of the quantitative descriptive statistics specified in \code{funs}. However,
#'  setting \code{discrete = TRUE} will provide count-type information for these
#'  discrete variables, in which case arguments to \code{funs} are ignored
#'
#' @param collapse logical; should the result be returned as a list output
#'   structured using \code{\link{by}} or as a \code{tibble}?
#'   Default is \code{FALSE}
#'
#' @param funs functions to apply when \code{discrete = FALSE}. Can be modified
#'  by the user to include or exclude further functions, however each supplied
#'  function must return a scalar. Use \code{get_discreteFuns()} to return
#'  the full list of functions, which may then be augmented or subsetted
#'  based on the user's requirements. Default descriptive statistic returned are:
#'
#'  \describe{
#'   \item{\code{n}}{number of non-missing observations}
#'   \item{\code{mean}}{mean}
#'   \item{\code{trim}}{trimmed mean (10\%)}
#'   \item{\code{sd}}{standard deviation}
#'   \item{\code{skew}}{skewness (from \code{e1701})}
#'   \item{\code{kurt}}{kurtosis (from \code{e1071})}
#'   \item{\code{min}}{minimum}
#'   \item{\code{P25}}{25th percentile (a.k.a., 1st/lower quartile, Q1), returned from \code{\link{quantile}})}
#'   \item{\code{P50}}{median (50th percentile)}
#'   \item{\code{P75}}{75th percentile (a.k.a, 3rd/upper quartile, Q3), returned from \code{\link{quantile}})}
#'   \item{\code{max}}{maximum}
#'  }
#'
#'  Note that by default the \code{na.rm} behavior is set to \code{TRUE}
#'  in each function call
#'
#' @param discrete logical; include summary statistics for \code{discrete}
#'  variables only? If \code{TRUE} then only count and proportion
#'  information for the discrete variables will be returned, and \code{by_group} will automatically
#'  be set to \code{TRUE}. For greater flexibility
#'  in creating cross-tabulated count/proportion information see \code{\link{xtabs}}
#'
#' @param by_group logical; when \code{group_by()} were used to define the conditioning levels,
#'   should the output from \code{by()} be organized by these group levels or by variable
#'   names? Only applicable when more than one variable is being described
#'
#' @importFrom e1071 skewness kurtosis
#'
#' @export
#'
#' @seealso \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{group_by}},
#'   \code{\link[dplyr]{select}}, \code{\link{xtabs}}
#'
#' @examples
#'
#' data(mtcars)
#'
#' if(FALSE){
#'   # run the following to see behavior with NA values in dataset
#'   mtcars[sample(1:nrow(mtcars), 3), 'cyl'] <- NA
#'   mtcars[sample(1:nrow(mtcars), 5), 'mpg'] <- NA
#' }
#'
#' fmtcars <- within(mtcars, {
#' 	cyl <- factor(cyl)
#' 	am <- factor(am, labels=c('automatic', 'manual'))
#' 	vs <- factor(vs)
#' })
#'
#' # with and without factor variables
#' mtcars |> descript()
#' fmtcars |> descript()               # factors/discrete vars omitted
#' fmtcars |> descript(discrete=TRUE)  # discrete variables only
#'
#' # for discrete variables, xtabs() is generally nicer as cross-tabs can
#' # be specified explicitly (though can be cumbersome)
#' xtabs(~ am, fmtcars)
#' xtabs(~ am, fmtcars) |> prop.table()
#' xtabs(~ am + cyl + vs, fmtcars)
#' xtabs(~ am + cyl + vs, fmtcars) |> prop.table()
#'
#' # usual pipe chaining
#' fmtcars |> select(mpg, wt) |> descript()
#' fmtcars |> subset(mpg > 20) |> select(mpg, wt) |> descript()
#'
#' # conditioning with group_by(), printing across each variable
#' fmtcars |> group_by(cyl) |> descript()
#' fmtcars |> group_by(cyl, am) |> descript()
#' fmtcars |> group_by(cyl, am) |> select(mpg, wt) |> descript()
#'
#' # same, but formatting output by group instead of VARIABLE
#' fmtcars |> group_by(cyl) |> descript(by_group=TRUE)
#' fmtcars |> group_by(cyl, am) |> descript(by_group=TRUE)
#' fmtcars |> group_by(cyl, am) |> select(mpg, wt) |> descript(by_group=TRUE)
#'
#' # with single variables, typical dplyr::summarise() output returned
#' fmtcars |> select(mpg) |> descript()
#' fmtcars |> group_by(cyl) |> select(mpg) |> descript()
#' fmtcars |> group_by(cyl, am) |> select(mpg) |> descript()
#'
#' # if you want a tibble from the list of information instead
#' fmtcars |> group_by(cyl) |> descript(collapse=TRUE)
#' fmtcars |> group_by(am, cyl) |> select(mpg, wt) |> descript(collapse=TRUE)
#'
#' # post-extraction (if you don't mind doing the extra computations
#' #   and extracting afterword)
#' fmtcars |> descript() |> select(n, mean)
#' fmtcars |> select(mpg) |> descript() |> select(n, mean)
#' fmtcars |> group_by(cyl) |> select(mpg) |> descript() |> select(n, mean)
#' fmtcars |> group_by(cyl, am) |> descript() |> select(n, mean)
#' fmtcars |> group_by(cyl) |> descript(collapse=TRUE) |>
#'   select(cyl, VARS, n, mean)
#'
#' # discrete variables also work with group_by(), though again
#' #  xtabs() is generally more flexible
#' fmtcars |> group_by(cyl) |> descript(discrete=TRUE)
#' fmtcars |> group_by(am) |> descript(discrete=TRUE)
#' fmtcars |> group_by(cyl, am) |> descript(discrete=TRUE)
#'
#' # only compute a subset of summary statistics
#' funs <- get_descriptFuns()
#' sfuns <- funs[c('n', 'mean', 'sd')] # subset
#' fmtcars |> descript(funs=sfuns) # only n, miss, mean, and sd
#'
#' # add a new functions
#' funs2 <- c(sfuns,
#'            trim_20 = \(x) mean(x, trim=.2, na.rm=TRUE),
#'            median= \(x) median(x, na.rm=TRUE))
#' fmtcars |> descript(funs=funs2)
#'
descript <- function(df, funs=get_descriptFuns(),
                     by_group=FALSE, discrete=FALSE, collapse=FALSE)
{
	discrete.fun <- function(x){
		tab <- table(x, useNA = "ifany")
		ret <- data.frame(values=factor(names(tab)),
						  count=as.integer(tab), proportion=as.numeric(prop.table(tab))) |>
			dplyr::as_tibble()
		ret
	}

	if(!is.data.frame(suppressMessages(df)))
		df <- as.data.frame(df)

	if(collapse || discrete) by_group <- TRUE

	if(length(dplyr::group_keys(df)) && !by_group){
	    groupkeys <- na.omit(dplyr::group_keys(df))
	    vars <- colnames(df)
	    vars <- vars[!(vars %in% colnames(groupkeys)) & !sapply(df, is.factor)]
	    if(length(vars) > 1){
	        ret <- vector('list', length(vars))
	        names(ret) <- vars
	        for(i in 1:length(vars)){
	            df0 <- df[c(colnames(groupkeys), vars[i])]
	            ret[[i]] <- descript(df0, funs=funs, discrete=discrete, by_group=TRUE)
	        }
	        attr(ret, 'dim') <- length(vars)
	        attr(ret, 'dimnames') <- list(VARIABLE=vars)
	        class(ret) <- c('bybye', 'by')
	        return(ret)
	    }
	}

	if(length(dplyr::group_keys(df))){
	    groupkeys <- na.omit(dplyr::group_keys(df))
		indices <- colnames(groupkeys)
		group <- as.list(df[indices])
		df <- dplyr::ungroup(df)
		pick <- setdiff(colnames(df), names(group))
		df <- df[ ,pick,drop=FALSE]
		out <- suppressWarnings(by(df, group, descript, funs=funs,
								   discrete=discrete, simplify=FALSE))
		class(out) <- c('bybye', class(out))
		if(!discrete && nrow(out[[1]]) == 1){
		    out <- do.call(rbind, out)
		    out$VARS <- NULL
		    out <- dplyr::bind_cols(groupkeys, out)
		}
		if(collapse){
		    nms <- expand.grid(attr(out, 'dimnames'))
		    each <- sapply(out, nrow)
		    nms <- nms[rep(1:nrow(nms), times=each), , drop=FALSE]
		    tmp <- lapply(out, dplyr::as_tibble)
		    tmp2 <- as.data.frame(bind_rows(tmp, .id='id'))
		    tmp2$id <- NULL
		    out <- dplyr::as_tibble(data.frame(nms, tmp2))
		}
		return(out)
	}

	dfnms <- colnames(df)
	pick <- !sapply(df, \(x) is(x, 'factor') || is(x, 'character'))
	if(discrete){
		if(all(pick))
			stop('There are no discrete variables in the dataset provided', call.=FALSE)
		df <- df[ ,!pick, drop=FALSE]
		funs <- discrete.fun
	} else {
		if(!any(pick))
			stop('Quantitative variable subset failed', call.=FALSE)
		if(sum(pick) < ncol(df)){
			df <- df[ ,pick, drop=FALSE]
		}
	}
	nmsout <- names(funs)
	retfull <- vector('list', ncol(df))
	for(j in 1:ncol(df)){
		if(is.list(funs)){
			out <- vector('list', length(funs))
			for(i in 1:length(funs))
				out[[i]] <- sapply(df[,j, drop=FALSE], funs[[i]])
			ret <- if(!discrete) do.call(c, out) else out
		} else {
			ret <- lapply(df[,j, drop=FALSE], funs)
		}
		if(!discrete)
			names(ret) <- nmsout
		else ret <- ret[[1]]
		retfull[[j]] <- ret
	}
	if(!discrete){
		retfull <- do.call(rbind, retfull)
		ret <- data.frame(VARS=factor(colnames(df)), retfull) |> dplyr::as_tibble()
		if(nrow(ret) == 1) ret$VARS <- NULL
	} else {
		ret <- retfull
		names(ret) <- colnames(df)
	}
	ret
}

#' @export
#' @rdname descript
get_descriptFuns <- function(){
    list(n        = function(x) sum(!is.na(x)),
         mean     = function(x) mean(x, na.rm=TRUE),
         trim     = function(x) mean(x, trim=.1, na.rm=TRUE),
         sd       = function(x) sd(x, na.rm=TRUE),
         skew     = function(x) e1071::skewness(x, na.rm=TRUE),
         kurt     = function(x) e1071::kurtosis(x, na.rm=TRUE),
         min      = function(x) min(x, na.rm=TRUE),
         P25      = function(x) quantile(x, .25, na.rm=TRUE),
         P50      = function(x) median(x, na.rm=TRUE),
         P75      = function(x) quantile(x, .75, na.rm=TRUE),
         max      = function(x) max(x, na.rm=TRUE))
}

#' @export
print.bybye <- function (x, ..., vsep)
{
    # copied and modified from base::print.by
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    if (missing(vsep))
        vsep <- paste0('\n', strrep("-", 0.75 * getOption("width")), '\n')
    lapply(X = seq_along(x), FUN = function(i, x, vsep, ...) {
        if (i != 1L && !is.null(vsep))
            cat(vsep, "\n")
        ii <- i - 1L
        for (j in seq_along(dn)) {
            iii <- ii%%d[j] + 1L
            ii <- ii%/%d[j]
            cat(dnn[j], ": ", dn[[j]][iii], "\n", sep = "")
        }
        print(x[[i]], ...)
    }, x, vsep, ...)
    invisible(x)
}

#' @export
select.bybye <- function(.data, ...){
    out <- lapply(.data, function(.d) dplyr::select(.d, ...))
    .data[] <- out
    .data
}

#' @export
#' @importFrom dplyr select
dplyr::select

#' @export
#' @importFrom dplyr group_by
dplyr::group_by
