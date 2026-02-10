#' Compute univariate descriptive statistics
#'
#' Function returns univariate data summaries for each variable supplied, however
#' discrete and continuous variables are treated separately. Structure provides
#' a more pipe-friendly API for selecting and subsetting variables using the
#' \code{dplyr} syntax, however conditional statistics are evaluated
#'  internally using the \code{\link{by}} function. Quantitative/continuous variable
#' information is kept distinct in the output, while discrete variables (e.g.,
#' \code{factors} and \code{character} vectors)
#' can be returned by using the \code{discrete} argument.
#'
#' \emph{Conditioning}: As the function is intended to support
#' pipe-friendly code specifications, conditioning/group subset
#' specifications are declared using \code{\link[dplyr]{group_by}}
#' and subsequently passed to \code{descript}.
#'
#' @param df a \code{data.frame} or \code{tibble}-like structure
#'  containing the variables of interest.
#'
#'  Note that \code{factor} and \code{character} vectors will be treated as
#'  discrete observations, and by default are omitted from the computation
#'  of the quantitative descriptive statistics specified in \code{funs}. However,
#'  setting \code{discrete = TRUE} will provide count-type information for these
#'  discrete variables, in which case arguments to \code{funs} are ignored
#'
#' @param funs functions to apply when \code{discrete = FALSE}. Can be modified
#'  by the user to include or exclude further functions, however each supplied
#'  function must return a scalar. Use \code{get_discreteFuns()} to return
#'  the full list of functions, which may then be augmented or subsetted
#'  based on the user's requirements. Default descriptive statistic returned are:
#'
#'  \describe{
#'   \item{\code{n}}{number of non-missing observations}
#'   \item{\code{miss}}{number of missing observations}
#'   \item{\code{mean}}{mean}
#'   \item{\code{trimmed}}{trimmed mean (10\%)}
#'   \item{\code{sd}}{standard deviation}
#'   \item{\code{mad}}{mean absolute deviation}
#'   \item{\code{skewness}}{skewness (from \code{e1701})}
#'   \item{\code{kurtosis}}{kurtosis (from \code{e1071})}
#'   \item{\code{min}}{minimum}
#'   \item{\code{Q_25}}{25\% quantile}
#'   \item{\code{Q_50}}{50\% quantile (a.k.a., the median)}
#'   \item{\code{Q_75}}{75\% quantile}
#'   \item{\code{max}}{maximum}
#'  }
#'
#'  Note that by default the \code{na.rm} behavior is set to \code{TRUE}
#'  in each function call
#'
#' @param discrete logical; include summary statistics for \code{discrete}
#'  variables only? If \code{TRUE} then only count and proportion
#'  information for the discrete variables will be returned
#'
#' @importFrom e1071 skewness kurtosis
#'
#' @export
#'
#' @seealso \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{group_by}}
#'
#' @examples
#'
#' library(dplyr)
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
#' # usual pipe chaining
#' fmtcars |> select(mpg, wt) |> descript()
#' fmtcars |> filter(mpg > 20) |> select(mpg, wt) |> descript()
#'
#' # conditioning with group_by()
#' fmtcars |> group_by(cyl) |> descript()
#' fmtcars |> group_by(cyl, am) |> descript()
#'
#' # discrete variables also work with group_by()
#' fmtcars |> group_by(cyl) |> descript(discrete=TRUE)
#' fmtcars |> group_by(am) |> descript(discrete=TRUE)
#' fmtcars |> group_by(cyl, am) |> descript(discrete=TRUE)
#'
#' # only return a subset of summary statistics
#' funs <- get_descriptFuns()
#' sfuns <- funs[c('n', 'miss', 'mean', 'sd')] # subset
#' fmtcars |> descript(funs=sfuns) # only n, miss, mean, and sd
#'
#' # add a new functions
#' funs2 <- c(sfuns,
#'            Q_5 = \(x) quantile(x, .05, na.rm=TRUE),
#'            median= \(x) median(x, na.rm=TRUE),
#'            Q_95 = \(x) quantile(x, .95, na.rm=TRUE))
#' fmtcars |> descript(funs=funs2)
#'
descript <- function(df, funs=get_descriptFuns(), discrete=FALSE)
{
	discrete.fun <- function(x){
		tab <- table(x, useNA = "ifany")
		ret <- data.frame(values=factor(names(tab)),
						  count=as.integer(tab), proportion=as.numeric(prop.table(tab))) |>
			dplyr::as_tibble()
		ret
	}

	if(!is.data.frame(df))
		df <- as.data.frame(df)
	if(length(dplyr::group_keys(df))){
		indices <- colnames(dplyr::group_keys(df))
		group <- as.list(df[indices])
		df <- dplyr::ungroup(df)
		pick <- setdiff(colnames(df), names(group))
		df <- df[ ,pick,drop=FALSE]
		out <- suppressWarnings(by(df, group, descript, funs=funs,
								   discrete=discrete, simplify=FALSE))
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
         miss     = function(x) {
             out <- sum(is.na(x))
             ifelse(out == 0, NA, out)
         },
         mean     = function(x) mean(x, na.rm=TRUE),
         trimmed  = function(x) mean(x, trim=.1, na.rm=TRUE),
         sd       = function(x) sd(x, na.rm=TRUE),
         mad      = function(x) mad(x, na.rm=TRUE),
         skewness = function(x) e1071::skewness(x, na.rm=TRUE),
         kurtosis = function(x) e1071::kurtosis(x, na.rm=TRUE),
         min      = function(x) min(x, na.rm=TRUE),
         Q_25     = function(x) quantile(x, probs=.25, na.rm=TRUE),
         Q_50     = function(x) median(x, na.rm=TRUE),
         Q_75     = function(x) quantile(x, probs=.75, na.rm=TRUE),
         max      = function(x) max(x, na.rm=TRUE))
}

# if(FALSE){
# 	library(dplyr)
#
# 	data(mtcars)
# 	fmtcars <- within(mtcars, {
# 		cyl <- factor(cyl)
# 		am <- factor(am)
# 		vs <- factor(vs)
# 	})
#
# 	# compare
# 	mtcars |> summarise(mean=mean(wt))
# 	mtcars |> descript()
# 	mtcars |> psych::describe()
# 	mtcars |> Hmisc::describe()
# 	mtcars |> pastecs::stat.desc()
#
# 	# factors included
# 	fmtcars |> descript()        # omitted
# 	fmtcars |> psych::describe() # not smart
# 	fmtcars |> Hmisc::describe() # good, but verbose
# 	fmtcars |> pastecs::stat.desc() # not smart
#
#
# 	##################
# 	# groupings
# 	fmtcars |> group_by(cyl) |> summarise(mean=mean(wt))
# 	fmtcars |> group_by(cyl) |> psych::describe() # ignored
# 	fmtcars |> group_by(cyl) |> descript()
#
# 	# discrete
# 	fmtcars |> descript(discrete=TRUE)
# 	fmtcars |> group_by(cyl) |> descript(discrete=TRUE)
# 	fmtcars |> group_by(cyl, am) |> descript(discrete=TRUE)
# 	fmtcars |> group_by(cyl, am, vs) |> descript(discrete=TRUE)
#
#
# 	fmtcars |> group_by(cyl) |> descript()
# 	fmtcars |> group_by(cyl, am) |> descript()
# 	psych::describeBy(fmtcars ~ cyl)
# 	psych::describeBy(fmtcars ~ cyl + am)
#
#
# 	fmtcars |> group_by(cyl) |> descript(discrete=TRUE)
# 	fmtcars |> group_by(cyl, am) |> descript(discrete=TRUE)
#
# }
