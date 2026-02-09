#' Compute univariate descriptive statistics
#'
#' Function returns univariate data summaries for each variable supplied, however
#' discrete and continuous variables are treated separately.
#' Conditional statistics are evaluated internally using the
#' \code{\link{by}} function, however group-level specifications are
#' declared using the \code{dplyr} style inputs; specifically,
#' \code{\link[dplyr]{group_by}}. Quantitative/continuous variable
#' information is kept distinct in the output, while \code{discrete} observations
#' can be returned using the explicit \code{discrete} argument.
#'
#' @param df a data.frame or tibble containing the variables of interest.
#'  Note that \code{factor} and \code{character} vectors will be treated as
#'  discrete observations
#'
#' @param funs functions to apply when \code{discrete = FALSE}. Can be modified
#'  by the user to include or exclude further functions, however each supplied
#'  function must return a scalar.
#'
#'  Default functions returns:
#'
#'  \describe{
#'   \item{\code{n}}{number of non-missing observations}
#'   \item{\code{mean}}{mean}
#'   \item{\code{trimmed}}{trimmed mean (10\%)}
#'   \item{\code{sd}}{standard deviation}
#'   \item{\code{mad}}{mean absolute deviation}
#'   \item{\code{skewness}}{skewness (from \code{EnvStats})}
#'   \item{\code{kurtosis}}{kurtosis (from \code{EnvStats})}
#'   \item{\code{min}}{minimum}
#'   \item{\code{Q_25}}{25\% quantile}
#'   \item{\code{Q_50}}{50\% quantile (a.k.a., the median)}
#'   \item{\code{Q_75}}{75\% quantile}
#'   \item{\code{max}}{maximum}
#'  }
#'
#' @param discrete logical; include summary statistics for \code{discrete}
#'  variables only? If \code{TRUE} then only count and proportion
#'  information will be returned
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
#' fmtcars <- within(mtcars, {
#' 	cyl <- factor(cyl)
#' 	am <- factor(am, labels=c('automatic', 'manual'))
#' 	vs <- factor(vs)
#' })
#'
#' # with and without factor variables
#' mtcars |> descript()  # omitted
#' fmtcars |> descript()  # omitted
#'
#' # discrete variables only
#' fmtcars |> descript(discrete=TRUE)
#'
#' # conditioning
#' fmtcars |> group_by(cyl) |> descript()
#' fmtcars |> group_by(cyl, am) |> descript()
#'
#' # conditioning also works with group_by()
#' fmtcars |> group_by(cyl) |> descript(discrete=TRUE)
#' fmtcars |> group_by(cyl, am) |> descript(discrete=TRUE)
#'
#'
descript <- function(df,
	funs=c(n=length, mean=mean, trimmed=function(x) mean(x, trim=.1),
		   sd=sd, mad=mad, skewness=EnvStats::skewness, kurtosis=EnvStats::kurtosis,
		   min=min, Q_25=function(x) quantile(x, .25), Q_50=median,
		   Q_75=function(x) quantile(x, .75), max=max), discrete=FALSE)
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
