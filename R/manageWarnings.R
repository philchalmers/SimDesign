#' Mange specific warning messages
#'
#' The purpose of this function is to provide more nuanced managing of warning
#' messages that appear in function calls outside the front-end users control
#' (e.g., via functions written in third-party packages). In particular,
#' this functions provides a less nuclear approach than, for example,
#' \code{\link{suppressWarning}}, which suppresses all warning messages
#' rather than those which are known
#' to be innocuous to the current application, or when setting \code{options(warn=2)},
#' which has the opposite effect of treating all warnings messages as errors globally.
#' Both of these approaches are generally bad practice when writing good code
#' as they may inadvertent suppress or increase the intensity of a yet to be seen
#' warning.
#'
#' The main goal of this function is to \emph{explicitly} indicate whether known warning
#' messages are innocuous or problematic in the current application.
#' For instance, in many function implementations only a subset
#' of identified warnings should be treated
#' as errors, rather than the more nuclear default of treating all warnings
#' as errors (e.g., see \code{warnings_as_errors} in
#' \code{\link{runSimulation}}, as well as \code{option(warn=2)} to
#' convert all warnings to errors globally).
#' On the other hand, some warnings could be treated as "noise", and therefore
#' not problematic problematic in the current application. Base functions
#' such as \code{\link{suppressWarnings}} can accomplish this, however will
#' suppress all of the warnings messages rather than specific ones,
#' potentially obfuscating important
#' warnings messages that the front-end user has simply not encounter yet and
#' should therefore still be raised.
#'
#' To avoid the above nuclear approaches the argument \code{warning2error} can be
#' used to specify which known
#' warning should be converted to errors, leaving other warnings not encounter
#' yet as warnings, while the argument \code{ignorable} can be used to specify
#' warnings messages that can generally be ignored and therefore not raised
#' in the R session. Note that for suppressing message
#' and printing calls from generally noisy functions see \code{\link{quiet}}.
#'
#' @param expr expression to be evaluated (e.g., \code{ret <- myfun(args)}
#'   should be wrapped as either \code{manageWarnings(ret <- myfun(args))},
#'   \code{ret <- manageWarnings(myfun(args))}, or more readably
#'   \code{ret <- myfun(args) |> manageWarnings()} )
#'
#' @param warning2error a character vector of warning messages
#'   that should be converted to errors. Each warning message is
#'   matched using a \code{\link{grepl}} expression, so partial matching
#'   is supported (though more specific messages are less likely to throw
#'   false positives). If \code{NULL}, all observed warning messages
#'   will be treated as errors (same behaviour as \code{options(warn=2)},
#'   though defined locally)
#'
#' @param ignorable a character vector indicating warning messages that
#'   are anticipated but generally ignorable if they are determined to be
#'   innocuous a priori. Each warning message is
#'   matched using a \code{\link{grepl}} expression, so partial matching
#'   is supported (though more specific messages are less likely to throw
#'   false positives). If \code{NULL}, no warning message will be treated
#'   as ignorable
#'
#' @return returns the original result of \code{eval(expr)}, with warning
#'  messages either left the same, increased to errors, or suppressed (depending
#'  on the specifications)
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' fun <- function(warn1=FALSE, warn2=FALSE, warn3=FALSE, error=FALSE){
#'    if(warn1) warning('Show this warning')
#'    if(warn2) warning('Show a different warning')
#'    if(warn3) warning('Last warning message')
#'    if(error) stop('terminate function call')
#'    return('Returned from fun()')
#' }
#'
#' # normal run (no warnings or errors)
#' out <- fun()
#' out
#'
#' # these are all the same
#' manageWarnings(out <- fun())
#' out <- manageWarnings(fun())
#' out <- fun() |> manageWarnings()
#'
#' # errors treated normally
#' fun(error=TRUE)
#' fun(error=TRUE) |> manageWarnings()
#'
#' # all warnings converted to errors (similar to options(warn=2))
#' fun(warn1=TRUE)
#' fun(warn1=TRUE) |> manageWarnings()
#' fun(warn2=TRUE) |> manageWarnings()
#'
#' # Specific warnings treated as errors (others stay as warnings)
#' # Here, treat first warning message as error but not the second or third
#' fun(warn1=TRUE) # warning
#' ret <- fun(warn1=TRUE) |> manageWarnings("Show this warning")  # now error
#' ret <- fun(warn2=TRUE) |> manageWarnings("Show this warning")  # still a warning
#'
#' # multiple warnings raised but not converted as they do not match criteria
#' fun(warn2=TRUE, warn3=TRUE)
#' fun(warn2=TRUE, warn3=TRUE) |> manageWarnings("Show this warning")
#'
#' # Explicitly convert multiple warning messages, allowing others through.
#' #   This is generally the best use of the function's specificity
#' fun(warn1=TRUE, warn2=TRUE)
#' ret <- fun(warn1=TRUE) |>   # error given either message
#'            manageWarnings(c("Show this warning", "Show a different warning"))
#' ret <- fun(warn2=TRUE) |>
#'            manageWarnings(c("Show this warning", "Show a different warning"))
#'
#' # last warning gets through (left as valid warning), but message still raised
#' ret3 <- fun(warn3=TRUE) |>
#'             manageWarnings(c("Show this warning", "Show a different warning"))
#' ret3
#'
#' ###########
#' # combine with quiet()
#'
#' fun <- function(warn1=FALSE, warn2=FALSE, warn3=FALSE, error=FALSE){
#'    message('This function is rather chatty')
#'    cat("It even prints in different output forms!\n")
#'    if(warn1) warning('This warning is fine')
#'    if(warn2) warning('Show this warning')
#'    if(warn3) warning('Last warning message')
#'    if(error) stop('terminate function call')
#'    return('Returned from fun()')
#' }
#'
#' # normal run (no warnings or errors, but messages)
#' out <- fun()
#' out <- quiet(fun()) # using "indoor voice"
#'
#' # suppress all print messages and warnings (latter not recommended)
#' fun(warn2=TRUE) |> quiet() |> suppressWarnings()
#'
#' # convert warning to errors, but keep suppressing messages via quiet()
#' fun(warn2=TRUE) |> quiet() |> manageWarnings()
#'
#' # tolerable warning message (warn1 ignored)
#' fun(warn1=TRUE) |> quiet() |>
#'   manageWarnings(ignorable = 'This warning is fine')
#'
#' # warn2 or warn3 raised to an error
#' fun(warn1=TRUE, warn2=TRUE) |> quiet() |>
#'   manageWarnings(ignorable = 'This warning is fine')
#'
#' # only warn2 raised to an error explicitly (warn3 remains as warning)
#' fun(warn1=TRUE, warn3=TRUE) |> quiet() |>
#'   manageWarnings(warning2error = 'Show this warning',
#'                  ignorable = 'This warning is fine')
#'
#' fun(warn1=TRUE, warn2 = TRUE, warn3=TRUE) |> quiet() |>
#'   manageWarnings(warning2error = 'Show this warning',
#'                  ignorable = 'This warning is fine')
#'
#' }
#'
manageWarnings <- function(expr, warning2error = NULL, ignorable = NULL){
    stopit <- function(message, warning2error, ignorable){
        if(message %in% ignorable) return(TRUE)
        if(is.null(warning2error)) stop(message, call.=FALSE)
        sapply(warning2error, function(warn){
            match <- grepl(warn, message)
            if(match) stop(message, call.=FALSE)
        })
        return(FALSE)
    }

    stopifnot(!missing(expr))
    ret <- withCallingHandlers({
        eval(expr)
    }, warning=function(w) {
        message <- conditionMessage(w)
        muffleL <- stopit(message, warning2error=warning2error, ignorable=ignorable)
        if(muffleL) invokeRestart("muffleWarning")
    })
    invisible(ret)
}
