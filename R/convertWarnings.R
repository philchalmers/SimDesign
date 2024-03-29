#' Wrapper to convert all/specific warning messages to errors
#'
#' Function is intended to be a message converter for functions
#' that are known to throw warning messages that should generally
#' be treated as errors instead (e.g., non-positive definite matrix
#' warnings, negative variance estimate warnings, etc). Specific
#' warning messages can be caught if specified, otherwise all
#' detected warning messages will be converted to errors for the evaluated
#' R expression.
#'
#' General goal of this function is to \emph{explicitly}
#' indicate warning that are problematic. In many function implementations
#' only a subset of identified warnings should be treated
#' as errors, rather than the more nuclear default of treating all warnings
#' as errors (e.g., see \code{warnings_as_errors} in
#' \code{\link{runSimulation}}, which is primarily included for debugging
#' purposes early in the simulation design,
#' as well as \code{option(warn=2)} to convert all warnings to errors
#' globally).
#'
#' @param expr expression to be evaluated (e.g., \code{ret <- myfun(args)}
#'   should be wrapped as either \code{convertWarnings(ret <- myfun(args))},
#'   \code{ret <- convertWarnings(myfun(args))}, or more readably
#'   \code{ret <- myfun(args) |> convertWarnings()} )
#'
#' @param warning2error a character vector of warning messages
#'   that should be converted to errors. Each warning message is
#'   matched using a \code{\link{grepl}} expression, so partial matching
#'   is supported (though more specific messages are less likely to throw
#'   false positives). If \code{NULL} then all observed warning messages
#'   will be treated as errors
#'
#' @param muffle logical; muffle any warning message not caught by
#'   \code{warning2error} specification? Generally not recommended unless
#'   you know \emph{all} of the warning messages returned by a function
#'
#' @return returns the original result of \code{eval(expr)}
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
#' convertWarnings(out <- fun())
#' out <- convertWarnings(fun())
#' out <- fun() |> convertWarnings()
#'
#' # errors treated normally
#' fun(error=TRUE)
#' fun(error=TRUE) |> convertWarnings()
#'
#' # all warnings converted to errors
#' fun(warn1=TRUE)
#' fun(warn1=TRUE) |> convertWarnings()
#' fun(warn2=TRUE) |> convertWarnings()
#'
#' # muffle all non-caught warnings (not recommended unless you know
#' #  the R expression/function very intimately!)
#' retmuffle <- fun(warn1=TRUE) |>
#'                  convertWarnings('Warning not caught', muffle=TRUE)
#' retmuffle
#'
#' # Specific warnings treated as errors (others stay as warnings)
#' # Here, treat first warning message as error but not the second or third
#' fun(warn1=TRUE) # warning
#' ret <- fun(warn1=TRUE) |> convertWarnings("Show this warning")  # now error
#'
#' fun(warn2=TRUE, warn3=TRUE) # warnings
#' ret23 <- fun(warn2=TRUE, warn3=TRUE) |>   # continues, but prints warnings
#'              convertWarnings("Show this warning")
#' ret23
#'
#' # Explicitly convert multiple warning messages, allowing others through.
#' #   This is generally the best use of the function's specificity
#' fun(warn1=TRUE, warn2=TRUE)
#' ret <- fun(warn1=TRUE) |>   # error given either message
#'            convertWarnings(c("Show this warning", "Show a different warning"))
#' ret <- fun(warn2=TRUE) |>
#'            convertWarnings(c("Show this warning", "Show a different warning"))
#'
#' # last warning gets through (left as valid warning), but message still raised
#' ret3 <- fun(warn3=TRUE) |>
#'             convertWarnings(c("Show this warning", "Show a different warning"))
#' ret3
#'
#' }
#'
convertWarnings <- function(expr, warning2error=NULL, muffle=FALSE){
    stopit <- function(message, warning2error){
        if(is.null(warning2error)) stop(message, call.=FALSE)
        sapply(warning2error, function(warn){
            match <- grepl(warn, message)
            if(match) stop(message, call.=FALSE)
        })
    }

    stopifnot(!missing(expr))
    ret <- withCallingHandlers({
        eval(expr)
    }, warning=function(w) {
        message <- conditionMessage(w)
        stopit(message, warning2error = warning2error)
        if(muffle) invokeRestart("muffleWarning")
    })
    invisible(ret)
}
