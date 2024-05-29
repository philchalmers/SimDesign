#' Manage specific warning messages
#'
#' Function provides more nuanced management of known warning
#' messages that appear in function calls outside the front-end users control
#' (e.g., functions written in third-party packages). Specifically,
#' this functions provides a less nuclear approach than, for example,
#' \code{\link{suppressWarnings}} (which suppresses all warning messages
#' rather than those which are known
#' to be innocuous to the current application) or when globally setting \code{options(warn=2)},
#' which has the opposite effect of treating all warnings messages as errors
#' in the function executions. In either case, global/nuclear behaviour of
#' known warning messages should be avoided
#' as it is generally bad practice as important
#' warning messages (that the front-end user may not have even encounter yet) could be obfuscated, or
#' known to be innocuous warnings messages may be unnecessarily raised to an error.
#'
#' @param expr expression to be evaluated (e.g., \code{ret <- myfun(args)}
#'   should be wrapped as either \code{manageWarnings(ret <- myfun(args))},
#'   \code{ret <- manageWarnings(myfun(args))}, or more readably
#'   \code{ret <- myfun(args) |> manageWarnings()} )
#'
#' @param warning2error Logical or character vector (see below) to control the
#'   conversion of warnings to errors. Setting this input to \code{TRUE} will treat
#'   all observed warning messages as errors (same behaviour as \code{options(warn=2)},
#'   though defined locally), while setting to \code{FALSE} (default)
#'   will leave all warnings messages as-is.
#'
#'   Alternatively, and more usefully, a character vector  of warning messages
#'   can be defined to specify which warnings should be converted to errors.
#'   Each warning message is matched using a \code{\link{grepl}} expression,
#'   so partial matching is supported (though more specific messages are less likely to throw
#'   false positives).
#'
#' @param ignorable a character vector indicating warning messages that
#'   are anticipated but generally ignorable if they are determined to be
#'   innocuous a priori. Each warning message is
#'   matched using a \code{\link{grepl}} expression, so partial matching
#'   is supported (though more specific messages are less likely to throw
#'   false positives). If \code{NULL}, no warning message will be treated
#'   as ignorable.
#'
#' @return returns the original result of \code{eval(expr)}, with warning
#'  messages either left the same, increased to errors, or suppressed (depending
#'  on the input specifications)
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
#'    if(warn1) warning('Message one')
#'    if(warn2) warning('Message two')
#'    if(warn3) warning('Message three')
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
#' # all warnings/returns treated normally by default
#' ret1 <- fun(warn1=TRUE)
#' ret2 <- fun(warn1=TRUE) |> manageWarnings()
#' identical(ret1, ret2)
#'
#' # all warnings converted to errors (similar to options(warn=2), but local)
#' fun(warn1=TRUE) |> manageWarnings(warning2error=TRUE)
#' fun(warn2=TRUE) |> manageWarnings(warning2error=TRUE)
#'
#' # Specific warnings treated as errors (others stay as warnings)
#' # Here, treat first warning message as error but not the second or third
#' ret <- fun(warn1=TRUE) # warning
#' ret <- fun(warn1=TRUE) |> manageWarnings("Message one")  # now error
#' ret <- fun(warn2=TRUE) |> manageWarnings("Message one")  # still a warning
#'
#' # multiple warnings raised but not converted as they do not match criteria
#' fun(warn2=TRUE, warn3=TRUE)
#' fun(warn2=TRUE, warn3=TRUE) |> manageWarnings("Message one")
#'
#' # Explicitly convert multiple warning messages, allowing others through.
#' #   This is generally the best use of the function's specificity
#' fun(warn1=TRUE, warn2=TRUE)
#' fun(warn1=TRUE) |>   # error given either message
#'         manageWarnings(c("Message one", "Message two"))
#' fun(warn2=TRUE) |>
#'        manageWarnings(c("Message one", "Message two"))
#'
#' # last warning gets through (left as valid warning)
#' ret <- fun(warn3=TRUE) |>
#'             manageWarnings(c("Message one", "Message two"))
#' ret
#'
#' ###########
#' # Combine with quiet() and ingorable argument to suppress innocuous messages
#'
#' fun <- function(warn1=FALSE, warn2=FALSE, warn3=FALSE, error=FALSE){
#'    message('This function is rather chatty')
#'    cat("It even prints in different output forms!\n")
#'    if(warn1) warning('Message one')
#'    if(warn2) warning('Message two')
#'    if(warn3) warning('Message three')
#'    if(error) stop('terminate function call')
#'    return('Returned from fun()')
#' }
#'
#' # normal run (no warnings or errors, but messages)
#' out <- fun()
#' out <- quiet(fun()) # using "indoor voice"
#'
#' # suppress all print messages and warnings (not recommended)
#' fun(warn2=TRUE) |> quiet()
#' fun(warn2=TRUE) |> quiet() |> suppressWarnings()
#'
#' # convert all warning to errors, and keep suppressing messages via quiet()
#' fun(warn2=TRUE) |> quiet() |> manageWarnings(warning2error=TRUE)
#'
#' # define tolerable warning messages (warn1 deemed ignorable)
#' ret <- fun(warn1=TRUE) |> quiet() |>
#'   manageWarnings(ignorable = 'Message one')
#'
#' # all other warnings raised to an error except ignorable ones
#' fun(warn1=TRUE, warn2=TRUE) |> quiet() |>
#'   manageWarnings(warning2error=TRUE, ignorable = 'Message one')
#'
#' # only warn2 raised to an error explicitly (warn3 remains as warning)
#' ret <- fun(warn1=TRUE, warn3=TRUE) |> quiet() |>
#'   manageWarnings(warning2error = 'Message two',
#'                  ignorable = 'Message one')
#'
#' fun(warn1=TRUE, warn2 = TRUE, warn3=TRUE) |> quiet() |>
#'   manageWarnings(warning2error = 'Message two',
#'                  ignorable = 'Message one')
#'
#' }
#'
manageWarnings <- function(expr, warning2error = FALSE, ignorable = NULL){
    stopit <- function(message, warning2error, ignorable){
        if(message %in% ignorable) return(TRUE)
        if(is.null(warning2error)) stop(message, call.=FALSE)
        sapply(warning2error, function(warn){
            if(warn == "") return(invisible(NULL))
            match <- grepl(warn, message)
            if(match) stop(message, call.=FALSE)
        })
        return(FALSE)
    }

    stopifnot(!missing(expr))
    if(!is.null(warning2error)){
        if(is.logical(warning2error)){
            warning2error <-
                if(isTRUE(warning2error)) NULL else ""
        }
    }
    ret <- withCallingHandlers({
        eval(expr)
    }, warning=function(w) {
        message <- conditionMessage(w)
        muffleL <- stopit(message, warning2error=warning2error, ignorable=ignorable)
        if(muffleL) invokeRestart("muffleWarning")
    })
    ret
}
