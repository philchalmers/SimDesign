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
#' in the function executions. To avoid these two extremes, \code{character} vectors
#' can be supplied to this function to either leave the raised warnings
#' as-is (default behaviour), raise only specific warning messages to errors,
#' or specify specific warning messages that can be generally be ignored
#' (and therefore suppressed).
#'
#' In general, global/nuclear behaviour of warning messages should be avoided
#' as they are bad practice. On one extreme,
#' when suppressing all warning messages using \code{\link{suppressWarnings}},
#' potentially important warning messages will become muffled, which can be problematic
#' if the code developer has not yet become aware of these (now muffled) warnings.
#' Moreover, this can become a long-term sustainability issue when third-party functions
#' that the code developer's code depends throw new warnings in the future as the
#' code developer is less likely to become aware of these new warnings.
#'
#' On the other extreme, where all warning messages are turned into errors
#' using \code{options(warn=2)}, innocuous warning messages will be (unwantingly)
#' raised to an error. This can negatively affect the logical workflow of the code
#' developer's functions, where more error messages must be manually managed
#' (e.g., via \code{\link{tryCatch}}), including the innocuous warnings as these
#' are now considered as errors.
#'
#' To avoid these extremes, front-end users should make note of the warning messages
#' that have been raised in their prior code executions attempts, and organized these messages
#' into vectors of ignorable warnings (least severe), known/unknown warnings
#' that should remain as warnings (even if not known by the code developer yet),
#' and explicit warnings that ought to be considered errors for the current
#' application (most severe).
#'
#' @param expr expression to be evaluated (e.g., ret <- \code{myfun(args)}).
#'   Function should either be used as a wrapper,
#'   such as \code{manageWarnings(ret <- myfun(args), ...)} or
#'   \code{ret <- manageWarnings(myfun(args), ...)}, or more
#'   readably as a pipe, \code{ret <- myfun(args) |> manageWarnings(...)}
#'
#' @param warning2error \code{logical} or \code{character} vector to control the
#'   conversion of warnings to errors. Setting this input to \code{TRUE} will treat
#'   all observed warning messages as errors (same behavior as \code{options(warn=2)},
#'   though defined on a per expression basis rather than globally),
#'   while setting to \code{FALSE} (default) will leave all warning messages as-is,
#'   retaining the default behavior
#'
#'   Alternatively, and more useful for specificity reasons,
#'   input can be a \code{character} vector containing known-to-be-severe
#'   warning messages that should be converted to errors. Each supplied
#'   \code{character} vector element is matched using a \code{\link{grepl}} expression,
#'   so partial matching is supported (though more specific messages are less
#'   likely to throw false positives).
#'
#' @param suppress a \code{character} vector indicating warning messages that
#'   are known to be innocuous a priori and can therefore be suppressed.
#'   Each supplied warning message is
#'   matched using a \code{\link{grepl}} expression, so partial matching
#'   is supported (though more specific messages are less likely to throw
#'   false positives). If \code{NULL}, no warning message will be suppressed
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
#' # Combine with quiet() and suppress argument to suppress innocuous messages
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
#'   manageWarnings(suppress = 'Message one')
#'
#' # all other warnings raised to an error except ignorable ones
#' fun(warn1=TRUE, warn2=TRUE) |> quiet() |>
#'   manageWarnings(warning2error=TRUE, suppress = 'Message one')
#'
#' # only warn2 raised to an error explicitly (warn3 remains as warning)
#' ret <- fun(warn1=TRUE, warn3=TRUE) |> quiet() |>
#'   manageWarnings(warning2error = 'Message two',
#'                  suppress = 'Message one')
#'
#' fun(warn1=TRUE, warn2 = TRUE, warn3=TRUE) |> quiet() |>
#'   manageWarnings(warning2error = 'Message two',
#'                  suppress = 'Message one')
#'
#' }
#'
manageWarnings <- function(expr, warning2error = FALSE, suppress = NULL){
    stopit <- function(message, warning2error, suppress){
        if(message %in% suppress) return(TRUE)
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
            stopifnot(length(warning2error) == 1L)
            warning2error <-
                if(isTRUE(warning2error)) NULL else ""
        }
    }
    ret <- withCallingHandlers({
        eval(expr)
    }, warning=function(w) {
        message <- conditionMessage(w)
        muffleL <- stopit(message, warning2error=warning2error, suppress=suppress)
        if(muffleL) invokeRestart("muffleWarning")
    })
    ret
}
