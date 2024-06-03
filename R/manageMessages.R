#' Increase the intensity or suppress the output of an observed message
#'
#' Function provides more nuanced management of known message outputs
#' messages that appear in function calls outside the front-end users control
#' (e.g., functions written in third-party packages). Specifically,
#' this function provides a less nuclear approach than
#' \code{\link{quiet}} and friends, which suppresses all \code{cat} and
#' \code{message}s raised, and instead allows for specific messages to be
#' raised either to warnings or, even more extremely, to errors. Note that for
#' messages that are not suppressed the order with which the output and message
#' calls appear in the original function is not retained.
#'
#'
#' @param expr expression to be evaluated (e.g., ret <- \code{myfun(args)}).
#'   Function should either be used as a wrapper,
#'   such as \code{manageMassages(ret <- myfun(args), ...)} or
#'   \code{ret <- manageMassages(myfun(args), ...)}, or more
#'   readably as a pipe, \code{ret <- myfun(args) |> manageMassages(...)}
#'
#' @param allow (optional) a \code{character} vector indicating messages that
#'   should still appear, while all other messages should remain suppressed.
#'   Each supplied message is matched using a \code{\link{grepl}} expression, so partial matching
#'   is supported (though more specific messages are less likely to throw
#'   false positives). If \code{NULL}, all messages will be suppressed unless
#'   they appear in \code{message2error} or \code{message2warning}
#'
#' @param message2warning (optional) Input can be a \code{character} vector containing
#'   messages that should probably be considered warning messages for the current application
#'   instead. Each supplied \code{character} vector element is matched using
#'   a \code{\link{grepl}} expression,
#'   so partial matching is supported (though more specific messages are less
#'   likely to throw false positives).
#'
#' @param message2error (optional) Input can be a \code{character} vector containing known-to-be-severe
#'   messages that should be converted to errors for the current application.
#'   See \code{message2warning} for details.
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
#' myfun <- function(x, warn=FALSE){
#'    message('This function is rather chatty')
#'    cat("It even prints in different output forms!\n")
#'    message('And even at different....')
#'    cat("...times!\n")
#'    cat("Messages can be annoying sometimes...\n")
#'    if(warn)
#'      warning('It may even throw warnings!')
#'    x
#' }
#'
#' out <- myfun(1)
#' out
#'
#' # tell the function to shhhh
#' out <- quiet(myfun(1))
#' out
#'
#' # same default behaviour as quiet(), but potential for nuance
#' out2 <- manageMessages(myfun(1))
#' identical(out, out2)
#'
#' # allow some messages to still get printed
#' out2 <- manageMessages(myfun(1), allow = "...times!")
#' out2 <- manageMessages(myfun(1), allow = "This function is rather chatty")
#' out2 <- manageMessages(myfun(1), allow = c("...times",
#'                                            "This function is rather chatty"))
#'
#' # convert specific message to warning
#' out3 <- manageMessages(myfun(1), message2warning = "...times!")
#' identical(out, out3)
#'
#' # other warnings also get through
#' out3 <- manageMessages(myfun(1, warn=TRUE), message2warning = "...times!")
#' identical(out, out3)
#'
#' # convert message to error
#' manageMessages(myfun(1), message2error = "...times!")
#'
#' # multiple message intensity changes
#' manageMessages(myfun(1),
#'   message2warning = "It even prints in different output forms",
#'   message2error = "...times!")
#'
#' manageMessages(myfun(1),
#'   allow = c("This function is rather chatty",
#'             "Messages can be annoying sometimes..."),
#'   message2warning = "It even prints in different output forms",
#'   message2error = "...times!")
#'
#' }
#'
manageMessages <- function(expr, allow = NULL,
                           message2warning = NULL, message2error = NULL){
    ret <- quiet(expr, keep = TRUE)
    msgs <- attr(ret, "quiet.messages")
    attr(ret, "quiet.messages") <- NULL
    if(!is.null(allow)){
        whc <- msgs[lapply(allow, \(x) grepl(x, msgs)) |> sapply(which)]
        if(length(whc)){
            nms <- names(whc)
            whc.cat <- grepl('cat.', nms)
            whc.msg <- grepl('message.', nms)
            if(length(whc.cat))
                sapply(whc[whc.cat], \(x) cat(x,'\n'))
            if(length(whc.msg))
                sapply(whc[whc.msg], \(x) message(x))
        }
    }
    if(!is.null(message2warning)){
        whc <- msgs[lapply(message2warning, \(x) grepl(x, msgs)) |> sapply(which)]
        if(length(whc))
            sapply(whc, \(x) warning(x, call.=FALSE))
    }
    if(!is.null(message2error)){
        whc <- msgs[lapply(message2error, \(x) grepl(x, msgs)) |> sapply(which)]
        if(length(whc))
            stop(whc[1L], call.=FALSE)
    }
    ret
}
