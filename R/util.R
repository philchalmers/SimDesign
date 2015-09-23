#' Check if object contains and error and stop flow
#'
#' Check if the supplied object, which has previously been wrapped in a \code{\link{try}} function,
#' contains an error. If so, this function will throw an error to stop the workflow.
#'
#' @param object an object that has been returned from a \code{\link{try}} call
#'
#' @aliases check_error
#'
#' @export check_error
#'
#' @examples
#' \dontrun{
#'
#' fun <- function(x) x + 1
#'
#' res <- try(fun(1), silent = TRUE)
#' check_error(res) # no error thrown
#'
#' res <- try(fun('this'), silent = TRUE)
#' check_error(res) # throws error
#'
#' }
#'
check_error <- function(object){
    if(is(object, 'try-error'))
        stop('Error thrown from check_error()', call.=FALSE)
}
