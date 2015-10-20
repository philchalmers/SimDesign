#' Check if object contains an error and stop flow
#'
#' Check if the supplied object, which has previously been wrapped in a \code{\link{try}} function,
#' contains an error. If so, this function will throw an error to stop the work-flow. This function
#' works sequentially, therefore the first object to demonstrate a try-error in input will generate the error.
#'
#' @param ... objects that have been returned from a \code{\link{try}} call
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
#' # multiple error check
#' res2 <- try(fun('that'), silent = TRUE)
#' check_error(res, res2)
#' }
#'
check_error <- function(...){
    object <- list(...)
    for(i in 1L:length(object))
        if(is(object[[i]], 'try-error'))
            stop(gsub('Error : ', '', object[[i]][1L]), call.=FALSE)
}
