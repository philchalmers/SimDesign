#' Check if object contains an error and stop flow
#'
#' Check if the supplied object(s), which have previously been wrapped in a \code{\link{try}} function,
#' contain any errors. If so, this function will throw an error to stop the work-flow. This function
#' works sequentially, therefore the first object to demonstrate a \code{try-error} in the inputs
#' arguments will be the first to throw an error.
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

# return a character vector of functions defined in .GlobalEnv
parent_env_fun <- function(){
    nms <- ls(envir = globalenv())
    is_fun <- sapply(nms, function(x, envir) is.function(get(x, envir=envir)),
                     envir = globalenv())
    return(nms[is_fun])
}
