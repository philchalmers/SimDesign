# return a character vector of functions defined in .GlobalEnv
parent_env_fun <- function(){
    nms <- ls(envir = parent.frame(2L))
    is_fun <- sapply(nms, function(x, envir) is.function(get(x, envir=envir)),
                     envir = parent.frame(2L))
    return(nms[is_fun])
}

load_packages <- function(packages){
    if(!is.null(packages))
        for(pack in packages)
            library(substitute(pack), character.only = TRUE)
    invisible()
}

get_packages <- function(packages){
    sapply(packages, function(x) as.character(packageVersion(x)))
}

# base-code borrowed and modified from pbapply
timeFormater <- function(time, decimals = TRUE){
    dec <- time - floor(time)
    time <- floor(time - dec)
    dec <- round(dec, 2)
    sec <- round(time %% 60)
    if(decimals) sec <- sec + dec
    time <- floor(time / 60)
    minutes <- floor(time %% 60)
    time <- floor(time / 60)
    days <- floor(time / 24)
    time <- floor(time %% 24)
    hours <- floor(time %% 60)
    resTime <- ""
    if (days > 0)
        resTime <- sprintf("%02id ", days)
    if (hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02ih ", hours), sep = "")
    if (minutes > 0 || hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02im ", minutes), sep = "")
    resTime <- if(decimals) paste0(resTime, sprintf("%.2fs", sec))
    else paste0(resTime, sprintf("%02is", sec))
    resTime
}

print_progress <- function(row, trow, stored_time, progress){
    if(progress) cat('\n')
    cat(sprintf('\rDesign row: %i/%i;   Started: %s;   Total elapsed time: %s \n',
                row, trow, date(), timeFormater(sum(stored_time))))
    invisible()
}

myundebug <- function(fun) if(isdebugged(fun)) undebug(fun)

#' Suppress function messages and Concatenate and Print (cat)
#'
#' This function is used to suppress information printed from external functions
#' that make internal use of \code{link{message}} and \code{\link{cat}}, which
#' provide information in interactive R sessions. For simulations, the session
#' is not interactive, and therefore this type of output should be suppressed.
#' For similar behavior for suppressing warning messages see
#' \code{\link{suppressWarnings}}, though use this function carefully as some
#' warnings can be meaningful and unexpected.
#'
#' @param ... the functional expression to be evaluated
#'
#' @param messages logical; suppress all messages?
#'
#' @param cat logical; suppress all concatenate and print calls from \code{\link{cat}}?
#'
#' @export
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @examples
#' myfun <- function(x){
#'    message('This function is rather chatty')
#'    cat("It even prints in different output forms!\n")
#'    message('And even at different....')
#'    cat("...times!\n")
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
quiet <- function(..., messages=FALSE, cat=FALSE){
    if(!cat){
        sink(tempfile())
        on.exit(sink())
    }
    out <- if(messages) eval(...) else suppressMessages(eval(...))
    out
}

sim_results_check <- function(sim_results){
    if(is.data.frame(sim_results)){
        if(nrow(sim_results) > 1L)
            stop('When returning a data.frame in summarise() there should only be 1 row', call.=FALSE)
        nms <- names(sim_results)
        sim_results <- as.numeric(sim_results)
        names(sim_results) <- nms
    }
    if(length(sim_results) == 1L)
        if(is.null(names(sim_results)))
            names(sim_results) <- 'value'
        if(!is.vector(sim_results) || is.null(names(sim_results)))
            stop('summarise() must return a named vector or data.frame object with 1 row', call.=FALSE)
    sim_results
}
