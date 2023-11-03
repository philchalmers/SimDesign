#' Attach objects for easier reference
#'
#' The behaviour of this function is very similar to \code{\link{attach}},
#' however it is environment specific, and
#' therefore only remains defined in a given function rather than in the Global Environment.
#' Hence, this function is much safer to use than the \code{\link{attach}}, which
#' incidentally should never be used in your code. This
#' is useful primarily as a convenience function when you prefer to call the variable names
#' in \code{condition} directly rather than indexing with \code{condition$sample_size} or
#' \code{with(condition, sample_size)}, for example.
#'
#' @param ... a comma separated list of \code{data.frame} or \code{tibble} objects
#'   containing elements that should be placed in the current working environment
#'
#' @param omit an optional character vector containing the names of objects that should not
#'   be attached to the current environment. For instance, if the objects named 'a' and 'b' should
#'   not be attached then use \code{omit = c('a', 'b')}.
#'   When NULL (default) all objects are attached
#'
#' @param check logical; check to see if the function will accidentally replace previously defined
#'   variables with the same names as in \code{condition}? Default is \code{TRUE}, which will avoid
#'   this error
#'
#' @param attach_listone logical; if the element to be assign is a list of length one
#'   then assign the first element of this list with the associated name. This generally avoids
#'   adding an often unnecessary list 1 index, such as \code{name <- list[[1L]]}
#'
#' @seealso \code{\link{runSimulation}}, \code{\link{Generate}}
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
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' Design <- createDesign(N1=c(10,20),
#'                        N2=c(10,20),
#'                        sd=c(1,2))
#' Design
#'
#' # does not use Attach()
#' Generate <- function(condition, fixed_objects = NULL) {
#'     # condition = single row of Design input (e.g., condition <- Design[1,])
#'     N1 <- condition$N1
#'     N2 <- condition$N2
#'     sd <- condition$sd
#'
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
#'                       DV = c(group1, group2))
#'     dat
#' }
#'
#' # similar to above, but using the Attach() function instead of indexing
#' Generate <- function(condition, fixed_objects = NULL) {
#'     Attach(condition) # N1, N2, and sd are now 'attached' and visible
#'
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
#'                       DV = c(group1, group2))
#'     dat
#' }
#'
#' }
Attach <- function(..., omit = NULL, check = TRUE, attach_listone = TRUE){
    envir <- as.environment(-1L)
    dots <- list(...)
    if(!is.null(omit))
        for(i in length(dots):1L)
            if(omit %in% names(dots[[i]]))
                dots[[i]][names(dots[[i]]) %in% omit] <- NULL
    for(i in 1L:length(dots)){
        if(check)
            if(any(ls(envir = envir) %in% names(dots[[i]])))
                stop(sprintf('Using Attach() will mask the previously defined variable(s): %s',
                             paste(ls(envir = envir)[ls(envir = envir) %in% names(dots[[i]])],
                                   collapse=' ')), call. = FALSE)
        for(n in names(dots[[i]])){
            if(attach_listone && is.list(dots[[i]][[n]]) && length(dots[[i]][[n]]) == 1L){
                assign(n, dots[[i]][[n]][[1L]], envir = envir)
                next
            }
            assign(n, dots[[i]][[n]], envir = envir)
        }
    }
    invisible(NULL)
}
