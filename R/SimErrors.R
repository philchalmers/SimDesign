#' Extract Simulation Errors
#'
#' Extractor function in situations where \code{\link{runSimulation}} returned a simulation
#' with detected \code{ERRORS}.
#'
#' @param obj object returned from \code{\link{runSimulation}} containing an \code{ERRORS} column
#'
#' @param seeds logical; locate \code{.Random.seed} state that caused the error message?
#'
#' @param subset logical; take a subset of the \code{design} object showing only conditions that
#'   returned errors?
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
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @seealso \code{\link{SimWarnings}}, \code{\link{SimExtract}}
#'
#' @examples
#'
#' sample_sizes <- c(10, 20)
#' standard_deviations <- 1
#'
#' Design <- createDesign(N1=sample_sizes,
#'                        N2=sample_sizes,
#'                        SD=standard_deviations)
#' Design
#'
#' Generate <- function(condition, fixed_objects){
#'     Attach(condition)
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=SD)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)),
#'                       DV = c(group1, group2))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects){
#'
#'   # raise errors with unequal sample sizes only
#'   if(with(condition, N1 != N2)){
#'     if(runif(1, 0, 1) < .9) t.test('char')
#'     if(runif(1, 0, 1) < .9) aov('char')
#'     if(runif(1, 0, 1) < .2) stop('my error')
#'   }
#'
#'   welch <- t.test(DV ~ group, dat)
#'   ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)
#'   ret <- c(welch = welch$p.value, independent = ind$p.value)
#'   ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects) {
#'   ret <- EDR(results)
#'   ret
#' }
#'
#' # print any error messages and their frequency
#' res <- runSimulation(design=Design, replications=3, generate=Generate,
#'                      analyse=Analyse, summarise=Summarise, max_errors = Inf)
#' res |> select(N1, N2, SD, ERRORS)
#' SimErrors(res)
#' SimErrors(res, subset=FALSE)
#'
#' # for specific seeds (organized list of SEEDS returned)
#' (seeds <- SimErrors(res, seeds=TRUE))
#' names(seeds$SEEDS[[1]]) # first row errors
#'
#' # extract one .Random.seed state for the first design condition where
#' # error occurred, pointing to the second uniquely recorded error message
#' seeds$SEEDS[[1]][[2]][, 1] -> seed_state
#' seed_state     # note that Design_row_2 is where the error occurred
#'
#' \dontrun{
#' # pass to runSimulation() to replicate issue (not run as this calls debug())
#' runSimulation(design=Design, replications=3, generate=Generate,
#                analyse=Analyse, summarise=Summarise,
#                load_seed=seed_state, debug='analyse-2') # seed only applies to 2nd design row
#' }
#'
#'
#'
SimErrors <- function(obj, seeds=FALSE, subset=TRUE){
    if(!any(colnames(obj) == 'ERRORS')) return(dplyr::tibble())
    errors <- obj$ERRORS
    pick <- which(errors > 0)
    ret <- SimExtract(obj, what='errors')
    if(seeds){
        eseeds <- SimExtract(obj, what = 'error_seeds')
        design <- SimExtract(obj, what = 'design')
        out <- vector('list', nrow(design))
        for(i in seq_along(pick)){
            p <- pick[i]
            ecol <- eseeds[,grepl(paste0('Design_row_', p, '.'), colnames(eseeds))]
            nms <- gsub(paste0('Design_row_', p, '..'), '', colnames(ecol))
            nms <- gsub("^[0-9]+", '', nms)
            nms <- gsub("\\.",' ', nms)
            nms <- gsub("^\\s+", "", nms)
            u <- unique(nms)
            out[[p]] <- lapply(as.list(u), \(mtc) ecol[ ,which(mtc == nms)])
            names(out[[p]]) <- u
        }
        ret <- dplyr::tibble(design, SEEDS=out)
    }
    if(subset) ret <- ret[pick, ]
    ret
}
