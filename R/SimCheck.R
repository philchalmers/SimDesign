#' Check the status of the simulation's temporary results
#'
#' This function reads the temporary file saved by \code{\link{runSimulation}}
#' by collapsing the information into a suitable (albeit temporary) object of
#' class \code{'SimDesign'}. This is useful when taking a quick-peak at how the
#' early simulation results are performing (useful long running simulation
#' results with many rows in the \code{Design} object). Returns a tibble-based
#' data.frame object (\code{tbl_df}).
#'
#' @param file the temp file currently saving the simulation state. If missing
#'   the file is assumed to be in the current working directory, and start with the
#'   name \code{'SIMDESIGN-TEMPFILE'}
#'
#' @seealso \code{\link{runSimulation}}
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
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' # explicit
#' temp_results <- SimCheck(file = 'SIMDESIGN-TEMPFILE_mycomp.rds')
#' temp_results
#'
#' # works if file is in the current working directory
#' temp_results <- SimCheck()
#' temp_results
#'
#' }
#'
SimCheck <- function(file){
    pat <- 'SIMDESIGN-TEMPFILE'
    input <- if(missing(file)){
        files <- dir()
        pick <- grepl(pat, files)
        if(!any(pick)){
            message("No temporary file found")
            return(invisible(NULL))
        }
        readRDS(files[pick][1L])
    } else {
        if(!file.exists(file)){
            message("file name does not exist")
            return(invisible(NULL))
        }
        readRDS(file)
    }
    ret <- dplyr::as_tibble(dplyr::bind_rows(input))
    ret <- subset(ret, select = !(names(ret) %in% c('ID', 'REPLICATION')))
    ret
}
