#' Get job array ID (e.g., from SLURM or other HPC array distributions)
#'
#' Get the array ID from an HPC array distribution job (e.g., from SLURM or
#' from optional command line arguments).
#' The array ID is used to index the rows in the design
#' object in \code{\link{runArraySimulation}}. For instance,
#' a SLURM array with 10 independent jobs might have the following shell
#' instructions.
#'
#' \describe{
#' \item{\code{#!/bin/bash -l}}{}
#' \item{\code{#SBATCH --time=00:01:00}}{}
#' \item{\code{#SBATCH --array=1-10}}{}
#' }
#'
#' which names the associated jobs with the numbers 1 through 10.
#' \code{getArrayID()} then extracts this information per array, which
#' is used as the \code{runArraySimulation(design, ..., arrayID = getArrayID())} to
#' pass specific rows for the \code{design} object.
#'
#' @param type an integer indicating the element from the result of
#'   \code{\link{commandArgs}} to extract, or a \code{character} specifying the
#'   the type of. Default is \code{'slurm'}
#'
#' @param trailingOnly logical value passed to \code{\link{commandArgs}}.
#'   Only used when \code{type} is an integer
#'
#' @param ID.shift single integer value used to shift the array ID by a constant.
#'   Useful when there are array range limitation that must be specified in the
#'   shell files (e.g., array can only be 10000 but there are more rows
#'   in the \code{design} object). For example, if the array ID should be 10000 through
#'   12000, but the cluster computer enviroment does not allow these indices, then
#'   including the arrange range as 1-2000 in the shell file with \code{shift=9999}
#'   would add this constant to the detected arrayID, thereby indexing the remaining
#'   row elements in the \code{design} object
#'
#' @export
#'
#' @seealso \code{\link{runArraySimulation}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # get slurm array ID
#' arrayID <- getArrayID()
#'
#' # get ID based on first optional argument in shell specification
#' arrayID <- getArrayID(type = 1)
#'
#' # pass to
#' # runArraySimulation(design, ...., arrayID = arrayID)
#'
#' # increase detected arrayID by constant 9999 (for array
#'     specification limitations)
#'  arrayID <- getArrayID(ID.shift=9999)
#'
#' }
#'
getArrayID <- function(type = 'slurm', trailingOnly = TRUE, ID.shift = 0L){
    stopifnot(length(type) == 1L)
    ret <- if(is.numeric(type)){
        args <- commandArgs(trailingOnly = trailingOnly)
        args[as.integer(type)]
    } else if(type == 'slurm'){
        Sys.getenv("SLURM_ARRAY_TASK_ID")
    } else {
        stop('type not supported')
    }
    ret <- as.integer(ret) +
    if(is.na(ret)) warning("array ID is missing")
    ret + ID.shift
}
