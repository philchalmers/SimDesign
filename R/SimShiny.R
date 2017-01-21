#' Generate a basic Monte Carlo simulation GUI template
#'
#' This function generates suitble code from the \code{shiny} package to create simple
#' web-interfaces for performing single condition Monte Carlo simulations interactively. The template
#' generated is generally basic, but allows the user to edit the saved files to customize
#' the GUI as they see fit.
#'
#' @param filename an optional name of a text file to save the server and UI components.
#'   If ommitted, the code will be printed to the R console instead
#'
#' @param dir the directory to write the files to. Default is the working directory
#'
#' @param ... arguments to be passed to \code{\link{runSimulation}}. Note that the
#'   \code{design} object is not used directly, and instead provides options to be
#'   selected in the GUI
#'
#' @seealso \code{\link{runSimulation}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Design <- expand.grid(sample_size = c(30, 60, 90, 120),
#'                       group_size_ratio = c(1, 4, 8),
#'                       standard_deviation_ratio = c(.5, 1, 2))
#'
#' Generate <- function(condition, fixed_objects = NULL){
#'     N <- condition$sample_size
#'     grs <- condition$group_size_ratio
#'     sd <- condition$standard_deviation_ratio
#'     if(grs < 1){
#'         N2 <- N / (1/grs + 1)
#'         N1 <- N - N2
#'     } else {
#'         N1 <- N / (grs + 1)
#'         N2 <- N - N1
#'     }
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL){
#'     welch <- t.test(DV ~ group, dat)
#'     ind <- t.test(DV ~ group, dat, var.equal=TRUE)
#'
#'     # In this function the p values for the t-tests are returned,
#'     #  and make sure to name each element, for future reference
#'     ret <- c(welch = welch$p.value, independent = ind$p.value)
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     #find results of interest here (e.g., alpha < .1, .05, .01)
#'     ret <- EDR(results, alpha = .05)
#'     ret
#' }
#'
#' # test that it works
#' # Final <- runSimulation(design=Design, replications=5,
#' #                       generate=Generate, analyse=Analyse, summarise=Summarise)
#'
#' # print code to console
#' SimShiny(design=Design, generate=Generate, analyse=Analyse, summarise=Summarise)
#'
#' # save shiny code to file
#' SimShiny('shinySim.R', design=Design, generate=Generate, analyse=Analyse, summarise=Summarise)
#' }
SimShiny <- function(filename = NULL, dir = getwd(), ...){
    UI_CONDITION <- function(condition, name){
        cond <- unique(condition)
        is_numeric <- is.numeric(cond)
        cat(sprintf('      selectInput("%s", "Select condition:"\n', name))
        cat(sprintf('        choices = c(%s)),\n\n',
                    if(is_numeric) paste0(cond, collapse = ',')
                    else paste0('"', cond, '"', collapse = ',')))
        invisible()
    }

    UI_OUTPUT <- function(){
        cat('#-------------------------------------------------------------------\n')
    }

    if(!is.null(filename)){
        if(file.exists(paste0(filename, '.R')))
            stop('File already exists! Please rename input or rename/remove existing files', call.=FALSE)
    }
    if(!is.null(filename)){
        cat(sprintf('Writing SimShiny components to file \"%s\" in \n  directory \"%s\"',
                    paste0(filename, '.R'), dir))
        sink(paste0(filename, '.R'))
    }
    dots <- list(...)
    design <- dots$design
    nms <- names(design)

    #ui
    cat('library(shiny)\n\nui <- fluidPage(\n\n')
    cat('  titlePanel("Simulation"),\n\n')
    cat('  sidebarLayout(\n')
    cat('    sidebarPanel(\n')
    for(i in 1L:ncol(design))
        UI_CONDITION(design[,i], name=nms[i])
    cat('      submitButton("Run Simulation")\n')
    cat('    ),\n\n')
    cat('    mainPanel(\n')
    cat('      h4("Results"),\n')
    cat('      tableOutput("results")\n')
    cat('    )\n')
    cat('  )\n')

    # #server
    # cat('\n\nserver <- function(input, output) {')
    # for(i in 1L:ncol(design)){
    #     CONDITION(design[,i], name=nms[i])
    # }
    # browser()



    # cat('}\n\n')


    if(!is.null(filename)) sink()
    invisible()
}
