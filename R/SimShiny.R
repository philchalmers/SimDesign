#' Generate a basic Monte Carlo simulation GUI template
#'
#' This function generates suitable stand-alone code from the \code{shiny} package to create simple
#' web-interfaces for performing single condition Monte Carlo simulations. The template
#' generated is relatively minimalistic, but allows the user to quickly and easily
#' edit the saved files to customize the associated shiny elements as they see fit.
#'
#' @param filename an optional name of a text file to save the server and UI components (e.g., 'mysimGUI.R').
#'   If omitted, the code will be printed to the R console instead
#'
#' @param dir the directory to write the files to. Default is the working directory
#'
#' @param design \code{design} object from \code{\link{runSimulation}}
#'
#' @param ... arguments to be passed to \code{\link{runSimulation}}. Note that the
#'   \code{design} object is not used directly, and instead provides options to be
#'   selected in the GUI
#'
#' @seealso \code{\link{runSimulation}}
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
#' Design <- createDesign(sample_size = c(30, 60, 90, 120),
#'                        group_size_ratio = c(1, 4, 8),
#'                        standard_deviation_ratio = c(.5, 1, 2))
#'
#' Generate <- function(condition, fixed_objects = NULL) {
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
#' Analyse <- function(condition, dat, fixed_objects = NULL) {
#'     welch <- t.test(DV ~ group, dat)
#'     ind <- t.test(DV ~ group, dat, var.equal=TRUE)
#'
#'     # In this function the p values for the t-tests are returned,
#'     #  and make sure to name each element, for future reference
#'     ret <- c(welch = welch$p.value, independent = ind$p.value)
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL) {
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
#' SimShiny(design=Design, generate=Generate, analyse=Analyse,
#'          summarise=Summarise, verbose=FALSE)
#'
#' # save shiny code to file
#' SimShiny('app.R', design=Design, generate=Generate, analyse=Analyse,
#'          summarise=Summarise, verbose=FALSE)
#'
#' # run the application
#' shiny::runApp()
#' shiny::runApp(launch.browser = TRUE) # in web-browser
#'
#' }
SimShiny <- function(filename = NULL, dir = getwd(), design, ...){

    if(!is.null(filename)) on.exit(sink())

    UI_CONDITION <- function(condition, name){
        cond <- unique(condition)
        is_numeric <- is.numeric(cond)
        cat(sprintf('      selectInput("%s", "Select %s:",\n', name, name))
        cat(sprintf('        choices = c(%s)),\n\n',
                    if(is_numeric) paste0(cond, collapse = ',')
                    else paste0('"', cond, '"', collapse = ',')))
        invisible()
    }

    if(!is.null(filename)){
        cat(sprintf('Writing SimShiny components to file \"%s\" in \n  directory \"%s\"',
                    filename, dir))
        sink(filename)
    }
    nms <- names(design)
    dots <- list(...)
    inputs <- cbind(names(dots), '=', names(dots))
    pick <- inputs[!(inputs[,1] %in% c('generate', 'analyse', 'summarise')), 1L]
    if(length(pick)){
        for(i in 1:length(pick))
            inputs[which(pick[i] == inputs[,1]), 3] <- as.character(dots[pick[i]][[1L]])
    }
    inputs <- paste0(apply(inputs, 1, paste0, collapse=''), collapse=', \n                    ')
    design_is_numeric <- sapply(design, is.numeric)

    #functions
    Functions <- dots[c('generate', 'analyse', 'summarise')]
    for(i in seq_len(3L)){
        if(!is.null(Functions[[i]])){
            output <- capture.output(print(Functions[[i]]))
            if(grepl('<bytecode: ', output[length(output)]))
                output <- output[-length(output)]
            output <- paste0(output, '\n', collapse='')
            cat(sprintf('%s <- %s\n', names(Functions[i]), output))
        }
    }

    #ui
    cat('#---------------------------------------------------------\n')
    cat('library(SimDesign)\n')
    cat('library(shiny)\n\n')
    cat(sprintf('design_is_numeric <- c(%s)\n\n',
                paste0(design_is_numeric, collapse=',')))
    cat('ui <- fluidPage(\n\n')
    cat('  titlePanel("Simulation"),\n\n')
    cat('  sidebarLayout(\n')
    cat('    sidebarPanel(\n')
    cat('      numericInput("reps", "Number of replications:", 0),\n\n')
    for(i in seq_len(ncol(design)))
        UI_CONDITION(design[,i], name=nms[i])
    cat('      submitButton("Run Simulation")\n')
    cat('    ),\n\n')
    cat('    mainPanel(\n')
    cat('      tableOutput("results")\n')
    cat('    )\n')
    cat('  )\n')
    cat(')\n')

    # #server
    cat('\nserver <- function(input, output) {\n\n')
    cat('  Design <- reactive({\n')
    cat(sprintf('    df <- data.frame(%s, stringsAsFactors = FALSE)\n',
                paste0('input$', nms, collapse=',\n      ')))
    cat(sprintf('    names(df) <- c(%s)\n', paste0('"', nms, '"', collapse=',\n      ')))
    cat('    for(i in seq_len(length(df))) if(design_is_numeric[i]) df[,i] <- as.numeric(df[,i])\n')
    cat('    df\n')
    cat('  })\n\n')
    cat('  output$results <- renderTable({\n')
    cat('    reps <- input$reps\n')
    cat('    if(reps > 0){\n')
    cat('         res <- runSimulation(design=Design(), replications=reps,\n')
    cat(sprintf('                    %s)\n', inputs))
    cat('         res <- res[,(ncol(Design())+1):ncol(res)]\n')
    cat('         res$REPLICATIONS <- NULL\n')
    cat('         res$SEED <- NULL\n')
    cat('         return(res)\n')
    cat('    } else return(NULL)\n')
    cat('  }, digits = 3)\n')
    cat('}\n\n')

    cat('shinyApp(ui=ui, server=server)\n\n')

    invisible()
}
