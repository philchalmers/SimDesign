#' ---
#' title: "Muthen and Muthen (2002) CFA example"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' author: Phil Chalmers
#' ---


#-------------------------------------------------------------------

library(SimDesign)

Design <- createDesign(N=150, fcor=.25, loadings=.8, residuals=.36)

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    population.model <-
      sprintf('
        f1 =~ %s
        f2 =~ %s
        f1 ~~ 1*f1
        f2 ~~ 1*f2
        f1 ~~ %f*f2
        %s', paste0(loadings, "*x", 1:5, collapse=' + '),
             paste0(loadings, "*x", 6:10, collapse=' + '),
             fcor,
             paste0(paste0('x', 1:10, " ~~ "),
                    paste0(residuals, "*x", 1:10, '\n'), collapse=''))
    # cat(population.model)
    dat <- simulateData(population.model, sample.nobs=N)
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    myModel <- '
      f1 =~ NA*x1 + x2 + x3 + x4 + x5
      f2 =~ NA*x6 + x7 + x8 + x9 + x10
      f1 ~~ 1*f1
      f2 ~~ 1*f2'
    mod <- sem(myModel, data=dat)
    ests <- parameterEstimates(mod)
    ret <- with(ests, pvalue[paste0(lhs, op, rhs) == "f1~~f2"])
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    EDR(results, alpha = .05)
}

#-------------------------------------------------------------------

# original result from Muthen and Muthen (not run, but gives power ~= .81)
# res <- runSimulation(design=Design, replications=50000, generate=Generate,
#                      analyse=Analyse, summarise=Summarise, packages='lavaan', parallel=TRUE)
# res


#-------------------------------------------------------------------

Design <- createDesign(N=NA, fcor=.25, loadings=.8, residuals=.36)

solved <- SimSolve(design=Design, b=.8, interval=c(100,300), generate=Generate,
                   analyse=Analyse, summarise=Summarise, packages='lavaan',
                   parallel=TRUE, predCI.tol=.01)
solved
summary(solved)
plot(solved)
plot(solved, type = 'history')

