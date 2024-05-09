#' ---
#' title: "Mediation power analysis"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' author: Phil Chalmers
#' ---

library(SimDesign)

Design <- createDesign(N = NA,
                       a = sqrt(.35),
                       b = sqrt(c(.02, .07, .15, .35)))
Design    # solve for NA's

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summarise functions

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    cprime <- .39
    X <- rep(0:1, each=N)
    M <- a*X + rnorm(N)
    Y <- b*M + cprime * X + rnorm(N)
    dat <- data.frame(X, Y, M)
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
    fit <- lavaan::sem(model, data=dat)
    if(!lavInspect(fit, 'converged')) stop('Model did not converge')
    PE <- parameterEstimates(fit)
    ret <- PE$pvalue[PE$lhs == 'ab']   # joint test
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- EDR(results, alpha = .05)
    ret
}

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Optimize N over the rows in design
# Initial search between N = [10,500] for each row using the default
# integer solver (integer = TRUE)

# In this example, b = target power of 80%
interval <- c(20, 1000) # needless wide for most, but shows the point
solved <- SimSolve(design=Design, b=.8, interval=interval,
                   generate=Generate, analyse=Analyse, summarise=Summarise,
                   packages='lavaan', parallel=TRUE,
                   verbose=FALSE, check.interval=FALSE)
solved
summary(solved)
plot(solved, 1)
plot(solved, 2)
plot(solved, 3)

# also can plot median history and estimate precision
plot(solved, 1, type = 'history')
plot(solved, 1, type = 'density')

