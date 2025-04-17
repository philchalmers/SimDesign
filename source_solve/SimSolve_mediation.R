#' ---
#' title: "Mediation power analysis"
#' author: "Phil Chalmers"
#' format:
#'   html:
#'     theme:
#'       dark: darkly
#'       light: spacelab
#'     number_sections: true
#'     toc: true
#'     fontsize: 1.1em
#'     embed-resources: true
#' ---
#'

#'
#'
#' The following example obtains sample size estimates for a simple mediation
#' model to obtain a power of $1 - \beta = .80$. Specifically, four sample
#' sizes ($N$) are solved by varying the coefficients associated with the equations
#'
#' 1) $M = a*X + e$
#' 2) $Y = b*M + c^\prime * X + e$
#'
#' For simplicity the independent variable $X$ is constrained to be a dichotomous
#' variable, though this is not a requirement and users are free to change this
#' distribution to suit their applications. Residuals ($e$) are assumed to be
#' independent and distributed $e \sim N(0,1)$. The simple mediation model
#' itself is fitted using the `lavaan` package's maximum-likelihood estimation
#' criteria.
#'
#' # SimSolve code
library(SimDesign)

Design <- createDesign(N = NA,
                       a = sqrt(.35),
                       b = sqrt(c(.02, .07, .15, .35)),
                       cprime=.39)
Design    # solve for NA's

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summarise functions

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    X <- rep(0:1, each=N)
    M <- a*X + rnorm(N)
    Y <- b*M + cprime*X + rnorm(N)
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


#' Solve each $N$ given the rows in `Design`

# Initial search between N = [10,500] for each row using the default
# integer solver (integer = TRUE)

# In this example, b = target power of 80%
interval <- c(20, 1000) # needless wide for most, but shows the point

# Search is terminated when either 100 iterations reached or the
# prediction CI is within [.795, .805]
solved <- SimSolve(design=Design, b=.8, interval=interval,
                   generate=Generate, analyse=Analyse, summarise=Summarise,
                   packages='lavaan', parallel=TRUE, 
                   ncores = ceiling(parallel::detectCores()/2),
                   verbose=FALSE, check.interval=FALSE,
                   maxiter=100, predCI.tol=.01)
solved

#' ## Additional information about the solutions
summary(solved)
plot(solved, 1)
plot(solved, 2)
plot(solved, 3)

# also can plot median history and estimate precision
plot(solved, 1, type = 'history')
plot(solved, 1, type = 'density')

