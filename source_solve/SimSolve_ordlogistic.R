#' ---
#' title: "Ordinal Logistic Regression Power Analysis"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---


# adopted from Greg Snow's answer on CV:
#  https://stats.stackexchange.com/questions/22406/power-analysis-for-ordinal-logistic-regression/22410#22410

#' Step 1: Define design and NA condition to solve

library(SimDesign)

Design <- createDesign(n = NA,
                       beta0 = -1/2, beta1 = 1/4, beta2 = 1/4)
Design

#' Step 2 --- Define analyse and summarise functions

Analyse <- function(condition, dat, fixed_objects = NULL) {
    Attach(condition)
    x <- runif(n, 0, 10)
    eta1 <- beta0 + beta1*x
    eta2 <- eta1 + beta2
    p1 <- exp(eta1)/(1+exp(eta1))
    p2 <- exp(eta2)/(1+exp(eta2))
    tmp <- runif(n)
    y <- (tmp < p1) + (tmp < p2)
    fit <- rms::lrm(y~x)
    fit$stats[5]
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- ERR(results, alpha = .05)  ## empirical rejection rate given alpha
    ret
}

#' Step 3 --- Optimize over the rows in design

# find n associated with f(n) = 1 - B = .90 power
# terminate when prediction CI between [.895, .905]
solved <- SimSolve(design=Design, b=.9, interval=c(30,300), parallel=TRUE,
                   analyse=Analyse, summarise=Summarise,
                   predCI.tol=.01, maxiter = 200)
solved
summary(solved)  # note that prediction CI is within [.895, .905]
plot(solved)
plot(solved, type = 'history')


