#' ---
#' title: "Logistic Regression Power Analysis"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' author: Phil Chalmers (source adopted from Greg Snow)
#' ---


#' Adopted from Greg Snow's answer on CV:
#'  https://stats.stackexchange.com/questions/35940/simulation-of-logistic-regression-power-analysis-designed-experiments

#' # Step 1 --- Define design conditions with NA to solve (in this case, sample size)

library(SimDesign)

Design <- createDesign(n = NA)
Design

#' # Step 2 --- Define analyse and summarise functions

Analyse <- function(condition, dat, fixed_objects = NULL) {
    Attach(fixed_objects)    # make mydat and beta available
    w <- sample(1:6, condition$n, replace=TRUE, prob=c(3, rep(1,5)))
    mydat2 <- mydat[w, 1:2]
    eta <- with(mydat2,  cbind( 1, v1,
                                v1^2, v2,
                                v1*v2,
                                v1^2*v2 ) %*% beta )
    p <- exp(eta)/(1+exp(eta))
    mydat2$resp <- rbinom(condition$n, 1, p)

    fit1 <- glm( resp ~ poly(v1, 2)*v2, data=mydat2,
                 family=binomial)
    fit2 <- update(fit1, .~ poly(v1,2) )
    ret <- anova(fit1,fit2, test='Chisq')[2,5]
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- ERR(results, alpha = .05)  ## empirical rejection rate given alpha = .05
    ret
}

# extra fixed object information to match Greg's example
mydat <- data.frame( v1 = rep( c(3,6,9), each=2 ),
                     v2 = rep( 0:1, 3 ),
                     resp=c(0.0025, 0.00395, 0.003, 0.0042, 0.0035, 0.002) )
fit0 <- glm( resp ~ poly(v1, 2, raw=TRUE)*v2, data=mydat,
             weight=rep(100000,6), family=binomial)
b0 <- coef(fit0)
fo <- list(mydat=mydat, beta=b0)


#' # Step 3 --- find n over the rows in design

# find n associated with f(n) = 1 - B = .80 power
#+ eval=FALSE
solved <- SimSolve(design=Design, b=.8, interval=c(1000,100000),
                   analyse=Analyse, summarise=Summarise,
                   fixed_objects=fo, maxiter=200, parallel=TRUE)
solved
summary(solved)
plot(solved)
plot(solved, type = 'history')

# solution take a long time (terminates around 75000)


