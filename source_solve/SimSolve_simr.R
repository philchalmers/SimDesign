#' ---
#' title: "Sample size estimation for multi-level model with simr"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---

library(SimDesign)

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL){
    x <- 1:condition$N
    g <- letters[1:3]
    X <- expand.grid(x=x, g=g)
    b <- c(2, -0.1) # fixed intercept and slope
    V1 <- 0.5 # random intercept variance
    s <- 1 # residual standard deviation
    model <- makeLmer(y ~ x + (1|g), fixef=b, VarCorr=V1, sigma=s, data=X)
    model
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    # powerSim() is noisy; tell it to use its "indoor voice"
    ret <- quiet(powerSim(dat, nsim=1, progress=FALSE)$pval)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- EDR(results)
    ret
}

#-------------------------------------------------------------------


# N = sample size per group (3 groups used with equal size)
Design <- createDesign(N = NA)

# sample size to 95% power
res <- SimSolve(design=Design, b=.95, interval=c(10, 50),
                generate=Generate, parallel=TRUE,
                analyse=Analyse, summarise=Summarise, packages='simr')
res
summary(res)
so <- summary(res)
plot(res)
plot(res, type = 'history')

library(ggplot2)
ggplot(so$DesignRow_1$tab, aes(x=x, y=y, size = 1/sqrt(reps))) +
    geom_point(alpha=0.7)
