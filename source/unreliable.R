#' ---
#' title: "Unreliability example for paired-samples $t$-tests"
#' author: Phil Chalmers
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---
#'
#' # Introduction
#'
#' This basic example demonstrates how unreliability can be included in power
#' calculations for paired-sample $t$-test applications.
#' Four reliability ratios (`rxx`) are investigated for
#' four sample sizes (50, 100, 200, 400). Three non-zero effect sizes
#' are included using Cohen's d to evaluate the power.
#'
#' # Simulation code

#-------------------------------------------------------------------

library(SimDesign)

Design <- createDesign(rxx=c(1, .8, .6, .4),
                       N = c(50, 100, 200, 400),
                       dT = c(0, .2, .5, .8)) # different in true-scores

#-------------------------------------------------------------------

#' Scratch algebra notes for obtaining VAR(E) given rxx and VAR(T)
#'
#' $$rxx = VAR(T) / VAR(X) = VAR(T) / (VAR(T) + VAR(E))$$
#' $$rxx * (VAR(T) + VAR(E)) = VAR(T)$$
#' $$rxx * VAR(T) + rxx*VAR(E) = VAR(T)$$
#' $$rxx * VAR(E) = VAR(T) - rxx*VAR(T)$$
#' $$VAR(E) = VAR(T)/rxx - VAR(T)$$
#' $$VAR(E) = VAR(T) * (1/rxx - 1)$$

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    group <- gl(2, N, labels = c('pre', 'post'))
    true.scores <- c(rnorm(N, mean=dT), rnorm(N))
    # E(T) = E(mu_i)
    mu.t <- (0 + dT)/2
    # VAR(T) = E(var_i) + mu^2 = E(s_i^2 + mu_i^2) + mu^2
    var.t <- .5 * (1^2 + 0^2) + .5 * (1^2 + dT^2) - mu.t^2
    var.e <- var.t * (1/rxx - 1)
    X <- true.scores + rnorm(N*2, sd=sqrt(var.e))
    dat <- data.frame(group, X = X)
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    out <- t.test(X ~ group, data=dat, paired=TRUE)
    nc(p=out$p.value, mean_diff=out$estimate, SE=out$stderr)
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    power <- EDR(results$p)
    means <- colMeans(subset(results, select=mean_diff:SE))
    c(power=power, means)
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=5000, generate=Generate,
                     analyse=Analyse, summarise=Summarise, parallel=TRUE,
                     verbose=FALSE)
res


#' # Results
#'
#' Below is a plot of the outcome results, demonstrating the negative effect
#' of unreliability for the measurements.
#'
#'

library(dplyr)
res |> select(rxx:SE, -power) |>
    mutate(bias=dT-mean_diff) |>
    arrange(N, dT, rxx) |>
    select(-mean_diff) |> knitr::kable()


#' Estimates of the mean differences reflect the true difference between the pre-post observations
#' (unbiased), however the associated SE is larger for more unreliable instruments.
#' This has direct implications on power to reject the null hypothesis of no difference
#' between the pre-post test.

#' For the paired samples $t$-test:
library(ggplot2)
ggplot(res, aes(dT, power, colour=factor(rxx))) +
	geom_point() + geom_line() +
	geom_abline(slope = 0, intercept = .05, linetype = 'dashed') +
    xlab('Effect size (d)') + ylab('Detection Rate') + ggtitle('Empirical Power Curves') +
	scale_color_discrete('Reliability') + facet_wrap(~N) +
	ggtitle('Paired-samples t-test')
