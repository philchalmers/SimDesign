#' ---
#' title: "Unreliability example for paired-samples $t$-test"
#' author: Phil Chalmers
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---
#'
#' # Introduction
#'
#' This basic example demonstrates the influence of unreliable measurements
#' has on the empirical power to reject a null hypothesis of no effect
#' for paired-sample $t$-test applications.
#' Four reliability ratios (`rxx`) are investigated alongside
#' four sample sizes (50, 100, 200, 400). Three non-zero effect sizes
#' are included using Cohen's d to evaluate the empirical power.
#'
#' # Simulation code

#-------------------------------------------------------------------

library(SimDesign)

Design <- createDesign(rxx=c(1, .8, .6, .4),
                       N = c(50, 100, 200, 400),
                       dT = c(0, .2, .5, .8)) # difference in true-scores

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
    # return p-value, mean difference, and SE
    nc(p=out$p.value, mean_diff=out$estimate, SE=out$stderr)
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    power <- EDR(results[,"p"])
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
#' Below is summary information in the form of tables and plots to demonstrate
#' the negative effect of unreliability.
#'

library(dplyr)
res |> select(rxx:SE, -power) |>
    mutate(bias=dT-mean_diff) |>
    arrange(N, dT, rxx) |>
    select(-mean_diff) |> knitr::kable()


#' Estimates of the mean differences reflect the true difference between the pre-post observations
#' regardless of the reliability (unbiased), however the associated SE is
#' larger for more unreliable instruments. This is because the within-subject variability
#' is larger as now it is a function of individual differences variance plus the variance
#' of the measurement error. This has direct implications on power to reject
#' the null hypothesis of no difference between the pre-post tests, as demonstrated below.

#' For the paired samples $t$-test:
library(ggplot2)
ggplot(res, aes(dT, power, colour=factor(rxx))) +
	geom_point() + geom_line() +
	geom_abline(slope = 0, intercept = .05, linetype = 'dashed') +
    xlab('Effect size (d)') + ylab('Detection Rate') + ggtitle('Empirical Power Curves') +
	scale_color_discrete('Reliability') + facet_wrap(~N) +
	ggtitle('Paired-samples t-test')

#' # Solving sample size given reliability
#'
#' Alternatively, one can try to estimate the required sample size to achieve
#' a specific power of interest (e.g., $1-\beta=.80$) for more direct comparisons.
#' This can be achieved using the `SimSolve()` function, which is identical to
#' the `runSimulation()` structure except that the `Design` object contains `NA`
#' values for variables to be solved, while `SimSolve(b = ?)` reflects the target
#' quantity to solve for (in this case, power), and a suitable search interval for
#' the associated `N` values.

DesignNA <- createDesign(rxx=c(1, .8, .6, .4),
						 N = NA,
						 dT = c(.2, .5, .8)) # difference in true-scores
DesignNA


solved <- SimSolve(design=DesignNA, b=.80, interval=c(3, 1000),
				   generate=Generate, analyse=Analyse, summarise=Summarise,
				   verbose=FALSE)
solved

#' The take-home from this output is straightforward: within each effect size combinations,
#' in order to achieve an 80% power rate given the reliably of the test one must use
#' noticeably more observations as the test becomes more unreliable.
#'
#' This type of information is important when performing a priori power planning as the
#' require samples to achieve a given power rate can and will change as the instruments
#' used become less reliable. As Levin and Subkoviak (1977) put it, to determine sample
#' sizes "without simultaneously considering errors of measurement is to live
#' in a 'fool's paradise'" (p. 337).
