#' ---
#' title: "Flora and Curran (2004) CFA simulation"
#' author: "Phil Chalmers"
#' format:
#'   html:
#'     theme:
#'       dark: darkly
#'       light: spacelab
#'     number_sections: true
#'     toc: true
#'     fontsize: 1.1em
#' ---
#'

library(SimDesign)

Design <- createDesign(N = c(100, 200, 500, 1000),
                       categories = c(2, 5),
                       skewness_kurtosis = list(c(0, 0), c(.75, 1.75), c(.75, 3.75),
                                                c(1.25, 1.75), c(1.25, 3.75)),
                       factors = c(1, 2),
                       indicators = c(5, 10),
                       estimator = c('WLSMV', 'WLS'),
                       # remove poorly converging combinations
                       subset = !(estimator == 'WLS' & N %in% c(100, 200) &
                                      factors == 2 & indicators == 10))

# include syntax generation function
source('FloraCurran2004_functions.R')

######################################################

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    syntax <- genLavaanSyntax(factors=factors, indicators=indicators)
    cdat <- simulateData(syntax, model.type='cfa', sample.nobs=N,
                         skewness=skewness_kurtosis[1L],
                         kurtosis=skewness_kurtosis[2L])
    tau <- if(categories == 5)
        c(-1.645, -0.643, 0.643, 1.645) else 0
    # data generation fix described in Flora's (2002) unpublished dissertation
    if(categories == 5 && all(skewness_kurtosis == c(1.25, 1.75)))
        tau[1] <- -1.125
    dat <- apply(cdat, 2, function(x, tau){
        dat <- numeric(length(x))
        for(i in 1:length(tau))
            dat[x > tau[i]] <- i
        dat
    }, tau=tau)
    # throw error if number of categories not correct
    if(!all(apply(dat, 2, function(x) length(unique(x))) == categories))
        stop('Number of categories generated is incorrect')
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    Attach(condition)
    syntax <- genLavaanSyntax(factors=factors, indicators=indicators, analyse=TRUE)
    mod <- cfa(syntax, dat, ordered=colnames(dat), estimator=estimator)

    # check that model and coefficients are reasonable
    if(!lavInspect(mod, 'converged')) stop('Model did not converge')
    pick_lambdas <- matrix(TRUE, indicators*factors, factors)
    if(factors == 2)
        pick_lambdas[(indicators+1):(indicators*3)] <- FALSE
    cfs <- lavInspect(mod, what="std")$lambda[pick_lambdas]
    if(any(cfs > 1 | cfs < -1))
        stop('Model contains Heywood cases')
    if(factors > 2 && abs(lavInspect(mod, what="std")$psi[2,1]) >= 1)
        stop('Latent variable psi matrix not positive definite')

    # extract desired results
    fit <- fitMeasures(mod)
    ses <- lavInspect(mod, what="se")$lambda[pick_lambdas]
    fitstats <- if(estimator == 'WLS') fit[c('chisq', 'df', 'pvalue')]
    else if(estimator == 'WLSMV') fit[c('chisq.scaled', 'df.scaled', 'pvalue.scaled')]
    names(fitstats) <- c('chisq', 'df', 'pvalue')
    phi21 <- if(factors == 2)
        lavInspect(mod, what="std")$psi[1,2] else NULL

    ret <- c(fitstats, mean_ses=mean(ses), lambda=cfs, phi21=phi21)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    # model parameters
    lambdas <- results[ , grepl('lambda', colnames(results))]
    pool_mean_lambdas <- mean(colMeans(lambdas))     # Equation 10
    pool_SD_lambdas <- sqrt(mean(colVars(lambdas)))  # Equation 11
    RB_phi21 <- if(condition$factors == 2)
        bias(results$phi21, parameter=.3, type='relative', percent=TRUE) else NULL
    mean_se <- mean(results$mean_ses)

    # goodness-of-fit
    edr_05 <- EDR(results$pvalue, alpha = .05)
    mean_X2 <- mean(results$chisq)
    sd_X2 <- sd(results$chisq)
    RB_X2 <- bias(results$chisq, parameter=results$df, type='relative',
                  percent=TRUE, unname=TRUE)

    ret <- c(mean_X2=mean_X2, sd_X2=sd_X2, edr_05=edr_05,
             pool_mean_lambdas=pool_mean_lambdas,
             pool_SD_lambdas=pool_SD_lambdas, mean_se=mean_se,
             RB_X2=RB_X2, RB_phi21=RB_phi21)
    ret
}

######################################################

# run simulation
res <- runSimulation(design=Design, replications=500, generate=Generate,
                     analyse=Analyse, summarise=Summarise, max_errors=100,
                     packages='lavaan', parallel=TRUE, save=TRUE,
                     filename='FloraCurran2004', save_results=TRUE)
res
