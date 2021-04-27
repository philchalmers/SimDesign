#-------------------------------------------------------------------
## Analogous study of Kang and Cohen's 2007 simulation on information 
## criteria for selecting between dichotomous IRT models. 
##
## https://journals.sagepub.com/doi/10.1177/0146621606292213
#-------------------------------------------------------------------

library(SimDesign)

Design <- createDesign(nitems = c(20,40),
                       ability_mean = c(0,-1,1),
                       sample_size = c(500,1000),
                       model = c('1PL','2PL','3PL'),
                       estimator=c('MML')) # add 'MCMC' when supported
Design

# source in helper functions + fixed_objects definition
source("KangCohen2007_extras.R")


#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    
    # get population generating parameters
    theta <- matrix(rnorm(sample_size, mean=ability_mean))
    pars <- get_parameters(condition, fixed_objects)
    
    # simulate response data (via mirt)
    dat <- with(pars, 
                simdata(sample_size, a=a, d=d, guess=c, 
                        Theta = theta, itemtype = '3PL'))
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    Attach(condition)
    
    if(estimator == 'MML'){
        
        # MML estimation
        fit <- sprintf("Theta = 1-%i
                       START = (1-%i, a1, 1.0)
                       FIXED = (1-%i, a1)", nitems, nitems, nitems)
        mod1PL <- mirt(dat, model=fit, itemtype='2PL', verbose = FALSE)
        mod2PL <- mirt(dat, model=1, itemtype='2PL', verbose = FALSE)
        mod3PL <- mirt(dat, model=1, itemtype='3PL', verbose = FALSE)
        
        stopifnot(extract.mirt(mod1PL, 'converged'))
        stopifnot(extract.mirt(mod2PL, 'converged'))
        stopifnot(extract.mirt(mod3PL, 'converged'))
        
    } else if(estimator == 'MCMC'){
        
        # TODO
        
    }
    
    # just return model comparison information (main purpose of the paper)
    if(estimator == "MML"){
        ret <- as.data.frame(c(M1PL=anova(mod1PL)[c('AIC', 'BIC')], 
                               M2PL=anova(mod2PL)[c('AIC', 'BIC')],
                               M3PL=anova(mod3PL)[c('AIC', 'BIC')],
                               LR12=anova(mod1PL, mod2PL, verbose=FALSE)[2, c('X2', 'p')],
                               LR23=anova(mod2PL, mod3PL, verbose=FALSE)[2, c('X2', 'p')]))
    } else {
        # TODO
    }
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    nms <- colnames(results)
    means <- colMeans(results[,!grepl(".p\\b", nms)])
    sds <- apply(results[,!grepl(".p\\b", nms)], 2, sd)
    BIC <- best_info(results[,grepl(".BIC", nms)])
    ret <- c(means=means, sds=sds, BIC)
    if(condition$estimator == 'MML'){
        AIC <- best_info(results[,grepl(".AIC\\b", nms)])
        ps <- results[,grepl(".p\\b", nms)] < .05
        ps[ps[,2], 1] <- FALSE     ## if 3PLM test sig, 1PL vs 2PL doesn't matter
        LR <- colMeans(cbind(ifelse(rowSums(ps) == 0, TRUE, FALSE), ps))
        names(LR) <- c('M1PL.LR', 'M2PL.LR', 'M3PL.LR')
        ret <- c(ret, AIC, LR)
    }
    ret
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=500, generate=Generate, 
                     analyse=Analyse, summarise=Summarise, fixed_objects=fo,
                     packages='mirt', parallel=TRUE, max_errors = Inf, 
                     filename='KangCohen')
res

