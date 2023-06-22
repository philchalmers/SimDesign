#' ---
#' title: "Hu and Bentler (1999) Fit Cut-offs Simulation"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---


#-------------------------------------------------------------------

library(SimDesign)

Design <- createDesign(model=c('simple', 'complex'),
                       gen_condition=c('normal', paste0('nonnormal', 1:3),
                                       'elliptical',
                                       paste0('nonnormal_dep', 1:2)),
                       N = c(150, 250, 500, 1000, 2500, 5000),
                       fit_model=c('true', 'mis1', 'mis2'))
source('HuBentler1999_extras.R')
# str(fo)

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    L <- with(fixed_objects, if(model == 'simple') simple else complex)
    sigma.e <- diag(with(fixed_objects,
                    if(model == 'simple') simple.resids else complex.resids))
    sigma <- fixed_objects$fcor
    Z <- ifelse(gen_condition %in% c(paste0('nonnormal_dep', 1:2), 'elliptical'),
                fixed_objects$Ztransform(), 1)

    # theta
    theta <- if(gen_condition %in% c('normal', 'nonnormal3', 'nonnormal_dep1')){
        rmvnorm(N, sigma=sigma) / Z
    } else if(gen_condition %in% c('nonnormal1', 'nonnormal2', 'nonnormal_dep2')){
        rValeMaurelli(N, sigma = sigma, kurt=fixed_objects$kurtosis.common_2.3) / Z
    } else if(gen_condition == 'elliptical'){
        Sigma <- diag(18)
        Sigma[1:3, 1:3] <- sigma
        tmp <- rtelliptical(N, mu=numeric(18),
                            lower=rep(-Inf, 18), upper=rep(Inf, 18),
                            Sigma=Sigma, dist = 'Normal') / Z
        epsilon <- tmp[,4:18]
        tmp[,1:3]
    }

    # epsilon
    epsilon <- if(gen_condition == 'normal'){
        rmvnorm(N, sigma=sigma.e) / Z
    } else if(gen_condition == 'elliptical'){
        epsilon
    } else {
        rValeMaurelli(N, sigma=sigma.e,
                      kurt=fixed_objects$kurtosis.unique_2.4) / Z
    }

    # X = L * theta + e
    dat <- as.data.frame(t(t(L) %*% t(theta)) + epsilon)
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    Attach(condition)
    syntax <- genLavaanSyntax(model=model, fit_model=fit_model)
    mod <- sem(syntax, data=dat)
    if(!lavInspect(mod, 'converged')) stop('Model did not converge')
    ret <- fitMeasures(mod)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- c(mean=colMeans(results), sd=apply(results, 2, sd))
    ret
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=1000, parallel=TRUE,
                     debug='none', store_results=TRUE,
                     generate=Generate, fixed_objects=fo,
                     analyse=Analyse, summarise=Summarise,
                     packages=c("relliptical", "lavaan"),
                     filename='HuBentler1999')
res

#reSummarise <- function(condition, results, fixed_objects = NULL) {
#    cut1 <- colMeans(results[,c('cfi', 'tli', 'nnfi', 'rfi', 'nfi',
#                                'ifi', 'rni', 'gfi', 'agfi', 'mfi')] < .90)
#    cut2 <- colMeans(results[,c('rmsea', 'srmr')] > .05)
#    ret <- c(cut=c(cut1, cut2))
#    ret
#}
