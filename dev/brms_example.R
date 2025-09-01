#-------------------------------------------------------------------

library(SimDesign)
library(brms)

Design <- createDesign(N=c(30, 60, 90),
                       method=c('lm', 'brms'),
                       b0=1, b1=2, s=1)

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects) {
    Attach(condition)
    X <- rep(0:1, each=N)
    y <- b0 + b1*X + rnorm(N, sd=s)
    dat <- data.frame(X, y)
    dat
}

Analyse <- function(condition, dat, fixed_objects) {
    if(condition$method == 'brms'){
        mod <- brm(y ~ X, data=dat) # recompiles every time
        ests <- fixef(mod)
        vars <- VarCorr(mod)
        ret <- c(beta0=ests[1,1], beta1=ests[2,1],
                 sigma=vars$residual__$sd[1])
    } else {
        lm.mod <- lm(y~X, data=dat)
        ret <- c(beta0=unname(coef(lm.mod)[1]),
                 beta1=unname(coef(lm.mod)[2]),
                 sigma=summary(lm.mod)$sigma)
    }
    ret
}

Summarise <- function(condition, results, fixed_objects) {
    parameters <- as.numeric(condition[c('b0', 'b1', 's')])
    bias <- bias(results, parameters)
    rmsd <- RMSD(results, parameters)
    c(bias=bias, rmsd=rmsd)
}

#-------------------------------------------------------------------

if(FALSE){ # not run, as brms compiles too often
    res <- runSimulation(design=Design, replications=1000, generate=Generate,
                         analyse=Analyse, summarise=Summarise)
    res
}


#-------------------------------------------------------------------
# brms using precompiled models that are update()ed in simulation

# Precompile models, and meaningfully store. If possible, save to external file
filename <- 'brms_precompile.rds'
if(!file.exists(filename)){
    precompile <- list()
    for(i in 1:3){
        dat <- Generate(Design[i,])
        mod <- brm(y ~ X, data=dat) # compile model and store
        precompile[[as.character(Design[i,]$N)]] <- mod
    }
    saveRDS(precompile, filename)
} else precompile <- readRDS(filename)
names(precompile)

# redefine Analyse() to use precompiled objects and update()
Analyse_precompile <- function(condition, dat, fixed_objects) {
    if(condition$method == 'brms'){
        precompile <- fixed_objects[[as.character(condition$N)]]
        # update previous model. Note that data-dependent priors
        # are not updated automatically
        mod <- update(precompile, newdata=dat)
        ests <- fixef(mod)
        vars <- VarCorr(mod)
        ret <- c(beta0=ests[1,1], beta1=ests[2,1],
                 sigma=vars$residual__$sd[1])
    } else {
        mod <- lm(y~X, data=dat)
        ret <- c(beta0=unname(coef(mod)[1]),
                 beta1=unname(coef(mod)[2]),
                 sigma=summary(mod)$sigma)
    }
    ret
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=1000, generate=Generate,
                     analyse=Analyse_precompile, summarise=Summarise,
                     fixed_objects=precompile, parallel=TRUE, not_parallel=1:3)
res


#-------------------------------------------------------------------
# If you're lucky, some precompiles and be re-used across different 
# data conditions

# Precompile models, and meaningfully store. If possible, save to external file
filename <- 'brms_precompile_single.rds'
if(!file.exists(filename)){
	dat <- Generate(Design[1,])
	mod <- brm(y ~ X, data=dat) # compile model and store
	precompile <- mod
	saveRDS(precompile, filename)
} else precompile <- readRDS(filename)

# redefine Analyse() to use precompiled objects and update()
Analyse_precompile <- function(condition, dat, fixed_objects) {
	if(condition$method == 'brms'){
		precompile <- fixed_objects
		mod <- update(precompile, newdata=dat)
		ests <- fixef(mod)
		vars <- VarCorr(mod)
		ret <- c(beta0=ests[1,1], beta1=ests[2,1],
				 sigma=vars$residual__$sd[1])
	} else {
		mod <- lm(y~X, data=dat)
		ret <- c(beta0=unname(coef(mod)[1]),
				 beta1=unname(coef(mod)[2]),
				 sigma=summary(mod)$sigma)
	}
	ret
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=1000, generate=Generate,
					 analyse=Analyse_precompile, summarise=Summarise,
					 fixed_objects=precompile, parallel=TRUE, not_parallel=1:3)
res


