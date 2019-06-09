#-------------------------------------------------------------------

# Flora and Curran (2004)

library(SimDesign)

# NOTE: row 30 will only have 4 categores, not 5, due to tau cut-offs
Design <- expand.grid(N=c(100, 200, 500, 1000),
					  categories = c(2,5),
					  skew_kurt = list(c(0,0), c(.75, 1.75), c(.75, 3.75),
					  					 c(1.25, 1.75), c(1.25, 3.75)),
					  model = c(1,2,3,4),
					  estimator = c('DWLS', 'WLS'), stringsAsFactors = FALSE)

# remove super unstable WLS rows (p. 483)
Design <- subset(Design, !(estimator == 'WLS' & N %in% c(100, 200) &
                               model == 4))

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
	Attach(condition)
	J <- ifelse(model %in% c(1, 3), 5, 10)
	syntax <- if(model == 1 || model == 2){
		paste0(paste0('f1 =~ ', paste0(rep(.7, J), paste0('*x', 1:J),
		                               collapse=' + '), ' \n '),
			   paste0(sprintf('x%s ~~ 0.51*x%s', 1:J, 1:J), collapse=' \n '))
	} else if(model == 3 || model == 4){
		paste0(paste0('f1 =~ ', paste0(rep(.7, J), paste0('*x', 1:J),
		                               collapse=' + '), ' \n '),
			   paste0('f2 =~ ', paste0(rep(.7, J), paste0('*x', 1:J + J),
			                           collapse=' + '), ' \n '),
			   'f1 ~~ .3*f2 \n',
			   paste0(sprintf('x%s ~~ 0.51*x%s', 1:J, 1:J), collapse=' \n '))
	}
	while(TRUE){
		cdat <- simulateData(syntax, model.type = 'cfa', sample.nobs = N,
							 skewness = skew_kurt[[1L]][1L],
							 kurtosis = skew_kurt[[1L]][2L])
		tau <- if(categories == 2) 0
		else if(model != 4) c(-1.645, -0.643, 0.643, 1.645)
		# lowest value from DF's dissertation to fix sparseness
		else c(-1.125, -0.643, 0.643, 1.645)

		dat <- apply(cdat, 2, function(x, tau){
			dat <- numeric(length(x))
			if(length(tau) == 1) dat <- ifelse(x > tau, 1, 0)
			else for(i in seq_len(4)){
				dat[x > tau[i]] <- i
			}
			dat
		}, tau=tau)
		if(all(apply(dat, 2, function(x)
		    length(unique(x))) == categories)) break
		break
	}
	dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
	Attach(condition)
	J <- ifelse(model %in% c(1, 3), 5, 10)
	syntax <- if(model == 1 || model == 2){
		pick_lambdas <- matrix(TRUE, J, 1)
		paste0(paste0('f1 =~ NA*x1 + ', paste0(paste0('x', 2:J),
		                                       collapse=' + ')), '\n f1 ~~ 1*f1')
	} else if(model == 3 || model == 4){
		pick_lambdas <- matrix(TRUE, J*2, 2)
		pick_lambdas[(J+1):(J*3)] <- FALSE
		paste0(paste0('f1 =~ NA*x1 + ', paste0(paste0('x', 2:J), collapse=' + ')),
			   paste0(sprintf('\nf2 =~ NA*x%s + ', J+1),
			          paste0(paste0('x', 2:J + J), collapse=' + ')),
			   '\nf1 ~~ 1*f1 \n f2 ~~ 1*f2 \nf1 ~~ f2')
	}
	mod <- cfa(syntax, dat, ordered = colnames(dat), estimator = estimator)
	if(!lavInspect(mod, 'converged')) stop('Model did not converge')
	cfs <- lavInspect(mod, what="std")$lambda[pick_lambdas]
	psi12 <- if(model %in% c(3, 4)) lavInspect(mod, what="std")$psi[1,2] else .3
	fit <- fitMeasures(mod)
	ses <- lavInspect(mod, what="se")$lambda[pick_lambdas]
	ret <- c(X2_rbias = unname(bias(fit['chisq'], fit['df'], type='relative') * 100),
			 p = unname(EDR(fit['pvalue'])),
			 cfi = unname(fit['cfi']),
			 bias = unname(bias(cfs, .7)),
			 rbias = unname(bias(cfs, .7, type='relative')) * 100,
			 RMSE = unname(RMSE(cfs, .7)),
			 psi12 = psi12,
			 bias_psi12 = bias(psi12, .3),
			 rbias_psi12 = bias(psi12, .3, type='relative') * 100,
			 RMSE_psi12 = unname(RMSE(psi12, .3)),
			 se = mean(ses))
	ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
	c(colMeans(results),
	  se_ratio = sd(results[,'bias']) / mean(results[,'se']),
	  sd=apply(results, 2, sd))
}

#-------------------------------------------------------------------

results <- runSimulation(design=Design, replications=500, generate=Generate,
						 analyse=Analyse, summarise=Summarise,
						 packages = 'lavaan', parallel = TRUE, save = TRUE,
						 filename = 'FloraCurran2004')
