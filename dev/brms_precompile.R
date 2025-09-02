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

# Precompile models and save to external .rds file
f <- 'precompile.rds'
if(!file.exists(f)){
	precompile <- list()
	for(i in 1:3){
		dat <- Generate(Design[i,])
		mod <- brm(y ~ X, data=dat) # compile model and store
		precompile[[as.character(Design[i,]$N)]] <- mod
	}
	saveRDS(precompile, f)
}
