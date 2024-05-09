#' ---
#' title: "Independent samples t-tests"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' author: Phil Chalmers
#' ---

#' # Step 1: create design and solve for missing values (NA)

library(SimDesign)

Design <- createDesign(N = NA,
					   d = c(.2, .5, .8),
					   sig.level = .05)
Design    # solve for NA's


#' # Step 2 --- Define generate, analyse, and summarise functions

Generate <- function(condition, fixed_objects = NULL) {
	Attach(condition)
	group1 <- rnorm(N)
	group2 <- rnorm(N, mean=d)
	dat <- data.frame(group = gl(2, N, labels=c('G1', 'G2')),
					  DV = c(group1, group2))
	dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
	p <- t.test(DV ~ group, dat, var.equal=TRUE)$p.value
	p
}

Summarise <- function(condition, results, fixed_objects = NULL) {
	# Must return a single number corresponding to f(x) in the
	# root equation f(x) = b

	ret <- EDR(results, alpha = condition$sig.level)
	ret
}


#' # Step 3 --- Optimize N over the rows in design

# Initial search between N = [10,500] for each row using the default
# integer solver (integer = TRUE)

# In this example, b = target power.
# Terminate if prediction CI is consistently within [.795, .805]
solved <- SimSolve(design=Design, b=.8, interval=c(10, 500),
				   generate=Generate, analyse=Analyse,
				   summarise=Summarise, predCI.tol = .01)
solved
summary(solved)
plot(solved, 1)
plot(solved, 2)
plot(solved, 3)

# also can plot median history and estimate precision
plot(solved, 1, type = 'history')
plot(solved, 1, type = 'density')

# verify with true power from pwr package
library(pwr)
pwr.t.test(d=.2, power = .8, sig.level = .05)
pwr.t.test(d=.5, power = .8, sig.level = .05)
pwr.t.test(d=.8, power = .8, sig.level = .05)

# use estimated N results to see how close power was
N <- solved$N
pwr.t.test(d=.2, n=N[1], sig.level = .05)
pwr.t.test(d=.5, n=N[2], sig.level = .05)
pwr.t.test(d=.8, n=N[3], sig.level = .05)

# with rounding
N <- ceiling(solved$N)
pwr.t.test(d=.2, n=N[1], sig.level = .05)
pwr.t.test(d=.5, n=N[2], sig.level = .05)
pwr.t.test(d=.8, n=N[3], sig.level = .05)


#' # Solving effect sizes

#' Similar setup as above, however goal is now to solve d given sample
#' size and power inputs (inputs for root no longer required to be an integer)

#' # Step 1: Create design and solve for missing NAs (effect size d, in this case)
Design <- createDesign(N = c(100, 50, 25),
					   d = NA,
					   sig.level = .05)
Design    # solve for NA's

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 2 --- Define generate, analyse, and summarise functions (same as above)

#~~~~~~~~~~~~~~~~~~~~~~~~
#### Step 3 --- Optimize d over the rows in design
# search between d = [.1, 2] for each row

# In this example, b = target power
# note that integer = FALSE to allow smooth updates of d, and convergence
# is based on whether the prediction CI is consistently within [.795, .805]
solved <- SimSolve(design=Design, b = .8, interval=c(.1, 2),
				   generate=Generate, analyse=Analyse,
				   summarise=Summarise, integer=FALSE, predCI.tol=.01)
solved
summary(solved)
plot(solved, 1)
plot(solved, 2)
plot(solved, 3)

#' Verify with true power from pwr package.
library(pwr)
pwr.t.test(n=100, power = .8, sig.level = .05)
pwr.t.test(n=50, power = .8, sig.level = .05)
pwr.t.test(n=25, power = .8, sig.level = .05)

# use estimated d results to see how close power was
pwr.t.test(n=100, d = solved$d[1], sig.level = .05)
pwr.t.test(n=50, d = solved$d[2], sig.level = .05)
pwr.t.test(n=25, d = solved$d[3], sig.level = .05)

