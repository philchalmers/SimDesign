library(SimDesign)

Design <- createDesign(N = c(10, 20, 30))

Generate <- function(condition, fixed_objects) {
 dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
 dat
}

Analyse <- function(condition, dat, fixed_objects) {
 ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
 ret
}

Summarise <- function(condition, results, fixed_objects){
 colMeans(results)
}

# iseed <- genSeeds()
iseed <- 554184288

### On cluster submission, the active array ID is obtained via getArrayID(),
###   and therefore should be used in real SLURM submissions
arrayID <- getArrayID(type = 'slurm')

# run the simulation (results not caught on job submission, only files saved)
res <- runArraySimulation(design=Design, replications=50,
                   generate=Generate, analyse=Analyse, parallel=TRUE,
                   summarise=Summarise, arrayID=arrayID,dirname = 'parallel',
                   iseed=iseed, filename='mysim')
