library(SimDesign)

Design <- createDesign(N = c(10, 20, 30))

# help(Generate)
Generate <- function(condition, fixed_objects = NULL) {
    dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
    dat
}

# help(Analyse)
Analyse <- function(condition, dat, fixed_objects = NULL) {
    ret <- mean(dat) # mean of the sample data vector
    ret
}

# help(Summarise)
Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
    ret
}


library(doMPI)
cl <- startMPIcluster()
registerDoMPI(cl)

runSimulation(design=Design, replications=12, filename='mysimulation',
              generate=Generate, analyse=Analyse, summarise=Summarise,
              control = list(MPI=TRUE))

closeCluster(cl)
mpi.quit()
