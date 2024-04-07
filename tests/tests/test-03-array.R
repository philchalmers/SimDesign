context('array')

test_that('array', {

    library(SimDesign)

    Design <- createDesign(N = c(10, 20, 30))

    Generate <- function(condition, fixed_objects = NULL) {
        dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
        dat
    }

    Analyse <- function(condition, dat, fixed_objects = NULL) {
        ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
        ret
    }

    Summarise <- function(condition, results, fixed_objects = NULL){
        colMeans(results)
    }

    # define initial seed (do this only once to keep it constant!)
    # iseed <- gen_seeds()
    iseed <- 554184288

    ### On cluster submission, the active array ID is obtained via getArrayID(),
    ###   and therefore should be used in real SLURM submissions
    expect_warning(arrayID <- getArrayID(type = 'slurm'))

    # However, for the following example array ID is set to first row only
    arrayID <- 1L

    # run the simulation (results not caught on job submission, only files saved)
    res <- runArraySimulation(design=Design, replications=50,
                              generate=Generate, analyse=Analyse,
                              summarise=Summarise, arrayID=arrayID,
                              iseed=iseed, filename='mysim') # saved as 'mysim-1.rds'
    results <- SimExtract(res, what='results') # condition and replication count stored
    expect_equal(results$condition[1], 1)
    expect_equal(results$arrayID[1], 1)

    SimClean('mysim-1.rds')

    ########################
    # Same submission job as above, however split the replications over multiple
    # evaluations and combine when complete
    Design5 <- expandDesign(Design, 5)

    # iseed <- gen_seeds()
    iseed <- 554184288

    # arrayID <- getArrayID(type = 'slurm')
    arrayID <- 14L

    # run the simulation (replications reduced per row, but same in total)
    runArraySimulation(design=Design5, replications=10,
                       generate=Generate, analyse=Analyse,
                       summarise=Summarise, iseed=iseed,
                       filename='mylongsim', arrayID=arrayID)

    res <- readRDS('mylongsim-14.rds')
    results <- SimResults(res) # condition and replication count stored
    expect_equal(results$condition[1], 3)
    expect_equal(results$arrayID[1], 14)

    SimClean('mylongsim-14.rds')


    ###
    # emulate the arrayID distribution, storing all results in a 'sim/' folder
    dir.create('sim/')

    # Emulate distribution to nrow(Design5) = 15 independent job arrays
    sapply(1:nrow(Design5), \(arrayID)
           runArraySimulation(design=Design5, replications=10,
                              generate=Generate, analyse=Analyse,
                              summarise=Summarise, iseed=iseed, arrayID=arrayID,
                              filename='sim/condition',   # saved to 'sim/condition-#.rds'
                              control = list(max_time = 4))) |> invisible()

    #  If necessary, conditions above will manually terminate before
    #  4 hours, returning any successfully completed results before the HPC
    #  session times out (provided shell specified more than 4 hours)

    # list saved files
    files <- dir('sim/')
    expect_true(all(files %in% paste0('condition-', 1:nrow(Design5), '.rds')))

    setwd('sim')
    condition14 <- readRDS('condition-14.rds')
    results <- SimExtract(condition14, 'results')
    expect_equal(results$condition[1], 3)
    expect_equal(results$arrayID[1], 14)

    # aggregate simulation results into single file
    final <- aggregate_simulations(files=dir())
    results <- SimResults(final)

    expect_equal(final$REPLICATIONS, c(50, 50, 50))
    expect_equal(dim(results), c(150, 5))

    setwd('..')
    SimClean(dirs='sim/')

})

