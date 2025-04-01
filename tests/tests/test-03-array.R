context('array')

test_that('array', {

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

    Analyse.slow <- function(condition, dat, fixed_objects) {
        Sys.sleep(1)
        ret <- c(mean=mean(dat), median=median(dat)) # mean/median of sample data
        ret
    }

    Summarise <- function(condition, results, fixed_objects){
        colMeans(results)
    }

    # time test across conditions
    expect_error(runSimulation(design=Design, replications=5, generate=Generate,
                  analyse=Analyse.slow, summarise=Summarise,
                  control = list(max_time = "00:00:06", max_RAM = "4GB"),
                  verbose=FALSE))
    files <- dir()
    file <- files[grepl('SIMDESIGN-TEMPFILE', files)]
    res <- readRDS(file)
    expect_true(is.null(res[[3]]))

    # resume from time crash
    res2 <- runSimulation(design=Design, replications=5, generate=Generate,
                          analyse=Analyse.slow, summarise=Summarise,
                          control = list(max_time = "00:00:20", max_RAM = "4GB"),
                          verbose=FALSE)
    expect_true(is(res2, 'SimDesign'))
    results <- SimResults(res2)
    expect_true(nrow(results) == 15)

    # define initial seed (do this only once to keep it constant!)
    # iseed <- genSeeds()
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
                              iseed=iseed, filename='mysim',
                              control=list(store_Random.seeds=TRUE), verbose=FALSE)
    results <- SimExtract(res, what='results') # condition and replication count stored
    expect_equal(results$condition[1], 1)
    expect_equal(results$arrayID[1], 1)
    randseeds <- SimExtract(res, what='Random.seeds')
    expect_true(all(dim(randseeds[[1L]]) == c(50,7)))

    SimClean('mysim-1.rds')

    ########################
    # Same submission job as above, however split the replications over multiple
    # evaluations and combine when complete
    Design5 <- expandDesign(Design, 5)

    # iseed <- genSeeds()
    iseed <- 554184288

    # arrayID <- getArrayID(type = 'slurm')
    arrayID <- 14L

    # run the simulation (replications reduced per row, but same in total)
    runArraySimulation(design=Design5, replications=10,
                       generate=Generate, analyse=Analyse,
                       summarise=Summarise, iseed=iseed,
                       filename='mylongsim', arrayID=arrayID, verbose=FALSE)

    res <- readRDS('mylongsim-14.rds')
    results <- SimResults(res) # condition and replication count stored
    expect_equal(results$condition[1], 3)
    expect_equal(results$arrayID[1], 14)
    randseeds <- SimExtract(res, what='Random.seeds')
    expect_true(is.null(randseeds))

    SimClean('mylongsim-14.rds')

    Analyse_big <- function(condition, dat, fixed_objects) {
        ret <- runif(1e4)
        names(ret) <- paste0('x', 1:length(ret))
        # object.size(ret) |> format('MB')
        ret
    }

    tmp <- gc()
    runArraySimulation(design=Design5, replications=1000,
                       generate=Generate, analyse=Analyse_big,
                       summarise=Summarise, iseed=iseed,
                       filename='mylongsim', arrayID=arrayID,
                       control = list(max_RAM="150MB"), verbose=FALSE)
    res <- readRDS("mylongsim-14.rds")
    expect_true(res$REPLICATIONS == 1000L)
    SimClean('mylongsim-14.rds')

    ###
    # emulate the arrayID distribution, storing all results in a 'sim/' folder
    dir.create('sim/')

    # Emulate distribution to nrow(Design5) = 15 independent job arrays
    sapply(1:nrow(Design5), \(arrayID)
           runArraySimulation(design=Design5, replications=10,
                              generate=Generate, analyse=Analyse,
                              summarise=Summarise, iseed=iseed, arrayID=arrayID,
                              dirname='sim', filename='condition',   # saved to 'sim/condition-#.rds'
                              control = list(max_time = "04:00:00", max_RAM = "4GB"),
                              verbose=FALSE)) |> invisible()

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
    final <- SimCollect(files=dir())
    so <- summary(final)
    expect_equal(so$ncores, 15L)
    results <- SimResults(final)

    expect_equal(final$REPLICATIONS, c(50, 50, 50))
    expect_equal(dim(results), c(150, 5))

    setwd('..')
    SimClean(dirs='sim/')

    #####################

    # list return
    Analyse_list <- function(condition, dat, fixed_objects) {
        list(a=1:2, b=3:4)
    }

    Summarise_list <- function(condition, results, fixed_objects) {
        42
    }

    arrayID <- 1

    runArraySimulation(design=Design5, replications=10,
                       generate=Generate, analyse=Analyse_list,
                       summarise=Summarise_list, iseed=iseed, arrayID=arrayID,
                       dirname='sim', filename='condition', verbose=FALSE) |> invisible()
    res <- readRDS("sim/condition-1.rds")
    results <- SimExtract(res, 'results')
    expect_true(is.list(results))
    SimClean(dirs="sim/")

    # emulate the arrayID distribution, storing all results in a 'sim/' folder
    dir.create('sim/')

    # Emulate distribution to nrow(Design5) = 15 independent job arrays
    sapply(1:nrow(Design5), \(arrayID)
           runArraySimulation(design=Design5, replications=10,
                              generate=Generate, analyse=Analyse_list,
                              summarise=Summarise_list, iseed=iseed, arrayID=arrayID,
                              dirname='sim', filename='condition', verbose=FALSE)) |> invisible()

    files <- dir('sim/')
    expect_true(all(files %in% paste0('condition-', 1:nrow(Design5), '.rds')))

    setwd('sim')
    condition14 <- readRDS('condition-14.rds')
    results <- SimExtract(condition14, 'results')
    expect_equal(results[[1]]$condition, 3)
    expect_equal(results[[1]]$arrayID, 14)

    # aggregate simulation results into single file
    final <- SimCollect(files=dir())
    so <- summary(final)
    expect_equal(so$ncores, 15L)
    results <- SimResults(final)

    expect_equal(final$REPLICATIONS, c(50, 50, 50))
    expect_equal(length(results), 150)

    setwd('..')
    SimClean(dirs='sim/')

    #####################

    # summary list

    Summarise_list <- function(condition, results, fixed_objects) {
        list(a=42, b=rnorm(5))
    }

    # Emulate distribution to nrow(Design5) = 15 independent job arrays
    sapply(1:nrow(Design5), \(arrayID)
           runArraySimulation(design=Design5, replications=10,
                              generate=Generate, analyse=Analyse,
                              summarise=Summarise_list, iseed=iseed, arrayID=arrayID,
                              dirname='sim', filename='condition', verbose=FALSE)) |> invisible()

    files <- dir('sim/')
    expect_true(all(files %in% paste0('condition-', 1:nrow(Design5), '.rds')))

    setwd('sim')
    # final <- SimCollect(files=dir())
    so <- summary(final)
    expect_equal(so$ncores, 15L)
    results <- SimResults(final)

    expect_equal(final$REPLICATIONS, c(50, 50, 50))
    expect_equal(length(results), 150)

    setwd('..')
    SimClean(dirs='sim/')

})

