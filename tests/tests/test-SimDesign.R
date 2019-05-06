context('SimDesign')

test_that('SimDesign', {

    sample_sizes <- c(10, 20)
    standard_deviations <- c(1, 4)

    Design <- expand.grid(sample_sizes_group1=sample_sizes,
                          sample_sizes_group2=sample_sizes,
                          standard_deviations=standard_deviations)

    mysim <- function(condition, fixed_objects = NULL){

        Attach(condition)

        N1 <- sample_sizes_group1
        N2 <- condition$sample_sizes_group2
        sd <- condition$standard_deviations

        group1 <- rnorm(N1)
        group2 <- rnorm(N2, sd=sd)
        dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))

        return(dat)
    }

    mycompute <- function(condition, dat, fixed_objects = NULL){

        #wrap computational statistics in try() statements to control estimation problems
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    mycompute2 <- function(condition, dat, fixed_objects = NULL){

        if(condition$standard_deviations == 4) stop('error')

        #wrap computational statistics in try() statements to control estimation problems
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    mycollect <-  function(condition, results, fixed_objects = NULL){

        # handy functions
        bias <- function(observed, population) mean(observed - population)
        RMSD <- function(observed, population) sqrt(mean((observed - population)^2))

        #find results of interest here
        nms <- c('welch', 'independent')
        lessthan.05 <- EDR(results[,nms], alpha = .05)

        # return the results that will be appended to the Design input
        ret <- c(lessthan.05=lessthan.05)
        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    mycollect <-  function(condition, results, fixed_objects = NULL){

        # return the results that will be appended to the Design input
        ret <- EDR(results, .05)
        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE,
                           store_results = TRUE)
    out <- extract_results(Final)
    expect_equal(nrow(out[[1L]]), 2L)

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = parallel::detectCores(),
                           parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    # resume
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute2, summarise=mycollect,
                           replications = 2, save=TRUE, verbose = FALSE))
    compname = Sys.info()["nodename"]
    tmp <- readRDS(paste0('SIMDESIGN-TEMPFILE_', compname, '.rds'))
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 2, save=TRUE, verbose = FALSE, filename = 'newfile')
    expect_equal(tmp[[1]]$bias.random_number[1], Final$bias.random_number[1])
    SimClean('newfile.rds', 'SIMDESIGN_CRASHFILE_SEEDS.rds')

    #seeds
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                           replications = parallel::detectCores(),
                           parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    Final2 <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                           replications = parallel::detectCores(),
                           parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    expect_equal(Final$bias.random_number, Final2$bias.random_number, tolerance=1e-6)
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                           replications = parallel::detectCores(), parallel=FALSE, save=FALSE, verbose = FALSE)
    Final2 <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                            replications = parallel::detectCores(), parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_equal(Final$bias.random_number, Final2$bias.random_number, tolerance=1e-6)

    # aggregate test
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, filename='file',
                           replications = 2, parallel=FALSE, save=TRUE, verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=TRUE, filename = 'newfile', verbose = FALSE)
    Final <- aggregate_simulations()
    expect_is(Final, 'data.frame')
    expect_true(all(Final$REPLICATIONS == 4L))
    SimClean(dir()[grepl('\\.rds', dir())])

    # seeds
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, verbose=FALSE,
                         replications = 1, parallel=FALSE, save_seeds=TRUE, max_errors = Inf)
    load_seed <- paste0('design-row-1/seed-1')
    tmp2 <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, verbose=FALSE,
                         replications = 2, parallel=FALSE, load_seed = load_seed)
    expect_equal(tmp[1, ]$bias.random_number, tmp2[1, ]$bias.random_number, tollerance = 1e-4)
    SimClean(seeds = TRUE)

    mycompute <- function(condition, dat, fixed_objects = NULL){

        if(runif(1, 0, 1) < .9) t.test('char')
        if(runif(1, 0, 1) < .9) aov('char')
        if(runif(1, 0, 1) < .2) stop('my error')

        #wrap computational statistics in try() statements to control estimation problems
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, verbose = FALSE, max_errors = Inf)
    expect_is(Final, 'data.frame')
    expect_true(any(grepl('ERROR:', names(Final))))

    # aggregate test
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, filename='this', save=TRUE,
                         max_errors=Inf, verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, max_errors=Inf,
                         replications = 2, parallel=FALSE, filename = 'newfile', save=TRUE,
                         verbose = FALSE)
    Final <- aggregate_simulations()
    expect_is(Final, 'data.frame')
    expect_true(all(Final$REPLICATIONS == 4L))
    SimClean(dir()[grepl('\\.rds', dir())])

    #results
    tmp <- runSimulation(rbind(Design, Design), generate=mysim, analyse=mycompute, summarise=mycollect, verbose=FALSE,
                         replications = 2, parallel=FALSE, save_results = TRUE, max_errors = Inf)
    compname = Sys.info()["nodename"]
    DIR <- paste0("SimDesign-results_", compname)
    expect_true(dir.exists(DIR))
    files <- dir(DIR)
    expect_equal(length(files), 16L)
    x <- readRDS(paste0(DIR, '/', files[1]))
    expect_true(all(names(x) %in% c('condition', 'results', 'errors', 'warnings', "error_seeds")))
    row1 <- SimResults(tmp, 1)
    expect_is(row1, 'list')
    expect_equal(length(row1), 5)
    row1to5 <- SimResults(tmp, 1:5)
    expect_is(row1to5, 'list')
    expect_equal(length(row1to5), 5)
    SimClean(results = TRUE)

    # results no summarise
    mycompute3 <- function(condition, dat, fixed_objects = NULL){

        #wrap computational statistics in try() statements to control estimation problems
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    expect_message(tmp <- runSimulation(Design, generate=mysim, analyse=mycompute3, verbose=FALSE,
                         replications = 2, parallel=FALSE, save_results = TRUE))
    expect_true(all(sapply(tmp, function(x) length(x)) == 4L))

    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute3, summarise=NA,
                         verbose=FALSE, replications = 2, parallel=FALSE, save_results = TRUE)
    expect_is(tmp, 'data.frame')
    expect_true(dir.exists(DIR))
    expect_equal(nrow(tmp), 8)
    SimClean(results = TRUE)
    SimClean(dir()[grepl('\\.rds', dir())])

    # error test
    mycompute <- function(condition, dat, fixed_objects = NULL){
        stop('this error')
    }
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 1, parallel=TRUE, ncores=2L,
                           save=FALSE, verbose = FALSE))

    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=TRUE, ncores=2L,
                               save=TRUE, verbose = FALSE))
    seeds <- readRDS('SIMDESIGN_CRASHFILE_SEEDS.rds')
    # runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
    #               replications = 1, parallel=TRUE, ncores=2L, load_seed=seeds[1,],
    #               save=TRUE, verbose = FALSE, edit='analyse')
    SimClean('SIMDESIGN_CRASHFILE_SEEDS.rds')

    mycompute <- function(condition, dat, fixed_objects = NULL){
        ret <- does_not_exist(TRUE)
        ret
    }
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=TRUE, ncores=2L,
                               save=FALSE, verbose = FALSE))

    mysim <- function(condition, fixed_objects = NULL){
        stop('something silly', call.=FALSE)
    }
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=FALSE, save=FALSE, verbose = FALSE))


    mycompute <- function(condition, dat, fixed_objects = NULL){
        c(ret = 1)
    }
    mygenerate <- function(condition, fixed_objects = NULL){
        rgumbel(5)
    }
    mycollect <- function(condition, results, fixed_objects = NULL) {
        colMeans(results)
    }
    expect_error(runSimulation(Design, replications = 1,
                               generate=mygenerate, analyse=mycompute, summarise=mycollect,
                               parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_error(runSimulation(Design, replications = 1, ncores=2,
                               generate=mygenerate, analyse=mycompute, summarise=mycollect,
                               parallel=TRUE, save=FALSE, verbose = FALSE))
    out <- runSimulation(Design, replications = 1, packages = 'extraDistr',
                         generate=mygenerate, analyse=mycompute, summarise=mycollect,
                         parallel=FALSE, save=FALSE, verbose = FALSE)
    out2 <- runSimulation(Design, replications = 1, packages = 'extraDistr',
                         generate=mygenerate, analyse=mycompute, summarise=mycollect,
                         parallel=TRUE, save=FALSE, verbose = FALSE)
    expect_is(out, 'SimDesign')
    expect_is(out2, 'SimDesign')

    # warnings
    mycompute <- function(condition, dat, fixed_objects = NULL){
        if(sample(c(FALSE, TRUE), 1)) log(-1)
        if(sample(c(FALSE, TRUE), 1)) log(-2)
        if(sample(c(FALSE, TRUE), 1)) warning('Manual warning')
        c(ret = 1)
    }
    results <- runSimulation(Design, replications = 1, packages = 'extraDistr',
                  generate=mygenerate, analyse=mycompute, summarise=mycollect,
                  parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_true(any(grepl('WARNING:', names(results))))
    results <- runSimulation(Design, replications = 1, packages = 'extraDistr', max_errors = Inf,
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save=FALSE, verbose = FALSE, warnings_as_errors=TRUE)
    expect_true(any(grepl('ERROR:', names(results))))
    results <- runSimulation(Design, replications = 1, packages = 'extraDistr',
                  generate=mygenerate, analyse=mycompute, summarise=mycollect,
                  parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    expect_true(any(grepl('WARNING:', names(results))))

    #aggregate different files
    mycompute2 <- function(condition, dat, fixed_objects = NULL){
        if(sample(c(FALSE, TRUE), 1, prob = c(.9, .1))) stop('error')
        c(ret = 1)
    }
    mycompute3 <- function(condition, dat, fixed_objects = NULL){
        c(ret = 1)
    }
    set.seed(1)
    results <- runSimulation(Design, replications = 2, packages = 'extraDistr',
                  generate=mygenerate, analyse=mycompute, summarise=mycollect,
                  parallel=FALSE, save_results = TRUE, verbose = FALSE,
                  save_details = list(save_results_dirname = 'dir1'))
    results <- runSimulation(Design, replications = 2, packages = 'extraDistr',
                  generate=mygenerate, analyse=mycompute2, summarise=mycollect,
                  parallel=FALSE, save_results = TRUE, verbose = FALSE,
                  save_details = list(save_results_dirname = 'dir2'))
    results <- runSimulation(Design, replications = 2, packages = 'extraDistr',
                             generate=mygenerate, analyse=mycompute3, summarise=mycollect,
                             parallel=FALSE, save_results = TRUE, verbose = FALSE,
                             save_details = list(save_results_dirname = 'dir3'))
    aggregate_simulations(dirs = c('dir1', 'dir2', 'dir3'))
    expect_true(dir.exists('SimDesign_aggregate_results'))
    expect_equal(6, nrow(readRDS('SimDesign_aggregate_results/results-row-1.rds')$results))
    SimClean(dirs = c('SimDesign_aggregate_results','dir1', 'dir2', 'dir3'))

    mycompute <- function(condition, dat, fixed_objects = NULL){
        if(sample(c(FALSE, TRUE), 1, prob = c(.9, .1))) stop('error')
        list(ret = 1)
    }
    mycollect <- function(condition, results, fixed_objects = NULL) {
        c(ret=1)
    }
    results <- runSimulation(Design, replications = 2, packages = 'extraDistr',
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save_results = TRUE, verbose = FALSE,
                             save_details = list(save_results_dirname = 'dir1'))
    results <- runSimulation(Design, replications = 2, packages = 'extraDistr',
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save_results = TRUE, verbose = FALSE,
                             save_details = list(save_results_dirname = 'dir2'))
    aggregate_simulations(dirs = c('dir1', 'dir2'))
    expect_true(dir.exists('SimDesign_aggregate_results'))
    expect_equal(4, length(readRDS('SimDesign_aggregate_results/results-row-1.rds')$results))
    SimClean(dirs = c('SimDesign_aggregate_results','dir1', 'dir2'))

    mycompute <- function(condition, dat, fixed_objects = NULL){
        if(sample(c(FALSE, TRUE), 1, prob = c(.5, .5))) warning('This is a warning')
        if(sample(c(FALSE, TRUE), 1, prob = c(.5, .5))) stop('This is an error')
        if(sample(c(FALSE, TRUE), 1, prob = c(.5, .5))) stop('This is a different error')
        list(ret = 1)
    }
    results <- runSimulation(Design, replications = 2, packages = 'extraDistr', seed=1:8,
                             generate=mygenerate, analyse=mycompute, summarise=mycollect, verbose=FALSE)
    seeds <- extract_error_seeds(results)
    expect_is(seeds, 'data.frame')
    expect_true(nrow(seeds) == 626)
    if(interactive()){
        results <- runSimulation(Design, replications = 2, packages = 'extraDistr', seed=1:8,
                                 generate=mygenerate, analyse=mycompute, summarise=mycollect,
                                 load_seed=seeds$Design_row_1.1..This.is.an.error., edit='analyse')
    }

    # NAs
    mycompute <- function(condition, dat, fixed_objects = NULL){
        ret <- c(ret = sample(c(NA, 1), 1, prob = c(.1, .9)))
        ret
    }

    results <- runSimulation(Design, replications = 10, packages = 'extraDistr', seed=1:nrow(Design),
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_equal(names(results)[5], "ERROR: .Error : The following return NA and required redrawing: ret\n")
    expect_equal(results[,5], c(NA,1,NA,3,4,1,NA,4))

    #data.frame test
    mysim <- function(condition, fixed_objects = NULL){
        N1 <- condition$sample_sizes_group1
        N2 <- condition$sample_sizes_group2
        sd <- condition$standard_deviations
        group1 <- rnorm(N1)
        group2 <- rnorm(N2, sd=sd)
        dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
        dat
    }

    mycompute <- function(condition, dat, fixed_objects = NULL){
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)
        ret <- data.frame(welch = welch$p.value, independent = ind$p.value)
        ret
    }

    mycollect <-  function(condition, results, fixed_objects = NULL){
        ret <- EDR(results, alpha = .05)
        ret
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    # Maintain attributes after subsetting results
    F1 <- subset(Final, select=1:4) # 3 design factors, 1 simulation result
    expect_is(F1, 'SimDesign')
    expect_that(length(F1), equals(4))
    expect_that(length(attributes(F1)$design_names$design), equals(3))
    expect_that(length(attributes(F1)$design_names$sim), equals(1))

    F2 <- subset(Final, select = c(1,2,4,5)) # 2 design factors, 2 simulation results
    expect_is(F2, 'SimDesign')
    expect_that(length(F2), equals(4))
    expect_that(length(attributes(F2)$design_names$design), equals(2))
    expect_that(length(attributes(F2)$design_names$sim), equals(2))

    F3 <- subset(Final, subset = standard_deviations == 1)
    expect_is(F3, 'SimDesign')
    expect_that(nrow(F3), equals(4))

    # dummy run with no design
    Generate <- function(condition, fixed_objects = NULL)
        rnorm(100, mean = 10)
    Analyse <- function(condition, dat, fixed_objects = NULL)
        t.test(dat)$conf.int
    Analyse2 <- function(condition, dat, fixed_objects = NULL){
        CIs <- t.test(dat)$conf.int
        names(CIs) <- c('lower', 'upper')
        CIs
    }
    Summarise <- function(condition, results, fixed_objects = NULL)
        ECR(results, 10)

    results <- runSimulation(replications = 10, generate = Generate,
                             analyse=Analyse, verbose=FALSE)
    expect_is(results, 'matrix')
    expect_equal(ncol(results), 2L)

    results <- runSimulation(replications = 10, generate = Generate,
                             analyse=Analyse2, summarise = Summarise, verbose=FALSE)
    expect_is(results, 'data.frame')
    expect_equal(ncol(results), 5L)

    # dummy run with no design and returning lists
    Generate <- function(condition, fixed_objects = NULL)
        rnorm(100, mean = 10)
    Analyse <- function(condition, dat, fixed_objects = NULL){
        ret <- list(val1=0, val2=t.test(dat)$conf.int)
        ret
    }
    results <- runSimulation(replications = 10, generate = Generate,
                             analyse=Analyse, verbose=FALSE)
    expect_equal(length(results), 10L)
    expect_equal(length(results[[1L]]), 2L)
    expect_equal(length(results[[1L]][[2]]), 2L)

    # stop and resume
    Design <- data.frame(N=c(10, 20))
    Generate <- function(condition, fixed_objects = NULL)
        rnorm(condition$N, mean = 10)
    Analyse1 <- function(condition, dat, fixed_objects = NULL){
        Attach(condition)
        if(N == 20) stop('Oh no, not 20!')
        mean(dat)
    }
    Analyse2 <- function(condition, dat, fixed_objects = NULL)
        mean(dat)

    Summarise <- function(condition, results, fixed_objects = NULL)
        bias(results, 0)
    expect_error(runSimulation(Design, replications = 10, save=TRUE, save_details = list(tmpfilename = 'thisfile.rds'),
                  generate=Generate, analyse=Analyse1, summarise=Summarise, verbose=FALSE))
    expect_true('thisfile.rds' %in% dir())
    SimClean('thisfile.rds', 'SIMDESIGN_CRASHFILE_SEEDS.rds')
    results <- runSimulation(Design, replications = 10, save=TRUE, save_details = list(tmpfilename = 'thisfile'),
                               generate=Generate, analyse=Analyse2, summarise=Summarise, filename = 'thatfile', verbose=FALSE)
    expect_true('thatfile.rds' %in% dir())
    SimClean('thatfile.rds')

    results <- runSimulation(Design, replications = 10, save=TRUE, save_details = list(tmpfilename = 'thisfile', out_rootdir = "~/mytmpdir"),
                             generate=Generate, analyse=Analyse2, summarise=Summarise, filename = 'thatfile',
                             verbose=FALSE)
    expect_true('thatfile.rds' %in% dir("~/mytmpdir"))
    SimClean('thatfile.rds', save_details = list(out_rootdir = "~/mytmpdir"))
    expect_false('thatfile.rds' %in% dir("~/mytmpdir"))

    gen_anal <- function(condition, dat, fixed_objects = NULL){
        dat <- rnorm(100)
        mean(dat)
    }
    Summarise <- function(condition, results, fixed_objects = NULL)
        bias(results, 0)
    results <- runSimulation(replications = 10, analyse=gen_anal,
                             summarise=Summarise, verbose=FALSE)
    expect_is(results, 'SimDesign')

})

