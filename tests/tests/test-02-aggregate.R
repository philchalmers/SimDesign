context('aggregate')

test_that('aggregate', {

    library(SimDesign)

    sample_sizes <- c(10, 20)
    standard_deviations <- c(1, 4)

    Design <- createDesign(sample_sizes_group1=sample_sizes,
                           sample_sizes_group2=sample_sizes,
                           standard_deviations=standard_deviations)

    mysim <- function(condition, fixed_objects){

        Attach(condition)

        N1 <- sample_sizes_group1
        N2 <- condition$sample_sizes_group2
        sd <- condition$standard_deviations

        group1 <- rnorm(N1)
        group2 <- rnorm(N2, sd=sd)
        dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))

        return(dat)
    }

    mycompute <- function(condition, dat, fixed_objects){

        #wrap computational statistics in try() statements to control estimation problems
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- nc(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    mycompute2 <- function(condition, dat, fixed_objects){

        if(condition$standard_deviations == 4) stop('error')

        #wrap computational statistics in try() statements to control estimation problems
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- nc(welch$p.value, ind$p.value)

        return(ret)
    }

    mycollect <-  function(condition, results, fixed_objects){

        #find results of interest here
        nms <- c('welch', 'independent')
        lessthan.05 <- EDR(results[,nms], alpha = .05)

        # return the results that will be appended to the Design input
        ret <- c(lessthan.05=lessthan.05)
        return(ret)
    }

    # aggregate tests
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, filename='file',
                         replications = 2, parallel=FALSE, store_results = TRUE, verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, store_results = TRUE,
                         filename = 'newfile', verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, store_results = TRUE,
                         filename = 'newfile2', verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, store_results = TRUE,
                         filename = 'newfile3', verbose = FALSE)
    Final <- SimCollect(files = c('file.rds', 'newfile.rds'))
    expect_is(Final, 'data.frame')
    expect_true(all(Final$REPLICATIONS == 4L))
    expect_equal(nrow(SimExtract(Final, 'results')), 4 * nrow(Design))
    saveRDS(Final, 'collect1.rds')
    Final2 <- SimCollect(files = c('newfile2.rds', 'newfile3.rds'))
    expect_is(Final2, 'data.frame')
    expect_true(all(Final2$REPLICATIONS == 4L))
    expect_equal(nrow(SimExtract(Final2, 'results')), 4 * nrow(Design))
    saveRDS(Final2, 'collect2.rds')

    # aggregate the aggregates
    Final4 <- SimCollect(files = c('collect1.rds', 'collect2.rds'))
    expect_is(Final4, 'data.frame')
    expect_true(all(Final4$REPLICATIONS == 8L))
    expect_equal(nrow(SimExtract(Final4, 'results')), 8 * nrow(Design))

    # select
    expect_true(ncol(SimExtract(tmp, 'results')) == 5L)
    Final <- SimCollect(files = c('file.rds', 'newfile.rds'),
                                   select=c("welch", 'independent'))
    expect_true(ncol(SimExtract(Final, 'results')) == 2L)
    Final <- SimCollect(files = c('file.rds', 'newfile.rds'),
                                   select='NONE')
    expect_true(is.null(SimExtract(Final, 'results')))
    SimClean(dir()[grepl('\\.rds', dir())])

    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, save_results = TRUE, verbose = FALSE)
    tmp2 <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, save_results = TRUE,
                         verbose = FALSE)

    dirs <- c(SimExtract(tmp, 'save_results_dirname'),
              SimExtract(tmp2, 'save_results_dirname'))
    SimCollect(dirs = dirs)
    row1 <- readRDS('SimDesign_aggregate_results/results-row-1.rds')
    expect_equal(nrow(row1$results), 4L)
    SimClean(dirs = c(dirs, "SimDesign_aggregate_results"))

    # seeds
    # TODO this fails, but it shouldn't be used anyway
    # tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, verbose=FALSE,
    #                      replications = 1, parallel=FALSE, save_seeds=TRUE, max_errors = Inf)
    # load_seed <- paste0('design-row-1/seed-1')
    # tmp2 <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, verbose=FALSE,
    #                      replications = 2, parallel=FALSE, load_seed = load_seed)
    # SimClean(seeds = TRUE)

    mycompute <- function(condition, dat, fixed_objects){

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
    expect_true(any(grepl('ERROR', names(Final))))
    error_seeds <- SimExtract(Final, what = 'error_seeds')
    expect_true(dim(error_seeds)[1L] > 0)

    # aggregate test
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, filename='this', save=TRUE,
                         max_errors=Inf, verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, max_errors=Inf,
                         replications = 2, parallel=FALSE, filename = 'newfile', save=TRUE,
                         verbose = FALSE)
    Final <- SimCollect(c('this.rds', 'newfile.rds'))
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
    expect_true(all(names(x) %in% c('condition', 'results', 'errors', 'warnings', "error_seeds",
                                    'warning_seeds')))
    row1 <- SimResults(tmp, 1)
    expect_is(row1, 'list')
    expect_equal(length(row1), 6)
    row1to5 <- SimResults(tmp, 1:5)
    expect_is(row1to5, 'list')
    expect_equal(length(row1to5), 5)
    SimClean(results = TRUE)

    # reSummarise test
    mycomputeGood <- function(condition, dat, fixed_objects){

        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    tmp <- runSimulation(Design, generate=mysim, analyse=mycomputeGood, summarise=mycollect, verbose=FALSE,
                         replications = 10, boot_method = 'basic')
    expect_true(all(dim(tmp) == c(8,13)))

    tmp <- runSimulation(rbind(Design, Design), generate=mysim, analyse=mycomputeGood, summarise=mycollect, verbose=FALSE,
                         replications = 10, parallel=FALSE, save_results = TRUE)
    out <- reSummarise(summarise = mycollect, dir=DIR)
    expect_true(all(dim(out) == c(16,5)))
    out <- reSummarise(summarise = mycollect, dir=DIR, boot_method = 'basic')
    expect_true(all(dim(out) == c(16,9)))
    SimClean(results = TRUE)

    #aggregate different files
    mycompute <- function(condition, dat, fixed_objects) {
        c(ret = 1)
    }
    mygenerate <- function(condition, fixed_objects) {
        rgumbel(5)
    }
    mycollect <- function(condition, results, fixed_objects) {
        mean(results$ret)
    }

    mycompute2 <- function(condition, dat, fixed_objects){
        if(sample(c(FALSE, TRUE), 1, prob = c(.9, .1))) stop('error')
        c(ret = 1)
    }
    mycompute3 <- function(condition, dat, fixed_objects){
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
    SimCollect(dirs = c('dir1', 'dir2', 'dir3'))
    expect_true(dir.exists('SimDesign_aggregate_results'))
    expect_equal(6, nrow(readRDS('SimDesign_aggregate_results/results-row-1.rds')$results))
    SimClean(dirs = c('SimDesign_aggregate_results','dir1', 'dir2', 'dir3'))

    mycompute <- function(condition, dat, fixed_objects){
        if(sample(c(FALSE, TRUE), 1, prob = c(.9, .1))) stop('error')
        list(ret = 1)
    }
    mycollect <- function(condition, results, fixed_objects) {
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
    SimCollect(dirs = c('dir1', 'dir2'))
    expect_true(dir.exists('SimDesign_aggregate_results'))
    expect_equal(4, length(readRDS('SimDesign_aggregate_results/results-row-1.rds')$results))
    SimClean(dirs = c('SimDesign_aggregate_results','dir1', 'dir2'))

    ## warning and other information
    mysim_ew <- function(condition, fixed_objects){
        dat <- 1
        return(dat)
    }

    mycompute_ew <- function(condition, dat, fixed_objects){

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- 1

        if(condition$stop_some) if(runif(1) > .95) stop('stopped this time')
        if(condition$warn) warning('warn')
        if(condition$warn2) warning('warn2')

        return(ret)
    }

    mycollect <- function(condition, results, fixed_objects) {
        c(ret=1)
    }

    design <- createDesign(stop_some=c(FALSE, TRUE),
                           warn=c(FALSE, TRUE),
                           warn2=c(FALSE, TRUE))


    set.seed(1)
    results <- runSimulation(design, replications = 100, generate=mysim_ew,
                             analyse=mycompute_ew, summarise=mycollect, verbose=FALSE,
                             filename='sim1')
    set.seed(2)
    results2 <- runSimulation(design, replications = 100, generate=mysim_ew,
                             analyse=mycompute_ew, summarise=mycollect, verbose=FALSE,
                             filename='sim2')

    ret <- SimCollect(c('sim1.rds', 'sim2.rds'))
    expect_true(all(na.omit(ret$WARNINGS == c(NA,NA,200,200,200,200,400,400))))
    expect_true(all(ret$ERRORS > 0 | is.na(ret$ERRORS)))
    SimClean(c('sim1.rds', 'sim2.rds'))

})

