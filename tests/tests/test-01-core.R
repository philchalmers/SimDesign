context('core')

test_that('SimDesign', {

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

    mycollect_list <-  function(condition, results, fixed_objects){

        #find results of interest here
        nms <- c('welch', 'independent')
        lessthan.05 <- EDR(results[,nms], alpha = .05)

        # return the results that will be appended to the Design input
        ret <- list(lessthan.05=lessthan.05, something=42)
        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    # list in summarise
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect_list,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')
    expect_is(Final$SUMMARISE, 'list')

    # resummarise list info
    resum <- reSummarise(mycollect_list, results=Final)
    expect_is(resum$SUMMARISE, 'list')

    mycollect <-  function(condition, results, fixed_objects){

        # return the results that will be appended to the Design input
        ret <- EDR(results, .05)
        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    myanalyse_global <-  function(condition, dat, fixed_objects){

        that + 1   # object 'that' not defined in scope
        nms <- c('welch', 'independent')
        lessthan.05 <- EDR(results[,nms], alpha = .05)  # results out of scope

        # return the results that will be appended to the Design input
        ret <- c(lessthan.05=lessthan.05)
        return(ret)
    }

    mycollect_global <-  function(condition, results, fixed_objects){

        this + 1   # object 'this' not defined in scope
        nms <- c('welch', 'independent')
        lessthan.05 <- EDR(results[,nms], alpha = .05)

        # return the results that will be appended to the Design input
        ret <- c(lessthan.05=lessthan.05)
        return(ret)
    }

    ret <- runSimulation(Design, generate=mysim, analyse=myanalyse_global, summarise=mycollect_global ,
                  replications = 2, parallel=FALSE, check.globals=TRUE, verbose=FALSE)
    expect_true(all(ret %in% c('this', 'that', 'results')))

    # Simsolve test
    condition <- Design[1,]
    condition$mean_diff <- NA

    mysim_md <- function(condition, fixed_objects){

        Attach(condition)

        N1 <- sample_sizes_group1
        N2 <- condition$sample_sizes_group2
        sd <- condition$standard_deviations

        group1 <- rnorm(N1)
        group2 <- rnorm(N2, sd=sd, mean=condition$mean_diff)
        dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))

        return(dat)
    }

    set.seed(1234)
    res <- SimSolve(condition, b=.80, interval=c(.1, 3), integer=FALSE,
                    generate=mysim_md, analyse=mycompute, summarise=mycollect,
                    maxiter=40, verbose=FALSE)
    expect_is(res, 'SimSolve')
    expect_equal(res$mean_diff, 1.326, tol=1e-3)

    # test that future package works
    suppressPackageStartupMessages(suppressWarnings(library(future)))
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 10, parallel='future', save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')
    plan(multisession, workers = 2L)
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 10, parallel='future', save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')
    plan(sequential)
    detach("package:future")

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE,
                           store_results = TRUE)
    out <- SimExtract(Final, what = 'results')
    expect_equal(nrow(out), nrow(Design) * 2)

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = parallelly::availableCores(),
                           parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    # resume
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute2, summarise=mycollect,
                           replications = 2, save=TRUE, verbose = FALSE,
                           control = list(stop_on_fatal=TRUE)))
    compname = Sys.info()["nodename"]
    tmp <- readRDS(paste0('SIMDESIGN-TEMPFILE_', compname, '.rds'))
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 2, save=TRUE, verbose = FALSE, filename = 'newfile')
    SimClean('newfile.rds')

    #seeds
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                           replications = parallelly::availableCores(),
                           parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    Final2 <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                           replications = parallelly::availableCores(),
                           parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                           replications = parallelly::availableCores(), parallel=FALSE, save=FALSE, verbose = FALSE)
    Final2 <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, seed = 1:8,
                            replications = parallelly::availableCores(), parallel=FALSE, save=FALSE, verbose = FALSE)

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

    # results no summarise
    mycompute3 <- function(condition, dat, fixed_objects){

        #wrap computational statistics in try() statements to control estimation problems
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute3,
                         verbose=FALSE, replications = 2, parallel=FALSE)
    expect_true(nrow(tmp) == nrow(Design)*2 && ncol(tmp) == 5)

    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute3, summarise=NA,
                         verbose=FALSE, replications = 2, parallel=FALSE, save_results = TRUE)
    expect_is(tmp, 'data.frame')
    compname = Sys.info()["nodename"]
    DIR <- paste0("SimDesign-results_", compname)
    expect_true(dir.exists(DIR))
    expect_equal(nrow(tmp), 8)
    SimClean(results = TRUE)
    SimClean(dir()[grepl('\\.rds', dir())])

    # error test
    mycompute <- function(condition, dat, fixed_objects){
        stop('this error')
    }
    expect_warning(out <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 3, parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_error(out <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                                      replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE))

    expect_warning(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 3, parallel=TRUE, ncores=2L,
                           save=FALSE, verbose = FALSE))
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                                 replications = 2, parallel=TRUE, ncores=2L,
                                 save=FALSE, verbose = FALSE))

    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 3, parallel=TRUE, ncores=2L,
                               save=TRUE, verbose = FALSE, control = list(stop_on_fatal = TRUE)))

    mycompute <- function(condition, dat, fixed_objects){
        ret <- does_not_exist(TRUE)
        ret
    }
    expect_warning(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 3, parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_warning(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 3, parallel=TRUE, ncores=2L,
                               save=FALSE, verbose = FALSE))

    mysim <- function(condition, fixed_objects){
        stop('something silly', call.=FALSE)
    }
    expect_warning(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 3, parallel=FALSE, save=FALSE, verbose = FALSE))


    mycompute <- function(condition, dat, fixed_objects) {
        c(ret = 1)
    }
    mygenerate <- function(condition, fixed_objects) {
        rgumbel(5)
    }
    mycollect <- function(condition, results, fixed_objects) {
        mean(results$ret)
    }
    expect_warning(runSimulation(Design, replications = 3,
                               generate=mygenerate, analyse=mycompute, summarise=mycollect,
                               parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_warning(runSimulation(Design, replications = 3, ncores=2,
                               generate=mygenerate, analyse=mycompute, summarise=mycollect,
                               parallel=TRUE, save=FALSE, verbose = FALSE))
    out <- runSimulation(Design, replications = 3, packages = 'extraDistr',
                         generate=mygenerate, analyse=mycompute, summarise=mycollect,
                         parallel=FALSE, save=FALSE, verbose = FALSE)
    out2 <- runSimulation(Design, replications = 3, packages = 'extraDistr',
                         generate=mygenerate, analyse=mycompute, summarise=mycollect,
                         parallel=TRUE, save=FALSE, verbose = FALSE)
    expect_is(out, 'SimDesign')
    expect_is(out2, 'SimDesign')

    # warnings
    mycompute <- function(condition, dat, fixed_objects){
        if(sample(c(FALSE, TRUE), 1)) log(-1)
        if(sample(c(FALSE, TRUE), 1)) log(-2)
        if(sample(c(FALSE, TRUE), 1)) warning('Manual warning')
        c(ret = 1)
    }
    results <- runSimulation(Design, replications = 3, packages = 'extraDistr',
                  generate=mygenerate, analyse=mycompute, summarise=mycollect,
                  parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_true(any(grepl('WARNING', names(results))))
    results <- runSimulation(Design, replications = 3, packages = 'extraDistr',
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save=FALSE, verbose = FALSE,
                             control = list(store_warning_seeds = TRUE))
    expect_true(length(SimExtract(results, what = 'warning_seeds')) > 0)
    results <- runSimulation(Design, replications = 3, packages = 'extraDistr', max_errors = Inf,
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save=FALSE, verbose = FALSE,
                             control = list(warnings_as_errors=TRUE))
    expect_true(any(grepl('ERROR', names(results))))
    results <- runSimulation(Design, replications = 3, packages = 'extraDistr',
                  generate=mygenerate, analyse=mycompute, summarise=mycollect,
                  parallel=TRUE, ncores=2L, save=FALSE, verbose = FALSE)
    expect_true(any(grepl('WARNING', names(results))))

    # permissible warnings
    fn1 <- function(){
        if(sample(c(TRUE, FALSE), 1, prob = c(.1, .9))) warning('Show this warning')
        1
    }
    fn2 <- function(){
        if(sample(c(TRUE, FALSE), 1, prob = c(.1, .9))) warning('Show a different warning')
        1
    }
    mycompute <- function(condition, dat, fixed_objects){
        out1 <- fn1()
        out2 <- fn2()
        c(ret = 1)
    }
    results <- runSimulation(Design, replications = 100, packages='extraDistr',
                             seed=1:8,
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_equal(results$WARNINGS, c(21, 14, 22, 21, 15, 15, 17, 23))
    mycompute2 <- function(condition, dat, fixed_objects){
        out1 <- fn1()
        out2 <- manageWarnings(fn2(), "Show a different warning")
        c(ret = 1)
    }
    results <- runSimulation(Design, replications = 100, packages='extraDistr',
                             seed=1:8,
                             generate=mygenerate, analyse=mycompute2, summarise=mycollect,
                             parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_equal(results$ERRORS, c(10, 8, 11, 12,  7,  8,  5,  7))

    mycompute <- function(condition, dat, fixed_objects){
        if(sample(c(FALSE, TRUE), 1, prob = c(.5, .5))) warning('This is a warning')
        if(sample(c(FALSE, TRUE), 1, prob = c(.5, .5))) stop('This is an error')
        if(sample(c(FALSE, TRUE), 1, prob = c(.5, .5))) stop('This is a different error')
        list(ret = 1)
    }
    mycollect <- function(condition, results, fixed_objects){
        c(ret = 1)
    }
    results <- runSimulation(Design, replications = 3, packages = 'extraDistr', seed=1:8,
                             generate=mygenerate, analyse=mycompute, summarise=mycollect, verbose=FALSE)
    seeds <- SimExtract(results, what = 'error_seeds')
    expect_is(seeds, 'data.frame')
    expect_true(nrow(seeds) == 626)

    if(FALSE){
        # run interactively
        results <- runSimulation(Design, replications = 3, packages = 'extraDistr',
                                 generate=mygenerate, analyse=mycompute, summarise=mycollect, debug='error')

        results <- runSimulation(Design, replications = 3, packages = 'extraDistr', seed=1:8,
                                 generate=mygenerate, analyse=mycompute, summarise=mycollect,
                                 load_seed=seeds$Design_row_1.1..This.is.an.error., debug='analyse')
    }

    # NAs
    mycompute <- function(condition, dat, fixed_objects){
        ret <- c(ret = sample(c(NA, 1), 1, prob = c(.1, .9)))
        ret
    }

    results <- runSimulation(Design, replications = 10, packages = 'extraDistr', seed=1:nrow(Design),
                             generate=mygenerate, analyse=mycompute, summarise=mycollect,
                             parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_equal(results$ERRORS, c(0,1,0,3,4,1,0,4))

    #data.frame test
    mysim <- function(condition, fixed_objects){
        N1 <- condition$sample_sizes_group1
        N2 <- condition$sample_sizes_group2
        sd <- condition$standard_deviations
        group1 <- rnorm(N1)
        group2 <- rnorm(N2, sd=sd)
        dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
        dat
    }

    mycompute <- function(condition, dat, fixed_objects){
        welch <- t.test(DV ~ group, dat)
        ind <- stats::t.test(DV ~ group, dat, var.equal=TRUE)
        ret <- data.frame(welch = welch$p.value, independent = ind$p.value)
        ret
    }

    mycollect <-  function(condition, results, fixed_objects){
        ret <- EDR(results, alpha = .05)
        ret
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 3, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    # Maintain attributes after subsetting results
    F1 <- subset(Final, select=1:4) # 3 design factors, 1 simulation result
    expect_is(F1, 'SimDesign')
    expect_equal(length(F1), 4)
    expect_equal(length(attributes(F1)$design_names$design), 3)
    expect_equal(length(attributes(F1)$design_names$sim), 2)

    F2 <- subset(Final, select = c(1,2,4,5)) # 2 design factors, 2 simulation results
    expect_is(F2, 'SimDesign')
    expect_equal(length(F2), 4)
    expect_equal(length(attributes(F2)$design_names$design), 3)
    expect_equal(length(attributes(F2)$design_names$sim), 2)

    F3 <- subset(Final, subset = standard_deviations == 1)
    expect_is(F3, 'SimDesign')
    expect_equal(nrow(F3), 4)

    # dummy run with no design
    Generate <- function(condition, fixed_objects)
        rnorm(100, mean = 10)
    Analyse <- function(condition, dat, fixed_objects)
        t.test(dat)$conf.int
    Analyse2 <- function(condition, dat, fixed_objects){
        CIs <- t.test(dat)$conf.int
        names(CIs) <- c('lower', 'upper')
        CIs
    }
    Summarise <- function(condition, results, fixed_objects)
        ECR(results, 10)

    results <- runSimulation(replications = 10, generate = Generate,
                             analyse=Analyse, verbose=FALSE)
    expect_is(results, 'data.frame')
    expect_equal(ncol(results), 2L)

    results <- runSimulation(replications = 10, generate = Generate,
                             analyse=Analyse2, summarise = Summarise, verbose=FALSE)
    expect_is(results, 'data.frame')
    expect_equal(ncol(results), 5L)

    # dummy run with no design and returning lists
    Generate <- function(condition, fixed_objects)
        rnorm(100, mean = 10)
    Analyse <- function(condition, dat, fixed_objects){
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
    Generate <- function(condition, fixed_objects)
        rnorm(condition$N, mean = 10)
    Analyse1 <- function(condition, dat, fixed_objects){
        Attach(condition)
        if(N == 20) stop('Oh no, not 20!')
        mean(dat)
    }
    Analyse2 <- function(condition, dat, fixed_objects)
        mean(dat)

    Summarise <- function(condition, results, fixed_objects)
        bias(results, 0)
    expect_error(runSimulation(Design, replications = 10, save=TRUE,
                               save_details = list(tmpfilename = 'thisfile.rds'),
                  generate=Generate, analyse=Analyse1, summarise=Summarise, verbose=FALSE,
                  control = list(stop_on_fatal=TRUE)))
    expect_true('thisfile.rds' %in% dir())
    SimClean('thisfile.rds')
    results <- runSimulation(Design, replications = 10, save=TRUE,
                             save_details = list(tmpfilename = 'thisfile'),
                               generate=Generate, analyse=Analyse2, summarise=Summarise,
                             filename = 'thatfile', verbose=FALSE)
    expect_true('thatfile.rds' %in% dir())
    SimClean('thatfile.rds')

    if(Sys.info()["sysname"] != 'Windows'){
        results <- runSimulation(Design, replications = 10, save=TRUE,
                                 save_details = list(tmpfilename='thisfile', out_rootdir="~/mytmpdir"),
                                 generate=Generate, analyse=Analyse2, summarise=Summarise,
                                 filename = 'thatfile', verbose=FALSE)
        expect_true('thatfile.rds' %in% dir("~/mytmpdir"))
        SimClean('thatfile.rds', save_details = list(out_rootdir = "~/mytmpdir"))
        expect_false('thatfile.rds' %in% dir("~/mytmpdir"))
    }

    gen_anal <- function(condition, dat, fixed_objects){
        dat <- rnorm(100)
        mean(dat)
    }
    Summarise <- function(condition, results, fixed_objects)
        bias(results, 0)
    results <- runSimulation(replications = 10, analyse=gen_anal,
                             summarise=Summarise, verbose=FALSE)
    expect_is(results, 'SimDesign')

    # warnings/error in generate
    mycompute <- function(condition, dat, fixed_objects) {
        int <- sample(1:10, 1)
        if(int > 5) warning('greater than 5')
        if(int == 1) stop('generate error')
        c(ret = 1)
    }
    mygenerate <- function(condition, fixed_objects) {
        int <- sample(1:10, 1)
        if(int > 5) warning('greater than 5 in analyse')
        if(int == 1) stop('generate error in analyse')
        rnorm(5)
    }
    mycollect <- function(condition, results, fixed_objects) {
        mean(results[,1])
    }
    result <- runSimulation(replications = 100, seed=1234, verbose=FALSE,
                            generate=mygenerate, analyse=mycompute, summarise=mycollect)
    expect_equal(ncol(result), 7L)

    expect_true(all(names(SimExtract(result, what = 'errors')) %in% c(
        'ERROR:  generate error in analyse\n', 'ERROR:  generate error\n')))
    expect_true(all(names(SimExtract(result, what = 'warnings')) %in% c(
        'WARNING:  greater than 5', 'WARNING:  greater than 5 in analyse')))

    result <- runSimulation(design=createDesign(N=c(100, 200)), replications = 100,
                                                seed=c(1234, 4321), verbose=FALSE,
                            generate=mygenerate, analyse=mycompute, summarise=mycollect)
    expect_equal(ncol(result), 8L)
    expect_true(all(names(SimExtract(result, what = 'errors')) %in% c("N",
        'ERROR:  generate error in analyse\n', 'ERROR:  generate error\n')))
    expect_true(all(names(SimExtract(result, what = 'warnings')) %in% c("N",
        'WARNING:  greater than 5', 'WARNING:  greater than 5 in analyse')))

    # Summarise returns a list
    Design <- createDesign(N=c(250, 500))

    fo <- list(mean = c(10,10),
               sigma = matrix(c(10,4,4,20), 2, 2))

    generate <- function(condition, fixed_objects) {
        Attach(fixed_objects)
        dat <- rmvnorm(condition$N, mean=mean, sigma=sigma)
        dat
    }

    analyse <- function(condition, dat, fixed_objects) {
        meanest <- colMeans(dat)
        names(meanest) <- paste0("M", 1:ncol(dat))
        covest <- cov(dat)
        colnames(covest) <- rownames(covest) <- paste0("M", 1:ncol(dat))
        ret <- list(meanest=meanest, covest=covest)
        ret
    }

    summarise <- function(condition, results, fixed_objects) {
        means <- map(results, 'meanest')
        mean_res <- list(bias=bias(means, fixed_objects$mean),
                         RMSD=RMSD(means, fixed_objects$mean))
        sigmas <- map(results, 'covest')
        sigma_res <- list(bias=bias(sigmas, fixed_objects$sigma),
                          RMSD=RMSD(sigmas, fixed_objects$sigma))
        ret <- list(mean=mean_res, sigmas=sigma_res)
        ret
    }

    res <- runSimulation(design=Design, replications=10, generate=generate,
                         analyse=analyse, summarise=summarise, fixed_objects=fo,
                         packages = 'purrr', verbose=FALSE)
    expect_is(res$SUMMARISE, 'list')

    ## modular
    Design <- createDesign(factor1 = 1,
                           factor2 = c(1,2))

    generate <- function(condition, fixed_objects) {
        dat <- 1
        dat
    }

    analyse1 <- function(condition, dat, fixed_objects) {
        ret <- c(a1=1)
        ret
    }

    analyse2 <- function(condition, dat, fixed_objects) {
        ret <- c(a2=2)
        ret
    }

    summarise <- function(condition, results, fixed_objects) {
        ret <- colMeans(results)
        ret
    }

    res <- runSimulation(design=Design, replications=5, generate=generate,
                         analyse=list(analyse1=analyse1, analyse2=analyse2),
                         summarise=summarise, parallel=FALSE, verbose=FALSE)
    expect_true(all(c("analyse1.a1", "analyse2.a2") %in% names(res)))

    # skip over some
    analyse1 <- function(condition, dat, fixed_objects) {
        AnalyseIf(factor2 != 2, condition)
        ret <- c(a1=1)
        ret
    }

    res <- runSimulation(design=Design, replications=5, generate=generate,
                         analyse=list(analyse1=analyse1, analyse2=analyse2),
                         summarise=summarise, parallel=TRUE, verbose=FALSE)
    expect_true(all(c("analyse1.a1", "analyse2.a2") %in% names(res)))
    expect_true(is.na(res$analyse1.a1[2]))

    # fuzzy strings
    Analyse <- function(condition, dat, fixed_objects) {
        C <- matrix(c(1,.2, 0, 1), 2)
        if(sample(c(TRUE, FALSE), 1))
            C[2,2] <- runif(1, -1e-8, 1e-8)
        ret <- det(solve(C %*% t(C)))
        ret
    }

    Summarise <- function(condition, results, fixed_objects) {
        ret <- c(bias = NaN, RMSE = NaN)
        ret
    }

    res <- runSimulation(replications=200, analyse=Analyse, summarise=Summarise,
                         verbose=FALSE, seed = 1234)
    out <- SimExtract(res, what = 'errors')
    expect_true(ncol(out) == 2L)
    out <- SimExtract(res, what = 'errors', fuzzy = FALSE)
    expect_true(ncol(out) > 2L)

    #-------------------------------------------------------------------
    ## multi-errors and warnings
    Design <- createDesign(N = c(10, 20, 30))

    Generate <- function(condition, fixed_objects) {
        ret <- with(condition, rnorm(N))
        ret
    }

    Analyse.a1 <- function(condition, dat, fixed_objects) {
        whc <- sample(c(0, 1, 2, 3), 1, prob = c(.7, .20, .05, .05))
        if (whc == 0) {
            ret <- mean(dat)
        } else if (whc == 1) {
            ret <- t.test() # missing arguments
        } else if (whc == 2) {
            ret <- t.test("invalid") # invalid arguments
        } else if (whc == 3) {
            # throw error manually
            stop("Manual error thrown")
        }
        # manual warnings
        if (sample(c(TRUE, FALSE), 1, prob = c(.1, .9))) {
            warning("This warning happens rarely")
        }
        if (sample(c(TRUE, FALSE), 1, prob = c(.5, .5))) {
            warning("This warning happens much more often")
        }
        ret
    }

    Analyse.a2 <- function(condition, dat, fixed_objects) {
        ret <- median(dat)
        ret
    }

    Summarise <- function(condition, results, fixed_objects) {
        ret <- c(bias = bias(results, 0))
        ret
    }

    result <- runSimulation(Design,
                            replications = 10, seed = 3:1,
                            generate = Generate,
                            analyse = list(a1 = Analyse.a1, a2 = Analyse.a2),
                            summarise = Summarise, verbose=FALSE, save=FALSE)
    expect_equal(result$ERRORS, c(2,4,2))
    expect_equal(result$WARNINGS, c(6,5,4))

    # save to same dir
    res <- runSimulation(Design[1,], replications=10,
                         generate = Generate, analyse=Analyse.a2, summarise=Summarise,
                  verbose=FALSE, save_results = TRUE,
                  save_details = list(save_results_dirname = 'mydirname',
                                      save_results_filename = 'myfilename'))
    expect_true(dir.exists("mydirname"))
    expect_true(length(dir('mydirname')) == 1L)

    res2 <- runSimulation(Design[2,], replications=10,
                          generate = Generate, analyse=Analyse.a2, summarise=Summarise,
                         verbose=FALSE, save_results = TRUE,
                         save_details = list(save_results_dirname = 'mydirname',
                                             save_results_filename = 'myfilename2'))
    expect_true(length(dir('mydirname')) == 2L)

    res2 <- runSimulation(Design[1:2,], replications=10,
                          generate = Generate, analyse=Analyse.a2, summarise=Summarise,
                          verbose=FALSE, save_results = TRUE,
                          save_details = list(save_results_dirname = 'mydirname',
                                              save_results_filename = 'myfilename'))
    expect_true(length(dir('mydirname')) == 4L)
    SimClean(dirs = 'mydirname')

    # modifying Design object
    Design <- createDesign(N = c(10, 20),
                           SD = c(1, 2, 3))
    expect_equal(attr(Design, 'Design.ID'), 1:6)

    sDesign <- Design[2:3, ]
    expect_equal(attr(sDesign, 'Design.ID'), 2:3)

    Design2 <- createDesign(N = c(30),
                            SD = c(1, 2, 3))
    Design3 <- createDesign(N = c(40, 50),
                            SD = c(1, 2, 3))

    bDesign <- rbindDesign(Design, Design2, Design3)
    expect_equal(attr(bDesign, 'Design.ID'), 1:nrow(bDesign))

    bDesign <- rbindDesign(Design3, Design2, Design)
    expect_equal(attr(bDesign, 'Design.ID'), 1:nrow(bDesign))

    bDesign <- rbindDesign(Design3, Design2, Design, keep.IDs=TRUE)
    expect_equal(attr(bDesign, 'Design.ID'), c(1:6, 1:3, 1:6))

})

