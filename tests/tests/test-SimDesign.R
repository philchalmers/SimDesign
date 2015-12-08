context('SimDesign')

test_that('SimDesign', {

    sample_sizes <- c(10, 20)
    standard_deviations <- c(1, 4)

    Design <- expand.grid(sample_sizes_group1=sample_sizes,
                          sample_sizes_group2=sample_sizes,
                          standard_deviations=standard_deviations)

    mysim <- function(condition, fixed_objects = NULL){

        #require packages/define functions if needed, or better yet index with the :: operator

        N1 <- condition$sample_sizes_group1
        N2 <- condition$sample_sizes_group2
        sd <- condition$standard_deviations

        group1 <- rnorm(N1)
        group2 <- rnorm(N2, sd=sd)
        dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
        pars <- list(random_number = rnorm(1)) # just a silly example of a simulated parameter

        return(list(dat=dat, parameters=pars))
    }

    mycompute <- function(condition, dat, fixed_objects = NULL, parameters = NULL){

        # require packages/define functions if needed, or better yet index with the :: operator
        require(stats)

        #wrap computational statistics in try() statements to control estimation problems
        welch <- try(t.test(DV ~ group, dat), silent=TRUE)
        ind <- try(stats::t.test(DV ~ group, dat, var.equal=TRUE), silent=TRUE)

        # check if error, and if so stop and return an 'error'. This will re-draw the data
        check_error(welch)
        if(is(ind, 'try-error')) stop('Independent t-test error message')

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    mycollect <-  function(condition, results, fixed_objects = NULL, parameters_list = NULL){

        # handy functions
        bias <- function(observed, population) mean(observed - population)
        RMSD <- function(observed, population) sqrt(mean((observed - population)^2))

        # silly test for bias and RMSD of a random number from 0
        pop_value <- 0
        bias.random_number <- bias(sapply(parameters_list, function(x) x$random_number), pop_value)
        RMSD.random_number <- RMSD(sapply(parameters_list, function(x) x$random_number), pop_value)

        #find results of interest here
        nms <- c('welch', 'independent')
        lessthan.05 <- EDR(results[,nms], alpha = .05)

        # return the results that will be appended to the Design input
        ret <- c(bias.random_number=bias.random_number,
                 RMSD.random_number=RMSD.random_number,
                 lessthan.05=lessthan.05)
        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = parallel::detectCores(), parallel=TRUE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    # aggregate test
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=TRUE, verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=TRUE, filename = 'newfile', verbose = FALSE)
    Final <- aggregate_simulations()
    expect_is(Final, 'data.frame')
    expect_true(all(Final$REPLICATIONS == 4L))
    system('rm *.rds')

    mycompute <- function(condition, dat, fixed_objects = NULL, parameters = NULL){

        # require packages/define functions if needed, or better yet index with the :: operator
        require(stats)

        if(runif(1, 0, 1) < .9) return(try(suppressWarnings(t.test('char')), silent=TRUE))
        if(runif(1, 0, 1) < .9) check_error(try(suppressWarnings(aov('char')), silent=TRUE))

        #wrap computational statistics in try() statements to control estimation problems
        welch <- try(t.test(DV ~ group, dat), silent=TRUE)
        ind <- try(stats::t.test(DV ~ group, dat, var.equal=TRUE), silent=TRUE)

        # check if error, and if so stop and return an 'error'. This will re-draw the data
        check_error(welch)
        if(is(ind, 'try-error')) stop('Independent t-test error message')


        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, verbose = FALSE, include_errors = TRUE, max_errors = Inf)
    expect_is(Final, 'data.frame')
    expect_true(any(grepl('ERROR_MESSAGE', names(Final))))

    # aggregate test
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                         replications = 2, parallel=FALSE, save=TRUE, include_errors = TRUE,
                         max_errors=Inf, verbose = FALSE)
    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, max_errors=Inf,
                         replications = 2, parallel=FALSE, save=TRUE, filename = 'newfile',
                         include_errors = TRUE, verbose = FALSE)
    Final <- aggregate_simulations()
    expect_is(Final, 'data.frame')
    expect_true(all(Final$REPLICATIONS == 4L))
    system('rm *.rds')

    tmp <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect, verbose=FALSE,
                         replications = 2, parallel=FALSE, save_results = TRUE, max_errors = Inf)
    expect_true(dir.exists('SimDesign_results'))
    files <- dir('SimDesign_results/')
    expect_equal(length(files), 8L)
    x <- readRDS(paste0('SimDesign_results/', files[1]))
    expect_true(all(names(x) %in% c('condition', 'results', 'errors')))
    system('rm -r SimDesign_results')

    # error test
    mycompute <- function(condition, dat, fixed_objects = NULL, parameters = NULL){
        stop('this error', call. = FALSE)
    }
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 1, parallel=TRUE, save=FALSE, ncores = 2, verbose = FALSE))

    mycompute <- function(condition, dat, fixed_objects = NULL, parameters = NULL){
        ret <- does_not_exist(TRUE)
        ret
    }
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=FALSE, save=FALSE, verbose = FALSE))
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=TRUE, save=FALSE, verbose = FALSE))

    mysim <- function(condition, fixed_objects = NULL){
        stop('something silly', call.=FALSE)
    }
    expect_error(runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                               replications = 1, parallel=FALSE, save=FALSE, verbose = FALSE))

})

