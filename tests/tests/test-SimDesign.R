context('SimDesign')

test_that('SimDesign', {

    sample_sizes <- c(10, 20)
    standard_deviations <- c(1, 4)

    Design <- expand.grid(sample_sizes_group1=sample_sizes,
                          sample_sizes_group2=sample_sizes,
                          standard_deviations=standard_deviations)

    mysim <- function(condition){

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

    mycompute <- function(simlist, condition){

        # require packages/define functions if needed, or better yet index with the :: operator
        require(stats)
        mygreatfunction <- function(x) print('Do some stuff')

        # begin extracting the data elements
        dat <- simlist$dat
        parameters <- simlist$parameters

        #wrap computational statistics in try() statements to control estimation problems
        welch <- try(t.test(DV ~ group, dat), silent=TRUE)
        ind <- try(stats::t.test(DV ~ group, dat, var.equal=TRUE), silent=TRUE)

        # check if error, and if so stop and return an 'error'. This will re-draw the data
        if(is(welch, 'try-error')) stop('Welch error message')
        if(is(ind, 'try-error')) stop('Independent t-test error message')

        # In this function the p values for the t-tests are returned,
        #  and make sure to name each element, for future reference
        ret <- c(welch = welch$p.value,
                 independent = ind$p.value)

        return(ret)
    }

    mycollect <- function(results, parameters, condition){

        # handy functions
        bias <- function(observed, population) mean(observed - population)
        RMSD <- function(observed, population) sqrt(mean((observed - population)^2))

        # silly test for bias and RMSD of a random number from 0
        pop_value <- 0
        bias.random_number <- bias(sapply(parameters, function(x) x$random_number), pop_value)
        RMSD.random_number <- RMSD(sapply(parameters, function(x) x$random_number), pop_value)

        #find results of interest here (alpha < .1, .05, .01)
        nms <- c('welch', 'independent')
        lessthan.10 <- colMeans(results[,nms] < .10)
        lessthan.05 <- colMeans(results[,nms] < .05)
        lessthan.01 <- colMeans(results[,nms] < .01)

        # return the results that will be appended to the Design input
        ret <- c(bias.random_number=bias.random_number,
                 RMSD.random_number=RMSD.random_number,
                 lessthan.10=lessthan.10,
                 lessthan.05=lessthan.05,
                 lessthan.01=lessthan.01)
        return(ret)
    }

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE)
    expect_is(Final, 'data.frame')

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = 2, parallel=FALSE, save=FALSE, verbose = FALSE)
    expect_is(Final, 'data.frame')

    Final <- runSimulation(Design, generate=mysim, analyse=mycompute, summarise=mycollect,
                           replications = parallel::detectCores(), parallel=TRUE, save=FALSE)
    expect_is(Final, 'data.frame')
})

