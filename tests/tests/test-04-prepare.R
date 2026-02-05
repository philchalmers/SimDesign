context('prepare')

test_that('prepare RNG management', {

    library(SimDesign)

    # Setup common functions
    Design <- createDesign(N = c(10, 20))

    prepare <- function(condition, fixed_objects) {
        fixed_objects$data <- rnorm(condition$N)
        return(fixed_objects)
    }

    generate <- function(condition, fixed_objects) {
        data.frame(x = rnorm(5))
    }

    analyse <- function(condition, dat, fixed_objects) {
        c(mean_x = mean(dat$x))
    }

    summarise <- function(condition, results, fixed_objects) {
        c(mean_x = mean(results[,'mean_x']))
    }

    # Test 1: Basic prepare() seed capture
    res <- runSimulation(Design, replications=2,
                         prepare=prepare,
                         generate=generate,
                         analyse=analyse,
                         summarise=summarise,
                         control=list(store_Random.seeds=TRUE),
                         verbose=FALSE)

    expect_is(res, 'SimDesign')
    prepare_seeds <- SimExtract(res, what='prepare_seeds')
    expect_true(length(prepare_seeds) == nrow(Design))
    expect_true(all(sapply(prepare_seeds, length) == 626))
    expect_true(!is.null(prepare_seeds[[1]]))
    expect_true(!is.null(prepare_seeds[[2]]))

    # Test 2: prepare() seeds NOT captured when flag is FALSE
    res2 <- runSimulation(Design, replications=2,
                          prepare=prepare,
                          generate=generate,
                          analyse=analyse,
                          summarise=summarise,
                          verbose=FALSE)

    prepare_seeds2 <- SimExtract(res2, what='prepare_seeds')
    expect_true(is.null(prepare_seeds2[[1]]))
    expect_true(is.null(prepare_seeds2[[2]]))

    # Test 3: Seed reproducibility with load_seed_prepare
    res3a <- runSimulation(Design[1,], replications=2,
                           prepare=prepare,
                           generate=generate,
                           analyse=analyse,
                           summarise=summarise,
                           seed=1234,
                           control=list(store_Random.seeds=TRUE),
                           verbose=FALSE)

    seed1 <- SimExtract(res3a, what='prepare_seeds')[[1]]

    res3b <- runSimulation(Design[1,], replications=2,
                           prepare=prepare,
                           generate=generate,
                           analyse=analyse,
                           summarise=summarise,
                           load_seed_prepare=seed1,
                           seed=1234,
                           control=list(store_Random.seeds=TRUE),
                           verbose=FALSE)

    expect_equal(res3a$mean_x, res3b$mean_x)

    # Test 4: prepare() error seed capture
    prepare_error <- function(condition, fixed_objects) {
        if(condition$N > 15) stop('N too large in prepare')
        fixed_objects$data <- rnorm(condition$N)
        return(fixed_objects)
    }

    expect_error(
        runSimulation(Design[2,], replications=1,
                      prepare=prepare_error,
                      generate=generate,
                      analyse=analyse,
                      summarise=summarise,
                      verbose=FALSE))

    # Test 5: load_seed_prepare with file path
    res5 <- runSimulation(Design, replications=2,
                          prepare=prepare,
                          generate=generate,
                          analyse=analyse,
                          summarise=summarise,
                          control=list(save_seeds=TRUE,
                                      store_Random.seeds=TRUE),
                          filename='test',
                          verbose=FALSE)

    seed_dirs <- list.dirs('.', recursive=FALSE)
    seed_dir <- seed_dirs[grep('SimDesign-seeds', seed_dirs)]

    if(length(seed_dir) > 0) {
        seed_file <- file.path(seed_dir, 'design-row-1', 'prepare-seed')

        if(file.exists(seed_file)) {
            res5b <- runSimulation(Design[1,], replications=2,
                                   prepare=prepare,
                                   generate=generate,
                                   analyse=analyse,
                                   summarise=summarise,
                                   load_seed_prepare=seed_file,
                                   verbose=FALSE)

            expect_equal(res5$mean_x[1], res5b$mean_x[1])
        }
    }

    SimClean(results=TRUE, temp=TRUE)

    # Test 6: Backward compatibility - no prepare()
    res6 <- runSimulation(Design, replications=2,
                          generate=generate,
                          analyse=analyse,
                          summarise=summarise,
                          verbose=FALSE)

    expect_is(res6, 'SimDesign')
    expect_equal(nrow(res6), nrow(Design))

    prepare_seeds6 <- SimExtract(res6, what='prepare_seeds')
    expect_true(all(sapply(prepare_seeds6, is.null)))

    # Test 7: prepare() seed isolation between conditions
    res7 <- runSimulation(Design, replications=2,
                          prepare=prepare,
                          generate=generate,
                          analyse=analyse,
                          summarise=summarise,
                          control=list(store_Random.seeds=TRUE),
                          verbose=FALSE)

    prepare_seeds7 <- SimExtract(res7, what='prepare_seeds')
    expect_false(identical(prepare_seeds7[[1]], prepare_seeds7[[2]]))

    # Test 8: Integration with existing seed management
    res8 <- runSimulation(Design, replications=10,
                          prepare=prepare,
                          generate=generate,
                          analyse=analyse,
                          summarise=summarise,
                          control=list(store_Random.seeds=TRUE),
                          verbose=FALSE)

    prepare_seeds8 <- SimExtract(res8, what='prepare_seeds')
    random_seeds8 <- SimExtract(res8, what='Random.seeds')

    expect_true(length(prepare_seeds8) == nrow(Design))
    expect_is(random_seeds8, 'list')
    expect_true(length(random_seeds8) == nrow(Design))
    expect_true(all(sapply(random_seeds8, function(x) nrow(x) == 10)))

    SimClean('test.rds')
    SimClean(dirs = 'test-seeds_*')
})
