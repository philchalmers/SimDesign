context('extras')

test_that('extras', {

    library(SimDesign)

    Design <- createDesign(N = c(10, 20), kind = c("a", "b"))

    Generate  <- function(condition, fixed_objects) rnorm(condition$N)
    Analyse   <- function(condition, dat, fixed_objects) c(m = mean(dat))
    Summarise <- function(condition, results, fixed_objects) {
        c(
            nrow_condition = nrow(condition), # 1 under runSimulation()
            N_seen = condition$N[1L], # this row's own N
            n_rep = nrow(results) # replications pooled into this row
        )
    }

    res <- runSimulation(
        expandDesign(Design, repeat_conditions = 2),
        replications = 5,
        generate = Generate,
        analyse = Analyse,
        summarise = Summarise,
        store_results = TRUE,
        save = FALSE,
        progress = FALSE,
        verbose=FALSE,
        seed = 1
    )
    expect_identical(cbind(Design, nrow_condition=1, N_seen=c(10,10,20,20)),
                     as.data.frame(SimCollect(simobj = res))[, c("N", "kind", "nrow_condition", "N_seen")])


    # independent saves
    Design1 <- createDesign(N = c(10, 20))

    dir_multi <- file.path(tempdir(), "multi_row")
    dir.create(dir_multi, showWarnings = FALSE)

    for (i in 1:2) {
        r <- runSimulation(
            Design1,
            replications = 5,
            generate = Generate,
            analyse = Analyse,
            summarise = Summarise,
            store_results = TRUE,
            save = FALSE,
            progress = FALSE,
            verbose=FALSE,
            seed = c(i * 10, i * 10 + 1)
        )
        saveRDS(r, file.path(dir_multi, paste0("run", i, ".rds")))
    }

    expect_identical(cbind(Design1, nrow_condition=1, N_seen=c(10,20), n_rep=10),
                     as.data.frame(SimCollect(dir = dir_multi))[, c("N", "nrow_condition", "N_seen", "n_rep")])

    # one factor
    res1 <- runSimulation(
        expandDesign(Design1, repeat_conditions = 2),
        replications = 5,
        generate = Generate,
        analyse = Analyse,
        summarise = Summarise,
        store_results = TRUE,
        save = FALSE,
        progress = FALSE,
        verbose=FALSE,
        seed = 1
    )

    expect_equal(20, nrow(SimResults(res1)))

    expect_identical(cbind(Design1, nrow_condition=1, N_seen=c(10,10), n_rep=10),
                     as.data.frame(SimCollect(simobj = res1))[, c("N", "nrow_condition", "N_seen", "n_rep")])



})
