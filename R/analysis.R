# Function for computing Monte Carlo simulation results for each unique condition
#
# @param Functions list of functions
# @param condition a single row from the design input
# @param replications number of times to repeat the Monte Carlo simulations
# @param fixed_design_elements optional object contaiing fixed conditoins across design
# @param cl cluster object defined from the parallel package
# @param MPI logical; flag passed down from the runSimulation function
# @param save_results logical; save the results to .rds files
# @param results_filename the file name used to store results in
#
Analysis <- function(Functions, condition, replications, fixed_design_elements, cl, MPI, seed,
                     save_results, results_filename)
{
    # This defines the work-flow for the Monte Carlo simulation given the condition (row in Design)
    #  and number of replications desired
    if(is.null(cl)){
        if(!is.null(seed)) set.seed(seed[condition$ID])
        cell_results <- lapply(1L:replications, Functions$main, condition=condition,
                               generate=Functions$generate,
                               analyse=Functions$analyse,
                               fixed_design_elements=fixed_design_elements)
    } else {
        if(MPI){
            i <- 1L
            cell_results <- foreach(i=1L:replications) %dopar%
                Functions$main(i, condition=condition, generate=Functions$generate,
                               analyse=Functions$analyse, fixed_design_elements=fixed_design_elements)
        } else {
            if(!is.null(seed)) parallel::clusterSetRNGStream(cl=cl, seed[condition$ID])
            cell_results <- parallel::parLapply(cl, 1L:replications, Functions$main,
                                                condition=condition, generate=Functions$generate,
                                                analyse=Functions$analyse,
                                                fixed_design_elements=fixed_design_elements)
        }
    }
    # split lists up
    results <- lapply(cell_results, function(x) x$result)
    if(!is.null(cell_results[[1L]]$parameters))
        parameters <- lapply(cell_results, function(x) x$parameters)
    else parameters <- NULL

    #collect meta simulation statistics (bias, RMSE, type I errors, etc)
    if(!is.list(results[[1L]])){
        N_CELL_RUNS <- sum(sapply(results, function(x) x['n_cell_runs']))
        results <- do.call(rbind, results)
        results <- results[ ,colnames(results) != 'n_cell_runs', drop=FALSE]
    } else {
        N_CELL_RUNS <- sum(do.call(c, sapply(results, function(x) x['n_cell_runs'])))
        for(i in 1L:length(results))
            results[[i]]$n_cell_runs <- NULL
    }
    if(save_results)
        saveRDS(list(condition=condition, results=results),
                paste0('SimDesign_results/', results_filename, 'ROWID-', condition$ID, '.rds'))
    sim_results <- Functions$summarise(results=results, parameters_list=parameters,
                           condition=condition, fixed_design_elements=fixed_design_elements)

    if(!is.vector(sim_results) || is.null(names(sim_results)))
        stop('summarise() must return a named vector', call.=FALSE)
    if(any(names(sim_results) == 'N_CELL_RUNS'))
        stop('summarise() cannot contain an element with the name N_CELL_RUNS')
    sim_results <- c(sim_results, N_CELL_RUNS=N_CELL_RUNS)

    return(sim_results)
}
