# Function for computing Monte Carlo simulation results for each unique condition
#
# @param Functions list of functions
# @param condition a single row from the design input
# @param replications number of times to repeat the Monte Carlo simulations
# @param fixed_objects optional object containing fixed conditions across design
# @param cl cluster object defined from the parallel package
# @param MPI logical; flag passed down from the runSimulation function
# @param save_results logical; save the results to .rds files
# @param results_filename the file name used to store results in
#
Analysis <- function(Functions, condition, replications, fixed_objects, cl, MPI, seed,
                     save_results, save_results_dirname, max_errors,
                     save_generate_data, save_generate_data_dirname,
                     export_funs, packages)
{
    # This defines the work-flow for the Monte Carlo simulation given the condition (row in Design)
    #  and number of replications desired
    if(is.null(cl)){
        if(!is.null(seed)) set.seed(seed[condition$ID])
        cell_results <- lapply(1L:replications, mainsim, condition=condition,
                               generate=Functions$generate,
                               analyse=Functions$analyse,
                               fixed_objects=fixed_objects,
                               max_errors=max_errors, packages=packages,
                               save_generate_data=save_generate_data,
                               save_generate_data_dirname=save_generate_data_dirname)
    } else {
        if(MPI){
            i <- 1L
            cell_results <- foreach(i=1L:replications, .export=export_funs) %dopar%
                mainsim(i, condition=condition, generate=Functions$generate,
                     analyse=Functions$analyse, fixed_objects=fixed_objects,
                     max_errors=max_errors, save_generate_data=save_generate_data,
                     save_generate_data_dirname=save_generate_data_dirname, packages=packages)
        } else {
            if(!is.null(seed)) parallel::clusterSetRNGStream(cl=cl, seed[condition$ID])
            cell_results <- parallel::parLapply(cl, 1L:replications, mainsim,
                                                condition=condition, generate=Functions$generate,
                                                analyse=Functions$analyse,
                                                fixed_objects=fixed_objects, packages=packages,
                                                max_errors=max_errors, save_generate_data=save_generate_data,
                                                save_generate_data_dirname=save_generate_data_dirname)
        }
    }

    try_errors <- lapply(cell_results, function(x) attr(x, 'try_errors'))
    try_errors <- table(do.call(c, try_errors))
    for(i in 1L:length(cell_results))
        attr(cell_results[[i]], 'try_errors') <- NULL

    # split lists up
    results <- lapply(cell_results, function(x) x$result)
    if(!is.null(cell_results[[1L]]$parameters))
        parameters <- lapply(cell_results, function(x) x$parameters)
    else parameters <- NULL

    #collect meta simulation statistics (bias, RMSE, type I errors, etc)
    if(!is.list(results[[1L]])){
        results <- do.call(rbind, results)
        if(any(is.na(results) | is.nan(results)))
            message(sprintf('WARNING: analyse() returned NA or NaN values from row %i in Design',
                            condition$ID))
    }
    if(save_results){
        tmpfilename <- paste0(save_results_dirname, '/results-row-', condition$ID, '.rds')
        saveRDS(list(condition=condition, results=results, errors=try_errors), tmpfilename)
    }
    sim_results <- Functions$summarise(results=results, parameters_list=parameters,
                           condition=condition, fixed_objects=fixed_objects)

    if(!is.vector(sim_results) || is.null(names(sim_results)))
        stop('summarise() must return a named vector', call.=FALSE)
    sim_results <- c(sim_results, 'REPLICATIONS'=replications, 'ERROR_MESSAGE: '=try_errors)
    return(sim_results)
}
