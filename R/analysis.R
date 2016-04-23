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
                     save_seeds, save_seeds_dirname, load_seed, export_funs, packages,
                     summarise_asis)
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
                               save_generate_data_dirname=save_generate_data_dirname,
                               save_seeds=save_seeds, load_seed=load_seed,
                               save_seeds_dirname=save_seeds_dirname)
    } else {
        if(MPI){
            i <- 1L
            cell_results <- foreach(i=1L:replications, .export=export_funs) %dopar%
                mainsim(i, condition=condition, generate=Functions$generate,
                     analyse=Functions$analyse, fixed_objects=fixed_objects, load_seed=load_seed,
                     max_errors=max_errors, save_generate_data=save_generate_data,
                     save_generate_data_dirname=save_generate_data_dirname, packages=packages,
                     save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname)
        } else {
            if(!is.null(seed)) parallel::clusterSetRNGStream(cl=cl, seed[condition$ID])
            cell_results <- parallel::parLapply(cl, 1L:replications, mainsim,
                                                condition=condition, generate=Functions$generate,
                                                analyse=Functions$analyse, load_seed=load_seed,
                                                fixed_objects=fixed_objects, packages=packages,
                                                max_errors=max_errors, save_generate_data=save_generate_data,
                                                save_generate_data_dirname=save_generate_data_dirname,
                                                save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname)
        }
    }
    if(summarise_asis)
        return(do.call(rbind, lapply(cell_results, function(x) x$result)))
    try_errors <- do.call(c, lapply(cell_results, function(x) attr(x, 'try_errors')))
    try_errors <- if(length(try_errors)){
        table(try_errors[!is.na(try_errors)])
    } else table(try_errors)
    names(try_errors) <-
        gsub('Error in analyse\\(dat = simlist\\$dat, parameters = simlist\\$parameters, condition = condition,  : \\n  ',
             replacement = 'Manual Error : ', names(try_errors))
    warnings <- do.call(c, lapply(cell_results, function(x) attr(x, 'warnings')))
    warnings <- if(length(warnings)){
        table(warnings[!is.na(warnings)])
    } else table(warnings)
    for(i in 1L:length(cell_results))
        attr(cell_results[[i]], 'try_errors') <- attr(cell_results[[i]], 'warnings') <- NULL

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
        if(length(unique(colnames(results))) != ncol(results) && ncol(results) > 1L)
            stop('Vector of results returned from analyse must have unique names', call.=FALSE)
    }
    if(save_results){
        tmpfilename <- paste0(save_results_dirname, '/results-row-', condition$ID, '.rds')
        saveRDS(list(condition=condition, results=results, errors=try_errors), tmpfilename)
    }
    sim_results <- Functions$summarise(results=results, parameters_list=parameters,
                           condition=condition, fixed_objects=fixed_objects)
    if(!is.vector(sim_results) || is.null(names(sim_results)))
        stop('summarise() must return a named vector', call.=FALSE)
    sim_results <- c(sim_results, 'REPLICATIONS'=replications, 'ERROR: '=try_errors,
                     'WARNING: '=warnings)
    return(sim_results)
}
