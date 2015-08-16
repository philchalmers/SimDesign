# Analysis() - Function for computing Monte Carlo simulation results for each unique condition
#
# @param Functions list of functions
# @param condition a single row from the Design input
# @param each number of times to repeat the Monte Carlo simulations
# @param cl cluster object defined from the parallel package
# @param MPI logical; flag passed down from the runSimulation function
# @param edit a character indicating whether the function should be debugged. Can be
#   'sim', 'compute', 'collect', or 'main'
#
Analysis <- function(Functions, condition, each, cl, MPI, edit)
{
    # This defines the workflow for the Monte Carlo simulation given the condition (row in Design)
    #  and number of replications desired
    if(is.null(cl)){
        cell_results <- lapply(1L:each, Functions$main, condition=condition, sim=Functions$sim,
                               compute=Functions$compute, edit=edit)
    } else {
        if(MPI){
            cell_results <- foreach(i=1L:each) %dopar%
                Functions$main(i, condition=condition, sim=Functions$sim,
                               compute=Functions$compute, edit=edit)
        } else {
            cell_results <- parallel::parLapply(cl, 1L:each, Functions$main, condition=condition,
                                                sim=Functions$sim, compute=Functions$compute,
                                                edit=edit)
        }
    }
    # split lists up
    results <- lapply(cell_results, function(x) x$result)
    if(!is.null(cell_results[[1L]]$parameters))
        parameters <- lapply(cell_results, function(x) x$parameters)
    else parameters <- list()

    #collect meta simulation statistics (bias, RMSD, type I errors, etc)
    sim_results <- Functions$collect(results=results, parameters=parameters,
                           condition=condition, edit=edit)

    if(any(names(sim_results) == 'N_CELL_RUNS'))
        stop('collect() cannot contain an element with the name N_CELL_RUNS')
    sim_results <- c(sim_results,
                     N_CELL_RUNS = sum(sapply(results, function(x) x['n_cell_runs'])))

    return(sim_results)
}
