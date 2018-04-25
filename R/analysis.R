Analysis <- function(Functions, condition, replications, fixed_objects, cl, MPI, seed,
                     save_results, save_results_outdir, save_results_dirname, max_errors, bootSE, boot_draws,
                     save_generate_data, save_generate_data_dirname,
                     save_seeds, save_seeds_dirname, load_seed, export_funs, packages,
                     summarise_asis, warnings_as_errors, progress, store_results)
{
    # This defines the work-flow for the Monte Carlo simulation given the condition (row in Design)
    #  and number of replications desired
    if(is.null(cl)){
        if(!is.null(seed)) set.seed(seed[condition$ID])
        results <- if(progress){
            pbapply::pblapply(1L:replications, mainsim, condition=condition,
                   generate=Functions$generate,
                   analyse=Functions$analyse,
                   fixed_objects=fixed_objects,
                   max_errors=max_errors, packages=packages,
                   save_results_outdir=save_results_outdir,
                   save_generate_data=save_generate_data,
                   save_generate_data_dirname=save_generate_data_dirname,
                   save_seeds=save_seeds, load_seed=load_seed,
                   save_seeds_dirname=save_seeds_dirname,
                   warnings_as_errors=warnings_as_errors)
        } else {
            lapply(1L:replications, mainsim, condition=condition,
                   generate=Functions$generate,
                   analyse=Functions$analyse,
                   fixed_objects=fixed_objects,
                   max_errors=max_errors, packages=packages,
                   save_results_outdir=save_results_outdir,
                   save_generate_data=save_generate_data,
                   save_generate_data_dirname=save_generate_data_dirname,
                   save_seeds=save_seeds, load_seed=load_seed,
                   save_seeds_dirname=save_seeds_dirname,
                   warnings_as_errors=warnings_as_errors)
        }
    } else {
        if(MPI){
            i <- 1L
            # This is most likely broken at the moment, OO.
            results <- foreach(i=1L:replications, .export=export_funs) %dopar%
                mainsim(i, condition=condition, generate=Functions$generate,
                     analyse=Functions$analyse, fixed_objects=fixed_objects, load_seed=load_seed,
                     max_errors=max_errors, save_generate_data=save_generate_data,
                     save_generate_data_dirname=save_generate_data_dirname, packages=packages,
                     save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                     warnings_as_errors=warnings_as_errors)
        } else {
            if(!is.null(seed)) parallel::clusterSetRNGStream(cl=cl, seed[condition$ID])
            results <- if(progress){
                pbapply::pblapply(1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    fixed_objects=fixed_objects, packages=packages,
                                    save_results_outdir=save_results_outdir,
                                    max_errors=max_errors, save_generate_data=save_generate_data,
                                    save_generate_data_dirname=save_generate_data_dirname,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors,
                                  cl=cl)
            } else {
                parallel::parLapply(cl, 1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    fixed_objects=fixed_objects, packages=packages,
                                    save_results_outdir=save_results_outdir,
                                    max_errors=max_errors, save_generate_data=save_generate_data,
                                    save_generate_data_dirname=save_generate_data_dirname,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors)
            }
        }
    }
    if(summarise_asis || store_results){
        tabled_results <- if(is.data.frame(results[[1]]) && nrow(results[[1]]) == 1L){
            plyr::rbind.fill(results)
        } else if((is.data.frame(results[[1]]) && nrow(results[[1]]) > 1L) || is.list(results[[1L]])){
            results
        } else {
            do.call(rbind, results)
        }
        if(save_results){
            tmpfilename <- paste0(save_results_dirname, '/results-row-', condition$ID, '.rds')
            saveRDS(list(condition=condition, results=tabled_results), file.path(save_results_outdir, tmpfilename))
        }
        if(summarise_asis) return(tabled_results)
    }

    try_errors <- do.call(c, lapply(results, function(x) attr(x, 'try_errors')))
    try_errors <- if(length(try_errors)){
        table(try_errors[!is.na(try_errors)])
    } else table(try_errors)
    names(try_errors) <-
        gsub('Error in analyse\\(dat = simlist, condition = condition, fixed_objects = fixed_objects) : \\n  ',
             replacement = 'Error : ', names(try_errors))
    warnings <- do.call(c, lapply(results, function(x) attr(x, 'warnings')))
    warnings <- if(length(warnings)){
        table(warnings[!is.na(warnings)])
    } else table(warnings)
    for(i in seq_len(length(results)))
        attr(results[[i]], 'try_errors') <- attr(results[[i]], 'warnings') <- NULL

    #collect meta simulation statistics (bias, RMSE, type I errors, etc)
    if(!is.list(results[[1L]]) ||
       (is.data.frame(results[[1L]]) && nrow(results[[1L]]) == 1L)){
        results <- do.call(rbind, results)
        if(length(unique(colnames(results))) != ncol(results) && ncol(results) > 1L)
            stop('Object of results returned from analyse must have unique names', call.=FALSE)
        rownames(results) <- NULL
    }
    if(save_results){
        tmpfilename <- paste0(save_results_dirname, '/results-row-', condition$ID, '.rds')
        saveRDS(list(condition=condition, results=results, errors=try_errors, warnings=warnings),
                file.path(save_results_outdir, tmpfilename))
    }
    sim_results <- Functions$summarise(results=results,
                           condition=condition, fixed_objects=fixed_objects)
    if(is.data.frame(sim_results)){
        if(nrow(sim_results) > 1L)
            stop('When returning a data.frame in summarise() there should only be 1 row', call.=FALSE)
        nms <- names(sim_results)
        sim_results <- as.numeric(sim_results)
        names(sim_results) <- nms
    }
    if(length(sim_results) == 1L)
        if(is.null(names(sim_results)))
            names(sim_results) <- 'value'
    if(!is.vector(sim_results) || is.null(names(sim_results)))
        stop('summarise() must return a named vector or data.frame object with 1 row', call.=FALSE)
    ret <- c(sim_results, 'REPLICATIONS'=replications, 'ERROR: '=try_errors,
             'WARNING: '=warnings)
    if(bootSE){
        # could parallelize, TODO
        SE_sim_results <- sapply(1L:boot_draws, function(r){
            pick <- rint(n = replications, min = 1L, max = replications)
            # results could be a list? TODO
            tmp <- results[pick, , drop=FALSE]
            Functions$summarise(results=tmp, condition=condition, fixed_objects=fixed_objects)
        })
        SE_sim_results <- apply(SE_sim_results, 1L, sd)
        names(SE_sim_results) <- paste0("BOOT_SE.", names(sim_results))
        ret <- c(ret, SE_sim_results)
    }
    if(store_results)
        attr(ret, 'full_results') <- tabled_results
    ret
}
