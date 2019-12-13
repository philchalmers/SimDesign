Analysis <- function(Functions, condition, replications, fixed_objects, cl, MPI, seed, save,
                     save_results, save_results_out_rootdir, save_results_dirname, max_errors, bootSE, boot_draws,
                     save_generate_data, save_generate_data_dirname,
                     save_seeds, save_seeds_dirname, load_seed, export_funs, packages,
                     summarise_asis, warnings_as_errors, progress, store_results,
                     allow_na, allow_nan, use_try)
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
                   max_errors=max_errors, packages=packages, save=save,
                   save_results_out_rootdir=save_results_out_rootdir,
                   save_generate_data=save_generate_data,
                   save_generate_data_dirname=save_generate_data_dirname,
                   save_seeds=save_seeds, load_seed=load_seed,
                   save_seeds_dirname=save_seeds_dirname,
                   warnings_as_errors=warnings_as_errors,
                   allow_na=allow_na, allow_nan=allow_nan, use_try=use_try)
        } else {
            lapply(1L:replications, mainsim, condition=condition,
                   generate=Functions$generate,
                   analyse=Functions$analyse,
                   fixed_objects=fixed_objects,
                   max_errors=max_errors, packages=packages, save=save,
                   save_results_out_rootdir=save_results_out_rootdir,
                   save_generate_data=save_generate_data,
                   save_generate_data_dirname=save_generate_data_dirname,
                   save_seeds=save_seeds, load_seed=load_seed,
                   save_seeds_dirname=save_seeds_dirname,
                   warnings_as_errors=warnings_as_errors,
                   allow_na=allow_na, allow_nan=allow_nan, use_try=use_try)
        }
    } else {
        if(MPI){
            i <- 1L
            results <- foreach(i=1L:replications, .export=export_funs) %dopar%
                mainsim(i, condition=condition, generate=Functions$generate,
                     analyse=Functions$analyse, fixed_objects=fixed_objects, load_seed=load_seed,
                     max_errors=max_errors, save_generate_data=save_generate_data, save=save,
                     save_generate_data_dirname=save_generate_data_dirname, packages=packages,
                     save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                     save_results_out_rootdir=save_results_out_rootdir,
                     warnings_as_errors=warnings_as_errors, allow_na=allow_na, allow_nan=allow_nan,
                     use_try=use_try)
        } else {
            if(!is.null(seed)) parallel::clusterSetRNGStream(cl=cl, seed[condition$ID])
            results <- if(progress){
                pbapply::pblapply(1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    fixed_objects=fixed_objects, packages=packages, save=save,
                                    save_results_out_rootdir=save_results_out_rootdir,
                                    max_errors=max_errors, save_generate_data=save_generate_data,
                                    save_generate_data_dirname=save_generate_data_dirname,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors, allow_na=allow_na, allow_nan=allow_nan,
                                    use_try=use_try, cl=cl)
            } else {
                parallel::parLapply(cl, 1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    fixed_objects=fixed_objects, packages=packages, save=save,
                                    save_results_out_rootdir=save_results_out_rootdir,
                                    max_errors=max_errors, save_generate_data=save_generate_data,
                                    save_generate_data_dirname=save_generate_data_dirname,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors, allow_na=allow_na,
                                    allow_nan=allow_nan, use_try=use_try)
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
            saveRDS(list(condition=condition, results=tabled_results), file.path(save_results_out_rootdir, tmpfilename))
        }
        if(summarise_asis) return(tabled_results)
    }

    try_errors <- do.call(c, lapply(results, function(x) attr(x, 'try_errors')))
    try_error_seeds <- do.call(rbind, lapply(results, function(x) attr(x, 'try_error_seeds')))
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
        attr(results[[i]], 'try_errors') <- attr(results[[i]], 'warnings') <-
        attr(results[[i]], 'try_error_seeds') <- NULL

    #collect meta simulation statistics (bias, RMSE, type I errors, etc)
    if(!is.list(results[[1L]]) ||
       (is.data.frame(results[[1L]]) && nrow(results[[1L]]) == 1L)){
        old_nms <- names(results[[1L]])
        results <- as.data.frame(do.call(rbind, results))
        if(length(unique(colnames(results))) != ncol(results) && ncol(results) > 1L)
            stop('Object of results returned from analyse must have unique names', call.=FALSE)
        rownames(results) <- NULL
        if(ncol(results) == 1L && is.null(old_nms)) results <- results[,1]
    }
    if(save_results){
        tmpfilename <- paste0(save_results_dirname, '/results-row-', condition$ID, '.rds')
        saveRDS(list(condition=condition, results=results, errors=try_errors, error_seeds=try_error_seeds,
                     warnings=warnings),
                file.path(save_results_out_rootdir, tmpfilename))
    }
    sim_results <- try(Functions$summarise(results=results,
                           condition=condition, fixed_objects=fixed_objects), TRUE)
    if(!use_try){
        if(is(sim_results, 'try-error')){
            on.exit(myundebug(Functions$summarise))
            debug(Functions$summarise)
            try(Functions$summarise(results=results,
                                condition=condition, fixed_objects=fixed_objects), TRUE)
            myundebug(Functions$summarise)
        }
    }
    sim_results <- sim_results_check(sim_results)
    ret <- c(sim_results, 'REPLICATIONS'=replications, 'ERROR: '=try_errors,
             'WARNING: '=warnings)
    if(bootSE){
        # could parallelize, TODO
        SE_sim_results <- sapply(1L:boot_draws, function(r){
            pick <- rint(n = replications, min = 1L, max = replications)
            tmp <- if(!is.data.frame(results)) results[pick]
                else results[pick, , drop=FALSE]
            Functions$summarise(results=tmp, condition=condition, fixed_objects=fixed_objects)
        })
        if(!is.matrix(SE_sim_results)) SE_sim_results <- matrix(SE_sim_results, nrow=1L)
        SE_sim_results <- apply(SE_sim_results, 1L, sd)
        names(SE_sim_results) <- paste0("BOOT_SE.", names(sim_results))
        ret <- c(ret, SE_sim_results)
    }
    attr(ret, 'error_seeds') <- try_error_seeds
    if(store_results)
        attr(ret, 'full_results') <- tabled_results
    ret
}
