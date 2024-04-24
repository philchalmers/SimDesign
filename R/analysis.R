Analysis <- function(Functions, condition, replications, fixed_objects, cl, MPI, seed, save,
                     save_results, save_results_out_rootdir, save_results_dirname, max_errors,
                     boot_method, boot_draws, CI,
                     save_seeds, save_seeds_dirname, load_seed,
                     export_funs, summarise_asis, warnings_as_errors, progress, store_results,
                     allow_na, allow_nan, use_try, stop_on_fatal, store_warning_seeds,
                     include_replication_index, packages, .options.mpi, useFuture, multirow,
                     allow_gen_errors, max_time, max_RAM, store_Random.seeds,
                     save_results_filename = NULL, arrayID = NULL)
{
    # This defines the work-flow for the Monte Carlo simulation given the condition (row in Design)
    #  and number of replications desired
    if(useFuture){
        if(!is.null(seed)) set_seed(seed[condition$ID])
        iters <- 1L:replications
        p <- progressr::progressor(along = iters)
        results <- try(future.apply::future_lapply(iters,
                                                   mainsim, condition=condition,
                                                   generate=Functions$generate,
                                                   analyse=Functions$analyse,
                                                   fixed_objects=fixed_objects,
                                                   max_errors=max_errors, save=save,
                                                   store_warning_seeds=store_warning_seeds,
                                                   save_results_out_rootdir=save_results_out_rootdir,
                                                   store_Random.seeds=store_Random.seeds,
                                                   save_seeds=save_seeds,
                                                   load_seed=load_seed,
                                                   save_seeds_dirname=save_seeds_dirname,
                                                   warnings_as_errors=warnings_as_errors,
                                                   include_replication_index=include_replication_index,
                                                   allow_na=allow_na, allow_nan=allow_nan, use_try=use_try,
                                                   p=p, future.seed=TRUE, allow_gen_errors=allow_gen_errors),
                       silent=TRUE)
    } else if(is.null(cl)){
        if(!is.null(seed)) set_seed(seed[condition$ID])
        results <- if(progress){
            try(pbapply::pblapply(1L:replications, mainsim, condition=condition,
                   generate=Functions$generate,
                   analyse=Functions$analyse,
                   fixed_objects=fixed_objects,
                   max_errors=max_errors, save=save,
                   store_Random.seeds=store_Random.seeds,
                   store_warning_seeds=store_warning_seeds,
                   save_results_out_rootdir=save_results_out_rootdir,
                   save_seeds=save_seeds, load_seed=load_seed,
                   save_seeds_dirname=save_seeds_dirname,
                   warnings_as_errors=warnings_as_errors,
                   include_replication_index=include_replication_index,
                   allow_na=allow_na, allow_nan=allow_nan, use_try=use_try,
                   allow_gen_errors=allow_gen_errors), TRUE)
        } else {
            try(lapply_timer(1L:replications, mainsim,
                           max_time=max_time, max_RAM=max_RAM,
                           condition=condition,
                           generate=Functions$generate,
                           analyse=Functions$analyse,
                           fixed_objects=fixed_objects,
                           max_errors=max_errors, save=save,
                           store_Random.seeds=store_Random.seeds,
                           save_results_out_rootdir=save_results_out_rootdir,
                           save_seeds=save_seeds, load_seed=load_seed,
                           store_warning_seeds=store_warning_seeds,
                           save_seeds_dirname=save_seeds_dirname,
                           warnings_as_errors=warnings_as_errors,
                           include_replication_index=include_replication_index,
                           allow_na=allow_na, allow_nan=allow_nan, use_try=use_try,
                           allow_gen_errors=allow_gen_errors), TRUE)
        }
    } else {
        if(MPI){
            stop('MPI structure no longer supported. Please use the parallel = \"future" approach',
                 call. = FALSE)
        } else {
            if(!is.null(seed)) parallel::clusterSetRNGStream(cl=cl, seed[condition$ID])
            results <- if(progress){
                try(pbapply::pblapply(1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    fixed_objects=fixed_objects, save=save,
                                    store_Random.seeds=store_Random.seeds,
                                    save_results_out_rootdir=save_results_out_rootdir,
                                    max_errors=max_errors, store_warning_seeds=store_warning_seeds,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors, allow_na=allow_na,
                                    include_replication_index=include_replication_index,
                                    allow_nan=allow_nan, allow_gen_errors=allow_gen_errors,
                                    use_try=use_try, cl=cl), TRUE)
            } else {
                try(parallel::parLapply(cl, 1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    store_Random.seeds=store_Random.seeds,
                                    fixed_objects=fixed_objects, save=save,
                                    save_results_out_rootdir=save_results_out_rootdir,
                                    max_errors=max_errors, store_warning_seeds=store_warning_seeds,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors, allow_na=allow_na,
                                    include_replication_index=include_replication_index,
                                    allow_nan=allow_nan, allow_gen_errors=allow_gen_errors,
                                    use_try=use_try), TRUE)
            }
        }
    }
    if(is(results, 'try-error')){
        # deal with fatal errors
        if(stop_on_fatal){
            stop(as.character(results))
        } else {
            out <- gsub('\\n', '', as.character(results))
            splt <- strsplit(out, "Last error message was:   ")[[1L]]
            if(splt[1L] == "Error : Invalid object returned from Analyse()")
                stop("Analyse() must return a numeric vector, list, or data.frame", call.=FALSE)
            ret <- c(FATAL_TERMINATION=splt[2L])
            if(progress)
                message(c('\nWARNING: Condition terminated because of consecutive errors;',
                          ' using NA placeholders. \n\t Last error message was: '),
                        unname(ret))
            return(ret)
        }
    }
    ID <- ifelse(multirow, paste0('-', condition$ID), "")
    if(summarise_asis || store_results){
        tabled_results <- toTabledResults(results)
        if(save_results){
            tmp <- ifelse(is.null(save_results_filename), 'results-row', save_results_filename)
            tmpfilename <- paste0(save_results_dirname,
                                  sprintf('/%s', tmp), ID, '.rds')
            saveRDS(list(condition=condition, results=tabled_results),
                    file.path(save_results_out_rootdir, tmpfilename))
        }
        if(summarise_asis) return(tabled_results)
    }

    if(store_Random.seeds){
        stored_Random.seeds <- do.call(rbind,
                                       lapply(results, function(x) attr(x, 'current_Random.seed')))
    }
    try_errors <- do.call(c, lapply(results, function(x) attr(x, 'try_errors')))
    try_error_seeds <- do.call(rbind, lapply(results, function(x) attr(x, 'try_error_seeds')))
    try_errors <- if(length(try_errors)){
        table(try_errors[!is.na(try_errors)])
    } else table(try_errors)
    string <- 'Error in analyse\\(dat = simlist, condition = condition, fixed_objects = fixed_objects) : \\n  '
    names(try_errors) <-
        gsub(string, replacement = 'Error : ', names(try_errors))
    warnings <- do.call(c, lapply(results, function(x) attr(x, 'warnings')))
    warnings <- if(length(warnings)){
        table(warnings[!is.na(warnings)])
    } else table(warnings)
    warning_message_seeds <- do.call(rbind, lapply(results, function(x) attr(x, 'warning_message_seeds')))
    for(i in seq_len(length(results)))
        attr(results[[i]], 'try_errors') <- attr(results[[i]], 'warnings') <-
        attr(results[[i]], 'try_error_seeds') <- attr(results[[i]], 'warning_message_seeds') <- NULL

    #collect meta simulation statistics (bias, RMSE, type I errors, etc)
    obs_reps <- length(results)
    results <- stackResults(results)
    if(save_results){
        tmp <- ifelse(is.null(save_results_filename), 'results-row', save_results_filename)
        tmpfilename <- paste0(save_results_dirname,
                              sprintf('/%s', tmp), ID, '.rds')
        tmpcondition <- condition
        tmpcondition$ID <- NULL
        saveRDS(list(condition=tmpcondition, results=results, errors=try_errors,
                     error_seeds=try_error_seeds,
                     warnings=warnings, warning_seeds=warning_message_seeds),
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
    summarise_list <- attr(sim_results, 'summarise_list')
    ret <- c(sim_results, 'REPLICATIONS'=obs_reps,
             'ERROR: '=clip_names(try_errors),
             'WARNING: '=clip_names(warnings))
    if(boot_method != 'none'){
        # could parallelize, but likely not worth the overhead
        # TODO test whether this works with Summarise() list outputs
        CIs <- SimBoot(results, summarise=Functions$summarise,
                       condition=condition, fixed_objects=fixed_objects,
                       boot_method=boot_method,
                       boot_draws=boot_draws, CI=CI)
        ret <- c(ret, CIs)
    }
    attr(ret, 'error_seeds') <- try_error_seeds
    attr(ret, 'warning_seeds') <- warning_message_seeds
    attr(ret, 'summarise_list') <- summarise_list
    if(store_results)
        attr(ret, 'full_results') <- tabled_results
    if(store_Random.seeds)
        attr(ret, 'stored_Random.seeds') <- stored_Random.seeds
    ret
}
