Analysis <- function(Functions, condition, replications, fixed_objects, cl, MPI, seed, save,
                     save_results, save_results_out_rootdir, save_results_dirname, max_errors,
                     boot_method, boot_draws, CI, save_seeds, save_seeds_dirname, load_seed, export_funs, packages,
                     summarise_asis, warnings_as_errors, progress, store_results, load_balancing,
                     allow_na, allow_nan, use_try, stop_on_fatal, store_warning_seeds)
{
    # This defines the work-flow for the Monte Carlo simulation given the condition (row in Design)
    #  and number of replications desired
    if(is.null(cl)){
        if(!is.null(seed)) set.seed(seed[condition$ID])
        results <- if(progress){
            try(pbapply::pblapply(1L:replications, mainsim, condition=condition,
                   generate=Functions$generate,
                   analyse=Functions$analyse,
                   fixed_objects=fixed_objects,
                   max_errors=max_errors, packages=packages, save=save,
                   store_warning_seeds=store_warning_seeds,
                   save_results_out_rootdir=save_results_out_rootdir,
                   save_seeds=save_seeds, load_seed=load_seed,
                   save_seeds_dirname=save_seeds_dirname,
                   warnings_as_errors=warnings_as_errors,
                   allow_na=allow_na, allow_nan=allow_nan, use_try=use_try), TRUE)
        } else {
            try(lapply(1L:replications, mainsim, condition=condition,
                   generate=Functions$generate,
                   analyse=Functions$analyse,
                   fixed_objects=fixed_objects,
                   max_errors=max_errors, packages=packages, save=save,
                   save_results_out_rootdir=save_results_out_rootdir,
                   save_seeds=save_seeds, load_seed=load_seed,
                   store_warning_seeds=store_warning_seeds,
                   save_seeds_dirname=save_seeds_dirname,
                   warnings_as_errors=warnings_as_errors,
                   allow_na=allow_na, allow_nan=allow_nan, use_try=use_try), TRUE)
        }
    } else {
        if(MPI){
            i <- 1L
            results <- try(foreach(i=1L:replications, .export=export_funs) %dopar%
                mainsim(i, condition=condition, generate=Functions$generate,
                     analyse=Functions$analyse, fixed_objects=fixed_objects, load_seed=load_seed,
                     max_errors=max_errors, save=save, packages=packages,
                     save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                     save_results_out_rootdir=save_results_out_rootdir,
                     store_warning_seeds=store_warning_seeds,
                     warnings_as_errors=warnings_as_errors, allow_na=allow_na, allow_nan=allow_nan,
                     use_try=use_try), TRUE)
        } else {
            if(!is.null(seed)) parallel::clusterSetRNGStream(cl=cl, seed[condition$ID])
            results <- if(progress){
                try(pbapply::pblapply(1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    fixed_objects=fixed_objects, packages=packages, save=save,
                                    save_results_out_rootdir=save_results_out_rootdir,
                                    max_errors=max_errors, store_warning_seeds=store_warning_seeds,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors, allow_na=allow_na,
                                    allow_nan=allow_nan, use_try=use_try, cl=cl), TRUE)
            } else {
                usefun <- if(load_balancing) parallel::parLapplyLB else parallel::parLapply
                try(usefun(cl, 1L:replications, mainsim,
                                    condition=condition, generate=Functions$generate,
                                    analyse=Functions$analyse, load_seed=load_seed,
                                    fixed_objects=fixed_objects, packages=packages, save=save,
                                    save_results_out_rootdir=save_results_out_rootdir,
                                    max_errors=max_errors, store_warning_seeds=store_warning_seeds,
                                    save_seeds=save_seeds, save_seeds_dirname=save_seeds_dirname,
                                    warnings_as_errors=warnings_as_errors, allow_na=allow_na,
                                    allow_nan=allow_nan, use_try=use_try), TRUE)
            }
        }
    }
    if(is(results, 'try-error')){
        # deal with fatal errors
        if(stop_on_fatal){
            stop(as.character(results))
        } else {
            out <- gsub('\\n', '', as.character(results))
            ret <- c(FATAL_TERMINATION=strsplit(out, "Last error message was:   ")[[1L]][2L])
            if(progress)
                message('\nWARNING: Condition terminated because of consecutive errors; using NA placeholders. \n\t Last error message was: ',
                        unname(ret))
            return(ret)
        }
    }
    if(summarise_asis || store_results){
        tabled_results <- if(is.data.frame(results[[1]]) && nrow(results[[1L]]) == 1L){
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
    warning_message_seeds <- do.call(rbind, lapply(results, function(x) attr(x, 'warning_message_seeds')))
    for(i in seq_len(length(results)))
        attr(results[[i]], 'try_errors') <- attr(results[[i]], 'warnings') <-
        attr(results[[i]], 'try_error_seeds') <- attr(results[[i]], 'warning_message_seeds') <- NULL

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
    ret <- c(sim_results, 'REPLICATIONS'=replications, 'ERROR: '=try_errors,
             'WARNING: '=warnings)
    if(boot_method != 'none'){
        # could parallelize, but likely not worth the overhead
        CIs <- SimBoot(results, summarise=Functions$summarise,
                       condition=condition, fixed_objects=fixed_objects,
                       boot_method=boot_method,
                       boot_draws=boot_draws, CI=CI)
        ret <- c(ret, CIs)
    }
    attr(ret, 'error_seeds') <- try_error_seeds
    attr(ret, 'warning_seeds') <- warning_message_seeds
    if(store_results)
        attr(ret, 'full_results') <- tabled_results
    ret
}
