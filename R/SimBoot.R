SimBoot <- function(results, summarise, condition, fixed_objects, boot_method,
                    boot_draws, CI)
{
    boot_fun <- function(r, dat, N, studentized = FALSE){
        pick <- rint(n = N, min = 1L, max = N)
        tmp <- if(!is.data.frame(dat)) dat[pick]
        else dat[pick, , drop=FALSE]
        ret <- summarise(results=tmp, condition=condition, fixed_objects=fixed_objects)
        if(studentized){
            ret2 <- vector('list', 50)
            for(i in 1L:50L){
                pick2 <- rint(n = N, min = 1L, max = N)
                tmp2 <- if(!is.data.frame(dat)) tmp[pick2]
                else tmp[pick2, , drop=FALSE]
                ret2[[i]] <- summarise(results=tmp2, condition=condition, fixed_objects=fixed_objects)
            }
            ret2 <- do.call(rbind, ret2)
            SEb <- apply(ret2, 2, sd)
            attr(ret, 'SEb') <- SEb
        }
        ret
    }

    stopifnot(boot_method %in% c('basic', 'percentile', 'norm', 'studentized', 'CLT'))
    replications <- if(is.data.frame(results)) nrow(results) else length(results)
    t0 <- summarise(condition=condition, results=results, fixed_objects=fixed_objects)
    if(boot_method %in% c('basic', 'percentile', 'norm'))
        t <- do.call(rbind, lapply(1L:boot_draws, boot_fun, dat=results, N=replications))
    if(boot_method == 'basic'){
        ql <- apply(t, 2L, quantile, prob = (1 - CI)/2)
        qu <- apply(t, 2L, quantile, prob = 1 - (1 - CI)/2)
        lower <- 2*t0 - qu
        upper <- 2*t0 - ql
    } else if(boot_method == 'percentile'){
        lower <- apply(t, 2L, quantile, prob = (1 - CI)/2)
        upper <- apply(t, 2L, quantile, prob = 1 - (1 - CI)/2)
    } else if(boot_method == 'norm'){
        SEs <- apply(t, 2L, sd)
        lower <- t0 + qnorm((1 - CI)/2) * SEs
        upper <- t0 + qnorm(1 - (1 - CI)/2) * SEs
    } else if(boot_method == 'studentized'){
        tf <- lapply(1L:boot_draws, boot_fun, dat=results, N=replications, studentized=TRUE)
        SEb <- do.call(rbind, lapply(tf, function(x) attr(x, 'SEb')))
        t <- do.call(rbind, tf)
        SEs <- apply(t, 2L, sd)
        qs <- (t - t0) / SEb
        ql <- apply(qs, 2L, quantile, prob = (1-CI)/2)
        qu <- apply(qs, 2L, quantile, prob = 1 - (1-CI)/2)
        lower <- t0 + ql * SEs
        upper <- t0 + qu * SEs
    } else if(boot_method == 'CLT'){
        SEs <- apply(results, 2, sd) / sqrt(replications)
        ql <- qnorm((1-CI)/2)
        qu <- qnorm(1 - (1-CI)/2)
        lower <- t0 + ql * SEs
        upper <- t0 + qu * SEs
    }
    CIs <- as.vector(rbind(lower, upper))
    names(CIs) <- paste0(as.vector(sapply(paste0(ifelse(boot_method != 'CLT', "BOOT_", "CI_"),
                                                 names(t0), "_"), function(x)
        paste0(x, c( (1 - CI)/2 * 100, (1 - (1 - CI)/2)*100)))))
    CIs
}
