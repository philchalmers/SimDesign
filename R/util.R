# return a character vector of functions defined in .GlobalEnv
parent_env_fun <- function(){
    nms <- ls(envir = parent.frame(2L))
    is_fun <- sapply(nms, function(x, envir) is.function(get(x, envir=envir)),
                     envir = parent.frame(2L))
    return(nms[is_fun])
}

load_packages <- function(packages){
    if(!is.null(packages))
        for(pack in packages)
            library(substitute(pack), character.only = TRUE)
    invisible()
}

get_packages <- function(packages){
    sapply(packages, function(x) as.character(packageVersion(x)))
}

# base-code borrowed and modified from pbapply
timeFormater <- function(time, decimals = TRUE){
    dec <- round(time - floor(time), 2)
    sec <- round(time %% 60)
    if(decimals) sec <- sec + dec
    time <- floor(time / 60)
    minutes <- floor(time %% 60)
    time <- floor(time / 60)
    days <- floor(time / 24)
    time <- floor(time %% 24)
    hours <- floor(time %% 60)
    resTime <- ""
    if (days > 0)
        resTime <- sprintf("%02id ", days)
    if (hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02ih ", hours), sep = "")
    if (minutes > 0 || hours > 0 || days > 0)
        resTime <- paste(resTime, sprintf("%02im ", minutes), sep = "")
    resTime <- if(decimals) paste0(resTime, sprintf("%.2fs", sec))
    else paste0(resTime, sprintf("%02is", sec))
    resTime
}

print_progress <- function(p, time1, time0, stored_time, progress){
    if(!progress)
        cat(sprintf('\rCompleted: %i%s,  Previous condition time: %s,  Total elapsed time: %s ',
                    round(p * 100), '%', timeFormater(time1 - time0), timeFormater(sum(stored_time))))
    else
        cat(sprintf('\nCompleted: %i%s,  Total elapsed time: %s \n',
                    round(p * 100), '%', timeFormater(sum(stored_time))))
    invisible()
}
