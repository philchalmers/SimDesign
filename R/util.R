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
