#-------------------------------------------------------------------

library(SimDesign)

Design <- createDesign(N = c(500, 1000),
                       focal = c('reference', 'focal1', 'focal2', 'focal3'),
                       direction = c('uni', 'bi'))

source('Raju1995_extras.R')  ## read-in large tables of parameter sets

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    refpars <- fixed_objects[[direction]]$reference
    if(direction == 'bi' && focal == 'focal3') # for modified reference group combination
        refpars <- fixed_objects[['bi']]$reference_focal3
    focalpars <- fixed_objects[[direction]][[focal]]
    datref <- simdata(a=refpars[,"a1"], d=refpars[,"d"], N=N/2, itemtype='2PL')
    datfocal <- simdata(a=focalpars[,"a1"], d=focalpars[,"d"], N=N/2, itemtype='2PL')
    dat <- data.frame(group = rep(c('G1', 'G2'), each=N/2),
                      rbind(datref, datfocal))
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    group <- dat$group
    items <- subset(dat, select = -group)

    ##  MML estimation instead of MMAP
    ##    Following is equivalent to fitting independent single-group models
    ##    (linking not required since both groups generated from same scale)
    mod <- multipleGroup(items, 1, group=group, verbose=FALSE)
    stopifnot(extract.mirt(mod, 'converged'))

    cfs <- coef(mod, simplify=TRUE)
    ret <- map(cfs, 'items')
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    Attach(condition)
    refpars <- fixed_objects[[direction]]$reference
    if(direction == 'bi' && focal == 'focal3') # for modified reference group combination
        refpars <- fixed_objects[['bi']]$reference_focal3
    focalpars <- fixed_objects[[direction]][[focal]]
    refest <- map(results, "G1")
    focalest <- map(results, "G2")
    ret <- list(bias.ref=bias(refest, refpars),
                RMSE.ref=RMSE(refest, refpars),
                bias.focal=bias(focalest, focalpars),
                RMSE.focal=RMSE(focalest, focalpars))
    ret
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=5000, generate=Generate,
                     analyse=Analyse, summarise=Summarise, debug='none',
                     fixed_objects=fo, packages = c('mirt', 'purrr'), parallel=TRUE,
                     save_results = FALSE, filename = 'Raju1995_items')
res
