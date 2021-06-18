#-------------------------------------------------------------------

library(SimDesign)

Design <- createDesign(N = c(500, 1000),
                       focal = c('reference', 'focal1', 'focal2', 'focal3'),
                       direction = c('uni', 'bi'))

source('Raju1995_extras.R')
source('DFIT.R')

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    refpars <- fixed_objects[[direction]]$reference
    if(direction == 'bi' && focal == 'focal3')
        refpars <- fixed_objects[['bi']]$reference_focal3
    focalpars <- fixed_objects[[direction]][[focal]]
    datref <- simdata(a=refpars[,"a1"], d=refpars[,"d"], N=N/2, itemtype='2PL')
    datfocal <- simdata(a=focalpars[,"a1"], d=focalpars[,"d"], N=N/2, itemtype='2PL')
    dat <- data.frame(group = rep(c('ref', 'focal'), each=N/2),
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

    DTF <- DFIT(mod)
    DIF <- DFIT(mod, DIF = TRUE)

    ret <- c(DTF=DTF$DTF, DTF.p=DTF$p.X2,
             CDIF=DIF$CDIF, NCDIF=DIF$NCDIF, NCDIF.p=DIF$p.X2)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    pick <- subset(results, select = c(DTF, CDIF1:CDIF40, NCDIF1:NCDIF40))
    means <- colMeans(pick)
    SDs <- apply(pick, 2, sd)
    pick.p <- subset(results, select = c(DTF.p, NCDIF.p1:NCDIF.p40))
    ps <- EDR(pick.p, .01)
    pick.NCDIF <- subset(results, select = NCDIF1:NCDIF40)
    NCDIF.006 <- colMeans(pick.NCDIF > .006)
    ret <- c(mean=means, SD=SDs, ps, NCDIF.006=unname(NCDIF.006))
    ret
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=5000, generate=Generate,
                     analyse=Analyse, summarise=Summarise,
                     fixed_objects=fo, packages = 'mirt', parallel=TRUE,
                     save_results = FALSE, filename = 'Raju1995')
res
