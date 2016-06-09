#-------------------------------------------------------------------
# Core generate-analyse-summarise steps for SimDesign

Generate <- function(condition, fixed_objects = NULL) {
    N <- with(condition, sample_size)
    v <- with(condition, var_ratio)
    sd1 <- ifelse(v <= 1, 1, sqrt(v))
    sd2 <- ifelse(v <= 1, sqrt(1/v), 1)
    if(with(condition, groups_equal)){
        N1 <- N2 <- N/2
    } else {
        N1 <- N * 1/3
        N2 <- N - N1
    }
    dat <- switch(as.character(condition$distribution),
        Gaussian = c(rnorm(N1, sd = sd1), rnorm(N2, sd = sd2)),
        t4 = c(rt(N1, df = 4), rt(N2, df = 4)),
        Chi4 = c(rchisq(N1, df = 4), rchisq(N2, df = 4)),
        Cauchy = c(rcauchy(N1), rcauchy(N2))
    )
    dat <- data.frame(DV=dat, group=c(rep('G1', N1), rep('G2', N2)))
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    DV <- dat$DV
    group <- dat$group
    F_test <- var.test(DV ~ group)$p.value
    Jacknife <- Jacknife_vartest(DV, group)$p.value
    Layard <- Layard_vartest(DV, group)$p.value
    W50 <- levene.test(DV, group=group, location = 'median')$p.value
    W10 <- levene.test(DV, group=group, location = 'trim.mean', trim.alpha = .1)$p.value
    Levene <- levene.test(DV, group=group, location = 'mean')$p.value
    ret <- c(F=F_test, Jacknife=Jacknife, Layard=Layard,
             Levene=Levene, W10=W10, W50=W50)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- EDR(results, alpha = .05)
    ret
}
