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
    Gaussian <- c(rnorm(N1, sd = sd1), rnorm(N2, sd = sd2))
    if(v == 1){
        t4 <- c(rt(N1, df = 4), rt(N2, df = 4))
        Chi4 <- c(rchisq(N1, df = 4), rchisq(N2, df = 4))
        Cauchy <- c(rcauchy(N1), rcauchy(N2))
    } else t4 <- Chi4 <- Cauchy <- rep(NA, N)
    dat <- data.frame(Gaussian=Gaussian, t4=t4, Chi4=Chi4, Cauchy=Cauchy,
                      group=c(rep('G1', N1), rep('G2', N2)))
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL, parameters = NULL) {
    tests <- function(DV, group, name){
        F_test <- var.test(DV ~ group)$p.value
        Jacknife <- Jacknife_vartest(DV, group)$p.value
        Layard <- Layard_vartest(DV, group)$p.value
        W50 <- levene.test(DV, group=group, location = 'median')$p.value
        W10 <- levene.test(DV, group=group, location = 'trim.mean', trim.alpha = .1)$p.value
        Levene <- levene.test(DV, group=group, location = 'mean')$p.value
        ret <- c(F=F_test, Jacknife=Jacknife, Layard=Layard,
                 Levene=Levene, W10=W10, W50=W50)
        names(ret) <- paste0(name, '_', names(ret))
        ret
    }

    Gaussian <- tests(dat$Gaussian, group=dat$group, name = 'Gaussian')
    if(condition$var_ratio == 1){
        t4 <-  tests(dat$t4, group=dat$group, name = 't4')
        Chi4 <-  tests(dat$Chi4, group=dat$group, name = 'Chi4')
        Cauchy <-  tests(dat$Cauchy, group=dat$group, name = 'Cauchy')
    } else t4 <- Chi4 <- Cauchy <- NULL
    ret <- c(Gaussian, t4, Chi4, Cauchy)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL, parameters_list = NULL) {
    ret <- c(alpha.05=EDR(results, alpha = .05))
    ret
}
