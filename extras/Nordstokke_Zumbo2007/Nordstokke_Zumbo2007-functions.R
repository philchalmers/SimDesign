#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
    N <- with(condition, sample_size)
    R <- with(condition, sample_size_ratio)
    V <- with(condition, var_ratio)
    S <- with(condition, skewness)
    N1 <- N * R / (R + 1)
    N2 <- N - N1
    df <- switch(as.character(S),
                 '0' = 10000,
                 '1' = 7.4,
                 '2' = 2.2,
                 '3' = .83)
    DV <- c(rchisq(N1, df) * sqrt(V), rchisq(N2, df))
    dat <- data.frame(DV=DV, group=c(rep('G1', N1), rep('G2', N2)))
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL, parameters = NULL) {
    BF <- levene.test(dat$DV, dat$group, location = 'median')
    Levene <- levene.test(dat$DV, dat$group, location = 'mean')
    F_test <- var.test(DV ~ group, dat)
    ret <- c(BF=BF$p.value, Levene=Levene$p.value, F=F_test$p.value)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL, parameters_list = NULL) {
    ret <- EDR(results, alpha = .05)
    ret
}
