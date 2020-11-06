# Simulation from the article:
#
# Robust Tests for the Equality of Variances
# Author(s): Morton B. Brown and Alan B. Forsythe
# Source: Journal of the American Statistical Association,
#   Vol. 69, No. 346 (Jun., 1974), pp. 364-367
# Published by: American Statistical Association

#-------------------------------------------------------------------
# Initial setup (results copy and pasted int an R script file)

# devtools::install_github('philchalmers/SimDesign')
# SimDesign::SimFunctions('BF-sim', comment=FALSE)

# include extra user-defined functions
source('BF_sim_custom-functions.R')

#-------------------------------------------------------------------
# Define the design conditions

library(SimDesign)

Design <- createDesign(var_ratio=c(4, 2, 1, 1/2, 1/4),
                       N=c(80, 20),
                       groups_equal=c(TRUE, FALSE),
                       distribution=c('Gaussian', 't4', 'Chi4', 'Cauchy'),
                       # remove redundent or not-applicable rows
                       subset = !(groups_equal & var_ratio < 1) |
                           !(distribution != 'Gaussian' & var_ratio != 1))

# Brown and Forsythe use different sample sizes when groups were unequal
pick1 <- with(Design, !groups_equal & N == 80)
pick2 <- with(Design, !groups_equal & N == 20)
Design$N[pick1] <- 60
Design$N[pick2] <- 30

Design

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    sd1 <- ifelse(var_ratio <= 1, 1, sqrt(var_ratio))
    sd2 <- ifelse(var_ratio <= 1, sqrt(1/var_ratio), 1)
    if(groups_equal){
        N1 <- N2 <- N/2
    } else {
        N1 <- N * 1/3
        N2 <- N - N1
    }
    dv <- switch(distribution,
                 Gaussian = c(rnorm(N1, sd = sd1), rnorm(N2, sd = sd2)),
                 t4 = c(rt(N1, df = 4), rt(N2, df = 4)),
                 Chi4 = c(rchisq(N1, df = 4), rchisq(N2, df = 4)),
                 Cauchy = c(rcauchy(N1), rcauchy(N2))
    )
    dat <- data.frame(DV=dv,
                      group=c(rep('G1', N1), rep('G2', N2)))
    dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    Attach(dat)
    F_test <- var.test(DV ~ group)$p.value
    Jacknife <- Jacknife_vartest(DV, group)$p.value
    Layard <- Layard_vartest(DV, group)$p.value
    W50 <- levene.test(DV, group=group, location = 'median')$p.value
    W10 <- levene.test(DV, group=group, location = 'trim.mean',
                       trim.alpha = .1)$p.value
    Levene <- levene.test(DV, group=group, location = 'mean')$p.value
    ret <- c(F=F_test, Jacknife=Jacknife, Layard=Layard,
             Levene=Levene, W10=W10, W50=W50)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- EDR(results, alpha = .05)
    ret
}

#-------------------------------------------------------------------
# Run the MCS

res <- runSimulation(design=Design, replications=1000, parallel=TRUE,
                     generate=Generate, analyse=Analyse,
                     summarise=Summarise, packages='lawstat',
                     filename='BF_simulation')
res

TypeI <- subset(res, var_ratio == 1)
TypeI

Power <- subset(res, var_ratio != 1)
Power
