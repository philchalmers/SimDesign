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

groups_equal <- c(TRUE, FALSE)
var_ratio <- c(4, 2, 1, 1/2, 1/4)
sample_size <- c(80, 20)
distributions <- c('Gaussian', 't4', 'Chi4', 'Cauchy')
Design <- expand.grid(var_ratio=var_ratio,
                      sample_size=sample_size,
                      groups_equal=groups_equal,
                      distribution=distributions)

#remove redundent or not-applicable rows
Design <- subset(Design, !(groups_equal & var_ratio < 1))
Design <- subset(Design, !(distribution != 'Gaussian' & var_ratio != 1))

# Brown and Forsythe use different sample sizes when groups were unequal
pick1 <- with(Design, !groups_equal & sample_size == 80)
pick2 <- with(Design, !groups_equal & sample_size == 20)
Design$sample_size[pick1] <- 60
Design$sample_size[pick2] <- 30

print(Design)

#-------------------------------------------------------------------

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

results <- runSimulation(design=Design, replications=1000, parallel=TRUE,
                         generate=Generate, analyse=Analyse,
                         summarise=Summarise, packages='lawstat',
                         filename='BF_simulation')
View(results)

TypeI <- subset(results, var_ratio == 1)
View(TypeI)

Power <- subset(results, var_ratio != 1)
View(Power)
