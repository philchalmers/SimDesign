#-------------------------------------------------------------------

library(SimDesign)

Design <- expand.grid(skewness = 0:3,
                      sample_size = c(24,48,96),
                      sample_size_ratio = 1:3,
                      var_ratio = c(5:1, 1/2, 1/3, 1/4, 1/5))
head(Design)
dim(Design)

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

Analyse <- function(condition, dat, fixed_objects = NULL) {
    BF <- levene.test(dat$DV, dat$group, location = 'median')
    Levene <- levene.test(dat$DV, dat$group, location = 'mean')
    F_test <- var.test(DV ~ group, dat)
    ret <- c(BF=BF$p.value, Levene=Levene$p.value, F=F_test$p.value)
    ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    ret <- EDR(results, alpha = .05)
    ret
}

#-------------------------------------------------------------------

# save results to the drive
runSimulation(design=Design, replications=5000, packages = 'lawstat',
    generate=Generate, analyse=Analyse, summarise=Summarise, parallel=TRUE,
    save=TRUE, filename='Nordstokke_Zumbo2007')

results <- readRDS('Nordstokke_Zumbo2007.rds')
#head(results)

# Type I errors
TypeI <- subset(results, var_ratio == 1)
Table1 <- TypeI[order(TypeI$skewness, TypeI$sample_size), ]
View(Table1)

# Power
Power <- subset(results, var_ratio != 1)
View(Power)

