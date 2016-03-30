#-------------------------------------------------------------------

library(SimDesign)

Design <- expand.grid(skewness = 0:3,
                      sample_size = c(24,48,96),
                      sample_size_ratio = 1:3,
                      var_ratio = c(5:1, 1/2, 1/3, 1/4, 1/5))

dim(Design)

# setwd("/home/phil/Desktop")
source("Nordstokke_Zumbo2007-functions.R")

#-------------------------------------------------------------------

runSimulation(design=Design, replications=5000, packages = 'lawstat',
    generate=Generate, analyse=Analyse, summarise=Summarise, edit='none', parallel=TRUE,
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

