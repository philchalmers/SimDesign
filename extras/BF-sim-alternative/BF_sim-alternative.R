# Simulation from the article:
#
# Robust Tests for the Equality of Variances
# Author(s): Morton B. Brown and Alan B. Forsythe
# Source: Journal of the American Statistical Association, Vol. 69, No. 346 (Jun., 1974), pp. 364-367
# Published by: American Statistical Association

#-------------------------------------------------------------------
# Initial setup (results copy and pasted int an R script file)

# devtools::install_github('philchalmers/SimDesign')
# SimDesign::SimFunctions('BF-sim', comment=FALSE)

#-------------------------------------------------------------------
# Define the design conditions

library(SimDesign)

groups_equal <- c(TRUE, FALSE)
var_ratio <- c(4, 2, 1, 1/2, 1/4)
sample_size <- c(80, 20)
Design <- expand.grid(var_ratio=var_ratio,
                      sample_size=sample_size,
                      groups_equal=groups_equal)

#remove redudent rows
Design <- subset(Design, !(groups_equal & var_ratio < 1))

# Brown and Forsythe use different sample sizes when groups were unequal
pick1 <- with(Design, !groups_equal & sample_size == 80)
pick2 <- with(Design, !groups_equal & sample_size == 20)
Design$sample_size[pick1] <- 60
Design$sample_size[pick2] <- 30

print(Design)

#-------------------------------------------------------------------
# Run the MCS

source('BF_sim_custom-functions.R')
source('BF_sim-functions-alternative.R')

results <- runSimulation(design=Design, replications=1000, parallel=TRUE,
                         generate=Generate, analyse=Analyse, summarise=Summarise,
                         packages='lawstat', filename='BF_simulation-alternative')
# View(results)

TypeI <- subset(results, var_ratio == 1)
View(TypeI)

Power <- subset(results, var_ratio != 1)
Power <- subset(Power, select = !is.na(Power[1,])) #remove pure NA columns
View(Power)
