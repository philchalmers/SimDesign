library(SimDesign)

res <- readRDS('Raju1995.rds')
res

View(res)

library(dplyr)

# average close to 1 for DTF, not 0, and too powerful (should be at alpha = .01)
res %>%
    group_by(direction, focal) %>%
    summarise(mean(mean.DTF), mean(DTF.p))

# Some Type I error examples
res %>%
    group_by(direction, focal) %>%
    summarise(CDF1=mean(mean.CDIF1), sd_CDF1=sd(mean.CDIF1),
              CDF2=mean(mean.CDIF2), sd_CDF2=sd(mean.CDIF2))

res %>%
    group_by(direction, focal) %>%
    summarise(NCDF1=mean(mean.NCDIF1), sd_CDF1=sd(mean.NCDIF1),
              NCDF2=mean(mean.NCDIF2), sd_CDF2=sd(mean.NCDIF2))

# These should be at alpha = .01.....wayyyyy too powerful
res %>%
    group_by(direction, focal) %>%
    summarise(mean(NCDIF.p1), mean(NCDIF.p2))

# Ad-hoc cut-offs not much better at all
res %>%
    group_by(direction, focal) %>%
    summarise(mean(NCDIF.0061), mean(NCDIF.0062))

#############################################

# Power ones
res %>%
    group_by(direction, focal) %>%
    summarise(CDF5=mean(mean.CDIF5), sd_CDF5=sd(mean.CDIF5),
              CDF10=mean(mean.CDIF10), sd_CDF10=sd(mean.CDIF10))

res %>%
    group_by(direction, focal) %>%
    summarise(NCDF5=mean(mean.NCDIF5), sd_CDF5=sd(mean.NCDIF5),
              NCDF10=mean(mean.NCDIF10), sd_CDF10=sd(mean.NCDIF10))

# Power still not even at 1.00 despite massive inflation
res %>%
    group_by(direction, focal) %>%
    summarise(mean(NCDIF.p5), mean(NCDIF.p10))

res %>%
    group_by(direction, focal) %>%
    summarise(mean(NCDIF.0065), mean(NCDIF.00610))


