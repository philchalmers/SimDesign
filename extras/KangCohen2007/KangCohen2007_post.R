# Post-analysis
library(dplyr)

res <- readRDS("KangCohen.rds")
res

##################################
# information criteria means/sds
Table6_means <- res %>% filter(model == "1PL") %>%
    select(nitems:sample_size, estimator, means.M1PL.AIC:means.LR23.X2)
Table6_sds <- res %>% filter(model == "1PL") %>%
    select(nitems:sample_size, estimator, sds.M1PL.AIC:sds.LR23.X2)
Table6_means
Table6_sds

Table7_means <- res %>% filter(model == "2PL") %>%
    select(nitems:sample_size, estimator, means.M1PL.AIC:means.LR23.X2)
Table7_sds <- res %>% filter(model == "2PL") %>%
    select(nitems:sample_size, estimator, sds.M1PL.AIC:sds.LR23.X2)
Table7_means
Table7_sds

Table8_means <- res %>% filter(model == "3PL") %>%
    select(nitems:sample_size, estimator, means.M1PL.AIC:means.LR23.X2)
Table8_sds <- res %>% filter(model == "3PL") %>%
    select(nitems:sample_size, estimator, sds.M1PL.AIC:sds.LR23.X2)
Table8_means
Table8_sds

##################################
## proportion of competing models selected (more general measure than nose count)
Table9 <- res %>% filter(estimator == "MML") %>%
    select(nitems:model, M1PL.BIC:M3PL.LR) %>%
    arrange(nitems, sample_size, ability_mean, model)
Table9


##################################

# graphics
library(tidyr)
long <- Table9 %>%
    pivot_longer(M1PL.BIC:M3PL.LR, names_to='fit_stat') %>%
    separate(fit_stat, c('fit', 'stat')) %>%
    mutate(stat=factor(stat), fit=factor(fit), model=factor(model))
long

library(ggplot2)

# unnormalized
# ggplot(long, aes(x=fit, y=value, fill=stat)) +
#     geom_bar(stat = "identity", position = 'dodge') +
#     facet_grid(model ~ nitems)

# normalized
sub1 <- long %>% group_by(fit, stat, model, nitems) %>%
    summarise(mvalue = mean(value)) %>% ungroup %>%
    group_by(model, stat, nitems) %>% mutate(value = mvalue / sum(mvalue))
(fig1 <- ggplot(sub1, aes(x=fit, y=value, fill=stat)) +
        geom_bar(stat = "identity", position = 'dodge') +
        facet_grid(model ~ nitems)) +
    ggtitle("Figure 1", subtitle = "Model Selection Proportions by Test Length")

# unnormalized
# ggplot(long, aes(x=fit, y=value, fill=stat)) +
#         geom_bar(stat = "identity", position = 'dodge') +
#         facet_grid(model ~ sample_size)

# normalized
sub2 <- long %>% group_by(fit, stat, model, sample_size) %>%
    summarise(mvalue = mean(value)) %>% ungroup %>%
    group_by(model, stat, sample_size) %>% mutate(value = mvalue / sum(mvalue))
ggplot(sub2, aes(x=fit, y=value, fill=stat)) +
    geom_bar(stat = "identity", position = 'dodge') +
    facet_grid(model ~ sample_size) +
    ggtitle("Figure 2", subtitle = "Model Selection Proportions by Sample Size")


# unnormalized
# ggplot(long, aes(x=fit, y=value, fill=stat)) +
#         geom_bar(stat = "identity", position = 'dodge') +
#         facet_grid(model ~ ability_mean)

# normalized
sub3 <- long %>% group_by(fit, stat, model, ability_mean) %>%
    summarise(mvalue = mean(value)) %>% ungroup %>%
    group_by(model, stat, ability_mean) %>% mutate(value = mvalue / sum(mvalue))
ggplot(sub3, aes(x=fit, y=value, fill=stat)) +
    geom_bar(stat = "identity", position = 'dodge') +
    facet_grid(model ~ ability_mean)  +
    ggtitle("Figure 3", subtitle = "Model Selection Proportions by Ability Distribution")

