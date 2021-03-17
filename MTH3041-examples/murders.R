library(tidyverse)
library(bayesplot)
library(reshape2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load("murders.RData")


MurdersData <- melt(murders, id.vars = 'MurdersPerYearPerMillion')
ggplot(MurdersData) +
  geom_point(aes(x=value, y= MurdersPerYearPerMillion, colour=variable)) +
  geom_smooth(aes(x=value, y= MurdersPerYearPerMillion, colour=variable), method = lm) +
  facet_wrap(~variable, scales="free_x")
