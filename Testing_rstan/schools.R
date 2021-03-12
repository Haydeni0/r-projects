library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(tidyverse)

# Eight schools dataset
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = 'schools.stan', data = schools_dat, warmup = 1e4, iter = 2e4,
            chains = 16)

fit.df = as_tibble(rstan::extract(fit))

pairs(fit, pars = c("mu", "tau", "lp__"))

summary(fit)
