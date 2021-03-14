library(tidyverse)
library(bayesplot)
library(reshape2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load("Rainfall.RData")

ggplot(Rainfall) + geom_point(aes(x = year, y = Rainfall)) + theme_bw()

ggplot(Rainfall) + geom_histogram(aes(x = Rainfall, y = ..density..))

# data {
#   int<lower=0> N;
#   vector<lower=0>[N] y;
#   vector[N] X;
#   real alpha_mean;
#   real alpha_sd;
#   real beta_sd;
#   real sigma_mean;
#   real sigma_sd;
#   // For generated quantities
#   int<lower=0> N_new;
#   vector[N_new] x_new;
# }
stan_list <- list(
  N = nrow(Rainfall),
  y = Rainfall$Rainfall,
  X = Rainfall$year,
  alpha_mean = log(50),
  alpha_sd = 1,
  beta_sd = 0.001,
  sigma_mean = 300,
  sigma_sd = 100,
  N_new = nrow(Rainfall),
  x_new = Rainfall$year
)

bmodel <-
  stan(
    file = "Rainfall.stan",
    data = stan_list,
    chains = 4,
    iter = 40000,
    init = 4
  )

summary(bmodel)$summary[1:6,]

mcmc_trace(bmodel, pars = c("alpha", "beta", "sigma"))
mcmc_trace(bmodel, pars = c("mu[1]","mu[2]"))

pairs(bmodel, pars = c("alpha", "beta", "sigma"))

pairs(bmodel, pars = c("mu[1]","mu[2]"))

# plot predictions
Pred <- extract(bmodel,pars=c("alpha","beta","sigma","mu","a","b","lp__"), include=FALSE)
postPred <- apply(Pred$y_new, 2, quantile, c(0.025,0.5,0.975)) # quantiles
PlottingData <- data.frame(lower=postPred[1,], upper=postPred[3,], #organise df
                           median=postPred[2,], Year=Rainfall$year)
ggplot(PlottingData) +
  geom_line(aes(x=Year,y=median),colour="red") +
  geom_line(aes(x=Year,y=lower),colour="red",lty="dashed") +
  geom_line(aes(x=Year,y=upper),colour="red",lty="dashed") +
  geom_point(data=Rainfall,aes(x = year, y=Rainfall))

# 99% bayesian prediction interval
quantile(Pred$y_new[,length(Pred$y_new[1,])],c(0.005,0.995))

