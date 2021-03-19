library(tidyverse)
library(bayesplot)
library(reshape2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load("murders.RData")

murders$Population <- log(murders$Population)

MurdersData <- melt(murders, id.vars = 'MurdersPerYearPerMillion')
ggplot(MurdersData) +
  geom_point(aes(x = value, y = MurdersPerYearPerMillion, colour = variable)) +
  geom_smooth(aes(x = value, y = MurdersPerYearPerMillion, colour = variable),
              method = lm) +
  facet_wrap( ~ variable, scales = "free_x")

# data{
#   int<lower=0> N; // number of observations
#   int<lower=0> p; // number of covariates
#   matrix[N, p] X; // regression matrix
#   vector[N] y;
#   // Prior hyperparameters
#   real alpha_mean; // Prior mean of alpha
#   real<lower=0> alpha_sd; // Prior sd of alpha
#   vector[p] beta_mean; // vector of prior means for beta
#   vector[p] beta_sd; // vector of prior sds for beta
#   real<lower=0> sigma_sd; // Prior sd of sigma
#   // For generated quantities
#   int<lower=0> N_new;
#   matrix[N_new, p] X_new; // Covariates to use for predictions
# }

stan_list <- list(
  N = nrow(murders),
  p = 3,
  X = as.matrix(murders[, 1:3]),
  y = murders$MurdersPerYearPerMillion,
  alpha_mean = 20,
  alpha_sd = 1000,
  beta_mean = c(0, 1, 1),
  beta_sd = c(10, 10, 10),
  sigma_sd = 20,
  N_new = nrow(murders),
  X_new = as.matrix(murders[, 1:3])
)

glmodel1 <-
  glm(MurdersPerYearPerMillion ~ PercentUnemployed + PercentLowIncome + Population,
      data = murders, family = gaussian())
glmodel2 <-
  glm(MurdersPerYearPerMillion ~ PercentUnemployed + PercentLowIncome,
      data = murders, family = gaussian())
anova(glmodel1,glmodel2, test="F") # Pr(>F) = 0.248, so there is no significant difference in fit

summary(glmodel2)

bmodel <-
  stan(
    file = "multipleReg.stan",
    data = stan_list,
    chains = 4,
    iter = 4000
  )

summary(bmodel)$summary[1:5,]

mcmc_trace(bmodel, pars = c("alpha", "beta[1]", "beta[2]", "beta[3]", "sigma"))


pred <- extract(bmodel,pars=c("alpha","beta","sigma","lp__"),include=FALSE)
pred.values <- apply(pred$y_new, 2, quantile, c(0.05,0.5,0.95))
PlottingData <- data.frame(lower=pred.values[1,], upper=pred.values[3,],
                           median=pred.values[2,])
PlottingData <- cbind(PlottingData,murders)
PlottingData2 <- melt(PlottingData, id.vars=c('median','lower','upper','MurdersPerYearPerMillion'))
ggplot(PlottingData2) +
  geom_point(aes(x=value, y=median,colour=variable)) +
  geom_errorbar(aes(x=value, ymin=lower, ymax=upper, colour=variable)) +
  geom_point(aes(x=value, y=MurdersPerYearPerMillion), colour="blue") +
  facet_wrap(~variable, scales="free_x") +
  ylab("MurdersPerYearPerMillion")

