library(tidyverse)
library(bayesplot)
library(reshape2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


library(MASS)
data("Pima.te")
data("Pima.tr")
# data("Pima.tr2")
# Pregnancies: Number of times pregnant
# Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test
# BloodPressure: Diastolic blood pressure (mm Hg)
# SkinThickness: Triceps skin fold thickness (mm)
# Insulin: 2-Hour serum insulin (mu U/ml)
# BMI: Body mass index (weight in kg/(height in m)^2)
# DiabetesPedigreeFunction: Diabetes pedigree function
# Age: Age (years)
# Outcome: Class variable (0 or 1)

Pima <- as_tibble(rbind(Pima.te, Pima.tr))
Pima$outcome <- Pima$type == "Yes"

# data {
#   int<lower=0> N;
#   int<lower=0> p; // Number of covariates
#   int y[N]; // Must be of datatype int to be an output of bernoulli_logit()
#   matrix[N, p] X; // Regression matrix
#   // Prior hyperparameters
#   real alpha_mean;
#   real<lower=0> alpha_sd;
#   vector[p] beta_mean;
#   vector<lower=0>[p] beta_sd;
#   // For generated quantities
#   int N_new;
#   matrix[N_new,p] X_new;
# }

use_row_idx = c(1,2,3,6)

# Some interactions, preg:bld, glucose:dpf
intr_idx = matrix(data = c(1,3,2,4), ncol = 2)
num_intr = nrow(intr_idx)
beta_sd = c(10, 30, 30, 5)

beta_intr_sd = array(data = NA, dim = num_intr)
for (j in 1:num_intr) {
  beta_intr_sd[j] = beta_sd[intr_idx[j,1]]*beta_sd[intr_idx[j,2]]
}

stan_list = list(
  N = nrow(Pima),
  p = 4,
  y = Pima$outcome,
  X = Pima[, use_row_idx],
  
  alpha_mean = 0.5,
  alpha_sd = 0.5,
  beta_mean = c(5, 100, 70, 0.8),
  beta_sd = beta_sd,
  
  N_new = nrow(Pima),
  X_new = Pima[, use_row_idx],
  
  N_intr = num_intr,
  intr_idx = intr_idx,
  beta_intr_mean = rep(0,num_intr),
  beta_intr_sd = beta_intr_sd
)

bmodel <-
  stan(
    file = "PimaInteraction.stan",
    data = stan_list,
    chains = 4,
    iter = 4000
  )

summary(bmodel)$summary[1:8,]

mcmc_trace(bmodel,pars = c("alpha", "beta[1]", "theta[1]", "lp__"))
mcmc_hist(bmodel,pars = c("alpha", "beta[1]", "theta[1]", "lp__"))
pairs(bmodel, pars = c("alpha", "beta[1]", "theta[1]", "lp__"))

pred <-
  extract(bmodel,
          pars = c("alpha", "beta", "theta", "lp__"),
          include = FALSE)

outcome_predicted <- apply(pred$theta_new > 0, 2, quantile, 0.5)

correctly_predicted <- Pima$outcome == outcome_predicted

# Proportion of correctly predicted results. 74% without using any interactions
sum(correctly_predicted)/nrow(Pima)
