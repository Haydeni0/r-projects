
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Amount data
  int<lower=0> p; // Number of covariates
  vector<lower=0>[N] y; // Uncensored days
  matrix[N,p] X; // regression matrix
  // Hyperparameters for the prior
  real alpha_mean;
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  real sigma_mean;
  real<lower=0> sigma_sd;
}
parameters {
  real alpha;
  vector[p] beta;
  real<lower=0> sigma;
}
model {
  // Priors
  // alpha ~ normal(alpha_mean,alpha_sd);
  beta[1:p] ~ normal(0,beta_sd);
  sigma ~ normal(sigma_mean,sigma_sd);
  // Multiple regression model with truncation
  for (n in 1:N)
    y[n] ~ normal(alpha + X[n,]*beta, sigma)T[0,364]; // Truncation to remove censored values
}
