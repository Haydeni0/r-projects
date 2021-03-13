
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Amount of data
  int<lower=0> Ncens; // amount of uncensored data
  int<lower=0> p; // Number of covariates
  vector<lower=0>[N] y; // Uncensored days
  matrix[N,p] X; // regression matrix
  matrix[Ncens,p] Xcens; // regression matrix for censored data
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
  vector<lower=365>[Ncens] ycens; // Unknown values for censored data. >= 365
}
model {
  // Priors
  // alpha ~ normal(alpha_mean,alpha_sd);
  beta[1:p] ~ normal(0,beta_sd);
  sigma ~ normal(sigma_mean,sigma_sd);
  // Multiple regression model with truncation
  for (n in 1:N)
    y[n] ~ normal(alpha + X[n,]*beta, sigma)T[0,]; // Truncation to remove censored values
  for (n in 1:Ncens)
    ycens[n] ~ normal(alpha + Xcens[n,]*beta, sigma);
}
