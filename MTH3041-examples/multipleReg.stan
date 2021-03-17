data{
  int<lower=0> N; // number of observations
  int<lower=0> p; // number of covariates
  matrix[N, p] X; // regression matrix
  vector[N] y;
  // Prior hyperparameters
  real alpha_mean; // Prior mean of alpha
  real<lower=0> alpha_sd; // Prior sd of alpha
  vector[p] beta_mean; // vector of prior means for beta
  vector[p] beta_sd; // vector of prior sds for beta
  real<lower=0> sigma_sd; // Prior sd of sigma
  // For generated quantities
  int<lower=0> N_new;
  matrix[N_new, p] X_new; // Covariates to use for predictions
}
parameters{
  real alpha; // Intercept
  vector[p] beta; // Coefficients
  real<lower=0> sigma; // Standard deviation of y
}
model{
  // Priors
  alpha ~ normal(alpha_mean, alpha_sd);
  beta[1:p] ~ normal(beta_mean[1:p],beta_sd[1:p]);
  sigma ~ normal(0,sigma_sd);
  // top layer
  y ~ normal(alpha+X*beta, sigma);
}
generated quantities{
  // Generate samples from the model to validate results
  vector[N_new] y_new;
  for(n in 1:N_new)
    y_new[n] = normal_rng(alpha+X_new[n,]*beta, sigma);
}
