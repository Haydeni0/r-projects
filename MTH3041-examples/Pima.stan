
data {
  int<lower=0> N;
  int<lower=0> p; // Number of covariates
  int y[N]; // Must be of datatype int to be an output of bernoulli_logit()
  matrix[N, p] X; // Regression matrix
  // Prior hyperparameters
  real alpha_mean;
  real<lower=0> alpha_sd;
  vector[p] beta_mean;
  vector<lower=0>[p] beta_sd;
  // For generated quantities
  int N_new;
  matrix[N_new,p] X_new;
}
parameters {
  real alpha;
  vector[p] beta;
}
transformed parameters {
  vector[N] theta;
  theta = alpha + X*beta;
}
model {
  alpha ~ normal(alpha_mean,alpha_sd);
  beta[1:p] ~ normal(beta_mean[1:p], beta_sd[1:p]);
  y ~ bernoulli_logit(theta);
}
generated quantities{
  vector[N_new] theta_new;
  for(i in 1:N_new){
    theta_new[i] = alpha + X_new[i,]*beta;
    // y_new[i] = bernoulli_logit_rng(alpha + X_new[i,]*beta);
  }
}
