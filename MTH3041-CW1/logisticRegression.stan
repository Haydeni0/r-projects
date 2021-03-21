
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
  // For out of sample prediction
  int N_test;
  matrix[N_test,p] X_test;
}
/*
stan_list = list(
  N = ,
  p = ,
  y = ,
  X = ,
  
  alpha_mean = ,
  alpha_sd = ,
  beta_mean = ,
  beta_sd = ,
  
  N_test = ,
  X_test = 
)
*/
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
  vector[N_test] theta_test;
  vector[N] theta_training;
  for(i in 1:N_test){
    theta_test[i] = alpha + X_test[i,]*beta; // Out of sample predictions
    // y_test[i] = bernoulli_logit_rng(alpha + X_test[i,]*beta);
  }
  for(i in 1:N){
    theta_training[i] = alpha + X[i,]*beta; // In sample predictions
  }
}
