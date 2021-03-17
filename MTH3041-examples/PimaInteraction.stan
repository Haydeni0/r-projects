
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
  // For interaction terms
  int<lower=0> N_intr;
  int intr_idx[N_intr,2];
  vector[N_intr] beta_intr_mean;
  vector<lower=0>[N_intr] beta_intr_sd;
}
parameters {
  real alpha;
  vector[p] beta;
  vector[N_intr] beta_intr;
}
transformed parameters {
  vector[N] theta;
  vector[N] intr_terms;
  intr_terms = rep_vector(0, N);
  for (j in 1:N_intr){
      intr_terms = intr_terms + X[,intr_idx[j,1]].*X[,intr_idx[j,2]]*beta_intr[j];
  }
  theta = alpha + X*beta + intr_terms;
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
    //y_new[i] = bernoulli_logit_rng(theta_new[i]);
  }
}
