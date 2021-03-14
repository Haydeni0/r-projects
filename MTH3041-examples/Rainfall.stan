
data {
  int<lower=0> N;
  vector<lower=0>[N] y;
  vector[N] X;
  real alpha_mean;
  real alpha_sd;
  real beta_sd;
  real sigma_mean;
  real sigma_sd;
  // For generated quantities
  int<lower=0> N_new;
  vector[N_new] x_new;
}
parameters {
  real alpha;
  real beta;
  real<lower=0.1> sigma;
}
transformed parameters {
  vector<lower=0>[N] mu;
  vector[N] a;
  vector[N] b;
  mu = exp(alpha + beta*X);
  a = mu .* mu / sigma;
  b = mu / sigma;
}
model {
  alpha ~ normal(alpha_mean, alpha_sd);
  beta ~ normal(0, beta_sd);
  sigma ~ normal(sigma_mean, sigma_sd);
  y ~ gamma(a, b);
}
generated quantities{
  // Predict values from the model
  vector[N_new] y_new;
  vector[N_new] mu_new;
  vector[N_new] a_new;
  vector[N_new] b_new;
  for(n in 1:N_new){
    mu_new[n] = exp(alpha+beta*x_new[n]);
    a_new[n] = mu_new[n]*mu_new[n]/sigma;
    b_new[n] = mu_new[n]/sigma;
    y_new[n] = gamma_rng(a_new[n], b_new[n]);
  }
}
