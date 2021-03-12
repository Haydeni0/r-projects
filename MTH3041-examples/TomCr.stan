data{
  int<lower=0> N;
  vector[N] x;
  vector[N] y;

  int<lower=0> N_new;
  vector[N_new] x_new;
  // Hyperparameters
  real b; // Prior mean of betas
  real<lower=0> Sigma_b; //Prior variance of betas
  real<lower=0> Sigma_s; // Prior variance of sigma
}
parameters{
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model{
  //alpha ~ normal(0,100); //Prior for alpha. Commented out gives a uniform prior.
  beta ~ normal(b,Sigma_b); //Prior for beta
  sigma ~ normal(0,Sigma_s);
  y ~ normal(alpha+beta*x, sigma);
}
generated quantities{
  vector[N_new] y_new;
  for(n in 1:N_new)
    y_new[n] = normal_rng(alpha+beta*x_new[n], sigma);
}
