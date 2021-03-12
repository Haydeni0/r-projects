data{
  int<lower=0> N; //number of observations
  int<lower=0> p; //number of regressors
  matrix[N, p] X; //regression matrix
  vector[N] y;

  int<lower=0> N_new; //validation points
  matrix[N_new, p] X_new;
  // Hyperparameters
  real a; //Prior mean of alpha
  real<lower=0> Sigma_a; //Prior variance of alpha
  vector[p] beta0; //vector of prior means for beta
  vector[p] Sigma_b; //vector of prior sigmas for beta
  real<lower=0> Sigma_s; // Prior variance of sigma
}
parameters{
  real alpha; //intercept
  vector[p] beta; //coefficients
  real<lower=0> sigma; //sd
}
model{
  alpha ~ normal(a, Sigma_a);
  beta[1:p] ~ normal(beta0[1:p],Sigma_b[1:p]); //Prior for beta
  sigma ~ normal(0,Sigma_s);
  y ~ normal(alpha+X*beta, sigma);
}
generated quantities{
  vector[N_new] y_new;
  for(n in 1:N_new)
    y_new[n] = normal_rng(alpha+X_new[n,]*beta, sigma);
}

