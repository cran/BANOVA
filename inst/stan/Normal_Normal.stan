data {
  int<lower=0> N;
  int<lower=0> J;
  int<lower=0> M;
  int<lower=0> K;
  matrix[N, J] X;
  matrix[M, K] Z;
  int<lower=0> id[N];
  vector[N] y;
}

parameters {
  matrix[J, M] beta1;
  matrix[K, J] beta2; 
  real<lower=0> tau_ySq;
  vector<lower=0>[J] tau_beta1Sq;
} 

model {
  vector[N] y_hat;
  matrix[M, J] mu_beta1;
  real tau_y;
  vector[J] tau_beta1;
  tau_y = sqrt(tau_ySq);
  tau_beta1 = sqrt(tau_beta1Sq);
  for (i in 1:N){
    y_hat[i] = X[i,]*beta1[,id[i]];
  }
  y ~ normal(y_hat, tau_y);
  tau_ySq ~ inv_gamma(1, 1);
  mu_beta1 = Z*beta2;
  for (i in 1:J){
    beta1[i,] ~ normal(mu_beta1[,i], tau_beta1[i]);
  }
  tau_beta1Sq ~ inv_gamma(1, 1);
  for (i in 1:J){
    beta2[,i] ~ normal(0, 100);
  }
}

