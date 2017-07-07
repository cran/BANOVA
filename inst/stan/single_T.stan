data {
  int<lower=0> N;
  int<lower=0> J;
  matrix[N, J] X;
  vector[N] y;
}

parameters {
  vector[J] beta1;
  real<lower=0> tau_ySq;
  real<lower=1, upper=10> df;
} 

model {
  vector[N] y_hat;
  real tau_y;
  tau_y = sqrt(tau_ySq);
  y_hat = X*beta1;
  y ~ student_t(df, y_hat, tau_y);
  df ~ normal(5,5);
  tau_ySq ~ inv_gamma(1, 1);
  for (i in 1:J){
    beta1[i] ~ normal(0, 1);
  }
}


