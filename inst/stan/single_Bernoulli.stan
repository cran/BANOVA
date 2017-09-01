data {
  int<lower=0> N;
  int<lower=0> J;
  matrix[N, J] X;
  int y[N];
}

parameters {
  vector[J] beta1;
} 

model {
  vector[N] y_hat;
  y_hat = X*beta1;
  y ~ bernoulli_logit(y_hat);
  for (i in 1:J){
    beta1[i] ~ normal(0, 1);
  }
}
