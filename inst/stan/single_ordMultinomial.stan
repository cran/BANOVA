data {
  int<lower=3> cat;
  int<lower=0> N;
  int<lower=0> J;
  matrix[N, J] X;
  int<lower=1, upper=cat> y[N];
}

parameters {
  vector[J] beta1;
  ordered[cat-1] c;  
}
transformed parameters {
  ordered[cat-1] c_trans;
  for (i in 1:(cat-1))
    c_trans[i] = c[i] - c[1];
}

model {
  vector[N] y_hat;
  y_hat = X*beta1;
  for (i in 1:N){
      y[i] ~ ordered_logistic(y_hat[i], c_trans);
  }
  c ~ normal(0, 10);
  for (i in 1:J){
    beta1[i] ~ normal(0, 10);
  }
}