data{
  int N; // purchase record size
  int D; // num of covariates
  int K; // brand size
  matrix[N, D] X; // design matrix
  array[N] int<lower=1, upper=K> Y; // brand choice logs
}

transformed data{
  vector[D] Zeros;
  Zeros = rep_vector(0, D);
}

parameters {
  matrix[D, K-1] beta_raw; // regression coefficients
}

transformed parameters {
  matrix[D, K] beta;
  matrix[N, K] utility;
  
  beta = append_col(Zeros, beta_raw);
  utility = X * beta;
}

model {
  for (n in 1:N)
    Y[n] ~ categorical_logit(utility[n,]');
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N)
    log_lik[n] = categorical_logit_lpmf(Y[n] | utility[n,]');
}
