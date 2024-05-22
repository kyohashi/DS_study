data{
  int N; // purchase record size
  int D; // num of covariates
  int K; // brand size
  matrix[N, D] P1; // design matrix
  matrix[N, D] P2; // design matrix
  matrix[N, D] P3; // design matrix
  matrix[N, D] P4; // design matrix
  matrix[N, D] P5; // design matrix
  matrix[N, D] P6; // design matrix
  matrix[N, D] P7; // design matrix
  matrix[N, D] P8; // design matrix
  matrix[N, D] P9; // design matrix
  matrix[N, D] P10; // design matrix
  array[N] int<lower=1, upper=K> Y; // brand choice logs
}

parameters {
  matrix[D, K] beta_raw; // regression coefficients
}

transformed parameters {
  matrix[D, K] beta;
  matrix[N, K] utility;
  
  beta = beta_raw;
  beta[1,1] = 0;
  utility[,1] = P1 * beta[,1];
  utility[,2] = P2 * beta[,2];
  utility[,3] = P3 * beta[,3];
  utility[,4] = P1 * beta[,4];
  utility[,5] = P2 * beta[,5];
  utility[,6] = P3 * beta[,6];
  utility[,7] = P1 * beta[,7];
  utility[,8] = P2 * beta[,8];
  utility[,9] = P3 * beta[,9];
  utility[,10] = P3 * beta[,10];
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
