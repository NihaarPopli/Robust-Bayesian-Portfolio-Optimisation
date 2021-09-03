data {
  int S;// number of observations
  int N; // number of assets
  real<lower=0> nu0; 
  real<lower=0> T0; 
  vector[N] mu0; 
  matrix[N,N] sigma0;
  vector[N] y[S];
}

parameters {
  cov_matrix[N] sigma;
  vector[N] mu; 
}

model {
  y ~ multi_normal (mu, sigma) ; //likelihood
  mu ~ multi_normal(mu0, sigma/T0); //prior
  sigma ~ inv_wishart(nu0, nu0*sigma0); //prior
}
