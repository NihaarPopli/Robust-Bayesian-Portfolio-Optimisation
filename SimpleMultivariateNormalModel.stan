data {
  int S;// number of observations
  int N; // number of assets
  vector[N] mu0; 
  matrix[N,N] sigma;
  matrix[N,N] sigma0;
  vector[N] y[S];
}

parameters {
  vector[N] mu; 
}

model {
  y ~ multi_normal (mu, sigma) ; //likelihood
  mu ~ multi_normal(mu0, sigma0); //prior
}
