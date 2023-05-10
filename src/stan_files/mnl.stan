// Index values, observations, and covariates.
data {
  int<lower = 1> N;                  // Number of observations.
  int<lower = 2> A;                  // Number of choice alternatives.
  int<lower = 1> L;                  // Number of (estimable) attribute levels.

  int<lower = 1, upper = A> Y[N];    // Vector of observations.
  matrix[A, L] X[N];                 // Matrix of observation-level covariates.
}

// Parameters for the multinomial logit.
parameters {
  vector[L] beta;                    // Vector of aggregate beta coefficients.
}

// Multinomial logit model.
model {
  // Standard normal prior for beta.
  beta ~ normal(0, 5);

  // Multinomial logit.
  for (n in 1:N) {
    Y[n] ~ categorical_logit(X[n] * beta);
  }
}

// Quantities conditioned on parameter draws.
generated quantities {
  // vector[N] Y_new;   // Vector of predicted choices.
  vector[N] log_lik; // Log likelihood to estimate loo.

  // Multinomial logit draws and log likelihood per observation.
  for (n in 1:N) {
    // Y_new[n] = categorical_logit_rng(X[n] * beta);
    log_lik[n] = categorical_logit_lpmf(Y[n] | X[n] * beta);
  }
}
