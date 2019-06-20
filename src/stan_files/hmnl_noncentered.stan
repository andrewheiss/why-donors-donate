// Non-centered parameterization of a hierarchical multinomial logit with a 
// multivariate normal model of heterogeneity. This is a standard choice 
// model for modeling conjoint experiment data.

// Data values, hyperparameters, observed choices, and the experimental design.
data {
  int<lower = 1> N;                  // Number of respondents.
  int<lower = 1> S;                  // Number of choice tasks per respondent.
  int<lower = 2> P;                  // Number of product alternatives per choice task.
  int<lower = 1> L;                  // Number of (estimable) attribute levels.
  int<lower = 1> C;                  // Number of respondent-level covariates.
  
  real Theta_mean;                   // Mean of coefficients for the heterogeneity model.
  real<lower=0> Theta_scale;         // Scale of coefficients for the heterogeneity model.
  real alpha_mean;                   // Mean of scale for the heterogeneity model.
  real<lower=0> alpha_scale;         // Scale of scale for the heterogeneity model.
  real<lower=0> lkj_corr_shape;      // Shape of correlation matrix for the heterogeneity model.
  
  int<lower = 1, upper = P> Y[N, S]; // Matrix of observed choices.
  matrix[P, L] X[N, S];              // Array of experimental designs per choice task.
  matrix[N, C] Z;                    // Matrix of respondent-level covariates.
}

// Parameters for the hierarchical multinomial logit.
parameters {
  matrix[L, C] Theta;                        // Matrix of coefficients for the heterogeneity model.
  vector<lower=0, upper=pi()/2>[L] tau_unif; // Initialized parameter value for a Cauchy draw.
  cholesky_factor_corr[L] L_Omega;           // Cholesky factorization for heterogeneity covariance.
  matrix[L, N] alpha;                        // Standard deviations for heterogeneity covariance.
}

// Deterministically transformed parameter values.
transformed parameters {
  matrix[N, L] Beta;                              // Matrix of Beta coefficients.
  vector<lower=0>[L] tau;                         // Scale for heterogeneity covariance.
  for (l in 1:L) tau[l] = 2.5 * tan(tau_unif[l]); // Inverse probability Cauchy draw.
  
  // Draw of Beta following non-centered parameterization.
  Beta = Z * Theta' + (diag_pre_multiply(tau,L_Omega) * alpha)';
}

// Hierarchical multinomial logit model.
model {
  // Hyperpriors on Theta, alpha, and L_Omega (and thus Sigma).
  to_vector(Theta) ~ normal(Theta_mean, Theta_scale);
  to_vector(alpha) ~ normal(alpha_mean, alpha_scale);
  L_Omega ~ lkj_corr_cholesky(lkj_corr_shape);

  // Hierarchical multinomial logit.
  for (n in 1:N) {
    for (s in 1:S) {
      Y[n, s] ~ categorical_logit(X[n, s] * Beta[n,]');
    }
  }
}

// generated quantities {
//   // Yp is predicted choices for new data.
//   real Y_ppc[N, S];
//   vector[N*S] log_lik;
//   {
//     matrix[N, S] temp_log_lik;
//     for (r in 1:N) {
//       for (t in 1:S) {
//         Y_ppc[r, t] = categorical_logit_rng(X[r, t] * Beta[r]');
//         temp_log_lik[r, t] = categorical_logit_lpmf(Y[r, t] | X[r, t] * Beta[r]');
//       }
//     }
//     log_lik = to_vector(temp_log_lik);
//   }
// }
