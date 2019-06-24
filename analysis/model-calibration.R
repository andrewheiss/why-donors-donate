# Import and Restructure Data ---------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
library(mvtnorm)
library(bayesm)

# Load data and design.
final_data <- read_csv(here::here("data", "derived_data", "final_data.csv"))
design <- read_csv(here::here("data", "derived_data", "dummy_design.csv")) %>%
  select(-X1) %>%
  rename(
    version = question_set,
    task = question,
    alt = choice
  )

# Specify the model to load.
intercept <- 1 # Intercept-only.

# Restructure choice data Y.
Y <- final_data %>%
  select(contains("Q4")) %>%
  as.matrix()

# Create the array for design X.
X <- array(
  data = NA,
  dim = c(
    nrow(Y),                # Number of respondents.
    max(design$task),       # Number of choice tasks per respondent.
    (max(design$alt) + 1),  # Number of product alternatives per choice task.
    (ncol(design) - 3 + 1)  # Number of (estimable) attribute levels.
  )
)

# Add the outside option coded as all zeros.
for (n in 1:dim(X)[1]) {
  # Filter for respondent n.
  X_n <- design %>% filter(version == final_data$version[n])
  for (s in 1:dim(X)[2]) {
    # Filter for task s.
    X_s <- X_n %>%
      filter(task == s) %>%
      mutate(org1 = 0) %>%
      select(org1, org2:gov_rel3)
    for (p in 1:(dim(X)[3]-1)) {
      # Filter for task s and alt p orgs.
      X_p <- X_n %>%
        filter(task == s, alt == p) %>%
        select(org2:org4) %>%
        as_vector()
      # Fill in org1.
      X_s[p, "org1"] <- ifelse(sum(X_p) == 1, 0, 1)
    }
    # Save modified design, including outside option.
    X[n, s,,] <- rbind(X_s, rep(0, ncol(X_s))) %>% as.matrix()
  }
}

# Restructure covariates Z.
if (intercept == 1) Z <- matrix(data = 1, nrow = dim(X)[1], ncol = 1)

# HMC ---------------------------------------------------------------------
# Set Stan options.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Specify the data for calibration in a list.
data <- list(
  N = dim(X)[1],           # Number of respondents.
  S = dim(X)[2],           # Number of choice tasks per respondent.
  P = dim(X)[3],           # Number of product alternatives per choice task.
  L = dim(X)[4],           # Number of (estimable) attribute levels.
  C = ncol(Z),             # Number of respondent-level covariates.

  # Theta_mean = 0,          # Mean of coefficients for the heterogeneity model.
  # Theta_scale = 10,        # Scale of coefficients for the heterogeneity model.
  # tau_scale = 2.5,         # Variation for scale parameters in the heterogeneity model.
  # Omega_shape = 2,         # Shape of correlation matrix for the heterogeneity model.

  Theta_mean = 0,          # Mean of coefficients for the heterogeneity model.
  Theta_scale = 1,         # Scale of coefficients for the heterogeneity model.
  alpha_mean = 0,          # Mean of scale for the heterogeneity model.
  alpha_scale = 10,        # Scale of scale for the heterogeneity model.
  lkj_corr_shape = 5,      # Shape of correlation matrix for the heterogeneity model.

  Y = Y,                   # Matrix of observed choices.
  X = X,                   # Array of experimental designs per choice task.
  Z = Z                    # Matrix of respondent-level covariates.
)

# Run the model.
fit <- stan(
  # file = here::here("src", "stan_files", "hmnl_centered.stan"),
  file = here::here("src", "stan_files", "hmnl_noncentered.stan"),
  data = data,
  seed = 42
)

# Save data and model output.
run <- list(data = data, fit = fit)
if (intercept == 1) write_rds(run, here::here("analysis", "output", "model_runs", "intercept_noncentered.rds"))

# Centered: 17990.1 seconds (Total)
# Noncentered: 5634.98 seconds (Total)

# MCMC --------------------------------------------------------------------
# Load estimation routine.
source(here::here("R", "hier_mnl.R"))

# nhold <- round(dim(X)[1]*.10) # Number of hold-out respondents.
nhold <- 0                    # Number of hold-out respondents.
nresp <- dim(X)[1] - nhold    # Number of respondents.
nscns <- dim(X)[2]            # Number of choice tasks per respondent.
nalts <- dim(X)[3]            # Number of product alternatives per choice task.
nvars <- dim(X)[4]            # Number of (estimable) attribute levels.
ncovs <- ncol(Z)              # Number of respondent-level covariates.

Y_new <- vector(mode = "list", length = nresp + nhold)
X_new <- vector(mode = "list", length = nresp + nhold)
for (resp in 1:(nresp + nhold)) {
  Y_new[[resp]] <- matrix(Y[resp, ])
  for(scns in 1:nscns) {
    X_new[[resp]] <- rbind(X_new[[resp]], X[resp, scns,,])
  }
}

# Specify the hold-out sample.
ho_ind <- matrix(0, nrow = (nresp + nhold), ncol = 1)
set.seed(42); ho_ind[sample(nresp + nhold, nhold), ] <- 1

# Estimate the model.
Data <- list(
  y = Y_new[which(ho_ind != 1)],
  X = X_new[which(ho_ind != 1)],
  Z = matrix(Z[which(ho_ind != 1),], ncol = ncol(Z)),
  ho_ind = ho_ind,
  y_ho = Y_new[which(ho_ind == 1)],
  X_ho = X_new[which(ho_ind == 1)],
  Z_ho = matrix(Z[which(ho_ind == 1),], ncol = ncol(Z))
)
Prior <- list(
  gammabar = matrix(rep(0, ncovs * nvars), ncol = nvars),
  Agamma = 0.01 * diag(ncovs),
  nu = nvars + 3,
  V = (nvars + 3) * diag(nvars)
)
Mcmc <- list(
  R = 100000,
  keep = 100,
  step = .08,
  sim_ind = 0,
  cont_ind = 0
)

fit <- hier_mnl(Data, Prior, Mcmc)

# Save data and model output.
run <- list(Data = Data, Prior = Prior, Mcmc = Mcmc, fit = fit)
if (intercept == 1) write_rds(run, here::here("analysis", "output", "model_runs", "intercept_conjugate.rds"))

# Conjugate: Total Time Elapsed (in Hours):  14.89

