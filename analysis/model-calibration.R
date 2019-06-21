# Import and Restructure Data ---------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
# library(mvtnorm)
# library(bayesm)

# Load data and design.
final_data <- read_csv(here::here("data", "218329_Final_Excel_050619.csv"))
survey_design <- read_csv(here::here("Data", "survey_design.csv")) %>% select(-X1)
dummy_design <- read_csv(here::here("Data", "dummy_design.csv")) %>% select(-X1)

intercept <- 0 # Intercept-only.
geo_locat <- 1 # Geolocation covariates.
demo_vars <- 0 # Demographic covariates.
geo_demos <- 0 # Geolocation and demographic covariates.
bnd_demos <- 0 # Brand covariates.
all_three <- 0 # Geolocation, demographic, and brand covariates.

# Restructure choice data Y.
Y <- final_data %>%
  select(record, contains("Q3")) %>%
  select(-c(record, Q3_Version)) %>%
  as.matrix()

# Recode price in the design X.
price_scale <- 10000
design <- dummy_design %>%
  select(-contains("price")) %>%
  left_join(
    survey_design %>%
      select(version, task, alt, price)
  ) %>%
  mutate(
    price = recode(
      price,
      `1` = 20000/price_scale,
      `2` = 25000/price_scale,
      `3` = 30000/price_scale,
      `4` = 35000/price_scale,
      `5` = 40000/price_scale,
      `6` = 45000/price_scale,
      `7` = 50000/price_scale,
      `8` = 55000/price_scale,
      `9` = 60000/price_scale,
      `10` = 65000/price_scale,
      `11` = 70000/price_scale,
      `12` = 75000/price_scale,
      `13` = 80000/price_scale,
      `14` = 85000/price_scale,
      `15` = 90000/price_scale,
      `16` = 95000/price_scale,
      `17` = 100000/price_scale
    )
  )

# Restructure the design X.
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
  X_n <- design %>% filter(version == final_data$Q3_Version[n])
  for (s in 1:dim(X)[2]) {
    # Filter for task s.
    X_s <- X_n %>%
      filter(task == s) %>%
      mutate(brand1 = 0) %>%
      select(brand1, brand2:price)
    for (p in 1:(dim(X)[3]-1)) {
      # Filter for task s and alt p brands.
      X_p <- X_n %>%
        filter(task == s, alt == p) %>%
        select(brand2:brand16) %>%
        as_vector()
      # Fill in brand1
      X_s[p, "brand1"] <- ifelse(sum(X_p) == 1, 0, 1)
    }
    # Save modified design, including outside option.
    X[n, s,,] <- rbind(X_s, rep(0, ncol(X_s))) %>% as.matrix()
  }
}

# Restructure covariates Z.
if (intercept == 1) Z <- matrix(data = 1, nrow = dim(X)[1], ncol = 1)
if (geo_locat == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Acura:Volkswagen)
        # mutate(
        #   dealer_visit = Acura	+ BMW	+ Chevrolet + Chrysler + Ferrari + `Ford Motor Company` +
        #     `GMC (General Motors Company)` + Honda + `Hyundai Motor` + Infiniti	+ `Kia Motors` +
        #     Lexus	+ Lincoln	+ Mazda	+ `Mercedes Benz`	+ `Mitsubishi Motors`	+ `Nissan North America` +
        #     Subaru + `Tesla Motors`	+ Toyota + Volkswagen
        # ) %>%
        # select(dealer_visit) %>%
        # mutate(dealer_visit = ifelse(dealer_visit >= 1, 1, 0))
    ) %>%
    as.matrix()
  Z <- ifelse(Z > 5, 1, Z)
}
if (demo_vars == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4)
    ) %>%
    as.matrix()
}
if (geo_demos == 1) {
  Z_geo <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Acura:Volkswagen)
        # mutate(
        #   dealer_visit = Acura	+ BMW	+ Chevrolet + Chrysler + Ferrari + `Ford Motor Company` +
        #     `GMC (General Motors Company)` + Honda + `Hyundai Motor` + Infiniti	+ `Kia Motors` +
        #     Lexus	+ Lincoln	+ Mazda	+ `Mercedes Benz`	+ `Mitsubishi Motors`	+ `Nissan North America` +
        #     Subaru + `Tesla Motors`	+ Toyota + Volkswagen
        # ) %>%
        # select(dealer_visit) %>%
        # mutate(dealer_visit = ifelse(dealer_visit >= 1, 1, 0))
    ) %>%
    as.matrix()
  Z_geo <- ifelse(Z_geo > 5, 1, Z_geo)
  Z_demo <- final_data %>%
    select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4) %>%
    as.matrix()
  Z <- cbind(Z_geo, Z_demo)
}
if (bnd_demos == 1) {
  Z_bnd <- final_data %>%
    select(contains("Q1x2"), contains("Q2x3"), Q2x1, Q2x2, Q2x8) %>%
    as.matrix()
  Z_demo <- final_data %>%
    select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4) %>%
    as.matrix()
  Z <- cbind(Z_bnd, Z_demo)
}
if (all_three == 1) {
  Z_geo <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Acura:Volkswagen)
      # mutate(
      #   dealer_visit = Acura	+ BMW	+ Chevrolet + Chrysler + Ferrari + `Ford Motor Company` +
      #     `GMC (General Motors Company)` + Honda + `Hyundai Motor` + Infiniti	+ `Kia Motors` +
      #     Lexus	+ Lincoln	+ Mazda	+ `Mercedes Benz`	+ `Mitsubishi Motors`	+ `Nissan North America` +
      #     Subaru + `Tesla Motors`	+ Toyota + Volkswagen
      # ) %>%
      # select(dealer_visit) %>%
      # mutate(dealer_visit = ifelse(dealer_visit >= 1, 1, 0))
    ) %>%
    as.matrix()
  Z_geo <- ifelse(Z_geo > 5, 1, Z_geo)
  Z_demo <- final_data %>%
    select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4) %>%
    as.matrix()
  Z_bnd <- final_data %>%
    select(contains("Q1x2"), contains("Q2x3"), Q2x1, Q2x2, Q2x8) %>%
    as.matrix()
  Z <- cbind(Z_geo, Z_demo, Z_bnd)
}

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

  Theta_mean = 0,          # Mean of coefficients for the heterogeneity model.
  Theta_scale = 10,        # Scale of coefficients for the heterogeneity model.
  tau_scale = 2.5,         # Variation for scale parameters in the heterogeneity model.
  Omega_shape = 2,         # Shape of correlation matrix for the heterogeneity model.

  Y = Y,                   # Matrix of observed choices.
  X = X,                   # Array of experimental designs per choice task.
  Z = Z                    # Matrix of respondent-level covariates.
)

# test <- diag(data$tau_scale, data$L) %*%
#   rethinking::rlkjcorr(1, K = data$L, eta = data$Omega_shape) %*%
#   # matrix(data = data$Omega_shape, nrow = data$L, ncol = data$L) %*%
#   diag(data$tau_scale, data$L)
#
# matrixcalc::is.positive.definite(test)

# Calibrate the model.
fit <- stan(
  file = here::here("Code", "hmnl_centered.stan"),
  data = data,
  seed = 42
)

# # Save model output.
# write_rds(fit, here::here("Output", "hmnl_intercept.RDS"))

# MCMC --------------------------------------------------------------------
# Load estimation routine.
source(here::here("Code", "hier_mnl.R"))

nhold <- round(dim(X)[1]*.10) # Number of hold-out respondents.
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
if (intercept == 1) write_rds(run, here::here("Output", "hmnl_intercept-100k_ho.RDS"))
if (geo_locat == 1) write_rds(run, here::here("Output", "hmnl_geo-locat-100k_ho.RDS"))
if (demo_vars == 1) write_rds(run, here::here("Output", "hmnl_demo-vars-100k_ho.RDS"))
if (geo_demos == 1) write_rds(run, here::here("Output", "hmnl_geo-demos-100k_ho.RDS"))
if (bnd_demos == 1) write_rds(run, here::here("Output", "hmnl_bnd-demos-100k_ho.RDS"))
if (all_three == 1) write_rds(run, here::here("Output", "hmnl_all-three-100k_ho.RDS"))

