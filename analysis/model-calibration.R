# Import and Restructure Data ---------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
library(mvtnorm)
library(bayesm)

# Load data and design.
final_data <- read_rds(here::here("data", "derived_data", "final_data.rds"))
design <- read_csv(here::here("data", "derived_data", "dummy_design.csv")) %>%
  select(-X1) %>%
  rename(
    version = question_set,
    task = question,
    alt = choice
  )

intercept <- 0                            # Intercept-only.
public_affairs <- 0                       # Public affairs: Public affairs knowledge + Public affairs activity.
political_ideology <- 0                   # Political ideology.
social_views <- 0                         # Social views: Public affairs trust + Social ideology + Religiosity.
charity_voluntarism <- 0                  # Charity and voluntarism: Charity trust, Charity activity, Volunteer activity, etc.
demographics <- 0                         # Demographics: Gender, Marital status, Education, Income, Race, Age
public_political <- 0                     # Public affairs + Political ideology
public_political_social <- 0              # Public affairs + Political ideology + Social views
public_political_social_charity <- 0      # Public affairs + Political ideology + Social views + Charity and voluntarism
public_political_social_charity_demo <- 1 # Public affairs + Political ideology + Social views + Charity and voluntarism + Demographics

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
if (public_affairs == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate_at(vars(contains("Q2.3")), coalesce, 0) %>%
        mutate(
          Q5.7_2 = if_else(Q5.7 == 2, 1, 0)
        ) %>%
        select(Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, contains("Q5.7_"))
    ) %>%
    as.matrix()
}
if (political_ideology == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Q5.2)
    ) %>%
    as.matrix()
}
if (social_views == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Q5.6, Q5.11, Q5.8, Q5.9, Q5.10) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate(
          Q5.10_2 = if_else(Q5.10 == 2, 1, 0),
          Q5.10_3 = if_else(Q5.10 == 3, 1, 0),
          Q5.10_4 = if_else(Q5.10 == 4, 1, 0),
          Q5.10_5 = if_else(Q5.10 == 5, 1, 0),
          Q5.10_6 = if_else(Q5.10 == 6, 1, 0),
          Q5.10_7 = if_else(Q5.10 == 7, 1, 0),
          Q5.10_8 = if_else(Q5.10 == 8, 1, 0),
          Q5.10_9 = if_else(Q5.10 == 9, 1, 0),
          Q5.10_10 = if_else(Q5.10 == 10, 1, 0)
        ) %>%
        select(Q5.6, Q5.11, Q5.8, Q5.9, contains("Q5.10_"))
    ) %>%
    as.matrix()
}
if (charity_voluntarism == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Q2.7, Q2.8, Q2.5, Q2.6, Q2.9, Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate(
          Q2.9_2 = if_else(Q2.9 == 2, 1, 0)
        ) %>%
        select(Q2.7, Q2.8, Q2.5, Q2.6, contains("Q2.9_"), Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10)
    ) %>%
    as.matrix()
}
if (demographics == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(Q5.12, Q5.13, Q5.14, Q5.15, Q5.16_1:Q5.16_6, Q5.17) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate_at(vars(contains("Q5.16")), coalesce, 0) %>%
        mutate(
          Q5.12_2 = if_else(Q5.12 == 2, 1, 0),
          Q5.12_3 = if_else(Q5.12 == 3, 1, 0),
          Q5.12_4 = if_else(Q5.12 == 4, 1, 0),
          Q5.12_5 = if_else(Q5.12 == 5, 1, 0),
          Q5.13_2 = if_else(Q5.13 == 2, 1, 0),
          Q5.13_3 = if_else(Q5.13 == 3, 1, 0),
          Q5.13_4 = if_else(Q5.13 == 4, 1, 0),
          Q5.13_5 = if_else(Q5.13 == 5, 1, 0)
        ) %>%
        select(contains("Q5.12_"), contains("Q5.13_"), Q5.14, Q5.15, Q5.16_1:Q5.16_6, Q5.17)
    ) %>%
    as.matrix()
}
if (public_political == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7, # Public affairs
          Q5.2                                   # Political ideology
        ) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate_at(vars(contains("Q2.3")), coalesce, 0) %>%
        mutate(
          Q5.7_2 = if_else(Q5.7 == 2, 1, 0)
        ) %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, contains("Q5.7_"), # Public affairs
          Q5.2                                                # Political ideology
        )
    ) %>%
    as.matrix()
}
if (public_political_social == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7, # Public affairs
          Q5.2,                                  # Political ideology
          Q5.6, Q5.11, Q5.8, Q5.9, Q5.10         # Social views
        ) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate_at(vars(contains("Q2.3")), coalesce, 0) %>%
        mutate(
          Q5.7_2 = if_else(Q5.7 == 2, 1, 0),
          Q5.10_2 = if_else(Q5.10 == 2, 1, 0),
          Q5.10_3 = if_else(Q5.10 == 3, 1, 0),
          Q5.10_4 = if_else(Q5.10 == 4, 1, 0),
          Q5.10_5 = if_else(Q5.10 == 5, 1, 0),
          Q5.10_6 = if_else(Q5.10 == 6, 1, 0),
          Q5.10_7 = if_else(Q5.10 == 7, 1, 0),
          Q5.10_8 = if_else(Q5.10 == 8, 1, 0),
          Q5.10_9 = if_else(Q5.10 == 9, 1, 0),
          Q5.10_10 = if_else(Q5.10 == 10, 1, 0)
        ) %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, contains("Q5.7_"), # Public affairs
          Q5.2,                                               # Political ideology
          Q5.6, Q5.11, Q5.8, Q5.9, contains("Q5.10_")         # Social views
        )
    ) %>%
    as.matrix()
}
if (public_political_social_charity == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7,                           # Public affairs
          Q5.2,                                                            # Political ideology
          Q5.6, Q5.11, Q5.8, Q5.9, Q5.10,                                  # Social views
          Q2.7, Q2.8, Q2.5, Q2.6, Q2.9, Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10  # Charity and voluntarism
        ) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate_at(vars(contains("Q2.3")), coalesce, 0) %>%
        mutate(
          Q5.7_2 = if_else(Q5.7 == 2, 1, 0),
          Q5.10_2 = if_else(Q5.10 == 2, 1, 0),
          Q5.10_3 = if_else(Q5.10 == 3, 1, 0),
          Q5.10_4 = if_else(Q5.10 == 4, 1, 0),
          Q5.10_5 = if_else(Q5.10 == 5, 1, 0),
          Q5.10_6 = if_else(Q5.10 == 6, 1, 0),
          Q5.10_7 = if_else(Q5.10 == 7, 1, 0),
          Q5.10_8 = if_else(Q5.10 == 8, 1, 0),
          Q5.10_9 = if_else(Q5.10 == 9, 1, 0),
          Q5.10_10 = if_else(Q5.10 == 10, 1, 0),
          Q2.9_2 = if_else(Q2.9 == 2, 1, 0)
        ) %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, contains("Q5.7_"),                          # Public affairs
          Q5.2,                                                                        # Political ideology
          Q5.6, Q5.11, Q5.8, Q5.9, contains("Q5.10_"),                                 # Social views
          Q2.7, Q2.8, Q2.5, Q2.6, contains("Q2.9_"), Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10 # Charity and voluntarism
        )
    ) %>%
    as.matrix()
}
if (public_political_social_charity_demo == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
    bind_cols(
      final_data %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7,                           # Public affairs
          Q5.2,                                                            # Political ideology
          Q5.6, Q5.11, Q5.8, Q5.9, Q5.10,                                  # Social views
          Q2.7, Q2.8, Q2.5, Q2.6, Q2.9, Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10, # Charity and voluntarism
          Q5.12, Q5.13, Q5.14, Q5.15, Q5.16_1:Q5.16_6, Q5.17               # Demographics
        ) %>%
        mutate_if(is.factor, as.integer) %>%
        mutate_at(vars(contains("Q2.3")), coalesce, 0) %>%
        mutate_at(vars(contains("Q5.16")), coalesce, 0) %>%
        mutate(
          Q5.7_2 = if_else(Q5.7 == 2, 1, 0),
          Q5.10_2 = if_else(Q5.10 == 2, 1, 0),
          Q5.10_3 = if_else(Q5.10 == 3, 1, 0),
          Q5.10_4 = if_else(Q5.10 == 4, 1, 0),
          Q5.10_5 = if_else(Q5.10 == 5, 1, 0),
          Q5.10_6 = if_else(Q5.10 == 6, 1, 0),
          Q5.10_7 = if_else(Q5.10 == 7, 1, 0),
          Q5.10_8 = if_else(Q5.10 == 8, 1, 0),
          Q5.10_9 = if_else(Q5.10 == 9, 1, 0),
          Q5.10_10 = if_else(Q5.10 == 10, 1, 0),
          Q2.9_2 = if_else(Q2.9 == 2, 1, 0),
          Q5.12_2 = if_else(Q5.12 == 2, 1, 0),
          Q5.12_3 = if_else(Q5.12 == 3, 1, 0),
          Q5.12_4 = if_else(Q5.12 == 4, 1, 0),
          Q5.12_5 = if_else(Q5.12 == 5, 1, 0),
          Q5.13_2 = if_else(Q5.13 == 2, 1, 0),
          Q5.13_3 = if_else(Q5.13 == 3, 1, 0),
          Q5.13_4 = if_else(Q5.13 == 4, 1, 0),
          Q5.13_5 = if_else(Q5.13 == 5, 1, 0)
        ) %>%
        select(
          Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, contains("Q5.7_"),                           # Public affairs
          Q5.2,                                                                         # Political ideology
          Q5.6, Q5.11, Q5.8, Q5.9, contains("Q5.10_"),                                  # Social views
          Q2.7, Q2.8, Q2.5, Q2.6, contains("Q2.9_"), Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10, # Charity and voluntarism
          contains("Q5.12_"), contains("Q5.13_"), Q5.14, Q5.15, Q5.16_1:Q5.16_6, Q5.17  # Demographics
        )
    ) %>%
    as.matrix()
}

# Model Calibration -------------------------------------------------------
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
  Theta_scale = 1,         # Scale of coefficients for the heterogeneity model.
  alpha_mean = 0,          # Mean of scale for the heterogeneity model.
  alpha_scale = 10,        # Scale of scale for the heterogeneity model.
  lkj_corr_shape = 5,      # Shape of correlation matrix for the heterogeneity model.

  Y = Y,                   # Matrix of observed choices.
  X = X,                   # Array of experimental designs per choice task.
  Z = Z                    # Matrix of respondent-level covariates.
)

# Run the model and save data and model output.
fit <- stan(
  file = here::here("src", "stan_files", "hmnl_noncentered.stan"),
  data = data,
  iter = 10000,
  thin = 5,
  seed = 42
)
run <- list(data = data, fit = fit)
if (intercept == 1) write_rds(run, here::here("analysis", "output", "model_runs", "intercept_noncentered.rds"))
if (public_affairs == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_affairs.rds"))
if (political_ideology == 1) write_rds(run, here::here("analysis", "output", "model_runs", "political_ideology.rds"))
if (social_views == 1) write_rds(run, here::here("analysis", "output", "model_runs", "social_views.rds"))
if (charity_voluntarism == 1) write_rds(run, here::here("analysis", "output", "model_runs", "charity_voluntarism.rds"))
if (demographics == 1) write_rds(run, here::here("analysis", "output", "model_runs", "demographics.rds"))
if (public_political == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political.rds"))
if (public_political_social == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political_social.rds"))
if (public_political_social_charity == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political_social_charity.rds"))
if (public_political_social_charity_demo == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political_social_charity_demo.rds"))

