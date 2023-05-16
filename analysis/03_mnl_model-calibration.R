# Preamble ----------------------------------------------------------------
# Load packages
library(tidyverse)
library(fastDummies)
library(rstan)
library(bayesplot)
library(tidybayes)
library(loo)
library(ggridges)
library(ggraph)
library(ggdag)
library(here)


# Set Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Choose the Model to Run -------------------------------------------------
intercept <- 1                            # Intercept-only
public_affairs <- 0                       # Public affairs: Public affairs knowledge + Public affairs activity
political_ideology <- 0                   # Political ideology
social_views <- 0                         # Social views: Public affairs trust + Social ideology + Religiosity
charity_voluntarism <- 0                  # Charity and voluntarism: Charity trust, Charity activity, Volunteer activity, etc
demographics <- 0                         # Demographics: Gender, Marital status, Education, Income, Race, Age
public_political <- 0                     # Public affairs + Political ideology
public_political_social <- 0              # Public affairs + Political ideology + Social views
public_political_social_charity <- 0      # Public affairs + Political ideology + Social views + Charity and voluntarism
public_political_social_charity_demo <- 0 # Public affairs + Political ideology + Social views + Charity and voluntarism + Demographics

# Import and Restructure Data ---------------------------------------------
# Load data and design
# final_data <- read_rds(here::here("data", "derived_data", "final_data.rds"))
# X <- read_rds(here::here("data", "derived_data", "final_design.rds"))
# #
# # Restructure choice data Y
# Y <- final_data %>%
#   select(contains("Q4")) %>%
#   as.matrix()
#
# # Restructure covariate matrix Z
# if (intercept == 1) {
#   Z <- matrix(data = 1, nrow = dim(X)[1], ncol = 1)
# }
# if (public_affairs == 1) {
#   Z <- final_data %>%
#     select(
#       Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7 # Public affairs
#     )
# }
# if (political_ideology == 1) {
#   Z <- final_data %>%
#     select(
#       Q5.2 # Political ideology
#     )
# }
# if (social_views == 1) {
#   Z <- final_data %>%
#     select(
#       Q5.6, Q5.11, Q5.8, Q5.9, Q5.10 # Social views
#     )
# }
# if (charity_voluntarism == 1) {
#   Z <- final_data %>%
#     select(
#       Q2.7, Q2.8, Q2.5, Q2.6, Q2.9, Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10 # Charity and voluntarism
#     )
# }
# if (demographics == 1) {
#   Z <- final_data %>%
#     select(
#       Q5.12, Q5.13, Q5.14, Q5.15, Q5.16_1:Q5.16_6, Q5.17 # Demographics
#     )
# }
# if (public_political == 1) {
#   Z <- final_data %>%
#     select(
#       Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7, # Public affairs
#       Q5.2                                   # Political ideology
#     )
# }
# if (public_political_social == 1) {
#   Z <- final_data %>%
#     select(
#       Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7, # Public affairs
#       Q5.2,                                  # Political ideology
#       Q5.6, Q5.11, Q5.8, Q5.9, Q5.10         # Social views
#     )
# }
# if (public_political_social_charity == 1) {
#   Z <- final_data %>%
#     select(
#       Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7,                           # Public affairs
#       Q5.2,                                                            # Political ideology
#       Q5.6, Q5.11, Q5.8, Q5.9, Q5.10,                                  # Social views
#       Q2.7, Q2.8, Q2.5, Q2.6, Q2.9, Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10  # Charity and voluntarism
#     )
# }
# if (public_political_social_charity_demo == 1) {
#   Z <- final_data %>%
#     select(
#       Q2.1, Q2.2, Q2.3_1:Q2.3_7, Q2.4, Q5.7,                           # Public affairs
#       Q5.2,                                                            # Political ideology
#       Q5.6, Q5.11, Q5.8, Q5.9, Q5.10,                                  # Social views
#       Q2.7, Q2.8, Q2.5, Q2.6, Q2.9, Q2.10, Q5.4, Q5.5, Q5.3_1:Q5.3_10, # Charity and voluntarism
#       Q5.12, Q5.13, Q5.14, Q5.15, Q5.16_1:Q5.16_6, Q5.17               # Demographics
#     )
# }

# # Replace factor labels with levels
# Z <- Z %>%
#   mutate_if(is.factor, as.integer) %>%
#   mutate_if(is.integer, as.factor)
#
# # Pad the variables names with _
# colnames(Z) <- str_c(colnames(Z), "_")
#
# # Use the output of lm to get a dummy-coded version of the covariate matrix Z
# Z$y <- matrix(rnorm(nrow(Z)), ncol = 1)
# out <- lm(y ~ ., Z, x = TRUE)

# Save out Z as a matrix, including an intercept
# Z <- tibble(intercept = rep(1, dim(X)[1])) %>%
#   bind_cols(as_tibble(out$x[,-1])) %>%
#   as.matrix()

# Accidentally overwrote the dummy data, whoops
# X = read_rds(here::here("data", "derived_data", "final_data_mnl_dummy.rds"))
# X = read_rds(here::here("data", "derived_data", "final_data_mnl_dummy_aggregate.rds"))
X = read_rds(here::here("data", "derived_data", "final_data_interaction_mnl_dummy.rds"))

Y = read_rds(here::here("data", "derived_data", "final_outcome_mnl_dummy.rds"))

# Model Calibration -------------------------------------------------------
# Specify the data for calibration in a list
data <- list(
  N = dim(X)[1],      # Number of respondents
  A = dim(X)[2],      # Number of choice alternatives
  L = dim(X)[3],      # Number of (estimable) attribute levels

  # Gamma_mean = 0,      # Mean of population-level means
  # Gamma_scale = 1,     # Scale of population-level means
  # Omega_shape = 5,     # Shape of population-level scale
  # tau_mean = 0,        # Mean of population-level scale
  # tau_scale = 10,      # Scale of population-level scale

  Y = Y,               # Matrix of observed choices
  X = X               # Array of experimental designs per choice task
)

# Run the model and save data and model output
fit <- stan(
  file = here::here("src", "stan_files", "mnl_dummy_intercept.stan"),
  data = data,
  iter = 10000,
  thin = 5,
  seed = 42
)
run <- list(data = data, fit = fit)
# write_rds(run, here::here("analysis", "output", "model_runs", "mnl_dummy_intercept.rds"))
# write_rds(run, here::here("analysis", "output", "model_runs", "mnl_aggregate_dummy.rds"))
write_rds(run, here::here("analysis", "output", "model_runs", "mnl_interaction_dummy.rds"))

# Follow naming convention: mnl-intercept.rds (for example). What models to run:
# - Dummy-coded mnl-intercept. Compare contrasts to main effect hypotheses.
# - Index-coded mnl-intercept. Compare contrasts to main effect hypotheses.
# - Depending on how that goes, let's consider all interactions.
# - Consider running the all interaction model with a modified MNL (see "Interactions
# in Generalized Linear Models: Theoretical Issues and an Application to Personal Vote-Earning Attributes").

# if (intercept == 1) write_rds(run, here::here("analysis", "output", "model_runs", "intercept_noncentered.rds"))
# if (public_affairs == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_affairs.rds"))
# if (political_ideology == 1) write_rds(run, here::here("analysis", "output", "model_runs", "political_ideology.rds"))
# if (social_views == 1) write_rds(run, here::here("analysis", "output", "model_runs", "social_views.rds"))
# if (charity_voluntarism == 1) write_rds(run, here::here("analysis", "output", "model_runs", "charity_voluntarism.rds"))
# if (demographics == 1) write_rds(run, here::here("analysis", "output", "model_runs", "demographics.rds"))
# if (public_political == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political.rds"))
# if (public_political_social == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political_social.rds"))
# if (public_political_social_charity == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political_social_charity.rds"))
# if (public_political_social_charity_demo == 1) write_rds(run, here::here("analysis", "output", "model_runs", "public_political_social_charity_demo.rds"))


# Extract Draws
dummy_run = read_rds(here::here("analysis", "output", "model_runs", "mnl_aggregate_dummy.rds"))


data <- dummy_run$data
fit <- dummy_run$fit

# Extract posterior draws
draws <- fit %>%
  spread_draws(beta[l]) %>%
  ungroup() %>%
  filter(.iteration > 500)

# write_rds(draws, here::here("data", "raw_data", "posterior_draws", "mnl_dummy_intercept_draws.rds"))
write_rds(draws, here::here("data", "raw_data", "posterior_draws", "mnl_aggregate_dummy_draws.rds"))
