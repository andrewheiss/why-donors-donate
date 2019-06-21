# Load Packages -----------------------------------------------------------
library(tidyverse)
library(bayesplot)
library(tidybayes)
library(bayesm)
library(ggridges)

# General MCMC ------------------------------------------------------------
intercept <- 0 # Intercept-only.
geo_locat <- 0 # Geolocation covariates.
demo_vars <- 0 # Demographic covariates.
geo_demos <- 0 # Geolocation and demographic covariates.
bnd_demos <- 0 # Brand covariates.
all_three <- 1 # Geolocation, demographic, and brand covariates.

# Load model output.
if (intercept == 1) run <- read_rds(here::here("Output", "hmnl_intercept-100k_ho.RDS"))
if (geo_locat == 1) run <- read_rds(here::here("Output", "hmnl_more-geo-locat-100k_ho.RDS"))
if (demo_vars == 1) run <- read_rds(here::here("Output", "hmnl_demo-vars-100k_ho.RDS"))
if (geo_demos == 1) run <- read_rds(here::here("Output", "hmnl_more-geo-demos-100k_ho.RDS"))
if (bnd_demos == 1) run <- read_rds(here::here("Output", "hmnl_bnd-demos-100k_ho.RDS"))
if (all_three == 1) run <- read_rds(here::here("Output", "hmnl_all-three-100k_ho.RDS"))

# Extract Data, Prior, Mcmc, and fit objects.
Data <- run$Data
Prior <- run$Prior
Mcmc <- run$Mcmc
fit <- run$fit

# Check trace plots.
fit$llikedraw %>% 
  matrix(dimnames = list(NULL, "llike")) %>% 
  mcmc_trace(
    n_warmup = 500
  )

# colnames(fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(fit$Gammadraw)), ",1]")
# fit$Gammadraw %>%
#   mcmc_trace(
#     n_warmup = 500,
#     facet_args = list(nrow = 5, labeller = label_parsed)
#   )
# 
# ggsave(
#   "mcmc_trace_conjugate.png",
#   path = here::here("Figures"),
#   width = 12, height = 6, units = "in"
# )

# Import model fit table.
# model_fit_table <- matrix(NA, nrow = 6, ncol = 6) %>%
#   as_tibble() %>%
#   rename(
#     model = V1,
#     lmd = V2,
#     dic = V3,
#     waic = V4,
#     hr = V5,
#     hp = V6
#   )
# write_rds(model_fit_table, here::here("Figures", "model_fit_table.RDS"))
model_fit_table <- read_rds(here::here("Figures", "model_fit_table.RDS"))

# Compute in-sample model fit.
source(here::here("Code", "model_fit.R"))

if (intercept == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Intercept 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (geo_locat == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Geolocation 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (demo_vars == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Demographics 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (geo_demos == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "More Geo-Demos 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (bnd_demos == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Brands 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (all_three == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Geolocation, Brands, and Demographics 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}

write_rds(model_fit_table, here::here("Figures", "model_fit_table.RDS"))

# Modify model fit tabel.
modified_fit_table <- model_fit_table %>% 
  filter(
    model %in% c(
      "Intercept 100k w/HO",
      "More Geolocation 100k w/HO",
      "More Geo-Demos 100k w/HO",
      "Brands 100k w/HO",
      "Geolocation, Brands, and Demographics 100k w/HO"
    )
  ) %>% 
  mutate(
    model = factor(model),
    model = fct_recode(
      model, 
      "Intercept-Only" = "Intercept 100k w/HO",
      "Geolocation" = "More Geolocation 100k w/HO",
      "Geolocation and Demographics" = "More Geo-Demos 100k w/HO",
      "Stated and Demographics" = "Brands 100k w/HO",
      "Geolocation, Stated, and Demographics" = "Geolocation, Brands, and Demographics 100k w/HO"
    )
  ) %>%
  select(-waic)

modified_fit_table

modified_fit_table %>% 
  select(model, hr, hp) %>% 
  mutate(model = reorder(model, hr)) %>% 
  gather(key = stat, value = fit, hr, hp) %>% 
  ggplot(aes(x = stat, y = fit, fill = model)) +
  geom_col(position = "dodge", ) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Predictive Fit")

ggsave(
  "predictive_fit.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)
  
# HMC-Specific ------------------------------------------------------------
# Stan diagnostics.
source(here::here("Code", "stan_utility.R"))

# Check for divergences (HMC-specific).
check_div(fit_centered)
check_div(fit_noncentered)

# Check the effective sample size. (Not working for a hierarchical model?)
check_n_eff(fit_centered)
check_n_eff(fit_noncentered)
check_n_eff(fit_conjugate)

# Check the Rhat statistic. (Issues with mixing? Need to run longer?)
check_rhat(fit_centered)
check_rhat(fit_noncentered)
check_rhat(fit_conjugate)

# Check trace plots.
fit %>%
  extract(
    inc_warmup = TRUE,
    permuted = FALSE
  ) %>%
  mcmc_trace(
    regex_pars = "Theta",
    n_warmup = 1000,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_centered.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

fit_noncentered %>%
  extract(
    inc_warmup = TRUE,
    permuted = FALSE
  ) %>%
  mcmc_trace(
    regex_pars = "Theta",
    n_warmup = 1000,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_noncentered.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

# Plot Marginals ----------------------------------------------------------
# Intercept run.
run <- read_rds(here::here("Output", "hmnl_intercept-100k_ho.RDS"))
colnames(run$fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(run$fit$Gammadraw)), ",1]")
draws_intercept <- as_tibble(run$fit$Gammadraw) %>% 
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>% 
  gather(key = i, value = Gamma, -c(.draw, .iteration)) %>% 
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>% 
  separate(col = i, into = c("i", "j"), sep = ",") %>% 
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>% 
  mutate(
    model = "Intercept",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>% 
  select(model, .chain, .iteration, .draw, i, j, Gamma) %>% 
  arrange(.iteration) %>% 
  filter(.iteration > 500)

# Demographics run.
run <- read_rds(here::here("Output", "hmnl_demo-vars-100k_ho.RDS"))
colnames(run$fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(run$fit$Gammadraw)), ",1]")
draws_demographics <- as_tibble(run$fit$Gammadraw) %>% 
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>% 
  gather(key = i, value = Gamma, -c(.draw, .iteration)) %>% 
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>% 
  separate(col = i, into = c("i", "j"), sep = ",") %>% 
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>% 
  mutate(
    model = "Demographics",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>% 
  select(model, .chain, .iteration, .draw, i, j, Gamma) %>% 
  arrange(.iteration) %>% 
  filter(.iteration > 500)

# Geolocation run.
run <- read_rds(here::here("Output", "hmnl_more-geo-locat-100k_ho.RDS"))
colnames(run$fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(run$fit$Gammadraw)), ",1]")
draws_geolocation <- as_tibble(run$fit$Gammadraw) %>% 
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>% 
  gather(key = i, value = Gamma, -c(.draw, .iteration)) %>% 
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>% 
  separate(col = i, into = c("i", "j"), sep = ",") %>% 
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>% 
  mutate(
    model = "Geolocation",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>% 
  select(model, .chain, .iteration, .draw, i, j, Gamma) %>% 
  arrange(.iteration) %>% 
  filter(.iteration > 500)

# Geolocation-demograpics run.
run <- read_rds(here::here("Output", "hmnl_more-geo-demos-100k_ho.RDS"))
colnames(run$fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(run$fit$Gammadraw)), ",1]")
draws_geo_demos <- as_tibble(run$fit$Gammadraw) %>% 
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>% 
  gather(key = i, value = Gamma, -c(.draw, .iteration)) %>% 
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>% 
  separate(col = i, into = c("i", "j"), sep = ",") %>% 
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>% 
  mutate(
    model = "Geo-Demos",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>% 
  select(model, .chain, .iteration, .draw, i, j, Gamma) %>% 
  arrange(.iteration) %>% 
  filter(.iteration > 500)

# Brand-demographics run.
run <- read_rds(here::here("Output", "hmnl_bnd-demos-100k_ho.RDS"))
colnames(run$fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(run$fit$Gammadraw)), ",1]")
draws_bnd_demos <- as_tibble(run$fit$Gammadraw) %>% 
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>% 
  gather(key = i, value = Gamma, -c(.draw, .iteration)) %>% 
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>% 
  separate(col = i, into = c("i", "j"), sep = ",") %>% 
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>% 
  mutate(
    model = "Brand-Demos",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>% 
  select(model, .chain, .iteration, .draw, i, j, Gamma) %>% 
  arrange(.iteration) %>% 
  filter(.iteration > 500)

# Geolocation, demographic, and brand run.
run <- read_rds(here::here("Output", "hmnl_all-three-100k_ho.RDS"))
colnames(run$fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(run$fit$Gammadraw)), ",1]")
draws_all_three <- as_tibble(run$fit$Gammadraw) %>% 
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>% 
  gather(key = i, value = Gamma, -c(.draw, .iteration)) %>% 
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>% 
  separate(col = i, into = c("i", "j"), sep = ",") %>% 
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>% 
  mutate(
    model = "All Three",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>% 
  select(model, .chain, .iteration, .draw, i, j, Gamma) %>% 
  arrange(.iteration) %>% 
  filter(.iteration > 500)

# Posterior means.
posterior_means <- draws_all_three %>% 
  group_by(i) %>% 
  summarize(
    mean = mean(Gamma),
    ci_lower = quantile(Gamma, .025),
    ci_upper = quantile(Gamma, .975),
    gt_zero = if_else(ci_lower > 0 & ci_upper > 0, 1, 0),
    lt_zero = if_else(ci_lower < 0 & ci_upper < 0, -1, 0)
  ) %>% 
  mutate(
    ncov = rep(1:ncol(run$Data$Z), ncol(run$Data$X[[1]])),
    nvar = sort(rep(1:ncol(run$Data$X[[1]]), ncol(run$Data$Z)))
  )

# Heatmap of Gamma matrix.
posterior_means %>% 
  mutate(
    gt_lt_zero = gt_zero + lt_zero,
    Coefficients = factor(gt_lt_zero, labels = c("Negative", "Not Significant", "Positive"))
  ) %>% 
  ggplot(aes(x = ncov, y = nvar, fill = Coefficients)) +
  geom_raster() +
  scale_fill_brewer(palette = "Blues") +
  scale_x_discrete(expand=c(0.001,0.001)) +
  scale_y_discrete(expand=c(0.001,0.001)) +
  labs(
    title = "Upper-Level Coefficient Matrix Estimates",
    x = "Covariates",
    y = "Attribute Levels"
  )

ggsave(
  "coefficient_matrix.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

# Which of the goeolocation covariates are significant?
significant_covs <- posterior_means %>% 
  filter(
    ncov %in% c(1:22),
    gt_zero != 0 | lt_zero != 0
  ) %>% 
  select(ncov, nvar, i)

# Compare levels associated with the geolocation data.
Z_geo <- c(
  "Acura", "BMW", "Chevrolet", "Chrysler", "Ferrari", "Ford", "GMC", "Honda", "Hyundai", "Infiniti", "Kia", 
  "Lexus", "Lincoln", "Mazda", "Mercedes Benz", "Mitsubishi", "Nissan", "Subaru", "Tesla", "Toyota", "Volkswagen"
)
X_levels <- c(
  str_c("Brand: ", c("Jeep", "Toyota", "Ford", "Chevrolet", "Honda", "Nissan", "Subaru", "Hyundai", "GMC", "Kia", "Lexus", "Mazda", "Buick", "Mercedes-Benz", "Volkswagen", "BMW")),
  str_c("Year: ", c("2016-2018", "2013-2015", "2010-2012", "2007-2009", "Older than 2007")),
  str_c("Miles: ", c("150,000-199,999", "100,000-149,999", "75,000-99,999", "50,000-74,999", "1,000-49,999", "0-1,000")),
  str_c("Warranty: ", c("2 year", "4 year", "6 year")),
  "Sale by Owner",
  "21-30 MPG", "31-40 MPG", "41-50 MPG",
  str_c("Safety: ", c("2 out of 5 stars", "3 out of 5 stars", "4 out of 5 stars", "5 out of 5 stars")),
  "Price"
)
draws_all_three %>%
  left_join(posterior_means, by = "i") %>% 
  inner_join(significant_covs) %>% 
  mutate(
    ncov = factor(
      ncov,
      labels = Z_geo
    ),
    nvar = factor(
      nvar,
      labels = X_levels
    )
  ) %>% 
  filter(ncov %in% c("BMW", "Chevrolet", "Kia", "Toyota")) %>%
  ggplot(aes(x = Gamma, y = nvar)) + 
  geom_halfeyeh(.width = .95) +
  geom_vline(xintercept = 0, color = "grey") +
  facet_wrap(
    ~ ncov,
    nrow = 1,
    scales = "free_x"
  ) +
  labs(
    title = "Marginal Posteriors by Geolocation Covariate",
    y = "Attribute Levels"
  )

ggsave(
  "marginal_posteriors_01.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

draws_all_three %>%
  left_join(posterior_means, by = "i") %>% 
  inner_join(significant_covs) %>% 
  mutate(
    ncov = factor(
      ncov,
      labels = Z_geo
    ),
    nvar = factor(
      nvar,
      labels = X_levels
    )
  ) %>% 
  filter(ncov %in% c("GMC", "Lexus", "Subaru", "Volkswagen")) %>%
  ggplot(aes(x = Gamma, y = nvar)) + 
  geom_halfeyeh(.width = .95) +
  geom_vline(xintercept = 0, color = "grey") +
  facet_wrap(
    ~ ncov,
    nrow = 1,
    scales = "free_x"
  ) +
  labs(
    title = "Marginal Posteriors by Geolocation Covariate",
    y = "Attribute Levels"
  )

ggsave(
  "marginal_posteriors_02.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

# draws_centered <- fit_centered %>% 
#   spread_draws(Theta[i, j]) %>% 
#   mutate(model = "centered") %>% 
#   select(model, .chain, .iteration, .draw, i, j, Theta) %>% 
#   ungroup()

draws <- draws_intercept %>% 
  bind_rows(draws_demographics) %>%
  bind_rows(draws_geolocation) %>%
  bind_rows(draws_geo_demos) %>% 
  bind_rows(draws_bnd_demos) %>% 
  bind_rows(draws_all_three) %>% 
  mutate(
    model = factor(model),
    model = fct_relevel(
      model, 
      "Intercept", 
      "Demographics", 
      "Geolocation", 
      "Geo-Demos",
      "Brand-Demos",
      "All Three"
    )
  )

# ggsave(
#   "mcmc_marginal_posteriors.png",
#   path = here::here("Figures"),
#   width = 12, height = 6, units = "in"
# )
