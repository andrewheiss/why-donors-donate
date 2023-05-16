# All hypotheses

draws <- read_rds(here::here("data", "raw_data", "posterior_draws", "mnl_aggregate_dummy_draws.rds")) %>%
  rename(i=l)


# Reference Level = Friendly relationship with government
# Compute the contrasts
contrasts_int <- tibble(
  # Attribute levels
  h1_high = draws %>% filter(i == 1) %>% select(beta) %>% pull(),
  h1_low = draws %>% filter(i == 2) %>% select(beta) %>% pull(),
  # h2a_high = draws %>% filter(i == 3) %>% select(beta) %>% pull(),
  h3a_high = draws %>% filter(i == 4)%>% select(beta) %>% pull(),
  h4a_high = draws %>% filter(i == 5)%>% select(beta) %>% pull(),
  h5a_high = draws %>% filter(i == 6) %>% select(beta) %>% pull(),
  h2b_high = draws %>% filter(i == 7)%>% select(beta) %>% pull(),
  h2b_low = draws %>% filter(i == 8)%>% select(beta) %>% pull(),
  # h3b_high = draws %>% filter(i == 9)%>% select(beta) %>% pull(),
  # h3b_low = draws %>% filter(i == 10)%>% select(beta) %>% pull(),
  h4b_high = draws %>% filter(i == 11)%>% select(beta) %>% pull(),
  h5b_high = draws %>% filter(i == 12)%>% select(beta) %>% pull(),
  h4c_high = draws %>% filter(i == 13)%>% select(beta) %>% pull(),
  h5c_high = draws %>% filter(i == 14)%>% select(beta) %>% pull(),
  h5d_high = draws %>% filter(i == 15)%>% select(beta) %>% pull(),
  h5e_high = draws %>% filter(i == 16)%>% select(beta) %>% pull()
)

# Plot the contrasts
contrasts_int %>%
  pivot_longer(
    h1_high:h5e_high,
    names_to = "level", values_to = "beta"
  ) %>%
  mutate(level = fct_rev(fct_inorder(level))) %>%
  ggplot(aes(x = beta, y = level)) +
  stat_halfeye(.width = .95) +
  geom_vline(xintercept = 0, color = "grey") +
  labs(y = "Contrasts", x = "Difference in betas")

# Save plot



draws <- read_rds(here::here("data", "raw_data", "posterior_draws", "mnl_dummy_intercept_draws.rds")) %>%
  rename(i=l)


contrasts_int <- tibble(
  # Attribute levels
  org1 = draws %>% filter(i == 1) %>% select(beta) %>% pull(),
  org2 = draws %>% filter(i == 2) %>% select(beta) %>% pull(),
  org3 = draws %>% filter(i == 3)%>% select(beta) %>% pull(),
  org4 = draws %>% filter(i == 4)%>% select(beta) %>% pull(),
  issue2 = draws %>% filter(i == 5) %>% select(beta) %>% pull(),
  issue3 = draws %>% filter(i == 6)%>% select(beta) %>% pull(),
  issue4 = draws %>% filter(i == 7)%>% select(beta) %>% pull(),
  transp2 = draws %>% filter(i == 8)%>% select(beta) %>% pull(),
  account2 = draws %>% filter(i == 9)%>% select(beta) %>% pull(),
  fund2 = draws %>% filter(i == 10)%>% select(beta) %>% pull(),
  fund3 = draws %>% filter(i == 11)%>% select(beta) %>% pull(),
  gov_rel2 = draws %>% filter(i == 12)%>% select(beta) %>% pull(),
  gov_rel3 = draws %>% filter(i == 13)%>% select(beta) %>% pull()
)


contrasts_int %>%
  pivot_longer(
    org1:gov_rel3,
    names_to = "level", values_to = "beta"
  ) %>%
  mutate(level = fct_rev(fct_inorder(level))) %>%
  ggplot(aes(x = beta, y = level)) +
  stat_halfeye(.width = .95) +
  geom_vline(xintercept = 0, color = "grey") +
  labs(y = "Contrasts", x = "Difference in Gammas", title = "Flat Model Main Effects")


ggsave(
  "flat_main.png",
  path = here::here("analysis", "output", "figures"),
  width = 12,
  height = 6,
  units = "in"
)

contrasts_int %>%
  pivot_longer(
    org1:gov_rel3,
    names_to = "level", values_to = "beta"
  ) %>%
  mutate(level = fct_rev(fct_inorder(level))) %>%
  ggplot(aes(x = plogis(beta), y = level)) +
  stat_halfeye(.width = .95) +
  geom_vline(xintercept = 0, color = "grey") +
  labs(y = "Contrasts", x = "Difference in Gammas", title = "Flat Model Main--plogis")


ggsave(
  "flat_main_plogis.png",
  path = here::here("analysis", "output", "figures"),
  width = 12,
  height = 6,
  units = "in"
)

draws <- read_rds(here::here("data", "raw_data", "posterior_draws", "intercept.rds")) %>%
  mutate(model = "Intercept only")

# Join levels and labels to draws
draws <- draws_intercept %>%
  left_join(level_lookup, by = "i")

contrasts_int <- tibble(
  # Attribute levels
  org1 = draws %>% filter(i == 1) %>% select(Gamma) %>% pull(),
  org2 = draws %>% filter(i == 2) %>% select(Gamma) %>% pull(),
  org3 = draws %>% filter(i == 3)%>% select(Gamma) %>% pull(),
  org4 = draws %>% filter(i == 4)%>% select(Gamma) %>% pull(),
  issue2 = draws %>% filter(i == 5) %>% select(Gamma) %>% pull(),
  issue3 = draws %>% filter(i == 6)%>% select(Gamma) %>% pull(),
  issue4 = draws %>% filter(i == 7)%>% select(Gamma) %>% pull(),
  transp2 = draws %>% filter(i == 8)%>% select(Gamma) %>% pull(),
  account2 = draws %>% filter(i == 9)%>% select(Gamma) %>% pull(),
  fund2 = draws %>% filter(i == 10)%>% select(Gamma) %>% pull(),
  fund3 = draws %>% filter(i == 11)%>% select(Gamma) %>% pull(),
  gov_rel2 = draws %>% filter(i == 12)%>% select(Gamma) %>% pull(),
  gov_rel3 = draws %>% filter(i == 13)%>% select(Gamma) %>% pull()
)


contrasts_int %>%
  pivot_longer(
    org1:gov_rel3,
    names_to = "level", values_to = "Gamma"
  ) %>%
  mutate(level = fct_rev(fct_inorder(level))) %>%
  ggplot(aes(x = Gamma, y = level)) +
  stat_halfeye(.width = .95) +
  geom_vline(xintercept = 0, color = "grey") +
  labs(y = "Contrasts", x = "Difference in Gammas", title = "Multilevel Model Main Effects")


ggsave(
  "multilevel_main.png",
  path = here::here("analysis", "output", "figures"),
  width = 12,
  height = 6,
  units = "in"
)

contrasts_int %>%
  pivot_longer(
    org1:gov_rel3,
    names_to = "level", values_to = "Gamma"
  ) %>%
  mutate(level = fct_rev(fct_inorder(level))) %>%
  ggplot(aes(x = plogis(Gamma), y = level)) +
  stat_halfeye(.width = .95) +
  geom_vline(xintercept = 0, color = "grey") +
  labs(y = "Contrasts", x = "Difference in Gammas", title = "Multilevel Model Main Effects--plogis")


ggsave(
  "multilevel_main_plogis.png",
  path = here::here("analysis", "output", "figures"),
  width = 12,
  height = 6,
  units = "in"
)
