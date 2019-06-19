# Load packages
library(tidyverse)

# Load data
col_names <- names(read_csv(here::here("data", "conjointqsf_final.csv")))
data <- read_csv(
  here::here("data", "conjointqsf_final.csv"),
  skip = 2,
  col_names = col_names
)

# Who finished?
data %>%
  summarize(
    n = n(),
    count = sum(V10)
  )

# 1437 completes

# Check Q1.1
data %>%
  count(Q1.1)

data %>%
  count(Q1.1, Q2.1)

# 30 didn't give consent

# Check Q2.5
data %>%
  count(Q2.5)

data %>%
  count(Q2.5, Q2.6)

# 15 were disqualified

# Check Q2.9 and Q2.10
data %>%
  count(Q2.9)

data %>%
  count(Q2.9, Q2.10)

# Check Q2.11
data %>%
  count(Q2.11)

# Everyone made the attention check

# Check Q5.17
data %>%
  count(Q5.17)

# No one under 18

# Check the assigned design
data %>%
  count(vers_CBCONJOINT) %>%
  arrange(desc(n))

# Everyone assigned to a difference version

# Check the choice data
data %>%
  count(Q4.1)

data %>%
  select(Q4.1:Q4.12) %>%
  gather(key = "task", value = "choice") %>%
  count(choice)

20 * 12

# Unqualified respondents don't see the conjoint

# # Check to make sure there is a balance across levels
# as_tibble(design) %>%
#   select(Organization:`Relationship with host government`) %>%
#   gather(key = atts, value = lvls) %>%
#   ggplot(aes(x = lvls)) +
#   geom_bar() +
#   facet_wrap(~ atts, scales = "free", nrow = 2)

