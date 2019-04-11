# Load libraries.
library(tidyverse)

# Number of responents.
nresp <- 1000

# Simulate data.
Q2.1 <- runif(nresp, min = 1, max = 5) %>% 
  round()
Q2.2 <- runif(nresp, min = 1, max = 3) %>% 
  round() 
Q2.3 <- runif(nresp, min = 1, max = 6) %>% 
  round() 
Q5.1 <- runif(nresp, min = 1, max = 2) %>% 
  round() 
Q5.2 <- runif(nresp, min = 1, max = 8)
Q5.6 <- runif(nresp, min = 1, max = 8)

sim_data1 <-tibble(Q2.1, Q2.2, Q2.3, Q5.1, Q5.2, Q5.6)

sim_data2 <- sim_data1 %>% 
  mutate(Q2.1 = factor(
    Q2.1, 
    levels = c(1:5),
    labels = c("Multiple times a day", "Every day", "Once a week", "Hardly ever","Never"),
    ordered = TRUE)
  ) %>% 
  mutate(Q2.2 = factor(
    Q2.2, 
    levels = c(1:3),
    labels = c("Always", "Sometimes", "Never"),
    ordered = TRUE)) %>% 
  mutate(Q2.3 = factor(
    Q2.3, 
    levels = c(1:6),
    labels = c("TV", "Print", "Online (excluding social media)", "Social media", "Radio", "Email newsletters"),
  )) %>% 
  mutate(Q5.1 = factor(
    Q5.1, 
    levels = c(1:2),
    labels = c("True", "False"),
    ordered = TRUE))

sim_data2

-----------------------------------------

# graph discrete 
sim_data2 %>% 
  ggplot(aes(x = Q5.1)) + 
  geom_bar() +
  labs(title = "Did you Vote in Last Election",
       subtitle = "Q5.1 in the Survey")

-----------------------------------------
  
# graph discrete vs discrete 
sim_data2 %>% 
  ggplot(aes(x = Q2.2, fill = Q5.1)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "Vote in Last Election", 
    values = c("darkred", "tomato2")) +
  labs(
    x = "How Often Follow International News",
    y = "Proportions", 
    title = "Often Follow International News by If Voted in Last Election") 

# hypothesis test discrete vs discrete 
sim_data2 %>% 
  select(Q2.2, Q5.1) %>% 
  table() %>% 
  chisq.test()

-----------------------------------------
  
# graph continuous 
sim_data2 %>% 
  ggplot(aes(x = Q5.2)) +
  geom_density() + 
  labs(x = "Political Ideology",
       title = "Scale of Political Ideology", 
       subtitle = "Q5.2 in the Survey")
  
# hypothesis test continuous 
sim_data2 %>%   
  select(Q5.2) %>% 
  pull() %>% 
  t.test(mu = 25, alternative = "two.sided")
  
# graph continuous vs continuous 
sim_data2 %>% 
  ggplot(aes(x = Q5.2, y = Q5.6)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm") + 
  labs(x = "Political Ideology",
       y = "Trust in Political Institutions",
       title = "Scale of Political Ideology vs Trust in Political Institutions", 
       subtitle = "Q5.2 & Q5.6 in the Survey")

# recover parameters
sim_data2 %>% 
  lm(Q5.6 ~ Q5.2, data = .) %>% 
  summary()
  
# hypothesis test continuous vs continuous  
sim_data2 %>% 
  cor.test(
      ~ Q5.6 + Q5.2, 
      data = .,
      alternative = "two.sided"
    )

-----------------------------------------
  
# summarize continuous vs discrete 
sim_data2 %>% 
  group_by(Q2.1) %>% 
  summarize(mean_Q5.2 = mean(Q5.2))

# graph continuous vs discrete 
sim_data2 %>% 
  ggplot(aes(x = Q5.2, color = Q2.1)) +
  geom_density(adjust = 2) +
  labs(x = "Political Views",
       title = "Political Views vs How Often You Follow National News", 
       subtitle = "Q5.2 & Q2.1 in the Survey")


# hypothesis test continuous vs discrete 
sim_data2 %>% 
  select(Q2.1, Q5.2) %>% 
  t.test(Q5.2 ~ Q2.1,
         data = ., 
         alternative = "two.sided")

sim_data2 %>% 
  lm(Q5.2 ~ Q2.1, 
     data = .) %>% 
  anova()

