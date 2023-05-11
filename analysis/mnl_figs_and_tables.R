library(tidyverse)
library(magrittr)
library(tidybayes)
library(broom)
library(ggridges)
library(ggraph)
library(ggdag)
library(pander)
library(patchwork)
library(scales)
library(here)

# Load custom plot functions
source(here("R", "graphics.R"))

# General settings
source(here("analysis", "options.R"))

# Make all the randomness reproducible
set.seed(1234)

fitted_model = read_rds(here::here("analysis", "output", "model_runs", "mnl_dummy_intercept.rds"))

draws = as.tibble(fitted_model)
