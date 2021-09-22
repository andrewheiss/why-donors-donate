# Packages,  options, settings, &c. ---------------------------------------

# Packages accessible just for this _targets.R file
library(targets)
library(tarchetypes)
library(tibble)
suppressPackageStartupMessages(library(dplyr))

# Packages accessible in every target
tar_option_set(packages = c("tidyverse", "here", "fs", "withr"))

# Options for this _targets.R file and all targets
options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

set.seed(9059)  # From random.org


# Functions ---------------------------------------------------------------

source("R/funs_data.R")

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}


# Actual pipeline ---------------------------------------------------------

list(
  osf_file_targets
)
