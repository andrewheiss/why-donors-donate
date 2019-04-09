# Universal pandoc options ------------------------------------------------

pander::panderOptions("table.split.table", Inf)
pander::panderOptions("table.split.cells", Inf)
pander::panderOptions("missing", "")
pander::panderOptions("big.mark", ",")
pander::panderOptions("digits", 3)
pander::panderOptions("round", 3)
pander::panderOptions("table.alignment.default", "left")


# Bayes options -----------------------------------------------------------

CHAINS <- 4
ITER <- 4000
WARMUP <- 2000
BAYES_SEED <- 1234
options(mc.cores = parallel::detectCores())


# Ordered factor modeling setup -------------------------------------------

# By default, R uses polynomial contrasts for ordered factors in linear models
# options("contrasts")
# So make ordered factors use treatment contrasts instead
options(contrasts = rep("contr.treatment", 2))
# Or do it on a single variable:
# contrasts(df$x) <- "contr.treatment"
