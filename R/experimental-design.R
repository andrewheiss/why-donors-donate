# Load libraries.
library(AlgDesign)
library(tidyverse)

# Generate a full factorial design.
full_design <- gen.factorial(
  levels = c(16, 6, 7, 4, 2, 4, 5, 17),
  nVars = 8,
  center = FALSE,
  varNames = c("brand", "year", "miles", "warranty", "seller", "mpg", "safety", "price")
)

ntask <- 12  # Number of choice tasks.
nalts <- 4   # Number of alternatives (exclusing the outside option).
nvers <- 500 # Number of versions of the design.

# Create a fractional factorial design by sampling from the full factorial.
frac_design <- matrix(double(ntask * nalts * nvers * (ncol(full_design) + 3)), ncol = (ncol(full_design) + 3))
row_index <- c(1:nrow(full_design))
ii <- 1
for(i in 1:nvers) {
  for(j in 1:ntask) {
    ind <- sample(row_index, nalts)           # Sample nalts row indicators from row_index.
    x_temp <- full_design[ind,]               # Create a temp design for this task.
    x_temp <- cbind(c(1:nalts), x_temp)       # Append the alternative numbers.
    frac_design[c(ii:(ii+(nalts-1))), 1] <- i # Append the version number.
    frac_design[c(ii:(ii+(nalts-1))), 2] <- j # Append the task number.

    # Save out the temp design and increment ii.
    frac_design[c(ii:(ii+(nalts-1))), -c(1:2)] <- as.matrix(x_temp)
    ii <- ii + nalts
  }
}
colnames(frac_design) <- c("version", "task", "alt", colnames(full_design))

# frac_design <- read_csv(here::here("data", "design.csv"))
#
# # Check to make sure there is a balance across levels.
# as_tibble(frac_design) %>%
#   select(Organization:`Relationship with host government`) %>%
#   gather(key = atts, value = lvls) %>%
#   ggplot(aes(x = lvls)) +
#   geom_bar() +
#   facet_wrap(~ atts, scales = "free", nrow = 2)
#
# ggsave("design_balance.png", width = 8, height = 4, units = "in")

# Check to make sure there is a balance across levels.
as_tibble(frac_design) %>%
  select(brand:price) %>%
  gather(key = atts, value = lvls) %>%
  ggplot(aes(x = lvls)) +
  geom_bar() +
  facet_wrap(~ atts, scales = "free", nrow = 2)

ggsave("Figures/design_balance.png", width = 8, height = 4, units = "in")

# Save design matrix output.
write.csv(frac_design, "Data/survey_design.csv")

# Convert the attributes in frac_design into factors.
temp_design <- as.data.frame(frac_design)
for(i in 4:ncol(temp_design)) {
  temp_design[,i] <- as.factor(temp_design[,i])
}

# Use the output of lm to get a dummy-coded version of the design.
temp_design$y <- matrix(rnorm(nrow(temp_design)), ncol=1)
out <- lm(y ~ ., temp_design, x = TRUE)
design <- out$x[,-1]
rownames(design) <- NULL

# Save design matrix output.
write.csv(design, "Data/dummy_design.csv")

