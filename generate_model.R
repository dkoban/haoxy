library(tidyverse)
library(readr)
source("~/Documents/hoaxy/functions.R")

# Fit a theoretical model (discrete dynamical system)
cascades <- read_csv("~/Documents/hoaxy/cascades.csv")
titles <- cascades$title %>% unique()

# Plot the theoretical model with observed data
k = 5
plot_empirical_theoretical(empirical_data = cascades %>% filter(title == titles[k]),
                           baseline_tweet_likelihood = 1)


