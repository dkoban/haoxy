library(tidyverse)
library(readr)
source("~/Documents/hoaxy/functions.R")
library(grid)
library(gridExtra)
library(scales)

files <- list.files("~/Documents/hoaxy/top_stories_10Nov/")
edges <- list()
for (i in 1:length(files)){
  edges[[i]] <- read_csv(paste0("~/Documents/hoaxy/top_stories_10Nov/", files[i]))
}

# Plot tweets by type
plot_tweet_activity_by_type(edges[[10]])

# Fit a theoretical model (discrete dynamical system)
cascades <- read_csv("~/Documents/hoaxy/cascades.csv")
titles <- cascades$title %>% unique()

# Plot the theoretical model with observed data
k = 5
plots <- list()
for (i in 1:length(titles)){
plots[[i]] <- plot_empirical_theoretical(empirical_data = cascades %>% 
                                           filter(title == titles[i]),
                                         baseline_tweet_likelihood = 1)}

# Plot facet grid of articles
plot_empirical_theoretical_facet(empirical_data = cascades %>%
                                   filter(title %in% titles[1:15]),
                                 baseline_tweet_likelihood = 1)


# Vensim lookup generator
vensim_data <- cascades[cascades$title == titles[10],] %>% distinct()

# Round values
vensim_data$t_rounded <- vensim_data$t %>% round() 
vensim_data <- vensim_data %>% group_by(t_rounded) %>%
  summarise(mean_user_count = mean(user_count) %>%  round())

# Generate lookup table string
vensim_string <- c()
for (i in 1:nrow(vensim_data)){
  vensim_string[i] <- print(paste0("(", vensim_data$t_rounded[i + 1], ", ", 
                                   vensim_data$mean_user_count[i + 1] - 
                                     vensim_data$mean_user_count[i], ")"))
}

vensim_string <- c(vensim_string[-length(vensim_string)])
vensim_string <- vensim_string %>% paste0(collapse = ", ")
vensim_string





