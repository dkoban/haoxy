##################
# Define functions
##################
source("~/Documents/hoaxy/functions.R")

################
# Install Hoaxy
################
#library(devtools)
#install_local("~/Documents/EM6740/cascade_data/hoaxy-master", force = TRUE)

#########################################################################################
# Get Hoaxy Data - COVID
#########################################################################################
library(tidyverse)
library(stringr)
library(hoaxy)
library(RColorBrewer)

# query Hoaxy for titles
hoaxy_key('')
#covid_articles <- hx_articles("covid")
latest_articles <- hx_latest_articles(past_hours = 30)

# extract tags
latest_articles$tag <- NA
for (i in 1:length(latest_articles$site_tags)){
  latest_articles$tag[i] <- extract_misinfo_tag(latest_articles$site_tags[i])
}

# extract tags; filter on non NA tags
latest_articles$tag %>% table()
latest_articles <- latest_articles %>% filter(tag %in% c("clickbait", "conspiracy",
                                                         "junksci", "junkscience",
                                                         "hoax", "fake"))

# batch titles
breaks <- c(0, seq(10, 100*round((nrow(latest_articles)/10)), by = 10))
batches <- list()
for (i in 1:round((nrow(latest_articles)/10))){
  print(paste0("Batch ", i, ": articles ", breaks[i], "-", breaks[i+1]))
  batches[[i]] <- latest_articles[breaks[i]:breaks[i+1],]
}

# query Hoaxy for edges
edges <- list()
for (i in 1:length(batches)){
  temp <- hx_edges(batches[[i]]$id) %>% as_tibble()
  temp <- temp %>% mutate(tag = batches[[i]]$tag[1])
  edges[[i]] <- temp
  print(i)
}
edges <- dplyr::bind_rows(edges)

# filter to include only articles with at least 100 tweets
article_counts <- edges %>% group_by(title, tag) %>% count() %>% 
  arrange(desc(n)) 
edges_filtered <- edges %>% 
  filter(title %in% article_counts$title[article_counts$n >= 100])

# write data to files
stories <- edges_filtered $title %>% unique()
for (i in 1:length(stories)){
  file_name = paste0("~/Documents/hoaxy/top_stories_10Nov/", 
                     stories[i] %>% str_sub(1, 60) %>% 
                       str_replace_all("([[:punct:]])", "") %>%
                       str_replace_all(" ", "_"), ".csv")
  write.csv(edges[edges$title == stories[i], ], 
            file = file_name, 
            row.names = FALSE)}

##################
# Parse Hoaxy Data
##################
files <- list.files("~/Documents/hoaxy/top_stories_10Nov/")
cascades <- list()
for (i in 1:length(files)){
  cascades[[i]] <- bin_empirical_data(
    paste0("~/Documents/hoaxy/top_stories_10Nov/", files[i]))
}
cascades <- bind_rows(cascades)
cascades <- cascades %>% distinct()
cascades <- cascades %>% filter(title != "TheBlaze")
cascades <- cascades %>% filter(t <= 120)
write.csv(cascades, file = "~/Documents/hoaxy/cascades.csv", row.names=FALSE)

library(plotly)
p <- ggplot(data = cascades,
            mapping = aes(x = t, y = user_count, color = title)) +
  geom_point(size = 0.2) +
  geom_line() +
  scale_color_manual(values = rep(brewer.pal(12, "Paired"), length(unique(cascades$title)))) +
  guides(color = FALSE) +
  labs(title = "Cascade Size and Duration for Hoaxy Stories tweeted at least 100 times",
       x = "Hours",
       y = "Cascade Size") 

ggplotly(p)

#########################################################################################
# Get Hoaxy Data - Vaccines
#########################################################################################
# Query Hoaxy API for articles by topic
vaccine_articles <- hx_articles("vaccine")
covid_vaccine_articles <- hx_articles("vaccine OR COVID")

