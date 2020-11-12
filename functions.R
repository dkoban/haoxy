# Function to parse hoaxy twitter records into time-series cascade data
bin_empirical_data <- function(file_path){
  empirical_data <- read_csv(file_path)
  empirical_data <- empirical_data %>% 
    mutate(h_hour = min(tweet_created_at)) %>%
    mutate(h = as.numeric(difftime(tweet_created_at, h_hour), units = "hours"))
  
  time_increments <- empirical_data$h %>% unique()
  cascade_df <- list()
  for (i in 1:length(time_increments)){
    cascade_df[[i]] <- tibble(t = time_increments[i],
                              user_count = 
                                c(empirical_data %>% 
                                    filter(h <= time_increments[i]) %>% 
                                    .$from_user_id %>%
                                    unique() %>%
                                    length(),
                                  empirical_data %>% 
                                    filter(h <= time_increments[i]) %>% 
                                    .$to_user_id %>%
                                    unique()) %>%
                                length(),
                              title = empirical_data$title)
  }
  
  cascade_df <- bind_rows(cascade_df)
  cascade_df
}

# parse article tags from hoaxy article list
extract_misinfo_tag <- function(x){
  tag <- NA
  if ('clickbait' %in% unlist(x)) {tag = "clickbait"}
  if ('fake' %in% unlist(x)) {tag = "fake"}
  if ('hoax' %in% unlist(x)) {tag = "hoax"}
  if ('junksci' %in% unlist(x)) {tag = "junksci"}
  if ('junkscience' %in% unlist(x)) {tag = "junkscience"}
  if ('rumor' %in% unlist(x)) {tag = "rumor"}
  if ('satire' %in% unlist(x)) {tag = "satire"}
  if ('unreliable' %in% unlist(x)) {tag = "unreliable"}
  if ('conspiracy' %in% unlist(x)) {tag = "conspiracy"}
  tag
}

# Plot edge data by tweet types
plot_tweet_activity_by_type <- function(edges){
  
  edges$tweet_created_at <- edges$tweet_created_at %>% 
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%S.000Z")
  
  ggplot(data = edges,
         mapping = aes(x = tweet_created_at,
                       y = tweet_type)) +
    geom_jitter(size = 0.2) + 
    scale_x_datetime(breaks=date_breaks("24 hour"), labels=date_format("%m-%d")) +
    labs(title = edges$title,
         x = "Tweet Created At",
         y = "Tweet Type")
}

# DDS for logistic growth
logistic_growth <- function(p0, baseline_r, novel_exposure_limit, n){
  pn = p0
  i = 1
  while (i <= n) {
    for (i in 1:n) {
      pn = pn + pn*baseline_r*(1-pn/novel_exposure_limit)
      i = i + 1}
  }
  return(pn)}

# plot theoretical model
plot_theortical <- function(empirical_data, baseline_tweet_likelihood){
  
  p0 = empirical_data$user_count[empirical_data$t == 0]
  time_period = 0:ceiling(max(empirical_data$t))
  novel_exposure_limit = max(empirical_data$user_count)
  
  plotdf <- data_frame(t = time_period,
                       user_count = sapply(t, 
                                           logistic_growth, 
                                           p0 = p0, 
                                           baseline_r = baseline_tweet_likelihood,
                                           novel_exposure_limit = novel_exposure_limit))
  
  ggplot(data = plotdf,
         mapping = aes(x = t, y = user_count)) +
    geom_line() + 
    labs(x = "Time (Hours)", y = "Cascade Size (Number of Exposed Users)", 
         title = paste0(empirical_data$title[1]))
  }

# plot theoretical model with observed data
plot_empirical_theoretical <- function(empirical_data, baseline_tweet_likelihood){
  empirical <- empirical_data
  theoretical <- data_frame(t = 0:round(max(empirical_data$t)),
                            user_count = sapply(t, 
                                                logistic_growth, 
                                                p0 = empirical_data$user_count[empirical_data$t == 0] , 
                                                baseline_r = baseline_tweet_likelihood,
                                                novel_exposure_limit = max(empirical_data$user_count)))  
  ggplot(data = empirical_data,
         mapping = aes(x = t, y = user_count)) +
    geom_point(size = 0.25) +
    geom_line(data = theoretical,
              mapping = aes(x = t, y = user_count), 
              color = "blue") +
    labs(x = "Time (Hours)", y = "Cascade Size (Number of Exposed Users)", 
         title = paste0(empirical_data$title[1]))
  }

# Plot empirical theoretical data with a facet grid
plot_empirical_theoretical_facet <- function(empirical_data, baseline_tweet_likelihood){
  
  titles <- empirical_data$title %>% unique()
  models <- list()
  for (i in 1:length(titles)){
    models[[i]] <- data_frame(t = 0:round(max(empirical_data[empirical_data$title==titles[i], ]$t)),
                         user_count = sapply(t, 
                                             logistic_growth, 
                                             p0 = empirical_data$user_count[empirical_data$t == 0 & 
                                                                              empirical_data$title == titles[i]] , 
                                             baseline_r = baseline_tweet_likelihood,
                                             novel_exposure_limit = max(empirical_data$user_count[empirical_data$title == titles[i]])),
                         title = titles[i],
                         type = "model")  
  }
  models <- bind_rows(models)
  stacked_df <- bind_rows(empirical_data %>% mutate(type = "empirical"), models) %>%
    filter(t <= 72)
  
  ggplot(data = stacked_df,
         mapping = aes(x = t, y = user_count)) +
    geom_point(data = stacked_df %>% filter(type == "empirical"), 
               size = 0.25) +
    geom_line(data = stacked_df %>% filter(type == "model"),
              mapping = aes(x = t, y = user_count), 
              color = "blue") +
    labs(x = "Time (Hours)", y = "Cascade Size (Number of Exposed Users)") +
    facet_wrap(.~title, ncol = 3,
               labeller = labeller(title = label_wrap_gen(120),
                                   multi_line = TRUE)) +
    theme(strip.text = element_text(size = 6))
}

