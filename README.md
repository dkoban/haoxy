Hoaxy Data Pipeline
================

The following document describes an automated workflow for the following
tasks:

1.  Collect Tweet data from Hoaxy
2.  Enrich Twitter user accounts with profile information
3.  Generate time series data for us in Vensim

## Load libraries and functions

``` r
library(tidyverse)
library(stringr)
library(hoaxy)
library(RColorBrewer)
library(scales)
source("~/Documents/hoaxy/functions.R")
hoaxy_key('')
```

## Query Hoaxy for recent articles

``` r
articles <- hx_latest_articles(past_hours = 30)
articles$tag <- extract_misinfo_tags(articles)
```

## Filter article list for likely misinformation stories

``` r
misinfo_tags <- c("clickbait", "conspiracy", "junksci", 
                  "junkscience", "hoax", "fake")
articles <- articles %>% filter(tag %in% misinfo_tags)
articles$tag %>% table()
```

    ## .
    ##  clickbait conspiracy       hoax 
    ##        298        197         13

## Query articles for Hoaxy edges

Print sample headlines categorized as conspiracies.

``` r
articles <- articles %>% filter(tag == "conspiracy")
articles$title[1:10]
```

    ##  [1] "PizzaGate Is the Silver Bullet! Exposed as CIA Global Pedo-Operation! | Alternative"                                                          
    ##  [2] "US Just Admitted \"ISIS HQ\" They Blew Up Was Actually an Innocent Family's Home"                                                             
    ##  [3] "Shocking Study Shows Fracking Is Depleting US Drinking Water Sources at a Catastrophic Rate"                                                  
    ##  [4] "BREAKING – The fight for America begins: Trump to invoke Insurrection Act that authorizes National Guard, military action inside U.S. borders"
    ##  [5] "Daily Gun Deals: CMMG AR15 22LR Bravo Rifle Conversion Kit +3 Mags $219.95 FREE S&H"                                                          
    ##  [6] "Major Hollywood Pedophile Ring Bust"                                                                                                          
    ##  [7] "Trump Win Validated by Quantum Blockchain System Recount of Votes | Politics"                                                                 
    ##  [8] "Money Spent by Both Parties in 2020 Election Could've Nearly Ended Homelessness in US"                                                        
    ##  [9] "Boom! Tucker's Brilliant Summation! How Is He Still On Fox After He Ripped Their Guts Out?! Must Watch! | Politics"                           
    ## [10] "X22Report: The Deep State/MSM Start To Shift Their Narrative, Dark Winter, The World Is Watching!"

Query Hoaxy for edges

``` r
edges <- list()
for (i in 1:10){
edges[[i]] <- hx_edges(articles$id[i], nodes_limit = 50000)
print(paste0("Query ", i, " complete: ", nrow(edges[[i]]), " records"))
}
```

    ## [1] "Query 1 complete: 184 records"
    ## [1] "Query 2 complete: 113 records"
    ## [1] "Query 3 complete: 385 records"
    ## [1] "Query 4 complete: 8 records"
    ## [1] "Query 5 complete: 1 records"
    ## [1] "Query 6 complete: 166 records"
    ## [1] "Query 7 complete: 3924 records"
    ## [1] "Query 8 complete: 10 records"
    ## [1] "Query 9 complete: 0 records"
    ## [1] "Query 10 complete: 4 records"

The story with the most tweet activity
    was:

``` r
articles$title[edges %>% lapply(nrow) %>% unlist() %>% which.max()]
```

    ## [1] "Trump Win Validated by Quantum Blockchain System Recount of Votes | Politics"

## Hoaxy edge data

List column
names

``` r
top_article <- edges[[edges %>% lapply(nrow) %>% unlist() %>% which.max()]]
top_article <- top_article %>% as_tibble()
top_article %>% colnames()
```

    ##  [1] "canonical_url"         "date_published"        "domain"               
    ##  [4] "from_user_id"          "from_user_screen_name" "id"                   
    ##  [7] "is_mention"            "site_type"             "title"                
    ## [10] "to_user_id"            "to_user_screen_name"   "tweet_created_at"     
    ## [13] "tweet_id"              "tweet_type"            "url_id"

Count tweet activity by type

``` r
top_article %>% count(tweet_type)
```

    ## # A tibble: 4 x 2
    ##   tweet_type     n
    ##   <chr>      <int>
    ## 1 origin       138
    ## 2 quote        486
    ## 3 reply       1612
    ## 4 retweet     1688

Plot tweet activty by date and time

``` r
top_article <- top_article %>% 
  mutate(tweet_created_at = tweet_created_at %>% 
           as.POSIXct(format = "%Y-%m-%dT%H:%M:%S.000Z"))

ggplot(data = top_article,
         mapping = aes(x = tweet_created_at,
                       y = tweet_type)) +
    geom_jitter(size = 0.2) + 
    scale_x_datetime(breaks=date_breaks("24 hour"), labels=date_format("%m-%d")) +
    labs(title = top_article$title[1],
         x = "Tweet Created At",
         y = "Tweet Type")
```

![](DataPipleline_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
