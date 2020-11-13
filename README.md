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
source("~/Documents/hoaxy/functions.R")
hoaxy_key('')
```

## Query Hoaxy for recent articles

``` r
articles <- hx_latest_articles(past_hours = 30)
articles$tag <- extract_misinfo_tags(articles)
```

## Filter articles to likely misinformation stories

``` r
misinfo_tags <- c("clickbait", "conspiracy", "junksci", 
                  "junkscience", "hoax", "fake")
articles <- articles %>% filter(tag %in% misinfo_tags)
articles$tag %>% table()
```

    ## .
    ##  clickbait conspiracy       hoax 
    ##        283        195         13
