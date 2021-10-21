Socmed
================

-   [Intro](#intro)
-   [Requirements](#requirements)
-   [Read in Data](#read-in-data)
-   [Exploratory Data Anaysis](#exploratory-data-anaysis)

## Intro

placeholder

## Requirements

``` r
library(tidyverse)
```

## Read in Data

``` r
# read entire dataset
  # subset to get the data channel we want
    # remove unnessary columns
df <- read_csv('data/OnlineNewsPopularity.csv') %>%
        filter((!!sym(paste0('data_channel_is_', params$channel))) == 1) %>%
          select(-contains('data_channel_is'))
```

    ## Rows: 39644 Columns: 61

    ## ── Column specification ──────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): url
    ## dbl (60): timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_words, n_non_stop_unique_tokens, nu...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dim(df)
```

    ## [1] 2323   55

## Exploratory Data Anaysis

``` r
# simple scatter plot
g1 <- df %>% ggplot(aes(x=num_hrefs, y=shares)) +
        geom_point(size=2, shape=23) +
        ylim(0, 10000)
g1
```

    ## Warning: Removed 130 rows containing missing values (geom_point).

![](socmed_files/figure-gfm/1_eda-1.png)<!-- -->

``` r
# histogram for day of week vs shares
# first create a variable that holds day of week
levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday','Thursday', 'Friday', 'Saturday')
df <- df %>% mutate(weekday = ifelse(weekday_is_monday==1, 'Monday', 
                                ifelse(weekday_is_tuesday==1, 'Tuesday',
                                ifelse(weekday_is_wednesday==1, 'Wednesday',
                                ifelse(weekday_is_thursday==1, 'Thursday', 
                                ifelse(weekday_is_friday==1, 'Friday',
                                ifelse(weekday_is_saturday==1, 'Saturday', 'Sunday'
                                       ))))))) %>%
                mutate(weekday = factor(weekday, levels = levels))

g2 <- df %>% ggplot(aes(x=weekday, y=shares)) +
        geom_bar(stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = .75)) +
        ggtitle('Day of Week and Total Number of Shares')
g2
```

![](socmed_files/figure-gfm/2_eda-1.png)<!-- -->
