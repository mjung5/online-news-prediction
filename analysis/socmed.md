Socmed
================

-   [Intro](#intro)
-   [Requirements](#requirements)
-   [Data manipulaton](#data-manipulaton)
    -   [Read in Data](#read-in-data)
-   [Summarizations](#summarizations)
    -   [Exploratory Data Anaysis](#exploratory-data-anaysis)
    -   [Shares by week of day](#shares-by-week-of-day)
    -   [Number of links](#number-of-links)
    -   [Days of week](#days-of-week)
    -   [Number of words in the title and
        content](#number-of-words-in-the-title-and-content)
    -   [Correlation with numeric
        variables](#correlation-with-numeric-variables)

## Intro

placeholder

## Requirements

``` r
library(tidyverse)
```

## Data manipulaton

### Read in Data

``` r
# read entire dataset
  # subset to get the data channel we want
    # remove unnessary columns
df <- read_csv('data/OnlineNewsPopularity.csv') %>%
        filter((!!sym(paste0('data_channel_is_', params$channel))) == 1) %>%
          select(-contains('data_channel_is'))
```

    ## Rows: 39644 Columns: 61

    ## -- Column specification ---------------------------------------------------------
    ## Delimiter: ","
    ## chr  (1): url
    ## dbl (60): timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dim(df)
```

    ## [1] 2323   55

``` r
# Create a variable that holds day of week
levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday','Thursday', 'Friday', 'Saturday')
df <- df %>% mutate(weekday = ifelse(weekday_is_monday==1, 'Monday', 
                                ifelse(weekday_is_tuesday==1, 'Tuesday',
                                ifelse(weekday_is_wednesday==1, 'Wednesday',
                                ifelse(weekday_is_thursday==1, 'Thursday', 
                                ifelse(weekday_is_friday==1, 'Friday',
                                ifelse(weekday_is_saturday==1, 'Saturday', 'Sunday'
                                       ))))))) %>%
                mutate(weekday = factor(weekday, levels = levels))


# function to create the popularity column. 
# popularity rating was created with summary stat info (25%, median, and 75%)
popularityCol <- function(dataset){
  dataset <- dataset %>% 
    mutate("Popularity" = if_else(shares > 2800, "Very popular",
                           if_else(shares > 1400, "Somewhat popular", 
                            if_else(shares > 946, "Not too popular", "Not at all popular")) 
                           )
    )            
  return(dataset)
}

# Data set using popularityCol function.
df <- popularityCol(df) %>% as_tibble()

# Overwrite popularity column with factor version
df$Popularity <- as.factor(df$Popularity)

# Use ordered function on a factor to order the levels
df$Popularity <- ordered(df$Popularity, levels = c("Not at all popular", "Not too popular", "Somewhat popular", "Very popular"))


#Channel <- params$channel

#df <- filter(df, channel)

#split data intro train and test sets
#train_rows <- sample(nrow(df), 0.7*nrow(df))
#trainData <- df[train_rows,] %>%
#testData <- df[-train_rows,] 
```

## Summarizations

### Exploratory Data Anaysis

The summary statistics of targeted variable (shares)

``` r
# summary statistics
share_stat <- df %>% 
                summarise(Count = n(),
                          Min = min(shares), 
                          Median = median(shares),
                          Average = mean(shares),
                          Max = max(shares),
                          IQR = IQR(shares),
                          Std.Dev = sd(shares)
                          )

# Display a table of the summary stats.
knitr::kable(share_stat, caption = "Summary Stats by shares", digits = 2)
```

| Count | Min | Median | Average |    Max |  IQR | Std.Dev |
|------:|----:|-------:|--------:|-------:|-----:|--------:|
|  2323 |   5 |   2100 | 3629.38 | 122800 | 2400 | 5524.17 |

Summary Stats by shares

### Shares by week of day

``` r
df %>%
  group_by(weekday) %>%
  summarise(total_shares = sum(shares), avg_shares = round(mean(shares)), max_shares = max(shares)) %>%
  knitr::kable()
```

| weekday   | total\_shares | avg\_shares | max\_shares |
|:----------|--------------:|------------:|------------:|
| Sunday    |        619973 |        4525 |       54100 |
| Monday    |       1351519 |        4010 |       57600 |
| Tuesday   |       1604507 |        3503 |      122800 |
| Wednesday |       1459540 |        3509 |       59000 |
| Thursday  |       1431674 |        3092 |       26900 |
| Friday    |       1332276 |        4013 |       57000 |
| Saturday  |        631568 |        3509 |       34500 |

``` r
df %>% 
  group_by(Popularity) %>%
  summarise(total_shares = sum(shares), avg_shares = round(mean(shares)), max_shares = max(shares))
```

    ## # A tibble: 4 x 4
    ##   Popularity         total_shares avg_shares max_shares
    ##   <ord>                     <dbl>      <dbl>      <dbl>
    ## 1 Not at all popular       119409        678        940
    ## 2 Not too popular          590348       1210       1400
    ## 3 Somewhat popular        1690400       2044       2800
    ## 4 Very popular            6030900       7249     122800

### Number of links

``` r
# simple scatter plot
g1 <- df %>% ggplot(aes(x=num_hrefs, y=shares)) +
        geom_point(size=2, shape=23) +
        ylim(0, 10000)
g1
```

    ## Warning: Removed 130 rows containing missing values (geom_point).

![](socmed_files/figure-gfm/1_eda-1.png)<!-- -->

### Days of week

``` r
# histogram for day of week vs shares

g2 <- df %>% ggplot(aes(x=weekday, y=shares)) +
        geom_bar(stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = .75)) +
        ggtitle('Day of Week and Total Number of Shares')
g2
```

![](socmed_files/figure-gfm/2_eda-1.png)<!-- -->

### Number of words in the title and content

``` r
#scatter plots of Number of words in the title
g3 <- ggplot(data = df, aes(x =  n_tokens_title, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Word count in the title") + 
     geom_smooth(method = lm, color = "blue")  
g3
```

    ## `geom_smooth()` using formula 'y ~ x'

![](socmed_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#scatter plots of Number of words in the content
g4 <- ggplot(data = df, aes(x =  n_tokens_content, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Word count in the content") + 
      geom_smooth(method = lm, color = "blue")  
g4
```

    ## `geom_smooth()` using formula 'y ~ x'

![](socmed_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

### Correlation with numeric variables

``` r
knitr::kable(round(cor(df[ , c(3:4, 10:11)]), 2))
```

|                    | n\_tokens\_title | n\_tokens\_content | num\_imgs | num\_videos |
|:-------------------|-----------------:|-------------------:|----------:|------------:|
| n\_tokens\_title   |             1.00 |              -0.02 |     -0.02 |       -0.02 |
| n\_tokens\_content |            -0.02 |               1.00 |      0.52 |       -0.02 |
| num\_imgs          |            -0.02 |               0.52 |      1.00 |       -0.10 |
| num\_videos        |            -0.02 |              -0.02 |     -0.10 |        1.00 |
