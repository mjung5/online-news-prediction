Lifestyle
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

    ## [1] 2099   55

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
|  2099 |  28 |   1700 | 3682.12 | 208300 | 2150 | 8885.02 |

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
| Sunday    |        795979 |        3790 |       33100 |
| Monday    |       1399319 |        4346 |      196700 |
| Tuesday   |       1386933 |        4152 |      208300 |
| Wednesday |       1231194 |        3173 |       73100 |
| Thursday  |       1253096 |        3500 |       56000 |
| Friday    |        922890 |        3026 |       40400 |
| Saturday  |        739366 |        4062 |       43000 |

``` r
df %>% 
  group_by(Popularity) %>%
  summarise(total_shares = sum(shares), avg_shares = round(mean(shares)), max_shares = max(shares))
```

    ## # A tibble: 4 x 4
    ##   Popularity         total_shares avg_shares max_shares
    ##   <ord>                     <dbl>      <dbl>      <dbl>
    ## 1 Not at all popular       265793        699        945
    ## 2 Not too popular          620086       1195       1400
    ## 3 Somewhat popular        1187200       2002       2800
    ## 4 Very popular            5655698       9317     208300

### Number of links

``` r
# simple scatter plot
g1 <- df %>% ggplot(aes(x=num_hrefs, y=shares)) +
        geom_point(size=2, shape=23) +
        ylim(0, 10000)
g1
```

    ## Warning: Removed 141 rows containing missing values (geom_point).

![](lifestyle_files/figure-gfm/1_eda-1.png)<!-- -->

### Days of week

``` r
# histogram for day of week vs shares

g2 <- df %>% ggplot(aes(x=weekday, y=shares)) +
        geom_bar(stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = .75)) +
        ggtitle('Day of Week and Total Number of Shares')
g2
```

![](lifestyle_files/figure-gfm/2_eda-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

### Correlation with numeric variables

``` r
knitr::kable(round(cor(df[ , c(3:4, 10:11)]), 2))
```

|                    | n\_tokens\_title | n\_tokens\_content | num\_imgs | num\_videos |
|:-------------------|-----------------:|-------------------:|----------:|------------:|
| n\_tokens\_title   |             1.00 |               0.01 |     -0.02 |        0.01 |
| n\_tokens\_content |             0.01 |               1.00 |      0.46 |        0.04 |
| num\_imgs          |            -0.02 |               0.46 |      1.00 |       -0.06 |
| num\_videos        |             0.01 |               0.04 |     -0.06 |        1.00 |
