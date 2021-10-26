Socmed
================

-   [Intro](#intro)
-   [Requirements](#requirements)
-   [Data manipulaton](#data-manipulaton)
    -   [Read in Data](#read-in-data)
-   [Summarizations](#summarizations)
    -   [Exploratory Data Anaysis](#exploratory-data-anaysis)
    -   [Shares by days of week](#shares-by-days-of-week)
    -   [shares by popularity](#shares-by-popularity)
    -   [count of news by popularity over different days of
        week](#count-of-news-by-popularity-over-different-days-of-week)
    -   [Days of week](#days-of-week)
    -   [Number of links](#number-of-links)
    -   [Number of words in the title and
        content](#number-of-words-in-the-title-and-content)
    -   [Unique words count](#unique-words-count)
    -   [Number of image and video](#number-of-image-and-video)
    -   [Number of positive and negative words
        rate](#number-of-positive-and-negative-words-rate)
    -   [Title subjectivity](#title-subjectivity)
    -   [Number of positive and negative words
        rate](#number-of-positive-and-negative-words-rate-1)
    -   [Correlation with numeric
        variables](#correlation-with-numeric-variables)
-   [Modeling](#modeling)
    -   [Linear Regression](#linear-regression)
    -   [Ensemble Tree](#ensemble-tree)
-   [Comparison](#comparison)

## Intro

This project aims at building predictive models on the [Online News
Popularity](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity#)
dataset. Our goal is to build models that predict the number of shares
an article receives based on characteristics of the article. This work
is important to help writers/companies understand factors that influence
article success (as measured by the number of shares an article
obtains). This work could also be integrated in the calculation of
advertisement space on articles (articles that receive more shares
should demand more for ad space).

The characteristics we will explore include day of week, number of
links, word count of title, word count of content

-   `weekday`: day of week that article was published (Monday, Tuesday,
    …)
-   `num_hrefs`: number of links referenced in article
-   `n_tokens_title`: word count of title
-   `n_tokens_content`: word count of article
-   `rate_positive_words` and `rate_negative_words`: rate of
    positive/negative words among non-neutral tokens

The models used to build … continue from here Data was split by channel
type…

## Requirements

``` r
library(tidyverse)
library(corrplot)
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

    ## -- Column specification ---------------------------------------------------------------------------------------------------------------------------------------
    ## Delimiter: ","
    ## chr  (1): url
    ## dbl (60): timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_words, n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_imgs, nu...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dim(df)
```

    ## [1] 2323   55

The object `df` now holds a subset of the data according to the
specified data channel. Now, we must create new features to consolidate
variables and prepare the dataset for exploratory data analysis:

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


# Deleting variables
#df <- df[-1:2, 34:38]
```

Our data set contains 30 000 instances; each consists of 60 attributes
and one dependent variable, the number of shares of the article.

The following variables can be omitted:

url: URL of the article (non-predictive) timedelta: Days between the
article publication and the dataset acquisition (non-predictive) five
LDA variables is\_weekend, since it seems to be duplicating days of week
kw\_min\_min, kw\_avg\_min, kw\_min\_avg have a number of negative
values

We now split the data into train and test sets for predicitve modeling.

``` r
set.seed(123)
# split data into train and test sets
train_rows <- sample(nrow(df), nrow(df)*0.7)
trainData <- df[train_rows,]
testData <- df[-train_rows,] 
```

## Summarizations

### Exploratory Data Anaysis

The summary statistics of targeted variable (shares)

``` r
# summary statistics
share_stat <- df %>% 
                summarise(Count = n(),
                          Min = min(shares), 
                          Q1 = quantile(shares, 0.25),
                          Median = median(shares),
                          Average = mean(shares),
                          Q3 = quantile(shares, 0.75),
                          Max = max(shares),
                          Std.Dev = sd(shares)
                          )

# Display a table of the summary stats.
knitr::kable(share_stat, caption = "Summary Stats by shares", digits = 2)
```

| Count | Min |   Q1 | Median | Average |   Q3 |    Max | Std.Dev |
|------:|----:|-----:|-------:|--------:|-----:|-------:|--------:|
|  2323 |   5 | 1400 |   2100 | 3629.38 | 3800 | 122800 | 5524.17 |

Summary Stats by shares

### Shares by days of week

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

The above table shows a breakdown of total, average, and maximum number
of shares for articles published on a specific weekday for this channel.
Some channels tend to have more popular days than others.

### shares by popularity

``` r
df %>% 
  group_by(Popularity) %>%
  summarise(total_shares = sum(shares), avg_shares = round(mean(shares)), max_shares = max(shares)) %>%
  knitr::kable()
```

| Popularity         | total\_shares | avg\_shares | max\_shares |
|:-------------------|--------------:|------------:|------------:|
| Not at all popular |        119409 |         678 |         940 |
| Not too popular    |        590348 |        1210 |        1400 |
| Somewhat popular   |       1690400 |        2044 |        2800 |
| Very popular       |       6030900 |        7249 |      122800 |

The above table show a summary of the newly created `popularity`
variable.

### count of news by popularity over different days of week

``` r
#Bar plot of weekday by popularity 

ggplot(data = df, aes(x = weekday)) +
  geom_bar(aes(fill = as.factor(Popularity))) + 
  labs(x = "Days of week", 
       title = "Days of week by popularity") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_discrete(name = "Popularity") 
```

![](socmed_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The bar plot above shows a breakdown of the number of articles published
vs day of week. We can also see a breakdown of the proportion of
popularity of articles for each day. This plot is important to
understand if a greater number of shares exhibited by a day of the week
is due to more articles being published on that day, or if it is due to
that day of week having a direct effect on the number of shares.

For example, if all days have the same number of ‘very popular’
articles, we could hypothesize that day of week does not have an effect
on producing ‘very popular’ articles.

### Days of week

``` r
# histogram for day of week vs shares

g1 <- df %>% ggplot(aes(x=weekday, y=shares)) +
        geom_bar(stat="identity", fill = "darkblue") + 
   theme(axis.text.x = element_text(angle = 45, vjust = .75)) +
        ggtitle('Day of Week and Total Number of Shares')
g1
```

![](socmed_files/figure-gfm/2_eda-1.png)<!-- -->

The above histogram shows a basic count of the total number of shares
for all articles published on each day of week.

### Number of links

``` r
# simple scatter plot
g2 <- df %>% ggplot(aes(x=num_hrefs, y=shares)) +
        geom_point(size=2, shape=23) +
        ylim(0, 10000) +
        ggtitle("Number of links")  
g2
```

    ## Warning: Removed 130 rows containing missing values (geom_point).

![](socmed_files/figure-gfm/1_eda-1.png)<!-- -->

In the above scatter, we compare the number of links in an article to
its shares. This plot is motivated by the implementation of Google’s
[PageRank Algorithm](https://en.wikipedia.org/wiki/PageRank).

### Number of words in the title and content

``` r
library(cowplot)

#scatter plots of Number of words in the title
g3 <- ggplot(data = df, aes(x =  n_tokens_title, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Word count in the title")  


#scatter plots of Number of words in the content
g4 <- ggplot(data = df, aes(x =  n_tokens_content, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Word count in the content")

plot_grid(g3, g4,  labels = c('A', 'B'))   
```

![](socmed_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> Graph A shows
the number of words in the title compared to the number of shares.
Perhaps a quadratic relationship is appropriate for this variable if a
bell shape appears in the plot. Graph B shows a comparison of the number
of words that appear in the article compared to the number of shares.
Perhaps a negative linear relationship is appropriate if the data
exihibits a slightly negative slops.

### Unique words count

``` r
ggplot(data = df, aes(x =  n_unique_tokens, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Unique Word count")
```

![](socmed_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Number of image and video

``` r
#scatter plots of Number of words in the content
g5 <- ggplot(data = df, aes(x =  num_imgs, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of image")


#scatter plots of Number of words in the content
g6 <- ggplot(data = df, aes(x =  num_videos, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of video")


plot_grid(g5, g6,  labels = c('C', 'D')) 
```

![](socmed_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> \#\#\# Number
of keywords

``` r
ggplot(data = df, aes(x =  num_keywords, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of keywords")
```

![](socmed_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Number of positive and negative words rate

``` r
#scatter plots of positive and negative words rate
g7 <- ggplot(data = df, aes(x =  rate_positive_words, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Positive words rate")


#scatter plots of Number of words in the content
g8 <- ggplot(data = df, aes(x =  rate_negative_words, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Negative words rate")


plot_grid(g7, g8,  labels = c('A', 'B')) 
```

![](socmed_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Title subjectivity

``` r
#scatter plot of title subjectivity
ggplot(data = df, aes(x =     title_subjectivity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("title subjectivity")
```

![](socmed_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Number of positive and negative words rate

``` r
#scatter plots of positive and negative words rate
g9 <- ggplot(data = df, aes(x =  avg_positive_polarity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Average positive polarity")


#scatter plots of Number of words in the content
g10 <- ggplot(data = df, aes(x =  avg_negative_polarity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Average negative polarity")


plot_grid(g9, g10,  labels = c('A', 'B')) 
```

![](socmed_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Correlation with numeric variables

``` r
df_tmp <- df %>% select(c('n_tokens_title', 
                          'n_tokens_content',
                          'n_unique_tokens',
                          'num_hrefs',
                          'num_imgs',
                          'num_videos',
                          'num_keywords',
                          'is_weekend',
                          'rate_positive_words',
                          'rate_negative_words',
                          'title_subjectivity',
                          'avg_positive_polarity',
                          'avg_negative_polarity',
                          'shares',))
corrplot(cor(df_tmp), type = 'lower', diag = FALSE)
```

![](socmed_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Above shows the correlation matrix for other numerical variables. Shares
is the bottom row. We use this plot to find other variables that might
have weak correlation with shares and make sure to include these in our
model building phase.

## Modeling

### Linear Regression

place holder

#### Linear model 1

The first linear regression model will have predictors selected by
stepwise selection. After choosing the subset of predictors, we will use
repeated cross-validation with 10 folder and will find the RMSE and R2.

``` r
# get all numeric columns
train_df <- trainData[ ,unlist(lapply(trainData, is.numeric))]
test_df <- testData[ ,unlist(lapply(testData, is.numeric))]

# Stepwise model selection
lmFitSelect <- lm(shares ~ n_tokens_title + n_tokens_content+ is_weekend +  num_hrefs+  num_imgs+ num_videos+num_keywords+ rate_positive_words + title_subjectivity  + I(n_tokens_content^2) + I(num_imgs^2)+ I(num_videos^2) + I(num_hrefs^2) , data = train_df)
models <- step(lmFitSelect)
```

    ## Start:  AIC=27851.47
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_imgs + num_videos + num_keywords + rate_positive_words + 
    ##     title_subjectivity + I(n_tokens_content^2) + I(num_imgs^2) + 
    ##     I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                         Df Sum of Sq        RSS   AIC
    ## - I(num_hrefs^2)         1     27791 4.3913e+10 27850
    ## - num_imgs               1   1028999 4.3914e+10 27850
    ## - num_videos             1   6141234 4.3920e+10 27850
    ## - I(num_videos^2)        1   8858395 4.3922e+10 27850
    ## - num_hrefs              1  19433157 4.3933e+10 27850
    ## - I(n_tokens_content^2)  1  20377012 4.3934e+10 27850
    ## - n_tokens_title         1  20920268 4.3934e+10 27850
    ## - num_keywords           1  24927711 4.3938e+10 27850
    ## - I(num_imgs^2)          1  40712686 4.3954e+10 27851
    ## - is_weekend             1  43299627 4.3957e+10 27851
    ## <none>                               4.3913e+10 27852
    ## - rate_positive_words    1  88733172 4.4002e+10 27853
    ## - n_tokens_content       1 119004784 4.4032e+10 27854
    ## - title_subjectivity     1 136955588 4.4050e+10 27855
    ## 
    ## Step:  AIC=27849.48
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_imgs + num_videos + num_keywords + rate_positive_words + 
    ##     title_subjectivity + I(n_tokens_content^2) + I(num_imgs^2) + 
    ##     I(num_videos^2)
    ## 
    ##                         Df Sum of Sq        RSS   AIC
    ## - num_imgs               1   1002293 4.3914e+10 27848
    ## - num_videos             1   6145294 4.3920e+10 27848
    ## - I(num_videos^2)        1   8898612 4.3922e+10 27848
    ## - I(n_tokens_content^2)  1  20730815 4.3934e+10 27848
    ## - n_tokens_title         1  20898078 4.3934e+10 27848
    ## - num_keywords           1  25284948 4.3939e+10 27848
    ## - I(num_imgs^2)          1  41345210 4.3955e+10 27849
    ## - is_weekend             1  43343600 4.3957e+10 27849
    ## <none>                               4.3913e+10 27850
    ## - num_hrefs              1  68188750 4.3982e+10 27850
    ## - rate_positive_words    1  88786140 4.4002e+10 27851
    ## - n_tokens_content       1 125270844 4.4039e+10 27852
    ## - title_subjectivity     1 136938098 4.4050e+10 27853
    ## 
    ## Step:  AIC=27847.51
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_videos + num_keywords + rate_positive_words + title_subjectivity + 
    ##     I(n_tokens_content^2) + I(num_imgs^2) + I(num_videos^2)
    ## 
    ##                         Df Sum of Sq        RSS   AIC
    ## - num_videos             1   5670138 4.3920e+10 27846
    ## - I(num_videos^2)        1   8539212 4.3923e+10 27846
    ## - n_tokens_title         1  20958614 4.3935e+10 27846
    ## - I(n_tokens_content^2)  1  22323046 4.3937e+10 27846
    ## - num_keywords           1  25862101 4.3940e+10 27847
    ## - is_weekend             1  43949642 4.3958e+10 27847
    ## <none>                               4.3914e+10 27848
    ## - num_hrefs              1  67705417 4.3982e+10 27848
    ## - rate_positive_words    1  88579043 4.4003e+10 27849
    ## - n_tokens_content       1 132454958 4.4047e+10 27850
    ## - title_subjectivity     1 137856247 4.4052e+10 27851
    ## - I(num_imgs^2)          1 142608361 4.4057e+10 27851
    ## 
    ## Step:  AIC=27845.72
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_keywords + rate_positive_words + title_subjectivity + 
    ##     I(n_tokens_content^2) + I(num_imgs^2) + I(num_videos^2)
    ## 
    ##                         Df Sum of Sq        RSS   AIC
    ## - I(num_videos^2)        1   2939013 4.3923e+10 27844
    ## - n_tokens_title         1  21287555 4.3941e+10 27845
    ## - I(n_tokens_content^2)  1  22469496 4.3943e+10 27845
    ## - num_keywords           1  27761227 4.3948e+10 27845
    ## - is_weekend             1  42927227 4.3963e+10 27845
    ## <none>                               4.3920e+10 27846
    ## - num_hrefs              1  68020044 4.3988e+10 27846
    ## - rate_positive_words    1  86720886 4.4007e+10 27847
    ## - n_tokens_content       1 131106155 4.4051e+10 27849
    ## - title_subjectivity     1 140132786 4.4060e+10 27849
    ## - I(num_imgs^2)          1 143801275 4.4064e+10 27849
    ## 
    ## Step:  AIC=27843.83
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_keywords + rate_positive_words + title_subjectivity + 
    ##     I(n_tokens_content^2) + I(num_imgs^2)
    ## 
    ##                         Df Sum of Sq        RSS   AIC
    ## - n_tokens_title         1  20979393 4.3944e+10 27843
    ## - num_keywords           1  27225078 4.3950e+10 27843
    ## - I(n_tokens_content^2)  1  27437018 4.3951e+10 27843
    ## - is_weekend             1  43129570 4.3966e+10 27843
    ## <none>                               4.3923e+10 27844
    ## - num_hrefs              1  66711431 4.3990e+10 27844
    ## - rate_positive_words    1  87960365 4.4011e+10 27845
    ## - title_subjectivity     1 138500826 4.4062e+10 27847
    ## - n_tokens_content       1 138555066 4.4062e+10 27847
    ## - I(num_imgs^2)          1 141029370 4.4064e+10 27847
    ## 
    ## Step:  AIC=27842.61
    ## shares ~ n_tokens_content + is_weekend + num_hrefs + num_keywords + 
    ##     rate_positive_words + title_subjectivity + I(n_tokens_content^2) + 
    ##     I(num_imgs^2)
    ## 
    ##                         Df Sum of Sq        RSS   AIC
    ## - I(n_tokens_content^2)  1  25586596 4.3970e+10 27842
    ## - num_keywords           1  25895600 4.3970e+10 27842
    ## - is_weekend             1  40814689 4.3985e+10 27842
    ## <none>                               4.3944e+10 27843
    ## - num_hrefs              1  64471971 4.4009e+10 27843
    ## - rate_positive_words    1  85944803 4.4030e+10 27844
    ## - n_tokens_content       1 134955992 4.4079e+10 27846
    ## - title_subjectivity     1 136117220 4.4080e+10 27846
    ## - I(num_imgs^2)          1 144209254 4.4088e+10 27846
    ## 
    ## Step:  AIC=27841.55
    ## shares ~ n_tokens_content + is_weekend + num_hrefs + num_keywords + 
    ##     rate_positive_words + title_subjectivity + I(num_imgs^2)
    ## 
    ##                       Df Sum of Sq        RSS   AIC
    ## - num_keywords         1  29865084 4.4000e+10 27841
    ## - is_weekend           1  46298240 4.4016e+10 27841
    ## <none>                             4.3970e+10 27842
    ## - num_hrefs            1  63251607 4.4033e+10 27842
    ## - rate_positive_words  1  80766954 4.4050e+10 27843
    ## - title_subjectivity   1 135974484 4.4106e+10 27845
    ## - I(num_imgs^2)        1 177624459 4.4147e+10 27846
    ## - n_tokens_content     1 242099647 4.4212e+10 27849
    ## 
    ## Step:  AIC=27840.66
    ## shares ~ n_tokens_content + is_weekend + num_hrefs + rate_positive_words + 
    ##     title_subjectivity + I(num_imgs^2)
    ## 
    ##                       Df Sum of Sq        RSS   AIC
    ## - num_hrefs            1  53993568 4.4054e+10 27841
    ## <none>                             4.4000e+10 27841
    ## - is_weekend           1  55073921 4.4055e+10 27841
    ## - rate_positive_words  1  73942889 4.4073e+10 27841
    ## - title_subjectivity   1 129645740 4.4129e+10 27843
    ## - I(num_imgs^2)        1 189149226 4.4189e+10 27846
    ## - n_tokens_content     1 249390586 4.4249e+10 27848
    ## 
    ## Step:  AIC=27840.65
    ## shares ~ n_tokens_content + is_weekend + rate_positive_words + 
    ##     title_subjectivity + I(num_imgs^2)
    ## 
    ##                       Df Sum of Sq        RSS   AIC
    ## - is_weekend           1  43697864 4.4097e+10 27840
    ## <none>                             4.4054e+10 27841
    ## - rate_positive_words  1  75312603 4.4129e+10 27841
    ## - title_subjectivity   1 126704191 4.4180e+10 27843
    ## - n_tokens_content     1 196933320 4.4250e+10 27846
    ## - I(num_imgs^2)        1 321184332 4.4375e+10 27851
    ## 
    ## Step:  AIC=27840.26
    ## shares ~ n_tokens_content + rate_positive_words + title_subjectivity + 
    ##     I(num_imgs^2)
    ## 
    ##                       Df Sum of Sq        RSS   AIC
    ## <none>                             4.4097e+10 27840
    ## - rate_positive_words  1  76636258 4.4174e+10 27841
    ## - title_subjectivity   1 127354759 4.4225e+10 27843
    ## - n_tokens_content     1 215899221 4.4313e+10 27846
    ## - I(num_imgs^2)        1 334238075 4.4431e+10 27851

``` r
# train data with variables chosen by stepWise
set.seed(10)
lm.fit1 <- train(shares ~ num_imgs + num_videos + rate_positive_words + I(n_tokens_content^2) + 
                        I(num_imgs^2) + I(num_videos^2), data = train_df,
                        method="lm",
                        preProcess = c("center","scale"),
                        trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))

lm.fit1
```

    ## Linear Regression 
    ## 
    ## 1626 samples
    ##    4 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 1464, 1462, 1463, 1464, 1463, 1463, ... 
    ## Resampling results:
    ## 
    ##   RMSE     Rsquared     MAE     
    ##   5110.75  0.005811627  2699.561
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

#### Linear model 2 - Logarithmic Linear Regression

Now, let’s look at regresssing on the log-transformed target variable of
shares.

``` r
# get all numeric columns
train_df <- trainData[ ,unlist(lapply(trainData, is.numeric))]
test_df <- testData[ ,unlist(lapply(testData, is.numeric))]

# code used to get regression variables
## fit using forward selection
#forward <- regsubsets(log(shares) ~ .,
#                      data = train_df,
#                      nvmax = 10,
#                      method = "forward")
## summary
#mod_summary <- summary(forward)

# train model
lm.fit2 <- lm(log(shares) ~ n_tokens_content + num_hrefs + average_token_length + 
                        num_keywords + kw_min_min + kw_max_avg + kw_avg_avg + 
                        is_weekend + LDA_04 + global_subjectivity, 
          data = train_df
          )

          
lm.fit2
```

    ## 
    ## Call:
    ## lm(formula = log(shares) ~ n_tokens_content + num_hrefs + average_token_length + 
    ##     num_keywords + kw_min_min + kw_max_avg + kw_avg_avg + is_weekend + 
    ##     LDA_04 + global_subjectivity, data = train_df)
    ## 
    ## Coefficients:
    ##          (Intercept)      n_tokens_content             num_hrefs  average_token_length          num_keywords            kw_min_min            kw_max_avg  
    ##            7.053e+00             2.026e-04            -8.215e-03            -3.023e-03             5.979e-02             9.622e-04            -3.995e-05  
    ##           kw_avg_avg            is_weekend                LDA_04   global_subjectivity  
    ##            2.545e-04             1.045e-01            -3.224e-01            -5.662e-01

``` r
#predict on test data
#predLm2 <- predict(lm.fit2, test_df)

# calculate rmse
#rmseLm2 <- sqrt(mean((predLm2 - test_df$shares)^2))
#rmseLm2
```

### Ensemble Tree

place holder

#### Random Forest Model

``` r
# get all numeric columns
train_df <- trainData[ ,unlist(lapply(trainData, is.numeric))]
test_df <- testData[ ,unlist(lapply(testData, is.numeric))]
# tuning parameter is mtry, use values of 1,2,..,5
rfFit <- train(shares ~ .,
               data = train_df, 
               method = "rf", 
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(mtry = (1:5)))

rfFit
```

    ## Random Forest 
    ## 
    ## 1626 samples
    ##   53 predictor
    ## 
    ## Pre-processing: centered (53), scaled (53) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 1626, 1626, 1626, 1626, 1626, 1626, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared    MAE     
    ##   1     4908.548  0.08901309  2546.972
    ##   2     4868.714  0.09484941  2562.037
    ##   3     4868.993  0.09305972  2578.468
    ##   4     4876.782  0.09025143  2588.134
    ##   5     4880.607  0.09047868  2602.417
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 2.

#### Boosted Tree Model

The boosted tree is another tree-based regression model. This model aims
to predict the residuals between the number of shares (target variable)
of each observation and the average number of shares. To do this, the
model builds smaller trees of specified depth that add/subtract to
predictions with the hope that predictions move closer to their residual
values. To prevent overfitting of data, trees are constricted to a
‘shrinkage’ parameter (that takes a value between 0 and 1) which limits
the amount of boosting on predictions.

For a really good video explanation, watch
[this](https://www.youtube.com/watch?v=3CC4N4z3GJc).

``` r
# get all numeric columns
train_df <- trainData[ ,unlist(lapply(trainData, is.numeric))]
test_df <- testData[ ,unlist(lapply(testData, is.numeric))]

# declare grid of values to test in cross validation
## code retrieved from https://topepo.github.io/caret/model-training-and-tuning.html
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), # complexity of tree
                        n.trees = (1:30)*50, # number of iterations (i.e. trees)
                        shrinkage = 0.1, # learning rate
                        n.minobsinnode = 20) # minimum number of samples in a node to commence splitting

# train using crossvalidation, print out best fitting parameters
boostFit <- train(shares ~ .,
                data = train_df,
                method = "gbm",
                trControl = trainControl("cv", number = 10),
                verbose = FALSE,
                #tuneGrid = gbmGrid
                )
boostFit$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 4      50                 2       0.1             10

``` r
# evaluate on test dataset
#boostPred <- predict(boostFit, newdata = dplyr::select(test_df, -shares), n.trees = 100)
#boostRMSE <- sqrt(mean((boostPred-test_df$shares)^2))
#boostRMSE
```

## Comparison

Now, we compare the 4 models developed above. Each model was evaluated
on a test data set and Root MSE values(roof of test prediction error)
were compared.

``` r
# Predict on test data

predLm1 <- predict(lm.fit1, newdata = test_df)
predLm2 <- predict(lm.fit2, newdata = test_df)
rfPred <- predict(rfFit, newdata = test_df)
boostPred <- predict(boostFit, newdata = test_df)

# Calculate rmse

rmseLm1 <- sqrt(mean((predLm1 - test_df$shares)^2))
rmseLm2 <- sqrt(mean((predLm2 - test_df$shares)^2))
rfMSE <- sqrt(mean((rfPred - test_df$shares)^2))
boostRMSE <- sqrt(mean((boostPred - test_df$shares)^2))

rmseTotal <- data.frame('Linear Regression Model 1' = rmseLm1, 
                   'Linear Regression Model 2' = rmseLm2, 
                   'Random Forest Model' = rfMSE, 
                   'Boosting Model' = boostRMSE)

knitr::kable(t(rmseTotal),
               digits=3,
               caption="Summary Table of RMSE score",
               col.names = "RMSE")
```

|                           |     RMSE |
|:--------------------------|---------:|
| Linear.Regression.Model.1 | 6118.800 |
| Linear.Regression.Model.2 | 7090.623 |
| Random.Forest.Model       | 6047.443 |
| Boosting.Model            | 6146.421 |

Summary Table of RMSE score
