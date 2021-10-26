Bus
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

    ## [1] 6258   55

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

| Count | Min |     Q1 | Median | Average |   Q3 |    Max |  Std.Dev |
|------:|----:|-------:|-------:|--------:|-----:|-------:|---------:|
|  6258 |   1 | 952.25 |   1400 | 3063.02 | 2500 | 690400 | 15046.39 |

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
| Sunday    |       1215518 |        3544 |       56900 |
| Monday    |       4482214 |        3887 |      690400 |
| Tuesday   |       3466021 |        2932 |      310800 |
| Wednesday |       3401897 |        2677 |      158900 |
| Thursday  |       3560327 |        2885 |      306100 |
| Friday    |       1966657 |        2364 |      102200 |
| Saturday  |       1075736 |        4427 |      144400 |

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
| Not at all popular |       1113353 |         722 |         946 |
| Not too popular    |       1977017 |        1171 |        1400 |
| Somewhat popular   |       3369800 |        1992 |        2800 |
| Very popular       |      12708200 |        9505 |      690400 |

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

![](bus_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

![](bus_files/figure-gfm/2_eda-1.png)<!-- -->

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

    ## Warning: Removed 193 rows containing missing values (geom_point).

![](bus_files/figure-gfm/1_eda-1.png)<!-- -->

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

![](bus_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> Graph A shows
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

![](bus_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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

![](bus_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> \#\#\# Number of
keywords

``` r
ggplot(data = df, aes(x =  num_keywords, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of keywords")
```

![](bus_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

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

![](bus_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Title subjectivity

``` r
#scatter plot of title subjectivity
ggplot(data = df, aes(x =     title_subjectivity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("title subjectivity")
```

![](bus_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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

![](bus_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

![](bus_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

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

    ## Start:  AIC=83958.44
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_imgs + num_videos + num_keywords + rate_positive_words + 
    ##     title_subjectivity + I(n_tokens_content^2) + I(num_imgs^2) + 
    ##     I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - I(n_tokens_content^2)  1    2389816 9.1942e+11 83956
    ## - num_hrefs              1   15520645 9.1943e+11 83957
    ## - n_tokens_content       1   18357954 9.1943e+11 83957
    ## - title_subjectivity     1   60708799 9.1948e+11 83957
    ## - I(num_imgs^2)          1   76468139 9.1949e+11 83957
    ## - num_keywords           1  115913138 9.1953e+11 83957
    ## - n_tokens_title         1  183257504 9.1960e+11 83957
    ## - I(num_hrefs^2)         1  343059646 9.1976e+11 83958
    ## - is_weekend             1  380736370 9.1980e+11 83958
    ## <none>                                9.1942e+11 83958
    ## - rate_positive_words    1  454034507 9.1987e+11 83959
    ## - num_imgs               1  509957989 9.1993e+11 83959
    ## - I(num_videos^2)        1 4923492505 9.2434e+11 83980
    ## - num_videos             1 7962577112 9.2738e+11 83994
    ## 
    ## Step:  AIC=83956.45
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_imgs + num_videos + num_keywords + rate_positive_words + 
    ##     title_subjectivity + I(num_imgs^2) + I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                       Df  Sum of Sq        RSS   AIC
    ## - num_hrefs            1   13442571 9.1943e+11 83955
    ## - n_tokens_content     1   27681499 9.1945e+11 83955
    ## - title_subjectivity   1   60488422 9.1948e+11 83955
    ## - I(num_imgs^2)        1   75691474 9.1949e+11 83955
    ## - num_keywords         1  116607614 9.1953e+11 83955
    ## - n_tokens_title       1  182397924 9.1960e+11 83955
    ## - I(num_hrefs^2)       1  352912279 9.1977e+11 83956
    ## - is_weekend           1  386200905 9.1980e+11 83956
    ## <none>                              9.1942e+11 83956
    ## - rate_positive_words  1  454394194 9.1987e+11 83957
    ## - num_imgs             1  508766206 9.1993e+11 83957
    ## - I(num_videos^2)      1 4935221151 9.2435e+11 83978
    ## - num_videos           1 7972001690 9.2739e+11 83992
    ## 
    ## Step:  AIC=83954.51
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_imgs + 
    ##     num_videos + num_keywords + rate_positive_words + title_subjectivity + 
    ##     I(num_imgs^2) + I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                       Df  Sum of Sq        RSS   AIC
    ## - n_tokens_content     1   16810558 9.1945e+11 83953
    ## - title_subjectivity   1   60555430 9.1949e+11 83953
    ## - I(num_imgs^2)        1   67666705 9.1950e+11 83953
    ## - num_keywords         1  107062089 9.1954e+11 83953
    ## - n_tokens_title       1  189188281 9.1962e+11 83953
    ## - is_weekend           1  373800148 9.1981e+11 83954
    ## <none>                              9.1943e+11 83955
    ## - rate_positive_words  1  471856118 9.1990e+11 83955
    ## - num_imgs             1  495524392 9.1993e+11 83955
    ## - I(num_hrefs^2)       1  779669345 9.2021e+11 83956
    ## - I(num_videos^2)      1 4948562422 9.2438e+11 83976
    ## - num_videos           1 7991855398 9.2742e+11 83990
    ## 
    ## Step:  AIC=83952.59
    ## shares ~ n_tokens_title + is_weekend + num_imgs + num_videos + 
    ##     num_keywords + rate_positive_words + title_subjectivity + 
    ##     I(num_imgs^2) + I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                       Df  Sum of Sq        RSS   AIC
    ## - title_subjectivity   1   61013974 9.1951e+11 83951
    ## - I(num_imgs^2)        1   82510616 9.1953e+11 83951
    ## - num_keywords         1  118777336 9.1957e+11 83951
    ## - n_tokens_title       1  190857375 9.1964e+11 83952
    ## - is_weekend           1  411014115 9.1986e+11 83953
    ## <none>                              9.1945e+11 83953
    ## - rate_positive_words  1  469457287 9.1992e+11 83953
    ## - num_imgs             1  565885356 9.2001e+11 83953
    ## - I(num_hrefs^2)       1 1022810424 9.2047e+11 83955
    ## - I(num_videos^2)      1 4932902737 9.2438e+11 83974
    ## - num_videos           1 7991108492 9.2744e+11 83988
    ## 
    ## Step:  AIC=83950.88
    ## shares ~ n_tokens_title + is_weekend + num_imgs + num_videos + 
    ##     num_keywords + rate_positive_words + I(num_imgs^2) + I(num_videos^2) + 
    ##     I(num_hrefs^2)
    ## 
    ##                       Df  Sum of Sq        RSS   AIC
    ## - I(num_imgs^2)        1   80434745 9.1959e+11 83949
    ## - num_keywords         1  122605039 9.1963e+11 83949
    ## - n_tokens_title       1  225446152 9.1973e+11 83950
    ## - is_weekend           1  413334457 9.1992e+11 83951
    ## <none>                              9.1951e+11 83951
    ## - rate_positive_words  1  463501231 9.1997e+11 83951
    ## - num_imgs             1  563887616 9.2007e+11 83952
    ## - I(num_hrefs^2)       1 1031009642 9.2054e+11 83954
    ## - I(num_videos^2)      1 5010337529 9.2452e+11 83973
    ## - num_videos           1 8174175655 9.2768e+11 83988
    ## 
    ## Step:  AIC=83949.26
    ## shares ~ n_tokens_title + is_weekend + num_imgs + num_videos + 
    ##     num_keywords + rate_positive_words + I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                       Df  Sum of Sq        RSS   AIC
    ## - num_keywords         1  121993414 9.1971e+11 83948
    ## - n_tokens_title       1  222870691 9.1981e+11 83948
    ## - is_weekend           1  411864190 9.2000e+11 83949
    ## <none>                              9.1959e+11 83949
    ## - rate_positive_words  1  466577205 9.2006e+11 83949
    ## - I(num_hrefs^2)       1 1075869693 9.2067e+11 83952
    ## - num_imgs             1 1202962798 9.2079e+11 83953
    ## - I(num_videos^2)      1 4963161979 9.2455e+11 83971
    ## - num_videos           1 8109419533 9.2770e+11 83986
    ## 
    ## Step:  AIC=83947.85
    ## shares ~ n_tokens_title + is_weekend + num_imgs + num_videos + 
    ##     rate_positive_words + I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                       Df  Sum of Sq        RSS   AIC
    ## - n_tokens_title       1  224315724 9.1994e+11 83947
    ## <none>                              9.1971e+11 83948
    ## - is_weekend           1  432814035 9.2014e+11 83948
    ## - rate_positive_words  1  437460788 9.2015e+11 83948
    ## - I(num_hrefs^2)       1 1130352543 9.2084e+11 83951
    ## - num_imgs             1 1250597745 9.2096e+11 83952
    ## - I(num_videos^2)      1 5008246954 9.2472e+11 83970
    ## - num_videos           1 8241457618 9.2795e+11 83985
    ## 
    ## Step:  AIC=83946.91
    ## shares ~ is_weekend + num_imgs + num_videos + rate_positive_words + 
    ##     I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                       Df  Sum of Sq        RSS   AIC
    ## <none>                              9.1994e+11 83947
    ## - is_weekend           1  440347865 9.2038e+11 83947
    ## - rate_positive_words  1  447596392 9.2038e+11 83947
    ## - I(num_hrefs^2)       1 1076755871 9.2101e+11 83950
    ## - num_imgs             1 1249196243 9.2119e+11 83951
    ## - I(num_videos^2)      1 5084761608 9.2502e+11 83969
    ## - num_videos           1 8401720408 9.2834e+11 83985

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
    ## 4380 samples
    ##    4 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 3943, 3941, 3942, 3941, 3943, 3943, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared    MAE     
    ##   11800.73  0.01039428  2872.871
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
    ##            6.636e+00             1.650e-04             7.681e-03            -1.368e-01             3.901e-02             8.792e-04            -3.985e-05  
    ##           kw_avg_avg            is_weekend                LDA_04   global_subjectivity  
    ##            3.153e-04             3.361e-01            -3.338e-01             6.236e-01

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
    ## 4380 samples
    ##   53 predictor
    ## 
    ## Pre-processing: centered (53), scaled (53) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 4380, 4380, 4380, 4380, 4380, 4380, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared    MAE     
    ##   1     13297.60  0.01999154  2749.655
    ##   2     13346.34  0.01880146  2821.609
    ##   3     13408.82  0.01693490  2870.683
    ##   4     13464.74  0.01600998  2907.766
    ##   5     13519.74  0.01544288  2941.016
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 1.

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
    ## 1      50                 1       0.1             10

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
| Linear.Regression.Model.1 | 16072.18 |
| Linear.Regression.Model.2 | 16288.01 |
| Random.Forest.Model       | 15806.52 |
| Boosting.Model            | 16195.73 |

Summary Table of RMSE score
