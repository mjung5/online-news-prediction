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
    -   [Number of link by weekday.](#number-of-link-by-weekday)
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
    -   [Ensemble Tree-based model](#ensemble-tree-based-model)
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

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (1): url
    ## dbl (60): timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_no...

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

We will use train set for EDA.

### Exploratory Data Anaysis

The summary statistics of targeted variable (shares)

``` r
# summary statistics
share_stat <- trainData %>% 
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

| Count | Min |   Q1 | Median | Average |   Q3 |   Max | Std.Dev |
|------:|----:|-----:|-------:|--------:|-----:|------:|--------:|
|  1626 |  23 | 1400 |   2200 | 3654.42 | 3800 | 59000 | 5242.53 |

Summary Stats by shares

### Shares by days of week

``` r
trainData %>%
  group_by(weekday) %>%
  summarise(total_shares = sum(shares), avg_shares = round(mean(shares)), max_shares = max(shares)) %>%
  knitr::kable()
```

| weekday   | total\_shares | avg\_shares | max\_shares |
|:----------|--------------:|------------:|------------:|
| Sunday    |        448406 |        5154 |       54100 |
| Monday    |        918756 |        3943 |       47700 |
| Tuesday   |        988498 |        3189 |       37300 |
| Wednesday |       1140722 |        3704 |       59000 |
| Thursday  |       1000986 |        3061 |       26900 |
| Friday    |       1034607 |        4258 |       57000 |
| Saturday  |        410105 |        3475 |       34500 |

The above table shows a breakdown of total, average, and maximum number
of shares for articles published on a specific weekday for this channel.
Some channels tend to have more popular days than others.

### shares by popularity

``` r
trainData %>% 
  group_by(Popularity) %>%
  summarise(total_shares = sum(shares), avg_shares = round(mean(shares)), max_shares = max(shares)) %>%
  knitr::kable()
```

| Popularity         | total\_shares | avg\_shares | max\_shares |
|:-------------------|--------------:|------------:|------------:|
| Not at all popular |         85748 |         692 |         940 |
| Not too popular    |        416432 |        1214 |        1400 |
| Somewhat popular   |       1160300 |        2054 |        2800 |
| Very popular       |       4279600 |        7205 |       59000 |

The above table show a summary of the newly created `popularity`
variable.

### count of news by popularity over different days of week

``` r
#Bar plot of weekday by popularity 

ggplot(data = trainData, aes(x = weekday)) +
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

g1 <- trainData %>% ggplot(aes(x=weekday, y=shares)) +
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
g2 <- trainData %>% ggplot(aes(x=num_hrefs, y=shares)) +
        geom_point(size=2, shape=23) +
        ylim(0, 10000) +
        ggtitle("Number of links")  
g2
```

    ## Warning: Removed 92 rows containing missing values (geom_point).

![](socmed_files/figure-gfm/1_eda-1.png)<!-- -->

In the above scatter, we compare the number of links in an article to
its shares. This plot is motivated by the implementation of Google’s
[PageRank Algorithm](https://en.wikipedia.org/wiki/PageRank).

### Number of link by weekday.

``` r
ggplot(trainData, aes(x = weekday, 
                                y = num_hrefs, 
                                fill = weekday)
                            ) +
  geom_boxplot() +
  scale_x_discrete("Time weekday") +
  ggtitle("Numbers link comparison by days of week") +
  scale_fill_brewer(palette = "BuPu") +
  scale_fill_discrete(name = "Weekday") 
```

    ## Scale for 'fill' is already present. Adding another scale for 'fill', which
    ## will replace the existing scale.

![](socmed_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Number of words in the title and content

``` r
library(cowplot)

#scatter plots of Number of words in the title
g3 <- ggplot(data = trainData, aes(x =  n_tokens_title, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Word count in the title")  


#scatter plots of Number of words in the content
g4 <- ggplot(data = trainData, aes(x =  n_tokens_content, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Word count in the content")

plot_grid(g3, g4,  labels = c('A', 'B'))   
```

![](socmed_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> Graph A shows
the number of words in the title compared to the number of shares.
Perhaps a quadratic relationship is appropriate for this variable if a
bell shape appears in the plot. Graph B shows a comparison of the number
of words that appear in the article compared to the number of shares.
Perhaps a negative linear relationship is appropriate if the data
exihibits a slightly negative slops.

### Unique words count

David - may be delete this? Dr. Post said we do not need to do EDA for
all the variables we include in model.

``` r
ggplot(data = trainData, aes(x =  n_unique_tokens, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Unique Word count")
```

![](socmed_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Number of image and video

``` r
#scatter plots of Number of words in the content
g5 <- ggplot(data = trainData, aes(x =  num_imgs, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of image")


#scatter plots of Number of words in the content
g6 <- ggplot(data = trainData, aes(x =  num_videos, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of video")


plot_grid(g5, g6,  labels = c('C', 'D')) 
```

![](socmed_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> \#\#\#
Number of keywords

David - may be delete this? Dr. Post said we do not need to do EDA for
all the variables we include in model.

``` r
ggplot(data = trainData, aes(x =  num_keywords, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of keywords")
```

![](socmed_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Number of positive and negative words rate

``` r
#scatter plots of positive and negative words rate
g7 <- ggplot(data = trainData, aes(x =  rate_positive_words, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Positive words rate")


#scatter plots of Number of words in the content
g8 <- ggplot(data = trainData, aes(x =  rate_negative_words, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Negative words rate")


plot_grid(g7, g8,  labels = c('A', 'B')) 
```

![](socmed_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Title subjectivity

David - may be delete this? Dr. Post said we do not need to do EDA for
all the variables we include in model

``` r
#scatter plot of title subjectivity
ggplot(data = trainData, aes(x =     title_subjectivity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("title subjectivity")
```

![](socmed_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Number of positive and negative words rate

``` r
#scatter plots of positive and negative words rate
g9 <- ggplot(data = trainData, aes(x =  avg_positive_polarity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Average positive polarity")


#scatter plots of Number of words in the content
g10 <- ggplot(data = trainData, aes(x =  avg_negative_polarity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Average negative polarity")


plot_grid(g9, g10,  labels = c('A', 'B')) 
```

![](socmed_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Correlation with numeric variables

David- We can add more numerical variables here. what do you think?

``` r
df_tmp <- trainData %>% select(c('n_tokens_title', 
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

![](socmed_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Above shows the correlation matrix for other numerical variables. Shares
is the bottom row. We use this plot to find other variables that might
have weak correlation with shares and make sure to include these in our
model building phase.

## Modeling

### Linear Regression

Linear regression is one of the commonly used supervised methods for
modeling and useful tool for predicting a quantitative response on the
basis of a single or multiple predictor variables. The idea of linear
regression is that the model finds the best fit line between predictor
variables and response variable, minimizing sum of squared errors. Also,
Linear Regression is flexible in which to include many explanatory
variables, higher order terms, and/or interaction, so we can see the
effect the combinations on prediction. Here we will fit two different
linear regression models.

#### Linear model 1

The first linear regression model will have predictors selected by
stepwise selection. After choosing the subset of predictors, we will use
repeated cross-validation with 10 folder and will find the RMSE and R2.

``` r
# Stepwise model selection
lmFitSelect <- lm(shares ~ n_tokens_title + n_tokens_content+ is_weekend +  num_hrefs+  num_imgs+ num_videos+num_keywords+ rate_positive_words + title_subjectivity  + I(n_tokens_content^2) + I(num_imgs^2)+ I(num_videos^2) + I(num_hrefs^2) + weekday, data = trainData)
models <- step(lmFitSelect, trace=0)
summary(models)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ n_tokens_content + rate_positive_words + 
    ##     title_subjectivity + I(num_imgs^2) + weekday, data = trainData)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -5352  -2223  -1326    308  54704 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          5602.7796   927.5243   6.041  1.9e-09 ***
    ## n_tokens_content        0.8018     0.2749   2.917  0.00359 ** 
    ## rate_positive_words -1536.8194   927.0013  -1.658  0.09754 .  
    ## title_subjectivity    880.3501   407.3675   2.161  0.03084 *  
    ## I(num_imgs^2)          -1.5352     0.4747  -3.234  0.00124 ** 
    ## weekdayMonday       -1092.7196   654.5438  -1.669  0.09522 .  
    ## weekdayTuesday      -1839.2498   632.6859  -2.907  0.00370 ** 
    ## weekdayWednesday    -1316.2665   632.6085  -2.081  0.03762 *  
    ## weekdayThursday     -1985.8650   628.0116  -3.162  0.00160 ** 
    ## weekdayFriday        -793.1685   652.2549  -1.216  0.22415    
    ## weekdaySaturday     -1684.4016   735.6580  -2.290  0.02217 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5198 on 1615 degrees of freedom
    ## Multiple R-squared:  0.02298,    Adjusted R-squared:  0.01693 
    ## F-statistic: 3.798 on 10 and 1615 DF,  p-value: 4.394e-05

``` r
#lmFitSelectFull <- lm(shares ~ ., data = trainData[-57])
#models2 <- step(lmFitSelectFull, trace=0)
```

David - this is the repeat of the first one, but including all the
variables. Which one should we use? Do we need to stick with the first
one?

``` r
lmFitSelectFull <- lm(shares ~ ., data = trainData[-c(1:2,57)])
models2 <- step(lmFitSelectFull, trace=0)
summary(models2)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ n_non_stop_words + n_non_stop_unique_tokens + 
    ##     num_self_hrefs + num_imgs + average_token_length + num_keywords + 
    ##     kw_min_min + kw_max_min + kw_avg_min + kw_min_max + kw_max_avg + 
    ##     kw_avg_avg + self_reference_max_shares + weekday_is_tuesday + 
    ##     weekday_is_wednesday + weekday_is_thursday + weekday_is_saturday + 
    ##     LDA_00 + max_positive_polarity + min_negative_polarity + 
    ##     title_subjectivity + abs_title_subjectivity, data = trainData[-c(1:2, 
    ##     57)])
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -12714  -2135  -1015    452  51628 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               -5.384e+02  1.886e+03  -0.286 0.775281    
    ## n_non_stop_words           7.925e+03  3.205e+03   2.473 0.013514 *  
    ## n_non_stop_unique_tokens  -6.223e+03  1.450e+03  -4.293 1.87e-05 ***
    ## num_self_hrefs            -5.648e+01  2.365e+01  -2.388 0.017048 *  
    ## num_imgs                  -5.576e+01  1.894e+01  -2.944 0.003286 ** 
    ## average_token_length      -8.578e+02  5.146e+02  -1.667 0.095737 .  
    ## num_keywords               1.443e+02  6.816e+01   2.117 0.034418 *  
    ## kw_min_min                 2.966e+00  1.652e+00   1.796 0.072705 .  
    ## kw_max_min                -3.410e-01  1.559e-01  -2.188 0.028813 *  
    ## kw_avg_min                 1.562e+00  5.774e-01   2.705 0.006912 ** 
    ## kw_min_max                -2.382e-03  1.270e-03  -1.876 0.060886 .  
    ## kw_max_avg                -2.703e-01  5.843e-02  -4.626 4.03e-06 ***
    ## kw_avg_avg                 1.275e+00  2.209e-01   5.772 9.37e-09 ***
    ## self_reference_max_shares  1.627e-02  3.710e-03   4.385 1.24e-05 ***
    ## weekday_is_tuesday        -1.004e+03  3.601e+02  -2.789 0.005349 ** 
    ## weekday_is_wednesday      -5.353e+02  3.597e+02  -1.488 0.136978    
    ## weekday_is_thursday       -1.265e+03  3.524e+02  -3.590 0.000341 ***
    ## weekday_is_saturday       -8.055e+02  5.343e+02  -1.508 0.131853    
    ## LDA_00                     1.566e+03  4.775e+02   3.280 0.001059 ** 
    ## max_positive_polarity     -1.167e+03  6.506e+02  -1.794 0.073034 .  
    ## min_negative_polarity     -1.726e+03  4.782e+02  -3.608 0.000318 ***
    ## title_subjectivity         1.368e+03  4.579e+02   2.987 0.002860 ** 
    ## abs_title_subjectivity     1.678e+03  7.880e+02   2.130 0.033325 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5039 on 1603 degrees of freedom
    ## Multiple R-squared:  0.08867,    Adjusted R-squared:  0.07616 
    ## F-statistic: 7.089 on 22 and 1603 DF,  p-value: < 2.2e-16

``` r
models2
```

    ## 
    ## Call:
    ## lm(formula = shares ~ n_non_stop_words + n_non_stop_unique_tokens + 
    ##     num_self_hrefs + num_imgs + average_token_length + num_keywords + 
    ##     kw_min_min + kw_max_min + kw_avg_min + kw_min_max + kw_max_avg + 
    ##     kw_avg_avg + self_reference_max_shares + weekday_is_tuesday + 
    ##     weekday_is_wednesday + weekday_is_thursday + weekday_is_saturday + 
    ##     LDA_00 + max_positive_polarity + min_negative_polarity + 
    ##     title_subjectivity + abs_title_subjectivity, data = trainData[-c(1:2, 
    ##     57)])
    ## 
    ## Coefficients:
    ##               (Intercept)           n_non_stop_words  
    ##                -5.384e+02                  7.925e+03  
    ##  n_non_stop_unique_tokens             num_self_hrefs  
    ##                -6.223e+03                 -5.648e+01  
    ##                  num_imgs       average_token_length  
    ##                -5.576e+01                 -8.578e+02  
    ##              num_keywords                 kw_min_min  
    ##                 1.443e+02                  2.966e+00  
    ##                kw_max_min                 kw_avg_min  
    ##                -3.410e-01                  1.562e+00  
    ##                kw_min_max                 kw_max_avg  
    ##                -2.382e-03                 -2.703e-01  
    ##                kw_avg_avg  self_reference_max_shares  
    ##                 1.275e+00                  1.627e-02  
    ##        weekday_is_tuesday       weekday_is_wednesday  
    ##                -1.004e+03                 -5.353e+02  
    ##       weekday_is_thursday        weekday_is_saturday  
    ##                -1.265e+03                 -8.055e+02  
    ##                    LDA_00      max_positive_polarity  
    ##                 1.566e+03                 -1.167e+03  
    ##     min_negative_polarity         title_subjectivity  
    ##                -1.726e+03                  1.368e+03  
    ##    abs_title_subjectivity  
    ##                 1.678e+03

``` r
# train data with variables chosen by stepWise
set.seed(10)
lm.fit1 <- train(shares ~ num_imgs + num_videos + rate_positive_words + I(num_videos^2) + 
    I(num_hrefs^2), data = train_df,
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
    ## Pre-processing: centered (5), scaled (5) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 3943, 3941, 3942, 3941, 3943, 3943, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared    MAE     
    ##   11788.26  0.01324197  2862.622
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

David - This is again the follow up model fit from all variables.

``` r
lm.fit3 <- train(shares ~ n_non_stop_unique_tokens + num_hrefs + 
    num_imgs + num_videos + kw_min_max + kw_max_max + kw_max_avg + 
    kw_avg_avg + LDA_03 + min_negative_polarity, data = trainData[-c(1:2, 
    57)],
                        method="lm",
                        preProcess = c("center","scale"),
                        trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))

lm.fit3
```

    ## Linear Regression 
    ## 
    ## 1626 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (10), scaled (10) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 1462, 1463, 1464, 1464, 1464, 1463, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared  MAE     
    ##   5064.742  0.043947  2614.768
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
    ##          (Intercept)      n_tokens_content             num_hrefs  
    ##            7.053e+00             2.026e-04            -8.215e-03  
    ## average_token_length          num_keywords            kw_min_min  
    ##           -3.023e-03             5.979e-02             9.622e-04  
    ##           kw_max_avg            kw_avg_avg            is_weekend  
    ##           -3.995e-05             2.545e-04             1.045e-01  
    ##               LDA_04   global_subjectivity  
    ##           -3.224e-01            -5.662e-01

``` r
# David- With train function. - It is upto you which one you use. Dr. P was emphasizing using cross validation, so I just added here so that you can choose.

lm.fit4 <- train(log(shares) ~ n_tokens_content + num_hrefs + average_token_length + 
                        num_keywords + kw_min_min + kw_max_avg + kw_avg_avg + 
                        is_weekend + LDA_04 + global_subjectivity, 
          data = train_df,
                        method="lm",
                        preProcess = c("center","scale"),
                        trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))

lm.fit4
```

    ## Linear Regression 
    ## 
    ## 1626 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (10), scaled (10) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 1462, 1462, 1465, 1463, 1464, 1465, ... 
    ## Resampling results:
    ## 
    ##   RMSE       Rsquared    MAE      
    ##   0.8068034  0.06313922  0.6144055
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
#predict on test data
#predLm2 <- predict(lm.fit2, test_df)

# calculate rmse
#rmseLm2 <- sqrt(mean((predLm2 - test_df$shares)^2))
#rmseLm2
```

### Ensemble Tree-based model

Tree-based method splits up response variable into subsets based on
their relationship to one or more predictor variables. Because it is
easy to understand and interpret output and no statistical assumptions
is necessary, regression tree (continuous variable) and classification
tree (group membership) are commonly used. However, these models are
influenced vastly even there is a small changes in the data. Therefore,
Ensemble tree-based model are highly used in machine learning to gain
strong prediction even though it lose interpretability because it get
rid of variation of data set to data set and average of predication for
a final prediction. In this project, we will use the two of most
commonly used ensemble methods: Random Forests and Boosting.

#### Random Forest Model

Random Forest method, which shares the idea of bagging, but extends the
idea and only include random subset of predictors for each bootstrap
sample/tree fit instead of including every predictor in each of the
tree. In doing so, one or two good predictors won’t dominate the tree
fit. Random forest method use mtry as tuning parameter and the number of
randomly selected predictor is obtained using *m* = *p*/3 formula (p as
the number of predictors). Below, you will see the result of training
with the random forest method. By choosing randomly selected subset of
predictors in each tree, we will possibly reduce the correlation and
gain stronger prediction.

``` r
# get all numeric columns
train_df <- trainData[ ,unlist(lapply(trainData, is.numeric))]
test_df <- testData[ ,unlist(lapply(testData, is.numeric))]
# tuning parameter is mtry, use values of 1,2,..,10
rfFit <- train(shares ~ n_tokens_title + n_tokens_content+
                 n_unique_tokens+avg_positive_polarity+
                 avg_negative_polarity + num_hrefs +  num_imgs +
                 num_videos + num_keywords + title_sentiment_polarity +
                 rate_positive_words + rate_negative_words +
                 title_subjectivity + weekday,
               data = trainData, 
               method = "rf", 
               trControl = trainControl(method = "cv", number = 5),
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(mtry = (1:10)))

rfFit
```

    ## Random Forest 
    ## 
    ## 1626 samples
    ##   14 predictor
    ## 
    ## Pre-processing: centered (19), scaled (19) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1302, 1300, 1299, 1301, 1302 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared    MAE     
    ##    1    5130.749  0.01730599  2669.886
    ##    2    5165.160  0.01448574  2733.912
    ##    3    5190.568  0.01763634  2773.665
    ##    4    5208.721  0.01688604  2792.936
    ##    5    5239.550  0.01560799  2811.019
    ##    6    5273.327  0.01405427  2848.012
    ##    7    5283.889  0.01580428  2842.073
    ##    8    5312.838  0.01420035  2868.124
    ##    9    5321.559  0.01359267  2874.862
    ##   10    5343.221  0.01416067  2874.159
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
    ## 2     100                 1       0.1             10

``` r
# evaluate on test dataset
#boostPred <- predict(boostFit, newdata = dplyr::select(test_df, -shares), n.trees = 100)
#boostRMSE <- sqrt(mean((boostPred-test_df$shares)^2))
#boostRMSE
```

David- are you going to use tuneGrid? You created one, but didn’t
include in the model.

## Comparison

Now, we compare the 4 models developed above. Each model was evaluated
on a test data set and Root MSE values(roof of test prediction error)
were compared.

``` r
# Predict on test data

predLm1 <- predict(lm.fit1, newdata = trainData)
predLm2 <- predict(lm.fit2, newdata = test_df)
rfPred <- predict(rfFit, newdata = trainData)
boostPred <- predict(boostFit, newdata = test_df)

# Calculate rmse

rmseLm1 <- sqrt(mean((predLm1 - trainData$shares)^2))
rmseLm2 <- sqrt(mean((predLm2 - test_df$shares)^2))
rfMSE <- sqrt(mean((rfPred - trainData$shares)^2))
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
| Linear.Regression.Model.1 | 6112.479 |
| Linear.Regression.Model.2 | 7090.623 |
| Random.Forest.Model       | 4241.112 |
| Boosting.Model            | 6179.773 |

Summary Table of RMSE score

David- I did this part because I will definitely need help on automation
if I do it.
