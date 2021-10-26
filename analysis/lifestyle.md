Lifestyle
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

    ## [1] 2099   55

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
|  2099 |  28 | 1100 |   1700 | 3682.12 | 3250 | 208300 | 8885.02 |

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
| Sunday    |        795979 |        3790 |       33100 |
| Monday    |       1399319 |        4346 |      196700 |
| Tuesday   |       1386933 |        4152 |      208300 |
| Wednesday |       1231194 |        3173 |       73100 |
| Thursday  |       1253096 |        3500 |       56000 |
| Friday    |        922890 |        3026 |       40400 |
| Saturday  |        739366 |        4062 |       43000 |

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
| Not at all popular |        265793 |         699 |         945 |
| Not too popular    |        620086 |        1195 |        1400 |
| Somewhat popular   |       1187200 |        2002 |        2800 |
| Very popular       |       5655698 |        9317 |      208300 |

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

![](lifestyle_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/2_eda-1.png)<!-- -->

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

    ## Warning: Removed 141 rows containing missing values (geom_point).

![](lifestyle_files/figure-gfm/1_eda-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/unnamed-chunk-83-1.png)<!-- --> Graph A
shows the number of words in the title compared to the number of shares.
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

![](lifestyle_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/unnamed-chunk-85-1.png)<!-- --> \#\#\#
Number of keywords

``` r
ggplot(data = df, aes(x =  num_keywords, 
                      y = shares)) +
      geom_point(alpha = 0.50) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Number of keywords")
```

![](lifestyle_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/unnamed-chunk-87-1.png)<!-- -->

### Title subjectivity

``` r
#scatter plot of title subjectivity
ggplot(data = df, aes(x =     title_subjectivity, 
                      y = shares)) +
      geom_point(alpha = 0.50) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("title subjectivity")
```

![](lifestyle_files/figure-gfm/unnamed-chunk-88-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/unnamed-chunk-89-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/unnamed-chunk-90-1.png)<!-- -->

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

    ## Start:  AIC=27072.93
    ## shares ~ n_tokens_title + n_tokens_content + is_weekend + num_hrefs + 
    ##     num_imgs + num_videos + num_keywords + rate_positive_words + 
    ##     title_subjectivity + I(n_tokens_content^2) + I(num_imgs^2) + 
    ##     I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - n_tokens_title         1    1267670 1.4540e+11 27071
    ## - num_hrefs              1    9716492 1.4541e+11 27071
    ## - title_subjectivity     1   25208652 1.4543e+11 27071
    ## - num_keywords           1   33887624 1.4544e+11 27071
    ## - I(num_hrefs^2)         1   57200627 1.4546e+11 27072
    ## - n_tokens_content       1  101796174 1.4550e+11 27072
    ## - is_weekend             1  103472401 1.4551e+11 27072
    ## - rate_positive_words    1  161198208 1.4556e+11 27073
    ## <none>                                1.4540e+11 27073
    ## - I(num_videos^2)        1  807947534 1.4621e+11 27079
    ## - num_imgs               1  983260491 1.4639e+11 27081
    ## - I(n_tokens_content^2)  1 1219768686 1.4662e+11 27083
    ## - I(num_imgs^2)          1 1662904868 1.4707e+11 27088
    ## - num_videos             1 1738534817 1.4714e+11 27088
    ## 
    ## Step:  AIC=27070.94
    ## shares ~ n_tokens_content + is_weekend + num_hrefs + num_imgs + 
    ##     num_videos + num_keywords + rate_positive_words + title_subjectivity + 
    ##     I(n_tokens_content^2) + I(num_imgs^2) + I(num_videos^2) + 
    ##     I(num_hrefs^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - num_hrefs              1   10825761 1.4541e+11 27069
    ## - title_subjectivity     1   25057846 1.4543e+11 27069
    ## - num_keywords           1   34631529 1.4544e+11 27069
    ## - I(num_hrefs^2)         1   59197279 1.4546e+11 27070
    ## - n_tokens_content       1  101212247 1.4550e+11 27070
    ## - is_weekend             1  103268535 1.4551e+11 27070
    ## - rate_positive_words    1  163750517 1.4557e+11 27071
    ## <none>                                1.4540e+11 27071
    ## - I(num_videos^2)        1  807011441 1.4621e+11 27077
    ## - num_imgs               1  992102545 1.4640e+11 27079
    ## - I(n_tokens_content^2)  1 1220039573 1.4662e+11 27081
    ## - I(num_imgs^2)          1 1668521209 1.4707e+11 27086
    ## - num_videos             1 1737934160 1.4714e+11 27086
    ## 
    ## Step:  AIC=27069.05
    ## shares ~ n_tokens_content + is_weekend + num_imgs + num_videos + 
    ##     num_keywords + rate_positive_words + title_subjectivity + 
    ##     I(n_tokens_content^2) + I(num_imgs^2) + I(num_videos^2) + 
    ##     I(num_hrefs^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - title_subjectivity     1   25394172 1.4544e+11 27067
    ## - num_keywords           1   40381308 1.4545e+11 27068
    ## - I(num_hrefs^2)         1   81641903 1.4550e+11 27068
    ## - is_weekend             1  108553116 1.4552e+11 27068
    ## - n_tokens_content       1  117463117 1.4553e+11 27068
    ## - rate_positive_words    1  179877228 1.4559e+11 27069
    ## <none>                                1.4541e+11 27069
    ## - I(num_videos^2)        1  818077708 1.4623e+11 27075
    ## - num_imgs               1 1059742702 1.4647e+11 27078
    ## - I(n_tokens_content^2)  1 1223492572 1.4664e+11 27079
    ## - I(num_imgs^2)          1 1676474296 1.4709e+11 27084
    ## - num_videos             1 1746323611 1.4716e+11 27085
    ## 
    ## Step:  AIC=27067.3
    ## shares ~ n_tokens_content + is_weekend + num_imgs + num_videos + 
    ##     num_keywords + rate_positive_words + I(n_tokens_content^2) + 
    ##     I(num_imgs^2) + I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - num_keywords           1   38239160 1.4548e+11 27066
    ## - I(num_hrefs^2)         1   84200351 1.4552e+11 27066
    ## - n_tokens_content       1  113365665 1.4555e+11 27066
    ## - is_weekend             1  118420949 1.4556e+11 27067
    ## - rate_positive_words    1  176382974 1.4562e+11 27067
    ## <none>                                1.4544e+11 27067
    ## - I(num_videos^2)        1  815786514 1.4626e+11 27074
    ## - num_imgs               1 1034589260 1.4647e+11 27076
    ## - I(n_tokens_content^2)  1 1212085494 1.4665e+11 27078
    ## - I(num_imgs^2)          1 1655381693 1.4710e+11 27082
    ## - num_videos             1 1741033922 1.4718e+11 27083
    ## 
    ## Step:  AIC=27065.69
    ## shares ~ n_tokens_content + is_weekend + num_imgs + num_videos + 
    ##     rate_positive_words + I(n_tokens_content^2) + I(num_imgs^2) + 
    ##     I(num_videos^2) + I(num_hrefs^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - I(num_hrefs^2)         1   80281107 1.4556e+11 27065
    ## - n_tokens_content       1  120282059 1.4560e+11 27065
    ## - is_weekend             1  124236631 1.4560e+11 27065
    ## - rate_positive_words    1  177452847 1.4566e+11 27066
    ## <none>                                1.4548e+11 27066
    ## - I(num_videos^2)        1  799934864 1.4628e+11 27072
    ## - num_imgs               1  998864042 1.4648e+11 27074
    ## - I(n_tokens_content^2)  1 1219504451 1.4670e+11 27076
    ## - I(num_imgs^2)          1 1635197363 1.4711e+11 27080
    ## - num_videos             1 1719686130 1.4720e+11 27081
    ## 
    ## Step:  AIC=27064.5
    ## shares ~ n_tokens_content + is_weekend + num_imgs + num_videos + 
    ##     rate_positive_words + I(n_tokens_content^2) + I(num_imgs^2) + 
    ##     I(num_videos^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - n_tokens_content       1   93075178 1.4565e+11 27063
    ## - is_weekend             1  127075225 1.4569e+11 27064
    ## - rate_positive_words    1  164151522 1.4572e+11 27064
    ## <none>                                1.4556e+11 27065
    ## - I(num_videos^2)        1  770050485 1.4633e+11 27070
    ## - I(n_tokens_content^2)  1 1193432673 1.4675e+11 27075
    ## - num_imgs               1 1316436646 1.4687e+11 27076
    ## - num_videos             1 1712491205 1.4727e+11 27080
    ## - I(num_imgs^2)          1 1725589722 1.4728e+11 27080
    ## 
    ## Step:  AIC=27063.44
    ## shares ~ is_weekend + num_imgs + num_videos + rate_positive_words + 
    ##     I(n_tokens_content^2) + I(num_imgs^2) + I(num_videos^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - is_weekend             1  116183556 1.4577e+11 27063
    ## - rate_positive_words    1  197151807 1.4585e+11 27063
    ## <none>                                1.4565e+11 27063
    ## - I(num_videos^2)        1  753753184 1.4641e+11 27069
    ## - num_imgs               1 1316706973 1.4697e+11 27075
    ## - num_videos             1 1672264755 1.4732e+11 27078
    ## - I(num_imgs^2)          1 2236254653 1.4789e+11 27084
    ## - I(n_tokens_content^2)  1 2466915436 1.4812e+11 27086
    ## 
    ## Step:  AIC=27062.61
    ## shares ~ num_imgs + num_videos + rate_positive_words + I(n_tokens_content^2) + 
    ##     I(num_imgs^2) + I(num_videos^2)
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## <none>                                1.4577e+11 27063
    ## - rate_positive_words    1  203764483 1.4597e+11 27063
    ## - I(num_videos^2)        1  723929902 1.4649e+11 27068
    ## - num_imgs               1 1206490461 1.4697e+11 27073
    ## - num_videos             1 1621806015 1.4739e+11 27077
    ## - I(num_imgs^2)          1 2176026245 1.4794e+11 27082
    ## - I(n_tokens_content^2)  1 2477368510 1.4824e+11 27085

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
    ## 1469 samples
    ##    4 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 1322, 1323, 1323, 1321, 1323, 1322, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared    MAE     
    ##   8951.312  0.03025285  3590.168
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
    ##            7.3740900             0.0001762             0.0055415            -0.0806876            -0.0016456             0.0011276            -0.0000270  
    ##           kw_avg_avg            is_weekend                LDA_04   global_subjectivity  
    ##            0.0002249             0.1577511            -0.0660078            -0.4025406

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
    ## 1469 samples
    ##   53 predictor
    ## 
    ## Pre-processing: centered (53), scaled (53) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 1469, 1469, 1469, 1469, 1469, 1469, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared     MAE     
    ##   1     10307.75  0.007386778  3667.477
    ##   2     10357.74  0.007342402  3762.110
    ##   3     10404.29  0.007495760  3808.659
    ##   4     10450.40  0.006578280  3842.424
    ##   5     10504.17  0.006241485  3868.899
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
| Linear.Regression.Model.1 | 5604.459 |
| Linear.Regression.Model.2 | 5804.895 |
| Random.Forest.Model       | 4830.607 |
| Boosting.Model            | 5215.282 |

Summary Table of RMSE score
