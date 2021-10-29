# Online News Popularity Data Set
## Min-Jung Jung and David Shaw

## Overview
This project explore the Online News Popularity data set for building predictive models and generating automation reports with the different channels (i.e. Lifestyle, Business, etc.).

## Requirements
This project requires the following packages:
`tidyverse`
`corrplot`
`caret`
`gbm`
`leaps`
`caret`
`cowplot`.

## Analysis
* [Business](analysis/bus.md)
* [Entertainment](analysis/entertainment.md)
* [Lifestyle](analysis/lifestyle.md)
* [Social Media](analysis/socmed.md)
* [Tech](analysis/tech.md)
* [World](analysis/world.md)

## Render Code

```{r}
# a list of all channels we wish to create analysis for
channel_list <- list('lifestyle',
                     'entertainment',
                     'bus',
                     'socmed',
                     'tech',
                     'world')

# a shortened list of channels so we can test rendering
#channel_list <- list('bus')

# render analysis for each channel
for (channel in channel_list) {
  rmarkdown::render(
    'analysis/analysis_template.Rmd', 
    output_file = paste0(channel, '.md'),
    params = list(channel=channel)
  )
}
```