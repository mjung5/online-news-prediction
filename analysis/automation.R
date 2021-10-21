# a list of all channels we wish to create analysis for
channel_list <- list('lifestyle',
                     'entertainment',
                     'bus',
                     'socmed',
                     'tech',
                     'world')

# a shortened list of channels so we can test rendering
channel_list <- list('bus')

# render analysis for each channel
for (channel in channel_list) {
  rmarkdown::render(
    'analysis/analysis_template.Rmd', 
    output_file = paste0(channel, '.md'),
    params = list(channel=channel)
  )
}
