# a list of all channels we wish to create analysis for
channel_list <- list('lifestyle',
                     'entertainment',
                     'bus',
                     'socmed',
                     'tech',
                     'world')

#channel_list <- list('bus')

# render analysis for each channel
for (channel in channel_list) {
  rmarkdown::render(
    'analysis_template.rmd', 
    output_file = paste0(channel, '.md'),
    output_dir = 'analysis',
    params = list(channel=channel)
  )
}
