########################################################
#
# This script stores functions called in the app
#
#######################################################

plot_rates <- function(df, select_year, eval, metric, select_school) {
  
  # create axis labels ------
  
  # create named list of axis components and their labels as strings
  # then we will access string to create axis label
  axis_labels <- list('lsat50' = 'Median LSAT',
                      'uggpa50' = 'Median Undergraduate GPA',
                      'scaled_admissions' = 'LSAT / Undergrad GPA Combo',
                      'pass_rate' = 'School Bar Pass Rate (%)',
                      'pass_diff' = 'Difference Between School and State Bar Pass Rate (%)')

  # filter dataset for years -------
  
  # filter dataset if one of the specific years is selected
  # also set opacity at .8 for single years
  if (select_year %in% unique(df$year)) {
    
    df <- df %>%
      filter(year == select_year)
  
  }

  # plot data -----
  
  df %>%
    # create column signifying whether school is selected
    mutate(selected_school = ifelse(.$schoolname == select_school, T, F)) %>%
    plot_ly(x = .[[metric]], y = .[[eval]], color = ~selected_school,
            # set color palette to one that display categoricals well
            colors = "Accent",
            # add tooltip (hover) information
            hoverinfo = 'text',
            text = ~paste0("School Name: ", schoolname,
                          '<br>Year: ', year,
                          '<br>School Pass Rate: ', pass_rate, '%',
                          '<br>School / State Difference: ',  pass_diff, '%',
                          '<br>Median Undergrad GPA: ', uggpa50,
                          '<br>Median LSAT Score: ', lsat50)) %>%
    add_markers(x = .[[metric]], y = .[[eval]],
                marker = list(
                  opacity = 0.9,
                  size = 8,
                  line = list(
                    width = 3
                  ))) %>%
    # add loess smoother fit line
    add_lines(y = ~fitted(loess(df[[eval]] ~ df[[metric]])), color = 'rgb(158,202,225)') %>%
    # remove legend
    layout(showlegend = FALSE,
           title = "Bar passage and median law school undergraduate GPA and LSAT scores",
           # axis labels to use, depending on selections
           xaxis = list(
             title = axis_labels[[metric]]),
           yaxis = list(
             title = axis_labels[[eval]],
             ticksuffix = "%"))
  
}