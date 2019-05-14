#########################################################################################
#
# Shiny app with a scatterplot highlighting the relationship between law school admissions
# factors (undergraduate GPA and LSAT scores) and bar exam results.
#
# Users can interactively change years and admissions factors, and users can highlight
# specific schools
#
# Initial creation: May 2019
#
#########################################################################################

library(tidyverse)
library(plotly)
library(shiny)
library(DT)

# function that creates scatterplot ---------------

plot_rates <- function(df, select_year, eval, metric, select_school) {
  
  # create axis labels ------
  
  # create named list of axis components and their labels as strings
  # then we will access string to create axis label
  axis_labels <- list('lsat50' = 'Median LSAT',
                      'uggpa50' = 'Median Undergraduate GPA',
                      'scaled_admissions' = 'LSAT / Undergrad GPA Combo (Standardized to mean of 0 and st. dev. of 1)',
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
             title = axis_labels[[metric]], 
             # remove axis line at zero intercept
             zeroline = F),
           yaxis = list(
             title = axis_labels[[eval]],
             # add percentage sign to y axis tick labels
             ticksuffix = "%",
             # remove axis line at zero intercept
             zeroline = F))
  
}

# import and clean data -----------------

# data is from ABA 509 disclosure, stores in an Amazon s3 bucket
# data was imported and cleaned in the file 'create_datasets.R'
full <- read_rds("data/bar_passage.rds")

# create unique values of years, to be used in drop down
# sort from largest to smalles
unique_years <- unique(full$year) %>%
  sort(decreasing = T)

# create unique list of schools to select from
unique_schools <- unique(full$schoolname) %>%
  sort(decreasing = F)

# create string that is the max year through the min year, to be used in drop
# down arrow to signify all years
all_years <- sprintf("%i - %i", min(unique_years), max(unique_years))

# create year drop down, which is each unique year and a value fo all years (ex: 2013 - 2017)
year_drop_down <- append(all_years, as.character(unique_years))

# convert years column to character; since the drop down menu is character the dataset
# also needs to be character for filtering
full$year <- as.character(full$year)

# ui ------------------
ui <- fluidPage(
  
  titlePanel("The relationship between a school's undergraduate GPA, LSAT, and bar passage rates"),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(width = 2,
      h4("Controls:"),
      
      # drop down arrow for year
      selectInput("select_year", "Year:",
                  year_drop_down),
      
      # overall passage rate or school / state difference
      radioButtons("eval", "Bar Passage (Y-axis)",
                   choices = c("Overall Pass Rate" = "pass_rate",
                               "School / State Difference" = "pass_diff")),
      
      # gpa, lsat, or gpa / lsat combo
      radioButtons("metric", "Admissions Criteria (X-axis)",
                   choices = c("Median GPA" = "uggpa50", 
                               "Median LSAT" = "lsat50", 
                               "GPA/LSAT Combo" = "scaled_admissions")),
      
      # selectizer for schools
      selectizeInput("select_school", label = "Select Schools to Highlight:", 
                     choices = unique_schools,
                     options = list(
                       placeholder = 'Type in school name',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      
      tags$p(icon("github"),
        tags$a(href="https://github.com/shanejorr/shiny-school-bar-passage",
                    "GitHub repo")
      )),
    
      
  
  mainPanel(width = 10,
    # plot data
    plotlyOutput("plot", height = "600px")
  )),
  
  fluidRow(
    # table of dataset
    column(width = 12,
           tags$hr(),
           h4("The entire data set is below:"),
           tags$p("Data Source: ", 
                  tags$a(href="https://www.accesslex.org/analytix-by-accesslex", "ABA 509 data from AccessLex")),
           DT::dataTableOutput("full_data"))
))

# server ------------------------
server <- function(input, output) {
  
  # scatterplot of data
  output$plot <- renderPlotly({
    
    plot_rates(full, input$select_year, input$eval, input$metric, input$select_school)
    
    })
  
  # table of data
  output$full_data <- DT::renderDataTable({
    
    DT::datatable(full, rownames=FALSE,
                  colnames = c('School Name' = 'schoolname', 'Pass Rate (%)' = 'pass_rate', 'School / State Diff (%)' = 'pass_diff', 
                               'Median GPA' = 'uggpa50', 'Median LSAT' = 'lsat50', 'GPA/LSAT Combo' = 'scaled_admissions', 'Year' = 'year')
    )
  })
}

shinyApp(ui, server)
