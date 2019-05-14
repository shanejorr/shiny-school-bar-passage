###school comparision

library(tidyverse)
library(plotly)
library(shiny)
library(DT)

source('functions.R')

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

ui <- fluidPage(
  
  titlePanel("The relationship between a school's undergraduate GPA, LSAT, and bar passage rates"),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(width = 2,
      h4("Control graph:"),
      
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
