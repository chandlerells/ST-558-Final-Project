library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
fluidPage(
  tabsetPanel(
    tabPanel("Data Exploration", fluid = TRUE,
             titlePanel("Exploratory Data Analysis"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("plotType", 
                             h3("Select Plot Type"),
                             choices = c("Scatter Plot", "Bar Plot"),
                             ),
                 
                 conditionalPanel(condition = "input.plotType == 'Scatter Plot'",
                                  selectInput("position",
                                              h4("Select Position"),
                                              selected = unique(nba_data$Position)[1],
                                              choices = as.factor(nba_data$Position)),
                                  selectInput("stat",
                                              h4("Select Statistic"),
                                              choices = names(nba_data %>%
                                                                select(contains("_")))),
                                  checkboxInput("playerName", h5("Show Data Labels?")),
                                  checkboxInput("ageColor", h5("Color Points by Age?"))
                                  ),
                 
                 conditionalPanel(condition = "input.plotType == 'Bar Plot'",
                                  selectInput("team",
                                              h4("Select Team"),
                                              choices = as.factor(nba_data$Team)),
                                  selectInput("variable",
                                              h4("Select Data Point"),
                                              choices = names(nba_data %>%
                                                                select(contains("_")))),
                                  checkboxInput("facetWrap", h5("Facet by Position?"))
                                  ),
                 selectInput("summaryType", 
                             h3("Select Summary Type"),
                             choices = c("Mean and Standard Deviation", 
                                         "Median and IQR")
                             ),
                 checkboxInput("top", h5("Show table of the highest earners based on selected position?")),
                 conditionalPanel(condition = "input.top == true",
                                  selectInput("number",
                                              h6("How Many?"),
                                              selected = 10,
                                              choices = c(5, 10, 15, 20)))
                 
                 ),
               mainPanel(
                 plotOutput("plot"),
                 htmlOutput("info"),
                 tableOutput("table")
                 )
               )
             )
    )
  )
