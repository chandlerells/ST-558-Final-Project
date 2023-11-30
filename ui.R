library(shiny)

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
                             choices = c("Mean", "Standard Deviation"),
                 )
                 ),
               mainPanel(
                 plotOutput("plot"),
                 textOutput("info")
                 )
               )
             )
    )
  )
