#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  tabsetPanel(
    tabPanel("Data Exploration", fluid = TRUE,
             titlePanel("Exploratory Data Analysis"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("plot_type", 
                             "Select Plot Type",
                             choices = c("Scatter Plot"),
                             ),
                 selectInput("position",
                             "Select Position",
                             selected = "PG",
                             choices = c("C", "PG", "SG", "PF","SF")),
                 selectInput("stat",
                             "Select Statistic",
                             selected = "PTS",
                             choices = c("MP", "FG%", "3P", "2P","AST", "PTS", "PER",
                                         "TS%","WS"))
                 ),
               mainPanel(
                 plotOutput("plot")
                 )
               )
             )
    )
  )
