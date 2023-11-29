library(shiny)
library(tidyverse)

nba_data <- read_csv("nba_2022-23_all_stats_with_salary.csv")


function(input, output, session) {

    output$plot <- renderPlot({
      
      position_filter <- nba_data %>%
        filter(Position == input$position)
      
      ggplot(position_filter, aes(x = !!sym(input$stat), y = Salary/1000000)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        labs(x = paste0(input$stat, " Per Game"),
             y = "Salary ($M)",
             title = paste0("Salary Based on ", input$stat, " per game for the ", input$position, " position"))
      
    })

}