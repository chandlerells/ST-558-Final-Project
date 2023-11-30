library(shiny)
library(tidyverse)
library(ggrepel)
library(scales)

nba_data <- read_csv("nba_2022-23_all_stats_with_salary.csv")

nba_data <- nba_data %>%
  rename(Minutes_Per_Game = MP, 
         Field_Goals_Made_Per_Game = FG,
         Field_Goal_Percentage = 'FG%',
         Three_Pointers_Made_Per_Game = '3P',
         Two_Pointers_Made_Per_Game = '2P',
         Assists_Per_Game = AST,
         Points_Per_Game = PTS,
         Player_Efficiency_Rating = PER,
         True_Shooting_Percentage = 'TS%',
         Win_Shares = WS)

function(input, output, session) {
  
  newVar1 <- reactive({
    position_filter <- nba_data %>%
      filter(Position == input$position)
    })
  
  newVar2 <- reactive({
    position_filter <- newVar1()
    
    basePlot1 <-
      ggplot(position_filter, aes(x = !!sym(input$stat), y = Salary/1000000)) +
      geom_point() +
      labs(x = paste0(input$stat),
           y = "Salary ($M)",
           title = paste0("2022-23 Salary Based on ", input$stat, " for the ", input$position, " position")) +
      theme(plot.title = element_text(hjust = 0.5))
    })
  
  newVar3 <- reactive({
    position_filter <- newVar1()
    
    basePlot2 <-
      ggplot(position_filter, aes(x = !!sym(input$stat), y = Salary/1000000, color = Age)) +
      geom_point() +
      labs(x = paste0(input$stat),
           y = "Salary ($M)",
           title = paste0("2022-23 Salary Based on ", input$stat, " for the ", input$position, " position")) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  newVar4 <- reactive({
    team_filter <- nba_data %>%
      filter(Team == input$team)
    })
  
  newvar5 <- reactive ({
    team_filter <- newVar4()
    
    basePlot3 <- 
      ggplot(team_filter, aes(x = reorder(`Player Name`, -!!sym(input$variable)), y = !!sym(input$variable))) +
      geom_bar(stat="identity", fill = "orange") +
      labs(x = "Player Name",
           y = input$variable,
           title = paste0(input$variable, " By Player On The ", input$team, " Team For The 2022-23 Season")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      geom_text(aes(label= !!sym(input$variable)), size = 3, position=position_dodge(width=0.9), vjust=-0.25)
  })
  
  

  
  output$plot <- renderPlot({
    
    position_filter <- newVar1()
    team_filter <- newVar4()
    
    basePlot1 <- newVar2()
    basePlot2 <- newVar3()
    basePlot3 <- newvar5()
    
    playerName <- geom_text_repel(aes(label = position_filter$`Player Name`), 
                                  size = 5,
                                  hjust = 0.5,
                                  vjust = -1)
    
    ageColor <- scale_color_gradient(low = "blue", high = "red")
    
    wrap <- facet_wrap(~Position, scales ="free_y", ncol = 1, strip.position = "left")
    
    
    if (input$plotType == "Scatter Plot" &
        input$playerName == TRUE & 
        input$ageColor == TRUE) {
      
      basePlot2 + playerName + ageColor
      
      } else if (input$plotType == "Scatter Plot" &
                 input$playerName == TRUE) {
        
        basePlot1 + playerName
        
      } else if (input$plotType == "Scatter Plot" &
                 input$ageColor == TRUE) {
        
        basePlot2 + ageColor
        
      } else if (input$plotType == "Scatter Plot") {
        
        basePlot1
        
      } else if (input$plotType == "Bar Plot" &
                 input$facetWrap == TRUE) {
        
        basePlot3 + wrap
        
      } else if (input$plotType == "Bar Plot") {
        
        basePlot3
        
      }
    
    }#, height = 700, width = 1100
    )
  
  getData <- reactive({
    newData <- nba_data %>% 
      filter(Position == input$position) %>%
      select(input$stat)
  })
  
  getData2 <- reactive({
    newData2 <- nba_data %>% 
      filter(Team == input$team) %>%
      select(input$variable)
  })
  
  output$info <- renderUI({
    
    newData <- getData()
    newData2 <- getData2()
    position_filter <- newVar1()
    team_filter <- newVar4()
    
    if (input$plotType == "Scatter Plot" &
        input$summaryType == "Mean and Standard Deviation") {
      
      str1 <- paste("The Average Salary (in Millions) and", input$stat, "for the", 
                    input$position, "Position is", 
                    dollar(round(mean(position_filter$Salary, na.rm = TRUE), 2)), 
                    "and",
                    format(round(mean(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                    ", respectively.")
      
      str2 <- paste("The Standard Deviation for Salary (in Millions) and", input$stat, "for the", 
                    input$position, "Position is", 
                    dollar(round(sd(position_filter$Salary, na.rm = TRUE), 2)), 
                    "and",
                    format(round(sd(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                    ", respectively.")
      
      HTML(paste(str1, str2, sep = '<br/>'))
      
      } else if (input$plotType == "Scatter Plot" &
                 input$summaryType == "Median and IQR") {
        
        str1 <- paste("The Median Salary (in Millions) and", input$stat, "for the", 
                      input$position, "Position is", 
                      dollar(round(median(position_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(median(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively.")
        
        str2 <- paste("The IQR for Salary (in Millions) and", input$stat, "for the", 
                      input$position, "Position is", 
                      dollar(round(IQR(position_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(IQR(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively.")
        
        HTML(paste(str1, str2, sep = '<br/>'))  
        
      } else if (input$plotType == "Bar Plot" &
                 input$summaryType == "Mean and Standard Deviation") {
        
        str1 <- paste("The Average Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(mean(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(mean(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively.")
        
        str2 <- paste("The Standard Deviation for Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(sd(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(sd(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively.")
        
        HTML(paste(str1, str2, sep = '<br/>'))
        
      } else if (input$plotType == "Bar Plot" &
                 input$summaryType == "Median and IQR") {
        
        str1 <- paste("The Median Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(median(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(median(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively.")
        
        str2 <- paste("The IQR for Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(IQR(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(IQR(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively.")
        
        HTML(paste(str1, str2, sep = '<br/>'))
        
      }
    })
  
  output$table <- renderTable({
    
    if (input$top == TRUE) {
      
      nba_data %>% 
        filter(Position == input$position) %>%
        select(`Player Name`, Salary) %>%
        top_n(as.integer(input$number)) %>%
        mutate(Salary = dollar(Salary))
    }

  })

}



