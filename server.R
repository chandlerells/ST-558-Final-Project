#import relevant packages
library(shiny)
library(tidyverse)
library(ggrepel)
library(scales)
library(caret)
#read in data
nba_data <- read_csv("nba_2022-23_all_stats_with_salary.csv")
#rename columns for interpretability that will be used as inputs for the user 
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
#store theme for sizing of various components of plots that can be added as layers and not have the same code over and over
my_theme <- theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14, face = "bold"),
                  legend.text = element_text(size=10),
                  legend.title = element_text(size = 12))
#set up server
function(input, output, session) {
  #create reactive context that filters the data based on the position selected by the user
  newVar1 <- reactive({
    position_filter <- nba_data %>%
      filter(Position == input$position)
    })
  #pull in filtered data by position and create reactive context of scatter plot based on the stat chosen by the user and salary
  newVar2 <- reactive({
    position_filter <- newVar1()
    
    basePlot1 <-
      ggplot(position_filter, aes(x = !!sym(input$stat), y = Salary/1000000)) +
      geom_point(size = 3) +
      labs(x = paste0(input$stat),
           y = "Salary ($M)",
           title = paste0("2022-23 Salary Based on ", input$stat, " for the ", input$position, " position")) +
      my_theme
    })
  #pull in filtered data by position and create reactive context of scatter plot based on the stat chosen by the user and salary colored by age
  newVar3 <- reactive({
    position_filter <- newVar1()
    
    basePlot2 <-
      ggplot(position_filter, aes(x = !!sym(input$stat), y = Salary/1000000, color = Age)) +
      geom_point(size = 3) +
      labs(x = paste0(input$stat),
           y = "Salary ($M)",
           title = paste0("2022-23 Salary Based on ", input$stat, " for the ", input$position, " position")) +
      my_theme
  })
  #create reactive context that filters the data based on the team selected by the user
  newVar4 <- reactive({
    team_filter <- nba_data %>%
      filter(Team == input$team)
    })
  #pull in filtered data by team and create reactive context of bar plot based on player name and the stat chosen by the user
  newvar5 <- reactive({
    team_filter <- newVar4()
    
    basePlot3 <- 
      ggplot(team_filter, aes(x = reorder(`Player Name`, -!!sym(input$variable)), y = !!sym(input$variable))) +
      geom_bar(stat="identity", fill = "orange") +
      labs(x = "Player Name",
           y = input$variable,
           title = paste0(input$variable, " By Player On The ", input$team, " Team For The 2022-23 Season")) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      geom_text(aes(label= !!sym(input$variable)), size = 5, position=position_dodge(width=0.9), vjust=-0.25) +
      my_theme
  })
  #create reactive context that filters the data based on the position selected by the user and creates higher level groups for age
  newVar7 <- reactive({
    age_group <- nba_data %>%
      filter(Position == input$position) %>%
      mutate(age_group = if_else(Age <= 25, "<= 25",
                                 if_else(Age <= 30, "26-30",
                                         if_else(Age <= 35, "31-35", "36-42"))))
  })
  #pull in filtered data by position and create reactive context of density plot based on the stat chosen by the user  
  newvar6 <- reactive({
    age_group <- newVar7()
    
    basePlot4 <-
      ggplot(age_group, aes(x = !!sym(input$stat))) +
      geom_density(color = 4,
                   fill = 4,
                   alpha = 0.25) +
      labs(x = paste0(input$stat),
           y = "Density",
           title = paste0("2022-23 Density of ", input$stat, " for the ", input$position, " position")) +
      my_theme
  })
  #create reactive context that filters the data based on the salary range selected by the user and creates higher level groups for age  
  newVar8 <- reactive({
    salary_range <- nba_data %>%
      filter(Salary >= input$salaryRange[1] & Salary <= input$salaryRange[2]) %>%
      mutate(age_group = if_else(Age <= 25, "<= 25",
                                 if_else(Age <= 30, "26-30",
                                         if_else(Age <= 35, "31-35", "36-42"))))
  })
  #pull in filtered data by salary range and create reactive context of box plot based on the stat chosen by the user across each position
  newvar9 <- reactive({
    salary_range <- newVar8()
    
    basePlot5 <-
      ggplot(salary_range, aes(x = Position, y = !!sym(input$stat2))) +
      geom_boxplot(color = 2,
                   fill = 2,
                   alpha = 0.25) +
      labs(x = paste0(input$stat2),
           y = "Value",
           title = paste0("2022-23 Boxplot of ", input$stat2, " by position")) +
      my_theme
  })
  
  #now that we have the proper base plots and filtered data, create plot object to use on the ui
  output$plot <- renderPlot({
    #pull in various filtered data objects
    position_filter <- newVar1()
    team_filter <- newVar4()
    salary_range <- newVar8()
    #pull in various base plot objects for the different types of plots
    basePlot1 <- newVar2()
    basePlot2 <- newVar3()
    basePlot3 <- newvar5()
    basePlot4 <- newvar6()
    basePlot5 <- newvar9()
    #repel overlapping text labels of player name
    playerName <- geom_text_repel(aes(label = position_filter$`Player Name`), 
                                  size = 5,
                                  hjust = 0.5,
                                  vjust = -1)
    #add better contrasting colors for coloring by age
    ageColor <- scale_color_gradient(low = "blue", high = "red")
    #facet wrap by position
    wrap <- facet_wrap(~Position, scales ="free_y", ncol = 1, strip.position = "left")
    #facet wrap by age group
    wrap2 <- facet_wrap(~age_group, scales ="free_y", ncol = 2, strip.position = "left")
    #use if else logic to display the appropriate plot based on which plot the user selected and if they selected certain options that are not apart of the base plot
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
                 input$facetWrapPosition == TRUE) {
        
        basePlot3 + wrap
        
      } else if (input$plotType == "Bar Plot") {
        
        basePlot3
        
      } else if (input$plotType == "Density" &
                 input$facetWrapAge == TRUE) {
        
        basePlot4 + wrap2
        
      } else if (input$plotType == "Density") {
        
        basePlot4
        
      } else if (input$plotType == "Box Plot" &
                 input$facetWrapAge2 == TRUE) {
        
        basePlot5 + wrap2
        
      } else if (input$plotType == "Box Plot") {
        
        basePlot5
        
      }
    #change size of plot
    }, height = 550, width = 1100
    )
  #create reactive context that filters the data based on the position selected by the user and only pull in the stat column selected by user
  getData <- reactive({
    newData <- nba_data %>% 
      filter(Position == input$position) %>%
      select(input$stat)
  })
  #create reactive context that filters the data based on the team selected by the user and only pull in the stat column selected by user  
  getData2 <- reactive({
    newData2 <- nba_data %>% 
      filter(Team == input$team) %>%
      select(input$variable)
  })
  #create reactive context that filters the data based on the salary range selected by the user and only pull in that the stat column selected by user and position column
  getData3 <- reactive({
    newData3 <- nba_data %>%
      filter(Salary >= input$salaryRange[1] & Salary <= input$salaryRange[2]) %>%
      select(Position, input$stat2)
  })
  #create render UI object that will be used to create the various text outputs of numeric summaries selected by the user
  output$info <- renderUI({
    #pull in filtered data objects
    newData <- getData()
    newData2 <- getData2()
    newData3 <- getData3()
    position_filter <- newVar1()
    team_filter <- newVar4()
    salary_range <- newVar8()
    #use if else logic to display the appropriate numeric summary based on which plot the user selected and the summary type selected
    if ((input$plotType == "Scatter Plot" || input$plotType == "Density") &
        input$summaryType == "Mean and Standard Deviation") {
      #create text string that displays mean of salary and stat chosen by user
      str1 <- h4(paste("The Average Salary (in Millions) and", input$stat, "for the", 
                    input$position, "Position is", 
                    dollar(round(mean(position_filter$Salary, na.rm = TRUE), 2)), 
                    "and",
                    format(round(mean(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                    ", respectively."))
      #create text string that displays sd of salary and stat chosen by user
      str2 <- h4(paste("The Standard Deviation for Salary (in Millions) and", input$stat, "for the", 
                    input$position, "Position is", 
                    dollar(round(sd(position_filter$Salary, na.rm = TRUE), 2)), 
                    "and",
                    format(round(sd(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                    ", respectively."))
      #paste two strings together so that both can be displayed on ui
      HTML(paste(str1, str2, sep = '<br/>'))
      
      } else if ((input$plotType == "Scatter Plot" || input$plotType == "Density") &
                 input$summaryType == "Median and IQR") {
        #create text string that displays median of salary and stat chosen by user
        str1 <- h4(paste("The Median Salary (in Millions) and", input$stat, "for the", 
                      input$position, "Position is", 
                      dollar(round(median(position_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(median(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE)),
                      ", respectively.")
        #create text string that displays IQR of salary and stat chosen by user
        str2 <- h4(paste("The IQR for Salary (in Millions) and", input$stat, "for the", 
                      input$position, "Position is", 
                      dollar(round(IQR(position_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(IQR(newData[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively."))
        #paste two strings together so that both can be displayed on ui
        HTML(paste(str1, str2, sep = '<br/>'))  
        
      } else if (input$plotType == "Bar Plot" &
                 input$summaryType == "Mean and Standard Deviation") {
        #create text string that displays mean of salary and stat chosen by user for the team selected by the user
        str1 <- h4(paste("The Average Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(mean(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(mean(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE)),
                      ", respectively.")
        #create text string that displays sd of salary and stat chosen by user for the team selected by the user
        str2 <- h4(paste("The Standard Deviation for Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(sd(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(sd(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively."))
        #paste two strings together so that both can be displayed on ui
        HTML(paste(str1, str2, sep = '<br/>'))
        
      } else if (input$plotType == "Bar Plot" &
                 input$summaryType == "Median and IQR") {
        #create text string that median of salary and stat chosen by user for the team selected by the user
        str1 <- h4(paste("The Median Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(median(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(median(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively."))
        #create text string that displays IQR of salary and stat chosen by user for the team selected by the user
        str2 <- h4(paste("The IQR for Salary (in Millions) and", input$variable, "for the", 
                      input$team, "Team is", 
                      dollar(round(IQR(team_filter$Salary, na.rm = TRUE), 2)), 
                      "and",
                      format(round(IQR(newData2[[1]], na.rm = TRUE), 2), big.mark = ",", scientific = FALSE),
                      ", respectively."))
        #paste two strings together so that both can be displayed on ui
        HTML(paste(str1, str2, sep = '<br/>'))
        
      } else if (input$plotType == "Box Plot" &
                 input$summaryType == "Mean and Standard Deviation") {
        #create text string that displays the mean of the stat chosen by user across the different positions
        str1 <- h4(paste("The Average ", input$stat2, " for the PG Position is", 
                      round(mean(select(filter(newData3, Position == "PG"), input$stat2)[[1]]), 2), ", ",
                      round(mean(select(filter(newData3, Position == "SG"), input$stat2)[[1]]), 2), " for the SG Position, ",
                      round(mean(select(filter(newData3, Position == "SF"), input$stat2)[[1]]), 2), " for the SF Position, ",
                      round(mean(select(filter(newData3, Position == "PF"), input$stat2)[[1]]), 2), " for the PF Position, and ",
                      round(mean(select(filter(newData3, Position == "C"), input$stat2)[[1]]), 2), " for the C Position."
                      ))
        #create text string that displays the sd of the stat chosen by user across the different positions
        str2 <- h4(paste("The Standard Deviation of ", input$stat2, " for the PG Position is", 
                      round(sd(select(filter(newData3, Position == "PG"), input$stat2)[[1]]), 2), ", ",
                      round(sd(select(filter(newData3, Position == "SG"), input$stat2)[[1]]), 2), " for the SG Position, ",
                      round(sd(select(filter(newData3, Position == "SF"), input$stat2)[[1]]), 2), " for the SF Position, ",
                      round(sd(select(filter(newData3, Position == "PF"), input$stat2)[[1]]), 2), " for the PF Position, and ",
                      round(sd(select(filter(newData3, Position == "C"), input$stat2)[[1]]), 2), " for the C Position."
                      ))
        #paste two strings together so that both can be displayed on ui
        HTML(paste(str1, str2, sep = '<br/>'))
        
      } else if (input$plotType == "Box Plot" &
                 input$summaryType == "Median and IQR") {
        #create text string that displays the median of the stat chosen by user across the different positions
        str1 <- h4(paste("The Median ", input$stat2, " for the PG Position is", 
                      round(median(select(filter(newData3, Position == "PG"), input$stat2)[[1]]), 2), ", ",
                      round(median(select(filter(newData3, Position == "SG"), input$stat2)[[1]]), 2), " for the SG Position, ",
                      round(median(select(filter(newData3, Position == "SF"), input$stat2)[[1]]), 2), " for the SF Position, ",
                      round(median(select(filter(newData3, Position == "PF"), input$stat2)[[1]]), 2), " for the PF Position, and ",
                      round(median(select(filter(newData3, Position == "C"), input$stat2)[[1]]), 2), " for the C Position."
                      ))
        #create text string that displays the IQR of the stat chosen by user across the different positions
        str2 <- h4(paste("The IQR of ", input$stat2, "for the PG Position is", 
                      round(IQR(select(filter(newData3, Position == "PG"), input$stat2)[[1]]), 2), ", ",
                      round(IQR(select(filter(newData3, Position == "SG"), input$stat2)[[1]]), 2), " for the SG Position, ",
                      round(IQR(select(filter(newData3, Position == "SF"), input$stat2)[[1]]), 2), " for the SF Position, ",
                      round(IQR(select(filter(newData3, Position == "PF"), input$stat2)[[1]]), 2), " for the PF Position, and ",
                      round(IQR(select(filter(newData3, Position == "C"), input$stat2)[[1]]), 2), " for the C Position."
                      ))
        #paste two strings together so that both can be displayed on ui
        HTML(paste(str1, str2, sep = '<br/>'))
        
      }
    })
  #create table object to display on ui
  output$table <- renderTable({
    #use if else logic to filter data based on the type of plot selected, and if the high earner option is selected from the user
    if (input$top == TRUE &
        (input$plotType == "Scatter Plot" || input$plotType == "Density")) {
      #filter the data based on the position selected and select only the player name, salary column, and top earners based on the number specified by the user
      nba_data %>% 
        filter(Position == input$position) %>%
        select(`Player Name`, Salary) %>%
        top_n(as.integer(input$number)) %>%
        mutate(Salary = dollar(Salary))
      
    } else if (input$top2 == TRUE) {
      #select only the player name, salary column, and top earners based on the number specified by the user
      nba_data %>% 
        select(`Player Name`, Salary) %>%
        top_n(as.integer(input$number2)) %>%
        mutate(Salary = dollar(Salary))
      
    }
#change size of table
  }, height = 550, width = 1100)

  observeEvent(input$rfVars,  {
    updateSliderInput(session = session, inputId = "mtry", max = length(input$rfVars))
  })

  
  #models <- reactive({
    #create index to split on
    #index <- createDataPartition(nba_data$Salary, 
                                 #p = input$split, 
                                 #list = FALSE)
    #create training and test set by indexing index object
    #train_set <- nba_data[index, ]
    #test_set <- nba_data[-index, ]
  #})
  
  
  #output$mlr2<- renderPrint({
    
    #train_set <- models()
    
    #mlr_train_set <- train_set %>%
      #select(Salary, !!!input$mlrVars)
    
    #mlr_test_set <- test_set %>%
      #select(Salary, !!!input$mlrVars)
    
    #train the model with proper method
    #mlr.fit <- train(Salary ~ .,
                     #data = mlr_train_set,
                     #method = 'lm',
                     #na.action = na.pass,
                     #trControl = trainControl(method = 'cv',
                                              #number = 5))
    
    #summary(mlr.fit)
    
  #})
  
  #output$rf <- renderPrint({
    
    #train_set <- models()
    
    #if(input$cvChoice == "Cross-Validation") {
      #set up the train control parameters we want
      #tr <- trainControl(method = 'cv',
                         #number = as.numeric(input$cv))
      
    #} else {
      #set up the train control parameters we want
      #tr <- trainControl(method = 'repeatedcv',
                        # number = as.numeric(input$rcv),
                         #repeats = as.numeric(input$rcv))
    #}
    
    #rf_train_set <- train_set %>%
      #select(Salary, !!!input$rfVars)
    
    #rf_test_set <- test_set %>%
      #select(Salary, !!!input$rfVars)
    
    #train the model with proper method
    #rf.fit <- train(Salary ~ .,
                     #data = rf_train_set,
                     #method = 'rf',
                     #na.action = na.pass,
                     #trControl = tr)
    
    #summary(rf.fit)
    
  #})
  
  
  mlr <- eventReactive(input$fit, {
    
    index <- createDataPartition(nba_data$Salary, 
                                 p = input$split, 
                                 list = FALSE)
    #create training and test set by indexing index object
    train_set <- nba_data[index, ]
    test_set <- nba_data[-index, ]
    
    mlr_train_set <- train_set %>%
      select(Salary, !!!input$mlrVars)
    
    mlr.fit <- train(Salary ~ .,
                     data = mlr_train_set,
                     method = 'lm',
                     na.action = na.pass,
                     trControl = trainControl(method = 'cv',
                                              number = 5))
    
    return(mlr.fit)
    
  })
  
  rf <- eventReactive(input$fit, {
    
    index <- createDataPartition(nba_data$Salary, 
                                 p = input$split, 
                                 list = FALSE)
    #create training and test set by indexing index object
    train_set <- nba_data[index, ]
    test_set <- nba_data[-index, ]
    
    rf_train_set <- train_set %>%
      select(Salary, !!!input$rfVars)
    
    if(input$cvChoice == "Cross-Validation") {
      #set up the train control parameters we want
      tr <- trainControl(method = 'cv',
                         number = input$cv)
    
    } else {
      #set up the train control parameters we want
      tr <- trainControl(method = 'repeatedcv',
                         number = input$rcv,
                         repeats = input$rcv2)
    }
    
    grid <- expand.grid(mtry = seq(from = input$mtry[1], to = input$mtry[2], by = 1))
    
    rf.fit <- train(Salary ~ .,
                    data = rf_train_set,
                    method = 'rf',
                    na.action = na.pass,
                    tuneGrid = grid,
                    trControl = tr
                    )
    
    return(rf.fit)
    
  })
  
  output$mlrSum <- renderPrint({
    summary(mlr())
  })
  
  output$rfSum <- renderPrint({
    rf()
  })
  
}



