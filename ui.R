#import relevant packages
library(shiny)
library(tidyverse)
library(shinyWidgets)

# Define UI for application
fluidPage(
  #set background color of app
  setBackgroundColor("bisque"),
  #set up app to have different tabs
  tabsetPanel(
    #set up about tab, that discusses project
    tabPanel("About", fluid = TRUE,
             #add nba logo image
             img(src = "nba-logo.png", style = "width: 200px", align = "right"),
             #add section that describes the purpose of the app
             h3("App Purpose"),
             br(),
             p("The ", strong("purpose"), " of this app is to explore and model NBA data from the 2022-23 season, sepcifically using salary as the response."),
             #add section that describes the data
             h3("Data Source"),
             br(),
             p("This dataset merges player per-game and advanced statistics for the ", strong("NBA's 2022-23 season"), " with player salary data, creating a comprehensive resource for understanding the performance and financial aspects of professional basketball players. The dataset is the result of web scraping player salary information from Hoopshype, and downloading traditional per-game and advanced statistics from Basketball Reference. For more information, click here: ", a(href = "https://www.kaggle.com/datasets/jamiewelsh2/nba-player-salaries-2022-23-season/data", "NBA Data")),
             br(),
             p(strong("Key Features:")),
             p("* ", strong("Player Information"), ": Player name, team(s) played for during the season.
               * Per Game Statistics: A wide array of per-game statistics, including points scored (PPG), assists (APG), rebounds (RPG), steals (SPG), blocks (BPG), and more."),
             p("* ", strong("Shooting Efficiency"), ": Metrics like field goal percentage (FG%), three-point percentage (3P%), two-point percentage (2P%), and free throw percentage (FT%) for assessing scoring efficiency."),
             p("* ", strong("Advanced Statistics"), ": A wide array of advanced metrics such as value over replacement player (VORP), win shares (WS) and true shooting percentage (TS%)"),
             p("* ", strong("Salaries"), ": The financial aspect of the dataset includes player salaries for the 2022-23 season, offering insights into player earning"),
             #add section that describes the purpose of each tab
             h3("Tab Description"),
             br(),
             p("* ", strong("Data Exploration"), ": This tab allows the user to create numerical and graphical summaries. Specifically, graphical summaries include a scatterplot, bar plot, density plot, and box plot. The user is able to change the type of plot shown, the variable being looked at, and filter the rows to change the data used in the plots. Numerical summaries include a combination of mean and standard deviation, or a combination of median and IQR. There is also an option to look at a data table with the top n earners based on salary."),
             p("* ", strong("Modeling"), ": The modeling tab fits two types of supervised learning models to our data, a multiple linear regression model and random forest model. Included on this tab are three subtabs. The first is a ", strong("Modeling Info"), " tab where an explanation is provided for each of the modeling approaches and the benefits/drawbacks of each. The second is a ", strong("Model Fitting"), " tab where the user is able to choose a % for test/train split and the predictor variables for each model. For the random forest model, the user can specify the tuning parameter grid and the CV settings. The last tab is a ", strong("Prediction"), " tab, where the user has the ability to use both models for prediction.")
             
             ),
    #set up data exploration tab
    tabPanel("Data Exploration", fluid = TRUE,
             #add title
             titlePanel(strong("Exploratory Data Analysis")),
             #set up sidebar layout
             sidebarLayout(
               sidebarPanel(
                 #add section to select plot type
                 selectInput("plotType", 
                             h3(strong("Select Plot Type")),
                             choices = c("Scatter Plot", "Bar Plot",
                                         "Density", "Box Plot"),
                             ),
                 #add conditional panel for more options if scatter plot or density is selected
                 conditionalPanel(condition = "input.plotType == 'Scatter Plot' || input.plotType == 'Density'",
                                  #option to select position
                                  selectInput("position",
                                              h4("Select Position"),
                                              choices = as.factor(nba_data$Position)),
                                  #option to select stat
                                  selectInput("stat",
                                              h4("Select Statistic"),
                                              choices = names(nba_data %>%
                                                                select(contains("_"))))
                                  ),
                 #add conditional panel that provides different options if just scatter plot is selected
                 conditionalPanel(condition = "input.plotType == 'Scatter Plot'",
                                  #option to show player name
                                  checkboxInput("playerName", h5("Show Data Labels?")),
                                  #option to color by age group
                                  checkboxInput("ageColor", h5("Color Points by Age?"))
                                  ),
                 #add conditional panel that provides different options if just density is selected
                 conditionalPanel(condition = "input.plotType == 'Density'",
                                  #option to facet wrap by age
                                  checkboxInput("facetWrapAge", h5("Facet by Age?"))
                                  ),
                 #add conditional panel that provides different options if just bar plot is selected
                 conditionalPanel(condition = "input.plotType == 'Bar Plot'",
                                  #option to select team
                                  selectInput("team",
                                              h4("Select Team"),
                                              choices = as.factor(nba_data$Team)),
                                  #option to select stat
                                  selectInput("variable",
                                              h4("Select Data Point"),
                                              choices = names(nba_data %>%
                                                                select(contains("_")))),
                                  #option to facet wrap by position
                                  checkboxInput("facetWrapPosition", h5("Facet by Position?"))
                                  ),
                 #add conditional panel that provides different options if just box plot is selected
                 conditionalPanel(condition = "input.plotType == 'Box Plot'",
                                  #option to select stat
                                  selectInput("stat2",
                                              h4("Select Statistic"),
                                              choices = names(nba_data %>%
                                                                select(contains("_")))),
                                  #option to select salary range
                                  sliderInput("salaryRange", "Select Salary Range",
                                              min = 0, max = 50000000, value = c(0, 50000000)),
                                  #option to facet wrap by age group
                                  checkboxInput("facetWrapAge2", h5("Facet by Age?"))
                                  ),
                 #add section to select summary type
                 selectInput("summaryType", 
                             h3(strong(("Select Summary Type"))),
                             choices = c("Mean and Standard Deviation", 
                                         "Median and IQR")
                             ),
                 #add condition panel based on selected plot that gives ability to display top earners by position
                 conditionalPanel(condition = "input.plotType == 'Scatter Plot' || input.plotType == 'Density'",
                                  #option to show top earners by selected position
                                  checkboxInput("top", h5("Show table of the highest earners based on selected position?")),
                                  #add condition panel based on top earners selected option that gives ability to show certain number of players
                                  conditionalPanel(condition = "input.top == true",
                                                   #option to show how many top earners
                                                   selectInput("number",
                                                               h6("How Many?"),
                                                               selected = 10,
                                                               choices = c(5, 10, 15, 20)))),
                 #add condition panel based on selected plot that gives ability to display top earners
                 conditionalPanel(condition = "input.plotType == 'Bar Plot' || input.plotType == 'Box'",
                                  #option to show top earners
                                  checkboxInput("top2", h5("Show table of the highest earners?")),
                                  #add condition panel based on top earners selected option that gives ability to show certain number of players
                                  conditionalPanel(condition = "input.top2 == true",
                                                   #option to show how many top earners
                                                   selectInput("number2",
                                                               h6("How Many?"),
                                                               selected = 10,
                                                               choices = c(5, 10, 15, 20))))
                 ),
               #set up main panel output
               mainPanel(
                 #add plot
                 plotOutput("plot"),
                 #add multiple break lines due to size of plot
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 #add numeric summary statistics
                 htmlOutput("info"),
                 br(),
                 #add table that shows top earners
                 tableOutput("table")
                 )
               )
             ),
    
    tabPanel("Modeling", fluid = TRUE,
             tabsetPanel(
               tabPanel("Modeling Info", fluid = TRUE,
                        #set up sidebar layout
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("split", "Select train/test split",
                                        min = 0, max = 1, value = .8),
                            varSelectInput("mlrVars", "Select Predictor Variables to use in the Multiple Linear Regression Model", 
                                           data = nba_data[, 4:52],
                                           selected = c("Age", "Points_Per_Game", "Player_Efficiency_Rating", "True_Shooting_Percentage", "Win_Shares"),
                                           multiple = TRUE),
                            varSelectInput("rfVars", "Select Predictor Variables to use in the Random Forest Model", 
                                           data = nba_data[, 4:52],
                                           selected = c("Position", "Age", "Team", "Points_Per_Game", "Player_Efficiency_Rating", "True_Shooting_Percentage", "Win_Shares"),
                                           multiple = TRUE),
                            sliderInput("mtry", "Select Tuning Parameter Range for the Number of Predictors Used at Each Split in the Random Forest Model",
                                        min = 1,
                                        max = 7,
                                        value = c(1,6),
                                        step = 1),
                            selectInput("cvChoice",
                                        h4("Select Tuning Methedology for Random Forest Model"),
                                        choices = c("Cross-Validation", "Repeated Cross-Validation")),
                            conditionalPanel(condition = "input.cvChoice == 'Cross-Validation'",
                                             sliderInput("cv",
                                                         h6("How Many Folds?"),
                                                         min = 3,
                                                         max = 7,
                                                         value = 5,
                                                         step = 1)),
                            conditionalPanel(condition = "input.cvChoice == 'Repeated Cross-Validation'",
                                             sliderInput("rcv",
                                                         h6("How Many Folds?"),
                                                         min = 3,
                                                         max = 7,
                                                         value = 5,
                                                         step = 1),
                                             sliderInput("rcv2",
                                                         h6("How Many Folds?"),
                                                         min = 2,
                                                         max = 5,
                                                         value = 3,
                                                         step = 1)),
                            actionButton("fit", "Fit Models")
                            

                          ),
                          mainPanel(
                            verbatimTextOutput("rfSum")
                          ) 
                        )

),
               tabPanel("Model Fitting", fluid = TRUE,
                        
                        
                        
                        
                        
                        ),
               tabPanel("Prediction", fluid = TRUE,
                        
                        
                        
                        
                        
                        
                        )
               )
    )
  )
)
  
  
