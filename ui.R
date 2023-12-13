#import relevant packages
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(mathjaxr)
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
                                              choices = as.factor(unique(nba_data$Position))),
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
                                              choices = as.factor(unique(nba_data$Team))),
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
                 conditionalPanel(condition = "input.plotType == 'Bar Plot' || input.plotType == 'Box Plot'",
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
    #add modeling tab
    tabPanel("Modeling", fluid = TRUE,
             #set up sub tabs for modeling tab
             tabsetPanel(
               #add modeling info sub tab on modeling tab
               tabPanel("Modeling Info", fluid = TRUE,
                        #add header and text for model explanations
                        h4(strong("Multiple Linear Regression and Random Forest Discussion")),
                        p("Below is a description of each of the modeling approaches and the benefits/drawbacks of each."),
                        br(),
                        #add description of multiple linear regression model
                        h5(strong("Multiple Linear Regression")),
                        #use mathjax to put in math equations
                        withMathJax(paste0("Multiple Linear Regression (MLR) is a simple supervised learning tool for modeling a quantitative response. One of the main benefits of MLR is that it is much simpler compared to other modern techniques and is easy to interpret. A MLR model has the form \\(Y_{i} = \\beta_{0} + X_{i1}\\beta_{1} + X_{i2}\\beta_{2} +...+ X_{ip}\\beta_{p} + \\epsilon_{i}\\), where \\(Y_{i}\\) is a quantitative response, \\(X_{i1},...,X_{ip}\\) are predictor variables, and \\(\\epsilon_{i}\\) is the unobserved random error. This model can be extended in many ways by including higher order polynomial terms and interaction effects between the predictors. \\(\\beta_{0}\\) is the intercept, the coefficients, \\(\\beta_{j} = 1,...,p\\), are coefficients associated with predictor \\(X_{ij}\\). The value of \\(\\beta_{j}\\) indicates the strength and direction of the linear relationship between \\(X_{ij}\\) and \\(Y_{i}\\) and can be interpreted as the rate of change in the mean response due to a one unit increase in the j-th predictor while keeping the other predictors fixed. The coefficients are true unknown values that need to be estimated from the data. Upon estimation, the resulting equation becomes, \\(\\hat{Y}_i = \\hat{\\beta}_0 + X_{i1}\\hat{\\beta}_1 + X_{i2}\\hat{\\beta}_2 +...+ X_{ip}\\hat{\\beta}_p\\), which is used to make predictions. A common estimation procedure for the regression coefficients is the least squares technique, which first starts by calculating the residual sum of squares, which is the sum of all the true observed values minus the corresponding predicted values (residuals), squared. The coefficients are then estimated by minimizing the sum of squared residuals, \\(\\sum_{i=1}^n (Y_{i}- \\hat{\\beta}_0 - X_{i1}\\hat{\\beta}_1 -...- X_{ip}\\hat{\\beta}_p)^2\\), with respect to \\(\\beta_{j}s\\). The major drawbacks of the MLR model are the numerous statistical assumptions made on the data and the lack of predictive power compared to other models. Specifically, this model assumes a linear relationship between the response and predictors, constant variance in the errors, and the errors being normally distributed. There can be a lack of performance if one of these assumptions are violated, or if there exists influential points and multicollinearity among the predictors.")),
                        br(),
                        br(),
                        br(),
                        #add description of random forest model
                        h5(strong("Random Forest")), 
                        p("This description will assume a regression task, but the concept is similar to a classification task. A tree based method partitions the feature space into a set of regions, and then fits a simple model, such as a constant function, in each region. The splits to create the partition are performed using recursive binary splitting, where every distinct value of every predictor is searched through to find the predictor and split point that partitions the data into two groups such that the overall sums of squares error is minimized. This process continues until some stopping criteria is reached. The predicted value is the average of the responses in the region in which the new observation falls in. This can be extended to many concepts such as bagging and random forests. Bagging uses bootstrap aggregation to fit many trees and creates a final prediction using an average of the predicted values from all of the trees. Random forest builds upon bagging by taking this one step further, where for each tree at each split, only a subset of predictors are used. This can improve prediction over a single tree fit and over bagged trees by reducing the overall variance of the predictions. A single tree fit is highly variable, where a small change in the data could result in a completely different tree fit. Bagging attempts to improve this by taking an average of many predicted values from many trees, since the variance of an average with trees that are independent and identically distributed is lower than the variance of one single tree fit. While bagging generally reduces the variance and overall test error, these trees are highly correlated since if there is one or two strong predictors that explain the response, they will most likely dominate all the tree fits, having similar splits. This means the average doesn't reduce the variance as much. Random forest corrects this correlation between the trees by only looking at a subset of predictors at the splits. The goal of this is to reduce the correlation between the trees which in hope would reduce the variance of the average predicted value and lower the overall test error. As you can see, the major benefit to a random forest model is its predictive power. Random forests are also able to utilize different methodologies to look at variable importance measures, which can be helpful for variable selection, and do not make statistical assumptions. The major drawbacks are the lack of interpretability and potentially high computation time.")
                        ),
               #add model fitting sub tab on modeling tab
               tabPanel("Model Fitting", fluid = TRUE,
                        #set up sidebar layout
                        sidebarLayout(
                          sidebarPanel(
                            #add instructions description
                            p(strong("Instructions:"),"Select model specifications below and click the 'Fit Models' button once ready. Once the button is clicked, the models will be trained on the training data, and once complete, will output various fit statistics on the training and test data sets. If you would like to fit the models with different specifications, change the options provided and re-click the 'Fit Models' button."),
                            #add slider input for selecting train/set split
                            sliderInput("split", "Select train/test split",
                                        min = 0, max = 1, value = .8),
                            #allow user to select any columns from data set as predictors
                            varSelectInput("mlrVars", "Select Predictor Variables to use in the Multiple Linear Regression Model", 
                                           data = nba_data[, 4:52],
                                           selected = c("Age", "Points_Per_Game", "Player_Efficiency_Rating", "True_Shooting_Percentage", "Win_Shares"),
                                           multiple = TRUE),
                            #allow user to select any columns from data set as predictors
                            varSelectInput("rfVars", "Select Predictor Variables to use in the Random Forest Model", 
                                           data = nba_data[, 4:52],
                                           selected = c("Position", "Age", "Team", "Points_Per_Game", "Player_Efficiency_Rating", "True_Shooting_Percentage", "Win_Shares"),
                                           multiple = TRUE),
                            #allow user to select number of predictors at each split
                            sliderInput("mtry", "Select Tuning Parameter Range for the Number of Predictors Used at Each Split in the Random Forest Model",
                                        min = 1,
                                        max = 7,
                                        value = c(1,6),
                                        step = 1),
                            #allow user to select between cv and repeated cv
                            selectInput("cvChoice",
                                        h4("Select Tuning Methodology for Random Forest Model"),
                                        choices = c("Cross-Validation", "Repeated Cross-Validation")),
                            #condition on cv choice based on needing repeats and folds, or just folds
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
                            #add button that user clicks to fit models
                            actionButton("fit", "Fit Models")
                          ),
                          #set up main panel
                          mainPanel(
                            #add mlr performance on test set
                            h5(strong("Multiple Linear Regression Model Performance on the Test Set")),
                            verbatimTextOutput("mlrPred"),
                            #add mlr performance on train set
                            h5(strong("Multiple Linear Regression Model Performance on the Training Set")),
                            verbatimTextOutput("mlrSum"),
                            #add rf performance on test set
                            h5(strong("Random Forest Model Performance on the Test Set")),
                            verbatimTextOutput("rfPred"),
                            #add rf performance on train set
                            h5(strong("Random Forest Model Performance on the Training Set")),
                            verbatimTextOutput("rfSum"),
                            #add variable importance plot for rf
                            h5(strong("Random Forest Model Variable Importance")),
                            plotOutput("rfPlot")
                            )
                          )
                        ),
               #add prediction sub tab on modeling tab
               tabPanel("Prediction", fluid = TRUE,
                        #set up sidebar layout
                        sidebarLayout(
                          sidebarPanel(
                            #add instructions paragraph
                            p(strong("Instructions:"),"Based on the predictor variables selected for each model on the 'Model Fitting' tab, select or numeric input boxes will be provided to make predictions on salary. In order for predictions to be shown, models need to have been trained on the 'Model Fitting' tab by clicking the 'Fit Models' button. As long as there are actively trained models, you can change the values below, and the predictions will automatically be adjusted. If you would like to make predictions from each model on different predictors, you must re-train the models on the 'Model Fitting' tab."),
                            br(),
                            #add dynamic text or numeric input boxes based on predictors selected from server
                            h3("Multiple Linear Regression Model"),
                            uiOutput("mlrBoxes"),
                            br(),
                            #add dynamic text or numeric input boxes based on predictors selected from server
                            h3("Random Forest Model"),
                            uiOutput("rfBoxes")
                            ),
                          #set up main panel
                          mainPanel(
                            #add table output of new prediction from mlr model
                            h4("Predicted Salary Using Multiple Linear Regression Model Based on Provided Inputs"),
                            h4(strong(tableOutput("mlrNewPred"))),
                            #add table output of new prediction from rf model
                            h4("Predicted Salary Using Random Forest Model Based on Provided Inputs"),
                            h4(strong(tableOutput("rfNewPred")))
                            )
                          )
                        )
               )
             )
    )
  )
  
  
