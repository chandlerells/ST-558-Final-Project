# ST-558-Final-Project

## Description of app and its purpose

The `purpose` of this app is to explore and model `NBA data from the 2022-23 season`, sepcifically using `salary` as the response. Included in this app are 3 tabs, titled `"About"`, `"Data Exploration"`, and `"Modeling"`. The `"About"` tab describes the purpose of the app, briefly discusses the data and its source, and tells the user about each tab. The `"Data Exploration"` tab allows the user to create numerical and graphical summaries. Specifically, graphical summaries include a scatterplot, bar plot, density plot, and box plot. The user is able to change the type of plot shown, the variable being looked at, and filter the rows to change the data used in the plots. Numerical summaries include a combination of mean and standard deviation, or a combination of median and IQR. There is also an option to look at a data table with the top n earners based on salary. Lastly, the `"Modeling"` tab fits two types of supervised learning models to our data, a multiple linear regression model and random forest model. Included on this tab are three subtabs. The first is a `"Modeling Info"` tab where an explanation is provided for each of the modeling approaches and the benefits/drawbacks of each. The second is a `"Model Fitting"` tab where the user is able to choose a % for test/train split and the predictor variables for each model. For the random forest model, the user can specify the tuning parameter grid and the CV settings. The last tab is a `"Prediction"` tab, where the user has the ability to use both models for prediction.

## List of packages needed to run the app

The following packages are need to run the app:  

* `shiny`
* `tidyverse`
* `ggrepel`
* `scales`
* `caret`
* `shinyWidgets`
* `mathjaxr`

### Code to install all the packages used:

`install.packages(c("shiny", "tidyverse", "ggrepel", "scales", "caret", "shinyWidgets", "mathjaxr"))`

## Code to run app in RStudio

`shiny::runGitHub("ST-558-Final-Project", "chandlerells", ref = "main")`
