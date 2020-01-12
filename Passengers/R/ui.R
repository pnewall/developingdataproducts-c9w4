if (!"shiny" %in% installed.packages()) {
    install.packages("shiny")
}
library(shiny)

if (!"DT" %in% installed.packages()) {
    install.packages("DT")
}
library(DT)

if (!"plotly" %in% installed.packages()) {
    install.packages("plotly")
}
library(plotly)

if (!"dplyr" %in% installed.packages()) {
    install.packages("dplyr")
}
library(dplyr)

if (!"ggplot2" %in% installed.packages()) {
    install.packages("ggplot2")
}
library(ggplot2)

if (!"reshape2" %in% installed.packages()) {
    install.packages("reshape2")
}
library(reshape2)

if (!"rpart" %in% installed.packages()) {
    install.packages("rpart")
}
#library(rpart)

if (!"rattle" %in% installed.packages()) {
    install.packages("rpart")
}
library(rattle)

if (!"caret" %in% installed.packages()) {
    install.packages("caret")
}
library(caret)

if (!"sjlabelled" %in% installed.packages()) {
    install.packages("sjlabelled")
}
library(sjlabelled)

if (!"sjmisc" %in% installed.packages()) {
    install.packages("sjmisc")
}
library(sjmisc)

if (!"sm" %in% installed.packages()) {
    install.packages("sm")
}
library(sm)

#' ui.R
#'
#' @param Sex/Gender - female/male
#' 
#' @param Class - passenger's class of travel - 1, 2 or 3
#' 
#' @param Embarked - port of embarcation, one of Cherbourg, Queenstown or Southampton
#' 
#' @param SibSp - Nr of Siblings or Spouses accompanying - 1 to 10
#' 
#' @param Parch - Nr of Parents or Children accompanying - 1 to 10
#' 
#' @description
#' This Shiny application uses an adapted version of the Titanic passenger dataset on Kaggle to
#' demonstrate how analyses and predictions can be made part of an interactive Web-based user
#' experience.
#'
#' The ui function defines the page layout for the Shiny application.  It places a parameter 
#' panel on the left-hand side leaving space for 3 tabs over the rest of the page.  The initial 
#' parameter settings and view display the whole dataset.  Users are then free to adjust any of 
#' the parameters and so slice the current data.  As the view of the data changes, so too will 
#' the probability of survival and the content of the 3 tabs.
#' 
#' The 3 tabs are:
#' 
#' 1 - Histogram of passengers vs survivors
#' 
#' 2 - 3D charts showing the influence of the predictor variables on survival
#' 
#' 3 - Decision tree of probabilities for the current data slice
#'
#' @export
ui <- (fluidPage(

    # Application title
    
    titlePanel("Wayfinder - Titanic Passenger Data"),
    
    # Sidebar with various inputs for the different predictors
    
    sidebarLayout(
        
        position = "left", fluid = TRUE,
        
        sidebarPanel(
            h4("De-select or slide buttons closer together to look at a smaller dataset"),
            
            checkboxGroupInput("Sex", "Gender:", 
                               choiceNames = list("Female", "Male"),
                               choiceValues = list("female", "male"), 
                               selected = list("female", "male")),
            
            checkboxGroupInput("Class", "Class of Travel:", 
                               choiceNames = list("1st", "2nd", "3rd"),
                               choiceValues = list("1", "2", "3"), 
                               selected = list("1", "2", "3")),
            
            checkboxGroupInput("Embarked", "Embarked at Port:", 
                               choiceNames = list("Cherbourg", "Queenstown", "Southampton"),
                               choiceValues = list("C", "Q", "S"), 
                               selected = list("C", "Q", "S")),
            
            sliderInput("SibSp", "Nr of Siblings or Spouses accompanying:",  
                        min = 0, max = 10, 
                        value = c(0, 10)),
            
            sliderInput("Parch", "Nr of Parents or Children accompanying:",  
                        min = 0, max = 10, 
                        value = c(0, 10)), 
        
            width = 2
        ),
        
        # Show a plot of the generated distribution
        
        mainPanel(titlePanel(h4(textOutput("row_count"), align = "center")),
                  tabsetPanel(
                      tabPanel("Survival by Age Group",
                               br(),
                               plotlyOutput("distPlot", height = "700px")
                      ),
                      tabPanel("3D Analysis",
                               br(),
                               fluidRow(
                                   column(6, plotlyOutput("ageSexParch"), height = "700px"),
                                   column(6, plotlyOutput("ageEmbarkedSibSp"), height = "700px")
                               )
                      ),
                      tabPanel("Decision Tree", plotOutput("dTree", height = "700px"))
                 ),
                 width = 10
        )        
    )
))
