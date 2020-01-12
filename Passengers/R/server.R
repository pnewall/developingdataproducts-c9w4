#' server
#' 
#' @param input - parameters input by ui.R
#' @param output - output to tabs etc
#' 
#' @description
#' This Shiny application uses an adapted version of the Titanic passenger dataset on Kaggle to
#' demonstrate how analyses and predictions can be made part of an interactive Web-based user
#' experience.
#' 
#' The server function tests for the presence of the dataset file in the current working 
#' directory and downloads if not there.  It then generates a dummy id for each passenger record 
#' before calling the following functions:
#' 
#' tfit -
#' uses rpart to generate the decision tree model for the whole dataset
#' 
#' tpred - 
#' returns a prediction based on that model for whatever slice of data is sent 
#' to the function
#' 
#' tfilter - 
#' reactive function to slice the data based on the curren parameter values
#' 
#' survivors - 
#' reactive function to slice the data returned by tfilter so that it contains 
#' only records for survivors
#' 
#' Then the script generates the output for the 3 tabs as follows:
#' 
#' Output for tab 1 - Survival by Age Group 
#' 
#' Updates the headline comparing the number of passengers and overall probability 
#' of survival for the current dataset with those of the whole dataset
#' 
#' Output for tab 2 - 3D Analysis
#' 
#' Plots two 3D scatter charts side by side.  Each plots survived (green) vs perished (red).
#' Despite the three dimensions, there is still some over-plotting, hence the opacity.
#' The first chart analyses the combined influence of the Age, Gender & Family Members predictors.
#' The second chart analyses the combined influence of the Age, Port of Embarcation & Spouses/Siblings
#' predictors. 
#' 
#' Outputs for tab 3 - Decision Tree -
#' 
#' Uses the rattle package and the fancyRpartPlot function
#' to generate a full decision tree.
#' 
#' @export
server <- (function(input, output){

    fileName <- "./Titanic_Simple.csv"
    fileUrl = paste(c(
        "https://gist.githubusercontent.com/pnewall/",
        "dbc4b0a52a19501cfdc71524ecd5b70c/raw/",
        "ec6664fad84393e57461cfe106ba80e924f855df/",
        "Titanic_Simple.csv"),
        collapse = "")
    
    if(!file.exists(fileName)) {
        download.file(fileUrl, "./Titanic_Simple.csv", method = "curl")
    }
    
    Titanic <- read.csv(fileName, header = T, sep = ",")
    
    # No names but give the passengers an id
    
    Titanic <- mutate(Titanic, id = sprintf("pr-%04s", rownames(Titanic)))
    
# tfit function 
#
# uses rpart to generate the decision tree model for the whole dataset

    tfit <- function(df){
        tnames <- c('Pclass', 'SibSp', 'Parch')
        df[, tnames] <- lapply(df[, tnames], factor)
        fit <- rpart::rpart(
            Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked, 
            data = df, method = "class", 
            control = rpart::rpart.control(minsplit=5, cp=0.002))
    }

# tpred function
#
# returns a prediction based on that model for whatever 
# slice of data is sent to the function
    
    tpred <- function(fit){
        tpred <- sprintf("%1.2f%%", mean(predict(fit, type = "prob")[, 2]) * 100)
    }
    
    Titanic$Survived <- set_labels(to_factor(Titanic$Survived), labels = c("Perished", "Survived"))

# tfilter function
# 
# reactive function to slice the data based on the curren parameter values
        
    tfilter <- reactive({
        
        tfdf <- Titanic %>%
            filter(Sex %in% input$Sex, 
                   Pclass %in% input$Class,
                   Embarked %in% input$Embarked,
                   between(SibSp, input$SibSp[1], input$SibSp[2]),
                   between(Parch, input$Parch[1], input$Parch[2])) %>%
            mutate(AgeRange = cut(Age, 
                                  breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                                  labels = c("0-10", "11-20", "21-30", "31-40", "41-50", 
                                             "51-60", "61-70", "71-80", "81-90", "91-100"), 
                                  include.lowest=TRUE))
        
        tfdf <- as.data.frame(tfdf)
    })
    
    
# survivors function
# 
# reactive function to slice the data returned by tfilter
# so that it contains only records for survivors
 
    survivors <- reactive({
        df <- tfilter() %>% filter(tfilter()$Survived == 1)
    })
    
# Outputs for tab 1 - Survival by Age Group
#
# Updates the headline comparing the number of passengers and overall probability 
# of survival for the current dataset with those of the whole dataset
 
    # Prepare chart
    
    pl_colorscale = list(c(0.0, '#ef553b'),
                         c(0.5, '#ef553b'),
                         c(0.5, '#7fc97f'),
                         c(1, '#7fc97f')) 
    
    axis = list(showline = FALSE,
                zeroline = FALSE,
                gridcolor = '#ffff',
                ticklen = 4,
                titlefont = list(size=13))    
       
    output$row_count <- renderText({
        df <- tfilter()
        paste("There are currently ", nrow(df), 
              " passengers selected out of a possible ", nrow(Titanic), "\n\n",
              "and their average chances of survival are ", tpred(tfit(tfilter())),
              "which compares to an overall average of ", tpred(tfit(Titanic)))
    })
    
    output$distPlot <- renderPlotly({

        plot_ly(type = "histogram",
                histnorm = "frequency",
                x = ~tfilter()$AgeRange,
                name = "Nr of Passengers",
                alpha = 0.3) %>%
        
        add_histogram(histnorm = "frequency",
                      x = ~survivors()$AgeRange,
                      name = "Nr of Survivors") %>%
            
        layout(title = "Histogram of Passengers and Survivors",
               xaxis = list(title = "Age Group", zeroline = FALSE),
               yaxis = list(title = "Count", zeroline = FALSE)) 
    })
    
# Outputs for tab 2 - 3D Analysis
#
# Plots two 3D scatter charts side by side.  Each plots survived (green) vs perished (red).
# Despite the three dimensions, there is still some over-plotting, hence the opacity.
# The first chart analyses the combined influence of the Age, Gender & Family Members predictors.
# The second chart analyses the combined influence of the Age, Port of Embarcation & Spouses/Siblings
# predictors.
    
    f1 <- list(
        family = "Arial, sans-serif",
        size = 16,
        color = "black"
    )
        
    f2 <- list(
        family = "Arial, sans-serif",
        size = 14,
        color = "black"
    )
    
    f3 <- list(
        family = "Arial, sans-serif",
        size = 12,
        color = "black"
    )
 
    # Plotly does not process factors correctly so convert to numerics(?)
    
    output$ageSexParch <- renderPlotly({
        plot_ly(x = ~tfilter()$Age, 
                y = ~as.numeric(tfilter()$Sex), 
                z = ~as.numeric(tfilter()$Parch),
                mode = 'markers',
                color = ~tfilter()$Survived,
                colors = c('#ef553b', '#7fc97f'), 
                opacity = 0.4,
                text = ~tfilter()$id,
                hovertemplate = paste(
                    "<b> Passenger:</b> %{text}<br>",
                    "<b>Age:</b> %{x:.d}<br>",
                    "<b>Gender:</b> %{y}<br>",
                    "<b>Parents/Children:</b> %{z:.d}<br>"
                )
        ) %>%
            
        add_markers() %>%
        layout(title = list(text = "Survivors (in green) by Age, Gender & Family Members",
                            font = f1),
               scene = list(xaxis = list(title = 'Age',
                                         titlefont = f2,
                                         showticklines = TRUE, 
                                         showgrid = TRUE,
                                         tickfont = f3),
                            
                            yaxis = list(title = 'Gender',
                                         titlefont = f2,
                                         tickmode = "array",
                                         ticktext = c("", "female", "", "", "", "male", ""), 
                                         tickvals = c(0.75, 1, 1.25, 1.5, 1.75, 2, 2.25),
                                         range = c(0.75, 2.25),
                                         showticklines = TRUE, 
                                         showgrid = TRUE,
                                         tickfont = f3),
                            
                            zaxis = list(title = 'Parents/Children',
                                         titlefont = f2,
                                         nticks = 10, range = c(1, 10), 
                                         showticklines = TRUE, 
                                         showgrid = TRUE,
                                         tickfont = f3)))
    })
    
    output$ageEmbarkedSibSp <- renderPlotly({
        plot_ly(x = ~tfilter()$Age, 
                y = ~as.numeric(tfilter()$Embarked), 
                z = ~as.numeric(tfilter()$SibSp), 
                color = ~tfilter()$Survived, 
                colors = c('#ec7063', '#58d68d'), 
                opacity = 0.4,
                text = ~tfilter()$id,
                hovertemplate = paste(
                    "<b> Passenger:</b> %{text}<br>",
                    "<b>Age:</b> %{x:.d}<br>",
                    "<b>Port:</b> %{y}<br>",
                    "<b>Spouses/Siblings:</b> %{z:.d}<br>")
        ) %>%
            
        add_markers() %>%
        layout(title = list(text = "Survivors (in green) by Age, Port of Embarcation & Spouses/Siblings",
                            font = f1),
               scene = list(xaxis = list(title = 'Age',
                            titlefont = f2,
                            showticklines = TRUE, 
                            showgrid = TRUE,
                            tickfont = f3),
                                
                            yaxis = list(title = 'Embarked',
                                         titlefont = f2,
                                         tickmode = "array",
                                         ticktext = c("", "C'bourg", "", "Q'town", "", "Soton", ""), 
                                         tickvals = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5),
                                         range = c(0.5, 3.5),
                                         showticklines = TRUE, 
                                         showgrid = TRUE,
                                         tickfont = f3),
                                
                            zaxis = list(title = 'Spouses/Siblings',
                                         titlefont = f2,
                                         nticks = 10, range = c(1, 10), 
                                         showticklines = TRUE, 
                                         showgrid = TRUE,
                                         tickfont = f3)))                        
    })
    
# Outputs for tab 3 - Decision Tree
#
# Uses the rattle package and the fancyRpartPlot function
# to generate a full decision tree
   
    output$dTree <- renderPlot({
        fancyRpartPlot(tfit(tfilter()), 
        palettes = c("Reds", "Greens"),
        main = "Survival Decision Tree",
        sub = "(green rather than red indicates survival more likely)")
    })
    
#
#   Trace outputs
#
    
    output$selected_parch <- renderText({
        paste("You have selected ", input$Parch, " parents/children")
    })
    
    output$tfilter_summary <- renderPrint({
        df <- tfilter()
        str(df)
    })
    
    output$survivors_summary <- renderPrint({
        df <- survivors()
        str(df)
    })
})
