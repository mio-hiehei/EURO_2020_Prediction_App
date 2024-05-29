#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    # ========== Predictions Goetzes Faust ============================= #
    output_outcome_goetze <- reactive({
        
        home_elo <- euro_data$elo[euro_data$team == input$input_home_goetze]
        away_elo <- euro_data$elo[euro_data$team == input$input_away_goetze]
        
        diff_elo <- home_elo - away_elo
        
        home_diff <- euro_data$attack[euro_data$team == input$input_home_goetze] - euro_data$defend[euro_data$team == input$input_away_goetze]
        away_diff <- euro_data$attack[euro_data$team == input$input_away_goetze] - euro_data$defend[euro_data$team == input$input_home_goetze]
        
        match_outcome <- predict(mlogit_model, newdat = data.frame(elo_diff_matches = diff_elo,
                                                                   home_diff_matches  = home_diff,
                                                                   away_diff_matches = away_diff), type = "class")
        
        result <- sample(goal_data$outcome[goal_data$result == match_outcome], 1)
        
        
        
        result_table <- data.frame(Heim = input$input_home_goetze,
                                   Ergebnis = result,
                                   Auswärts = input$input_away_goetze)
        
        return(result_table)
        
    })

    output$prediction_table_goetze <- renderTable({
        
        output_outcome_goetze()
        
    })
    
    # ========== Predictions CDSS ============================= #
    output_outcome_cdss <- reactive({
        
        home_elo <- euro_data$elo[euro_data$team == input$input_home_cdss]
        away_elo <- euro_data$elo[euro_data$team == input$input_away_cdss]
        
        diff_elo <- home_elo - away_elo
        
        home_diff <- euro_data$attack[euro_data$team == input$input_home_goetze] - euro_data$defend[euro_data$team == input$input_away_goetze]
        away_diff <- euro_data$attack[euro_data$team == input$input_away_goetze] - euro_data$defend[euro_data$team == input$input_home_goetze]        
        
        goals_home <- round(predict(nb_model_home, newdat = data.frame(elo_diff_matches = diff_elo,
                                                                       home_diff_matches = home_diff,
                                                                       away_diff_matches = away_diff), type = "response"),0)
        
        goals_away <- round(predict(nb_model_away, newdat = data.frame(elo_diff_matches = diff_elo,
                                                                       home_diff_matches = home_diff,
                                                                       away_diff_matches = away_diff), type = "response"),0)
        
        result <- paste0(goals_home,":", goals_away)
        
        result_table <- data.frame(Heim = input$input_home_cdss,
                                   Ergebnis = result,
                                   Auswärts = input$input_away_cdss)
        
        return(result_table)
        
    })
    
    output$prediction_table_cdss <- renderTable({
        
        output_outcome_cdss()
        
    })
    
    

})
