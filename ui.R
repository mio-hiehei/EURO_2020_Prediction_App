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
shinyUI(fluidPage(
    
    br(),
    HTML('<hr style = "width:95%;border-color: #d41d3b;</hr>'),
    br(),
    br(),
    HTML('<hr style = "width:95%;border-color: #192e97;</hr>',
         '<img src="euro2020.jpg", height="100px" style="align:left"</img>',
         '<img src="uefa_mafia.jpg", height="100px" style="align:left"</img>'),
    
    

    # Application title
    titlePanel("EURO 2020 - Mios Tippspiel-Hilfe"),
    tabsetPanel(
        tabPanel(title = "Goetzes Faust",
        
                mainPanel(
                        column(width = 6,
                    # Eingabefeld Heimteam
                        selectInput(inputId = "input_home_goetze", label = "Heimteam", choices = unique(euro_data$team), selected = "Germany")
                    ),
                    column(width = 6,
                    # Eingabefeld Auswärtsteam
                        selectInput(inputId = "input_away_goetze", label = "Auswärtsteam", choices = unique(euro_data$team), selected = "France")
                    ),
                    
                    tableOutput("prediction_table_goetze")
                    
                    
                )
                
            ),
        tabPanel("CDSS",
                 
                 mainPanel(
                     column(width = 6,
                            # Eingabefeld Heimteam
                            selectInput(inputId = "input_home_cdss", label = "Heimteam", choices = unique(euro_data$team), selected = "Germany")
                     ),
                     column(width = 6,
                            # Eingabefeld Auswärtsteam
                            selectInput(inputId = "input_away_cdss", label = "Auswärtsteam", choices = unique(euro_data$team), selected = "France")
                     ),
                     
                     tableOutput("prediction_table_cdss")
                 
        )
        )
    )
)
)

