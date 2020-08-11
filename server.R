#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("./NLP_WORD_predictor.R")
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$Predictedworrd <- renderText({
        
        Inputliness<-input$linee
        Predictnextword(Inputliness)

    })

})
