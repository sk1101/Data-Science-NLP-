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

    # Application title
    titlePanel(h1("Text Predictor", align="center")),

    # Sidebar with a slider input for number of bins
    

        # Show a plot of the generated distribution
        mainPanel(fluidRow( align="center",
            textInput("linee", "Write Something", value = "", width = "100%",
                      placeholder = NULL),
            textOutput("Predictedworrd"),
            img(src="https://i.dlpng.com/static/png/6786438_preview.png",height='300px',width='500px')
            #shiny::tags$video(src = "Brush Tool Gif.mp4",
             #                 type = "video/mp4")
        ),width = 12)
    )
)
