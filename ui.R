#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  headerPanel("Word Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("ngram", label = h3("Enter in the start of a phrase"), value = "start here"),
      hr()
    ),
  
  
    mainPanel(
      
      h3("Next possible words..."),
      h3(tableOutput("results"))
    )
  )
  )
)
