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
  
  sidebarLayout(
    sidebarPanel(
      textInput("Stock1", label = h3("Enter first stock symbol"), value = "AAPL"),
      hr(),
      textInput("Stock2", label = h3("Enter second stock symbol"), value = "GOOG"),
      hr(),
      actionButton("newplot", "Correlate")
    ),
  
  
    mainPanel(
      h3("Correlation Plots"),
      plotOutput("distPlot")
    )
  )
  )
)
