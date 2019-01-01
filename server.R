#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rdrop2)


shinyServer(function(input, output) {
  
  Data1 <- reactive({
    Stock1 <- toString(input$Stock1)
    #getSymbols(Stock1)
    #Stock1 <- as.data.frame(getSymbols(Stock1))
  })
  
  Data2 <- reactive({
    Stock2 <- toString(input$Stock2)
    #getSymbols(Stock2)
    #Stock2 <- getSymbols(Stock2)
  })
  
  output$distPlot <- renderPlot({
    Stock1 <- Data1()
    Stock2 <- Data2()
    #getSymbols(c(Data1(),Data2()))
    dataEnv <- new.env()
    getSymbols(c(Stock1,Stock2), env=dataEnv)
    plist <- eapply(dataEnv, Ad)
    pframe <- do.call(merge, plist)
    Data <- cbind(diff(log((pframe))),diff(log((pframe))))
    chart.Correlation(Data)
    #plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = 'y', xlab = 'x')
  })

  
})
