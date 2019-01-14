#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(dplyr)
#library(rdrop2)


#setwd("~/Dropbox/Coursera/SwiftKey Project")


## read in prepared dataframes
unigramDF <- readRDS(file = "data/unigramDF.RData")
bigramsDF <- readRDS(file = "data/bigramsDF.RData")
trigramsDF <- readRDS(file = "data/trigramsDF.RData")
quadgramsDF <- readRDS(file = "data/quadgramsDF.RData")

shinyServer(function(input, output) {
  
  guesses <- reactive({
    data <- toString(input$ngram)
    
    ## fuctions for helping generate probs
    getLastWords <- function(string, words) {
      pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
      return(substring(string, str_locate(string, pattern)[,1]))
    }
    
    predictor <- function(input) {
      input <- toString(input)
      n <- length(strsplit(input, " ")[[1]])
      prediction <- c()
      if(n >= 3 && length(prediction)<3) 
        prediction <- c(prediction, filter(quadgramsDF, getLastWords(input, 3) == FirstWords)$LastWord)
      if(n >= 2 && length(prediction)<3) 
        prediction <- c(prediction, filter(trigramsDF, getLastWords(input, 2) == FirstWords)$LastWord)
      if(n >= 1 && length(prediction)<3) 
        prediction <- c(prediction, filter(bigramsDF, getLastWords(input, 1) == FirstWords)$LastWord)
      if(length(prediction)<3 ) prediction <- c(prediction, unigramDF$Words)
      
      return(unique(prediction)[1:5])
    }
    
    guesses <- predictor(data)
  })



  output$results <- renderTable(guesses(), colnames = FALSE)
  
  
})
