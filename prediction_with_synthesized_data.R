
## libraries
library(RWekajars)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(RWeka)
library(rJava)
library(stringr)
library(dplyr)

# set working directory
setwd("~/Dropbox/Coursera/SwiftKey Project")


## read in prepared dataframes
unigramDF <- readRDS(file = "./unigramDF.Rds")
bigramsDF <- readRDS(file = "./bigramsDF.Rds")
trigramsDF <- readRDS(file = "./trigramsDF.Rds")
quadgramsDF <- readRDS(file = "./quadgramsDF.Rds")


## fuctions for helping generate probs
getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}


predictor <- function(input) {
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
