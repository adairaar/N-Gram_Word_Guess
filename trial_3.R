# source2: https://rpubs.com/Dezwirey/211935


# source("installpack.R")

library(RWekajars)
library(ggplot2)
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
if(!file.exists("./dataStore")){dir.create("./dataStore")}



## read in data
con1 <- file("./dataStore/final/en_US/en_US.twitter.txt", "r")
con2 <- file("./dataStore/final/en_US/en_US.news.txt", "r")
con3 <- file("./dataStore/final/en_US/en_US.blogs.txt", "r")

tweets <- readLines(con1)
news <- readLines(con2)
blogs <- readLines(con3)

close(con1); close(con2); close(con3)



## sampling
set.seed(50)
samplelines <- c(sample(tweets, length(tweets) * 0.1),
                 sample(news, length(news) * 1),
                 sample(blogs, length(blogs) * 0.1))



## training sets
set.seed(50)
samplelines <- sample(samplelines)
validationIndex <- floor(length(samplelines) * 0.8)
testingIndex <- floor(length(samplelines) * 0.9)

training <- samplelines[1:validationIndex]
validation <- samplelines[(validationIndex+1):testingIndex]
testing <- samplelines[(testingIndex+1):length(samplelines)]



## tiding data
tokenizer <- function(lines) {
  lines <- tolower(lines)
  lines <- gsub("'", "'", lines)
  lines <- gsub("[.!?]$|[.!?] |$", " ''split'' ", lines)
  tokens <- unlist(strsplit(lines, "[^a-z']"))
  tokens <- tokens[tokens != ""]
  return(tokens)
}

tokens <- tokenizer(training)
vtokens <- tokenizer(validation)
ttokens <- tokenizer(testing)



## tokens & n-grams
tokens2 <- c(tokens[-1], ".")
tokens3 <- c(tokens2[-1], ".")
tokens4 <- c(tokens3[-1], ".")
#tokens5 <- c(tokens4[-1], ".")
#tokens6 <- c(tokens5[-1], ".")

unigrams <- tokens
bigrams <- paste(tokens, tokens2)
trigrams <- paste(tokens, tokens2, tokens3)
quadgrams <- paste(tokens, tokens2, tokens3, tokens4)
#fivegrams <- paste(tokens, tokens2, tokens3, tokens4, tokens5)
#sixgrams <- paste(tokens, tokens2, tokens3, tokens4, tokens5, tokens6)

unigrams <- unigrams[!grepl("''split''", unigrams)]
bigrams <- bigrams[!grepl("''split''", bigrams)]
trigrams <- trigrams[!grepl("''split''", trigrams)]
quadgrams <- quadgrams[!grepl("''split''", quadgrams)]
#fivegrams <- fivegrams[!grepl("''split''", fivegrams)]
#sixgrams <- sixgrams[!grepl("''split''", sixgrams)]

unigrams <- sort(table(unigrams), decreasing=T)
bigrams <- sort(table(bigrams), decreasing=T)
trigrams <- sort(table(trigrams), decreasing=T)
quadgrams <- sort(table(quadgrams), decreasing=T)
#fivegrams <- sort(table(fivegrams), decreasing=T)
#sixgrams <- sort(table(sixgrams), decreasing=T)

saveRDS(unigrams, file = "./unigrams.Rds")
saveRDS(bigrams, file = "./bigrams.Rds")
saveRDS(trigrams, file = "./trigrams.Rds")
saveRDS(quadgrams, file = "./quadgrams.Rds")
#saveRDS(fivegrams, file = "./fivegrams.Rds")
#saveRDS(sixgrams, file = "./sixgrams.Rds")

## fuctions for helping generate probs
getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}

removeLastWord <- function(string) {
  sub(" [a-z']+$", "", string)
}


### Kneser-Ney Smoothing
### taking in ngram, use discount rate d (usually d = 0.75)
kneserney <- function(ngrams, d) {
  n <- length(strsplit(names(ngrams[1]), " ")[[1]])
  
  # Special case for unigrams
  if(n==1) {
    noFirst <- unigrams[getLastWords(names(bigrams), 1)]
    pContinuation <- table(names(noFirst))[names(unigrams)] / length(bigrams)
    return(pContinuation)
  }
  
  # Get needed counts
  nMinusOne <- list(unigrams, bigrams, trigrams, quadgrams)[[n-1]]
  noLast <- nMinusOne[removeLastWord(names(ngrams))]
  noFirst <- nMinusOne[getLastWords(names(ngrams), n-1)]
  
  # Calculate discounts, lambda and pContinuation
  discounts <- ngrams - d
  discounts[discounts < 0] <- 0
  lambda <- d * table(names(noLast))[names(noLast)] / noLast
  if(n == 2) pContinuation <- table(names(noFirst))[names(noFirst)] / length(ngrams)
  else pContinuation <- kneserney(noFirst, d)
  
  # Put it all together
  probabilities <- discounts / noLast + lambda * pContinuation / length(ngrams)
  return(probabilities)
}

### calculating probs
unigramProbs <- kneserney(unigrams, 0.75)
bigramProbs <- kneserney(bigrams, 0.75)
trigramProbs <- kneserney(trigrams, 0.75)
quadgramProbs <- kneserney(quadgrams, 0.75)
#fivegramProbs <- kneserney(fivegrams, 0.75)
#sixgramProbs <- kneserney(sixgrams, 0.75)

saveRDS(unigramProbs, file = "./unigramsProbs.Rds")
saveRDS(bigramProbs, file = "./bigramsProbs.Rds")
saveRDS(trigramProbs, file = "./trigramsProbs.Rds")
saveRDS(quadgramProbs, file = "./quadgramsProbs.Rds")
#saveRDS(fivegramProbs, file = "./fivegramsProbs.Rds")
#saveRDS(sixgramProbs, file = "./sixgramsProbs.Rds")

## modeling
vtokens2 <- c(vtokens[-1], ".")
vtokens3 <- c(vtokens2[-1], ".")
vtokens4 <- c(vtokens3[-1], ".")
#vtokens5 <- c(vtokens4[-1], ".")
#vtokens6 <- c(vtokens5[-1], ".")
vsixgrams <- paste(vtokens, vtokens2, vtokens3, vtokens4)

createModel <- function(n, threshold) {
  ngrams <- list(bigramProbs, trigramProbs, quadgramProbs)[[n-1]]
  
  model <- ngrams[getLastWords(vsixgrams[1:10000], n)]
  names(model) <- vsixgrams[1:10000]
  
  #if(n > 5) model[is.na(model) | model < threshold] <- 
  #  fivegramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 5)]
  if(n > 4) model[is.na(model) | model < threshold] <- 
    quadgramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 4)]
  if(n > 3) model[is.na(model) | model < threshold] <- 
    trigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 3)]
  if(n > 2) model[is.na(model) | model < threshold] <- 
    bigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 2)]
  if(n > 1) model[is.na(model) | model < threshold] <- 
    unigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 1)]
  return(model)
}


model <- createModel(4, 0.005)


## perplexity
perplexity <- function(probabilities) {
  return(exp(-sum(log(probabilities)) / length(probabilities)))
}

perplexity(model[!is.na(model)])


##predicting next word
unigramDF <- data.frame("Words" = (names(unigrams)), "Probability" = unigramProbs, stringsAsFactors=F)

bigramsDF <- data.frame("FirstWords" = removeLastWord(names(bigrams)), 
                        "LastWord" = getLastWords(names(bigrams), 1), 
                        "Probability" = bigramProbs, stringsAsFactors=F)

trigramsDF <- data.frame("FirstWords" = removeLastWord(names(trigrams)), 
                         "LastWord" = getLastWords(names(trigrams), 1), 
                         "Probability" = trigramProbs, stringsAsFactors=F)

quadgramsDF <- data.frame("FirstWords" = removeLastWord(names(quadgrams)), 
                          "LastWord" = getLastWords(names(quadgrams), 1), 
                          "Probability" = quadgramProbs, stringsAsFactors=F)

unigramDF <- (unigramDF %>% arrange(desc(Probability.Freq)))
bigramsDF <- bigramsDF %>% arrange(desc(Probability.Freq)) %>% filter(Probability.Freq > 0.0001)
trigramsDF <- trigramsDF %>% arrange(desc(Probability.Freq)) %>% filter(Probability.Freq > 0.0001)
quadgramsDF <- quadgramsDF %>% arrange(desc(Probability.Freq)) %>% filter(Probability.Freq > 0.0001)


saveRDS(unigramDF, file = "./unigramDF.Rds")
saveRDS(bigramsDF, file = "./bigramsDF.Rds")
saveRDS(trigramsDF, file = "./trigramsDF.Rds")
saveRDS(quadgramsDF, file = "./quadgramsDF.Rds")


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
  
  return(unique(prediction)[1:3])
}


