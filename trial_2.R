# source1: https://github.com/mhnierhoff/CapstoneCoursera/tree/master/dominoCapstone
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

## Building a clean corpus

theSampleCon <- file("./textSample.txt")
theSample <- readLines(theSampleCon)
close(theSampleCon)

profanityWords <- read.table("./profanityfilter.txt", header = FALSE)

## Build the corpus, and specify the source to be character vectors 
cleanSample <- Corpus(VectorSource(theSample))

##
rm(theSample)

## Make it work with the new tm package
cleanSample <- tm_map(cleanSample,
                      content_transformer(function(x) 
                        iconv(x, to="UTF-8", sub="byte")))

## Convert to lower case
cleanSample <- tm_map(cleanSample, content_transformer(tolower))

## remove punction, numbers, URLs, stop, profanity and stem wordson
cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
cleanSample <- tm_map(cleanSample, content_transformer(removeURL))
cleanSample <- tm_map(cleanSample, stripWhitespace)
cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
cleanSample <- tm_map(cleanSample, removeWords, profanityWords[,1])
cleanSample <- tm_map(cleanSample, stemDocument)
cleanSample <- tm_map(cleanSample, stripWhitespace)

## Saving the final corpus
saveRDS(cleanSample, file = "./finalCorpus.Rds")
#write.csv(cleanSample, file = "./finalCorpus.csv")


## Budilding the n-grams

finalCorpus <- readRDS("./finalCorpus.Rds")
finalCorpusDF <-data.frame(text=unlist(sapply(finalCorpus,`[`, "content")), 
                           stringsAsFactors = FALSE)

# finalCorpus <- read.csv(file = "./finalCorpus.csv")
# finalCorpusDF <- data.frame(finalCorpus[,3])

## Building the tokenization function for the n-grams
ngramTokenizer <- function(theCorpus, ngramCount) {
  ngramFunction <- NGramTokenizer(theCorpus, 
                                  Weka_control(min = ngramCount, max = ngramCount, 
                                               delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngramFunction <- data.frame(table(ngramFunction))
  ngramFunction <- ngramFunction[order(ngramFunction$Freq, 
                                       decreasing = TRUE),][1:10,]
  colnames(ngramFunction) <- c("String","Count")
  ngramFunction
}

unigram <- ngramTokenizer(finalCorpusDF, 1)
saveRDS(unigram, file = "./unigram.Rds")
bigram <- ngramTokenizer(finalCorpusDF, 2)
saveRDS(bigram, file = "./bigram.Rds")
trigram <- ngramTokenizer(finalCorpusDF, 3)
saveRDS(trigram, file = "./trigram.Rds")
quadgram <- ngramTokenizer(finalCorpusDF, 4)
saveRDS(quadgram, file = "./quadgram.Rds")