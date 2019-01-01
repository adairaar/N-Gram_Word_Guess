# libraries from CRAN
library(plyr)
library(dplyr)
#library(RWeka) #installation issues w/ rJava
library(quanteda)
library(knitr)
library(NLP)
library(tm)
library(data.table)
library(ngram)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(stringi)
library(ggplot2)

# set working directory
setwd("~/Dropbox/Coursera/SwiftKey Project")
if(!file.exists("./dataStore")){dir.create("./dataStore")}

# download data
link.data <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
check.url <- file(link.data,"r")
if (!isOpen(check.url)) {
  stop(paste("There's a problem with the data:",geterrmessage()))
}
download.file(link.data,destfile="./dataStore/SwiftKey.zip",method="auto")  

# zipfile.data is the variable to keep the *.zip file
zipfile.data = "SwiftKey.zip"

# make sure the data in the working directory if not download the zip file into the to zipfile.data and unzip the zipfile.data
if(!file.exists(zipfile.data)) {        
  unzip(zipfile = "./dataStore/SwiftKey.zip", exdir = "./dataStore")
} 

# read in text data
## English
path <- file("./dataStore/final/en_US/en_US.blogs.txt", "r") # blogs
en_US.blogs <- readLines(path, skipNul = TRUE)
close(path)
path <- file("./dataStore/final/en_US/en_US.news.txt", "r") # news
en_US.news <- readLines(path, skipNul = TRUE)
close(path)
path <- file("./dataStore/final/en_US/en_US.twitter.txt", "r") #twitter
en_US.twitter <- readLines(path, skipNul = TRUE)
close(path)

# Sampling
set.seed(3)
blog_sample <- sample(en_US.blogs, 0.05*length(en_US.blogs))
news_sample <- sample(en_US.news, 0.05*length(en_US.news))
twitter_sample <- sample(en_US.twitter, 0.05*length(en_US.twitter))
sample_data <- c(blog_sample,news_sample,twitter_sample)

sample_data <- iconv(sample_data, 'UTF-8', 'ASCII') #no emojis
# corpus <- Corpus(VectorSource(as.data.frame(sample_data, stringsAsFactors = FALSE))) 
# corpus <- corpus %>%
#   tm_map(tolower) %>%  
#   tm_map(PlainTextDocument) %>%
#   tm_map(removePunctuation) %>%
#   tm_map(removeNumbers) %>%
#   tm_map(stripWhitespace)

# N-grams
## making corpus and ngrams
sample_corp <- corpus(sample_data)
toks <- tokens(sample_corp, remove_punct = TRUE)
toks_stopped <- tokens_remove(toks, stopwords('en'))
unigram <- tokens_ngrams(toks,n=1)
unigram_stopped <- tokens_ngrams(toks_stopped,n=1)
bigram <- tokens_ngrams(toks,n=2)
trigram <- tokens_ngrams(toks,n=3)
## getting frequency stats
uni_freq <- textstat_frequency(dfm(unigram))
uni_freq <- uni_freq[1:10000]
uni_freq_stopped <- textstat_frequency(dfm(unigram_stopped))
#uni_freq_stopped <- uni_freq_stopped[1:10000]
bi_freq <- textstat_frequency(dfm(bigram))
bi_freq <- bi_freq[1:10000]
tri_freq <- textstat_frequency(dfm(trigram))
tri_freq <- tri_freq[1:10000]

# Knesser-Ney Method
## Interpolated bigram

Pcont <- function(x){
  # words <- as.list(strsplit(x, " ")[[1]])
  # w_length <- length(words)
  # words <- words[1:w_length]
  # word_last <- words[w_length]
  word_last <- x

  p_cont <- sum(grepl(paste("_",word_last,"$",sep=""),bi_freq$feature)) / nrow(bi_freq)
  
  return(p_cont)
}


##
Pcont_array <- function(){
  array_pcont <- 0
  array_pcont <- sapply(uni_freq_stopped$feature, Pcont, USE.NAMES = FALSE)
  
  df <- data.frame(word = uni_freq_stopped$feature, probs = array_pcont, stringsAsFactors = FALSE)
  df_sorted <- df[with(df,order(-probs)),]
  top_10000 <- df_sorted[1:10000,]
  
  return(top_10000)
}


Pkn_bi <- function(x){
  words <- as.list(strsplit(x, " ")[[1]])
  w_length <- length(words)
  word1 <- words[w_length-1]
  word2 <- words[w_length]
  word1_pos <- match(word1,uni_freq$feature)
  word12 <- paste(words[1],words[2], sep="_")
  word12_pos <- match(word12,bi_freq$feature)
  lambda <- (0.75/sum(bi_freq$frequency[grep(paste("^",word1,"_",sep=""),bi_freq$feature)])) * sum(grepl(paste("^",word1,"_",sep=""),bi_freq$feature))
  Pkn <- max(bi_freq$frequency[word12_pos] - 0.75,0) / (sum(bi_freq$frequency[grep(paste("^",word1,"_",sep=""),bi_freq$feature)])) + lambda*Pcont(word2)
  # if (is.na(Pkn) == TRUE) {
  #   Pkn <- 0
  # }
  
  return(Pkn)
}

## Interpolated trigram
Pkn_tri <- function(x){
  words <- as.list(strsplit(x, " ")[[1]])
  w_length <- length(words)
  word1 <- words[w_length-2]
  word2 <- words[w_length-1]
  word3 <- words[w_length]
  word1_pos <- match(word1,uni_freq$feature)
  word2_pos <- match(word2,uni_freq$feature)
  Pkn_bi_val <- Pkn_bi(paste(word2,word3))
  
  word12 <- paste(words[1],words[2], sep="_")
  word12_pos <- match(word12,bi_freq$feature)
  word123 <- paste(words[1],words[2],words[3],sep="_")
  word123_pos <- match(word123,tri_freq$feature)
  lambda <- (0.75/sum(bi_freq$frequency[grep(paste("^",word1,"_",word2,sep=""),tri_freq$feature)]))*sum(tri_freq$frequency[grep(paste("^",word1,"_",word2,"_",sep=""),tri_freq$feature)])
  Pkn <- max(tri_freq$frequency[word123_pos] - 0.75,0) / (sum(tri_freq$frequency[grep(paste("^",word1,"_",word2,"_",sep=""),tri_freq$feature)])) + lambda*Pkn_bi_val
  # if (is.na(Pkn) == TRUE) {
  #   Pkn <- 0
  # }
  
  return(Pkn)
}

# Main Event
# Predict next word

Pcont_guess <- Pcont_array()

predict_next <- function(x){
  words <- as.list(strsplit(x, " ")[[1]])
  w_length <- length(words)
  word1 <- words[w_length-2]
  word2 <- words[w_length-1]
  word3 <- words[w_length]
  
  guesses <- 0
  # for (i in 1:1000) {
  #   guesses[i] <- paste(word1,word2,word3,Pcont_guess[i,1])
  # }

  guesses <- paste(word1,word2,word3,Pcont_guess[,1])

  probs <- 0
  
  if (w_length >= 2) {
    probs <- sapply(guesses, Pkn_tri, USE.NAMES = FALSE)
  }
  else {
    probs <- sapply(guesses, Pkn_bi, USE.NAMES = FALSE)
  }
  
  df <- data.frame(word = guesses, probs = probs)
  df_sorted <- df[with(df,order(-probs)),]
  top_ten <- df_sorted[1:10,]

  return(top_ten)  
}








####################################################################
####################################################################
####################################################################
####################################################################

tokenmaker <- function(x) {
  corpus <- Corpus(VectorSource(x))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, PlainTextDocument)
  #        corpus <- tm_map(corpus, stemDocument)
  corpus <- Corpus(VectorSource(corpus))
}  

wordcounter <- function(x) {
  dtm<-DocumentTermMatrix(x)
  dtm_matrix <- as.matrix(dtm)
  word_freq <- colSums(dtm_matrix)
  word_freq <- sort(word_freq, decreasing = TRUE)
  words <- names(word_freq)
  return(list(words, word_freq))
}  


NextWordIs <- function(x,y){
  BQuest<-grepl(x, en_US.blogs, ignore.case=TRUE)
  BDocs<-en_US.blogs[BQuest]
  textoachado<-'a'
  NextWordIs<-'a'
  i<-length(BDocs)
  if (i>0)
  {
    for (i in 1:i)
    {  textoachado[i]<- str_extract(BDocs[i], y)
    NextWordIs[i]<- stri_extract_last_words(textoachado[i]) 
    }
  }
  NQuest<-grepl(x, en_US.news, ignore.case=TRUE)
  NDocs<-en_US.news[NQuest]
  j=length(NDocs)
  if (j>0)
  {
    for (j in 1:j)
    {  textoachado[i+j]<- str_extract(NDocs[j], y)
    NextWordIs[i+j]<- stri_extract_last_words(textoachado[i+j]) 
    }
  }
  TQuest<-grepl(x, en_US.twitter, ignore.case=TRUE)
  TDocs<-en_US.twitter[TQuest]
  k=length(TDocs)
  if (k>0)
  {
    for (k in 1:k)
    {  textoachado[i+j+k]<- str_extract(TDocs[k], y)
    NextWordIs[i+j+k]<- stri_extract_last_words(textoachado[i+j+k]) 
    }
  }
  bundle<-as.data.frame(NextWordIs, stringsAsFactors=FALSE)
  summary (bundle)
  blogs_token <- tokenmaker(bundle)
  blogs_words <- wordcounter(blogs_token)
  summary(nchar(bundle))
  head(bundle)
  tdm_Blogs<-TermDocumentMatrix(blogs_token)
  m_Blogs<-as.matrix(tdm_Blogs)
  v_Blogs<-sort(rowSums(m_Blogs),decreasing=TRUE)
  d_Blogs<-data.frame(word=names(v_Blogs),freq=v_Blogs)
  head(v_Blogs, 100)    
  return(list(head(v_Blogs,100)))
}
