# functions.R

## Johns Hopkins Data Science Certification Capstone Project via Coursera
## Kevin McCue

# functions for predicting a NextWord given an input of any length using the backoff algorithm
# 1/16/2017

# Load Packages
if (!require("dplyr")) {install.packages("dplyr")} 
suppressPackageStartupMessages(require(dplyr))
if (!require("quanteda")) {install.packages("quanteda")} 
suppressPackageStartupMessages(require(quanteda))
if (!require("wordcloud")) {install.packages("wordcloud")} 
suppressPackageStartupMessages(require(wordcloud))
if (!require("RColorBrewer")) {install.packages("RColorBrewer")} 
suppressPackageStartupMessages(require(RColorBrewer))
if (!require("qdap")) {install.packages("qdap")} 
suppressPackageStartupMessages(require(qdap))
if (!require("tm")) {install.packages("tm")}
suppressPackageStartupMessages(library(tm))
if (!require("openNLP")) {install.packages("openNLP")}
suppressPackageStartupMessages(library(openNLP))


FileURL <- "https://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip"
FileName <- "full-list-of-bad-words-banned-by-google-txt-file.zip"
outDir<-"BadWords"
if (!file.exists(FileName)) {
  download.file(FileURL ,FileName,method="auto") }
unzip(FileName,exdir=outDir)
US_Bad_Words <- readLines(paste(outDir, "/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt", sep =""))

mctokenizer = function(corpus, ngram) {
  if (!require("RWeka")) {install.packages("RWeka")}
  require(RWeka)
  x = NGramTokenizer(corpus$content, Weka_control(min = ngram, max = ngram))
  return(x)  
} 


fun.CleanCorpus <- function(sample) {
  #Input vector of strings from english sources, Output a cleaned dataframe.
  # convert string to vector of words
  if (!require("qdap")) {install.packages("qdap")} 
  suppressPackageStartupMessages(require(qdap))
  if (!require("tm")) {install.packages("tm")}
  suppressPackageStartupMessages(library(tm))
  if (!require("openNLP")) {install.packages("openNLP")}
  suppressPackageStartupMessages(library(openNLP))
  corpus <- unlist(strsplit(sample, split=" "))
  ##Remove emails
  RmvEmail <- function(x) {gsub("\\S+@\\S+", " ", x)} 
  ##Remove URLS
  RmvUrl <- function(x) {gsub("http[[:alnum:]]*"," ",x)}
  ##Remove Twitter hashtags
  RmvHashtag <- function(x) {gsub("#[[:alnum:]]*"," ",x)}
  ##Remove Twitter handles (e.g. @username)
  RmvHandle <- function(x) {gsub("@[[:alnum:]]*"," ",x)}
  ##Run custom functions
  corpus <- RmvEmail(corpus)
  corpus <- RmvUrl(corpus)
  corpus <- RmvHashtag(corpus)
  corpus <- RmvHandle(corpus)
    # find indices of words with non-ASCII characters, remove words with non-ASCII characters
  corpus <- gsub("[=]", "", iconv(corpus, "latin1", "ASCII", sub=" "))
  # make corpus
  require(tm)
  corpus <- Corpus(VectorSource(corpus))
  # finish cleaning 
  corpus <- tm_map(corpus, sent_detect_nlp)

  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, US_Bad_Words)
  corpus <- tm_map(corpus, removeWords, stopwords("english")) 
  corpus <- tm_map(corpus, removeWords, c("s e x y", "s e x", "rt" , "r t", "cd", "im", "aaa", "aa", "list content =")) 
  
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,stripWhitespace)
  #output
  corpus
}

TextScrubber <-function(sample) {
  #Input vector of strings from english sources, Output a cleaned dataframe.
  # convert string to vector of words
  if (!require("qdap")) {install.packages("qdap")} 
  suppressPackageStartupMessages(require(qdap))
  if (!require("tm")) {install.packages("tm")}
  suppressPackageStartupMessages(library(tm))
  if (!require("openNLP")) {install.packages("openNLP")}
  suppressPackageStartupMessages(library(openNLP))
  corpus <- unlist(strsplit(train, split=", "))
  ##Remove emails
  RmvEmail <- function(x) {gsub("\\S+@\\S+", "", x)} 
  ##Remove URLS
  RmvUrl <- function(x) {gsub("http[[:alnum:]]*","",x)}
  ##Remove Twitter hashtags
  RmvHashtag <- function(x) {gsub("#[[:alnum:]]*","",x)}
  ##Remove Twitter handles (e.g. @username)
  RmvHandle <- function(x) {gsub("@[[:alnum:]]*","",x)}
  ##Run custom functions
  corpus <- RmvEmail(corpus)
  corpus <- RmvUrl(corpus)
  corpus <- RmvHashtag(corpus)
  corpus <- RmvHandle(corpus)
  # find indices of words with non-ASCII characters, remove words with non-ASCII characters
  RmvASCII <- function(x) {iconv(x, "latin1", "ASCII", sub="")}
  # make corpus
  corpus <- Corpus(VectorSource(corpus))
  # finish cleaning
  corpus <- tm_map(corpus, sent_detect_nlp)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, US_Bad_Words)
  corpus <- tm_map(corpus, removeWords, stopwords("english")) 
  corpus <- tm_map(corpus, removeWords, c("s e x y", "s e x", "rt" , "r t", "cd", "im", "aaa", "aa", "list content =")) 
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,RmvASCII)
  corpus <- tm_map(corpus,stripWhitespace)
  #output
  corpus
}




uni.s.DTM = DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
dtm <- DocumentTermMatrix(corpus,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))
inspect(tdm[202:205, 1:3])
inspect(tdm[c("price", "texas"), c("127", "144", "191", "194")])
inspect(uni.s.DTM[1:111, 100:276])


bi_tokenizer = function (x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
tri_tokenizer = function (x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
qua_tokenizer = function (x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
uni.s.DTM = DocumentTermMatrix(corpus)
bi.s.DTM = DocumentTermMatrix(corpus, control = list(tokenize = bi_tokenizer))
tri.s.DTM = DocumentTermMatrix(corpus, control = list(tokenize = tri_tokenizer))
qua.s.DTM = DocumentTermMatrix(corpus, control = list(tokenize = qua_tokenizer))

class(qua.s.DTM)

freq = data.frame(sort(colSums(as.matrix(bi.s.DTM)), decreasing=TRUE))

TextProcesser <- function(x) {as.character(fun.CleanCorpus(x)[[1]])}
IdDocument <- function(x) {for(i in c(1 : length(x))) {DublinCore(x[[i]], "id") <- i} x }

fun.frequency = function(x, minCount = 1) {
  if (!require("dplyr")) {install.packages("dplyr")} 
  require(dplyr)
  x = x %>%
    group_by(Term) %>%
    summarize(count = n()) %>%
    filter(count >= minCount)
  x = x %>% 
    mutate(freq = count / sum(x$count)) %>% 
    select(-count) %>%
    arrange(desc(freq))
}

# Parse tokens from input text ####
func.input = function(x) {
  if (!require("stringi")) {install.packages("stringi")}
  require(stringi)
  tokenwordcnt = nrow( data.frame(word = mctokenizer(fun.CleanCorpus(x), 1)))
  if(tokenwordcnt == 0) {
    input1 = data.frame(word = "")
    input2 = data.frame(word = "")
  }
  # Tokenize with same functions as training data
  if (tokenwordcnt == 1) {
    y = data.frame(word = mctokenizer(fun.CleanCorpus(x), 1))
    input1 = data.frame(word = "")
    input2 = data.frame(word = "")
    input3 = data.frame(word = as.character(y[1,1]))
  } else if (tokenwordcnt == 2) {
    y = data.frame(word = mctokenizer(fun.CleanCorpus(x), 1))
    input1 = data.frame(word = "")
    input2 = data.frame(word = as.character(y[1,1]))
    input3 = data.frame(word = as.character(y[2,1]))
  } else if (tokenwordcnt >= 3) {
    y = data.frame(word = mctokenizer(fun.CleanCorpus(x), 1))
    input1 = data.frame(word = as.character(y[tokenwordcnt - 2,1]))
    input2 = data.frame(word = as.character(y[tokenwordcnt - 1,1]))
    input3 = data.frame(word = as.character(y[tokenwordcnt,1]))
  }
  #  Return data frame of inputs 
  inputs = data.frame(words = unlist(rbind(input1,input2, input3)))
  return(inputs)
}

# Predictions using backoff algorithm up to 4-gram ####
func.pred4 = function(x, y, z, n = 6) {
  # Predict giving just the top 1-gram words if no input given
  if(x == "" & y == "" & z == "") {
    prediction = dfTrain1 %>% select(Term, freq)
    # Predict using 4-gram model
  }   else if(dim(filter(dfTrain4, word1 == x & word2 == y & word3 == z))[1] != 0) {
    prediction = dfTrain4 %>%
      filter(word1 %in% x & word2 %in% y & word3 %in% z) %>%  select(Term, freq)
    # Predict using 3-gram model
  }   else if(dim(filter(dfTrain3, word1 == y & word2 == z ))[1] != 0)  {
    prediction = dfTrain3 %>% filter(word1 %in% y & word2 %in% z) %>% select(Term, freq)
    # Predict using 2-gram model
  }   else if(dim(filter(dfTrain2, word1 == y))[1] != 0) {
    prediction = dfTrain2 %>% filter(word1 %in% z) %>% select(Term, freq)
    # If no prediction found before, predict giving just the top 1-gram words
  }   else{
    prediction = dfTrain1 %>%
      select(Term, freq)
  }
  # Return predicted word in a data frame
  return(prediction[1:n, ])
}

x = "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"



func.input(x)

func.pred4(func.input(x)[1,], func.input(x)[2,], func.input(x)[3,] )
