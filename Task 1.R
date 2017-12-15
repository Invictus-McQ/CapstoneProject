##Clear
setwd("~")
dev.off()
rm(list = ls())

##Download and Load Packages
if (!require("data.table")) {install.packages("data.table")}
library(data.table)
if (!require("xtable")) {install.packages("xtable")}
library(xtable)
if (!require("grid")) {install.packages("grid")}
library(grid)
if (!require("gridExtra")) {install.packages("gridExtra")}
library(gridExtra)
if (!require("tm")) {install.packages("tm")}
library(tm)
if (!require("stringi")) {install.packages("stringi")}
library(stringi)
if (!require("ggplot2")) {install.packages("ggplot2")}
library(ggplot2)
if (!require("magrittr")) {install.packages("magrittr")}
library(magrittr)
if (!require("RWeka")) {install.packages("RWeka")}
library(RWeka)
if (!require("openNLP")) {install.packages("openNLP")}
library(openNLP)
if (!require("wordcloud")) {install.packages("wordcloud")}
library(wordcloud)
if (!require("utils")) {install.packages("utils")}
library(utils)

##Set the Working Directory
mainDir <- "~."
subDir <- "R"
subDir2 <- "Johns Hopkins Data Science Certification"
subDir3 <- "Capstone Project"
if (file.exists(subDir)) { setwd(file.path(mainDir, subDir))} else
{  dir.create(file.path(mainDir, subDir), showWarnings = FALSE) 
  setwd(file.path(mainDir, subDir))}
mainDir <- getwd()
if (file.exists(subDir2)) { setwd(file.path(mainDir, subDir2))}   else 
{dir.create(file.path(mainDir, subDir2), showWarnings = FALSE) 
  setwd(file.path(mainDir, subDir2))}
mainDir <- getwd()
if (file.exists(subDir3)) {setwd(file.path(mainDir, subDir3))}  else 
{dir.create(file.path(mainDir,subDir3), showWarnings = FALSE) 
  setwd(file.path(mainDir, subDir3))}

##Download and unzip the data
FileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
FileName <- "Coursera-SwiftKey.zip"
outDir<-"unzipfolder"
if (!file.exists(FileName)) {
  download.file(FileURL ,FileName,method="auto") }

unzipDir <- "/final/en_US/"
US_twitter_file <- "en_US.twitter.txt"
US_blogs_file <- "en_US.blogs.txt"
US_news_file <- "en_US.news.txt"

files_info <- file.info(list.files(path=paste(outDir, unzipDir, sep =""), full.names = TRUE))

if (!file.exists(paste(outDir, unzipDir, US_twitter_file, sep =""))) {
  unzip(FileName,exdir=outDir)}
if (!file.exists(paste(outDir, unzipDir, US_blogs_file, sep =""))) {
  unzip(FileName,exdir=outDir)}
if (!file.exists(paste(outDir, unzipDir, US_news_file, sep =""))) {
  unzip(FileName,exdir=outDir)}

US_twitter_Data <- readLines(paste(outDir, "/final/en_US/en_US.twitter.txt", sep =""))
US_blogs_Data <- readLines(paste(outDir, "/final/en_US/en_US.blogs.txt", sep =""))
US_news_Data <- readLines(paste(outDir, "/final/en_US/en_US.news.txt", sep =""))

##Download and unzip the data
FileURL <- "https://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip"
FileName <- "full-list-of-bad-words-banned-by-google-txt-file.zip"
outDir<-"BadWords"
if (!file.exists(FileName)) {
  download.file(FileURL ,FileName,method="auto") }

unzip(FileName,exdir=outDir)
US_Bad_Words <- readLines(paste(outDir, "/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt", sep =""))

max(length(US_twitter_Data))
max(length(US_blogs_Data))
max(length(US_news_Data))

# Random subset of the data
random_twitter <- ceiling(runif(n=50000, min=0, max=length(US_twitter_Data)))
random_blogs <- ceiling(runif(n=20000, min=0, max=length(US_blogs_Data)))
random_news <- ceiling(runif(n=20000, min=0, max=length(US_news_Data)))

# subset
twitter_random_sub <- twitter[random_twitter]
blogs_random_sub <- blogs[random_blogs]
news_random_sub <- news[random_news]

subDirTask <- "Task01"

if (file.exists(subDirTask)) {setwd(file.path(mainDir, subDirTask))}  else 
{dir.create(file.path(mainDir,subDir3), showWarnings = FALSE) 
  setwd(file.path(mainDir, subDir3))}

# export the data
write.table(twitter, file='Task1/output/twitter.txt', row.names=FALSE, col.names=FALSE)
write.table(news, file='Task1/output/news.txt', row.names=FALSE, col.names=FALSE)
write.table(blogs, file='Task1/output/blogs.txt', row.names=FALSE, col.names=FALSE)
# load the random lines
twitter <- readLines('Task1/output/twitter.txt')
news <- readLines('Task1/output/news.txt')
blogs <- readLines('Task1/output/blogs.txt')

str(US_twitter_Data)
str(US_blogs_Data)
str(US_news_Data)
corpora <- data.frame(data_source = factor("twitter"), sentence=US_twitter_Data, words=stri_count_words(US_twitter_Data), 
                                      bytes = stri_numbytes(US_twitter_Data),
                                      words_per_byte = stri_numbytes(US_twitter_Data) / stri_count_words(US_twitter_Data))
summary(corpora)
nrow(corpora)
corpora <- rbind(corpora, data.frame(data_source=factor("blogs"), sentence=US_blogs_Data, words=stri_count_words(US_blogs_Data), 
                                     bytes = stri_numbytes(US_blogs_Data), 
                                     words_per_byte = stri_numbytes(US_blogs_Data) / stri_count_words(US_blogs_Data)) )
summary(corpora)
nrow(corpora)
corpora <- rbind(corpora, data.frame(data_source=factor("news"),  sentence=US_news_Data, words=stri_count_words(US_news_Data),
                                     bytes = stri_numbytes(US_news_Data), 
                                     words_per_byte = stri_numbytes(US_news_Data) / stri_count_words(US_news_Data)) )
summary(corpora)
str(corpora)
nrow(corpora)
length(corpora)
head(corpora)
aggdata <-aggregate(corpora, by= list(as.factor(corpora$words_per_byte)), 
                    FUN=mean, na.rm=TRUE)


