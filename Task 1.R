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

US_twitter_Data <- readLines(paste(outDir, unzipDir, US_twitter_file, sep =""))
US_blogs_Data <- readLines(paste(outDir, unzipDir, US_blogs_file, sep =""))
US_news_Data <- readLines(paste(outDir, unzipDir, US_news_file, sep =""))

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
random_blogs <- ceiling(runif(n=50000, min=0, max=length(US_blogs_Data)))
random_news <- ceiling(runif(n=50000, min=0, max=length(US_news_Data)))

# subset
twitter_random_sub <- US_twitter_Data[random_twitter]
blogs_random_sub <- US_blogs_Data[random_blogs]
news_random_sub <- US_news_Data[random_news]

WorkingDir <- getwd()
subDirTask <- "Task01"
if (file.exists(subDirTask)) {setwd(file.path(WorkingDir, subDirTask))}  else 
{dir.create(file.path(WorkingDir,subDirTask), showWarnings = FALSE) 
  setwd(file.path(WorkingDir, subDirTask))}

SubDir <- getwd()
subDirFolderName <- "US_Subsetted_Outputs"
if (file.exists(subDirFolderName)) {setwd(file.path(SubDir, subDirFolderName))}  else 
{dir.create(file.path(SubDir,subDirFolderName), showWarnings = FALSE) 
  setwd(file.path(SubDir, subDirFolderName))}

SubDir <- getwd()


file <- "twitter.txt"
# export the subsetted data
write.table(random_twitter, file= paste(SubDir, file, sep ='/') , row.names=FALSE, col.names=FALSE)
file <- "blogs.txt"
write.table(blogs_random_sub, file= paste(SubDir, file, sep ='/') , row.names=FALSE, col.names=FALSE)
file <- "news.txt"
write.table(news_random_sub, file= paste(SubDir, file, sep ='/') , row.names=FALSE, col.names=FALSE)
# load the random lines
list.files()
file <- "twitter.txt"
twitter <- readLines(paste(SubDir, file, sep ='/'))
file <- "blogs.txt"
blogs <- readLines(paste(SubDir, file, sep ='/'))
file <- "news.txt"
news <- readLines(paste(SubDir, file, sep ='/'))


str(twitter)
str(blogs)
str(news)


corpus <- data.frame(data_source = factor("twitter"), sentence=twitter, words=stri_count_words(twitter), 
                                      bytes = stri_numbytes(twitter),
                                      words_per_byte = stri_numbytes(twitter) / stri_count_words(twitter))
summary(corpus)
nrow(corpus)
corpus <- rbind(corpus, data.frame(data_source=factor("blogs"), sentence=blogs, words=stri_count_words(blogs), 
                                     bytes = stri_numbytes(blogs), 
                                     words_per_byte = stri_numbytes(blogs) / stri_count_words(blogs)) )
summary(corpus)
nrow(corpus)
corpus <- rbind(corpus, data.frame(data_source=factor("news"),  sentence=news, words=stri_count_words(news),
                                     bytes = stri_numbytes(news), 
                                     words_per_byte = stri_numbytes(news) / stri_count_words(news)) )
summary(corpus)
str(corpus)
nrow(corpus)
desc(corpus)
length(corpus)
head(corpus)
observations <- corpus$words
aggdata <-aggregate(x = observations, by= list(unique.values = corpus$data_source) , 
                    FUN=function(observations){
                      fitdistr(observations, 
                               densfun = "Poisson")$estimate
                    })
aggdata
?fitdistr

fitdistr(observations, densfun = "Poisson")

hist(observations, 100)
library(MASS) 
