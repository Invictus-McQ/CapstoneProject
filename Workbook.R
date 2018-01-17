# prepare_data.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Prepare data for downstream analysis and models
# 2015-12-20
##Clear
rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
# Libraries and options ####
source('shiny/prediction.R')

if (!require("readr")) {install.packages("readr")} 
suppressPackageStartupMessages(require(readr))
if (!require("caTools")) {install.packages("caTools")} 
suppressPackageStartupMessages(require(caTools))
if (!require("tidyr")) {install.packages("tidyr")} 
suppressPackageStartupMessages(require(tidyr))


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

# Read and prepare data ####

# Read in data
twitterRaw <- readLines(paste(outDir, unzipDir, US_twitter_file, sep =""))
blogsRaw <- readLines(paste(outDir, unzipDir, US_blogs_file, sep =""))
newsRaw <- readLines(paste(outDir, unzipDir, US_news_file, sep =""))

AllRaw = c(blogsRaw, newsRaw, twitterRaw)

# Sample and combine data  
set.seed(1337331)
n = 1000
combined = sample(AllRaw, length(combinedRaw) / n)

# Split into train and validation sets
split <- sample.split(combined, 0.50)
train <- subset(combined, split == T)
valid <- subset(combined, split == F)

# Transfer to corpus format segmented into sentences and Tokenize (prediction.R)
valid <- fun.CleanCorpus(valid)
train = TextScrubber(train)

if (!require("rJava")) {install.packages("rJava")} 
suppressPackageStartupMessages(require(rJava))
if (!require("RWeka")) {install.packages("RWeka")}
suppressPackageStartupMessages(library(openNLP))



delimiter <- " \\r\\n\\t.,;:\"()?!"
train1 <- NGramTokenizer(train$content, Weka_control(min = 1, max = 1))
train2 <- NGramTokenizer(train$content, Weka_control(min = 2, max = 2, delimiters = delimiter))
train3 <- NGramTokenizer(train$content, Weka_control(min = 3, max = 3, delimiters = delimiter))
train4 <- NGramTokenizer(train$content, Weka_control(min = 4, max = 4, delimiters = delimiter))

# Frequency tables ####

dtm <- DocumentTermMatrix(train)
TermDocumentMatrix(train)

dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

if (!require("tidyr")) {install.packages("tidyr")}; require(tidyr)
dfTrain1 = data.frame(Term = train1); dfTrain1 = fun.frequency(dfTrain1)
dfTrain2 = data_frame(Term = train2); dfTrain2 = fun.frequency(dfTrain2) %>%  separate(Term, c('word1', 'Term'), " "); 
dfTrain3 = data_frame(Term = train3); dfTrain3 = fun.frequency(dfTrain3) %>%  separate(Term, c('word1', 'word2', 'Term'), " ")
dfTrain4 = data_frame(Term = train4); dfTrain4 = fun.frequency(dfTrain4) %>%  separate(Term, c('word1', 'word2', 'word3', 'Term'), " ")

inspect(DocumentTermMatrix(train,list(dictionary = c("you", "will"))))
findAssocs(dtm, "pray", 0.1)
findFreqTerms(dtm, 11)

# Save Data ####
saveRDS(dfTrain1, file = 'dfTrain1.rds')
saveRDS(dfTrain2, file = 'dfTrain2.rds')
saveRDS(dfTrain3, file = 'dfTrain3.rds')
saveRDS(dfTrain4, file = 'dfTrain4.rds')
