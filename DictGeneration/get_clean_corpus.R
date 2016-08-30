# Birgit Kiesewetter, August 2016, get_clean_corpus.R
# script includes 
# 1) loading, sampling files
# 2) creating the train corpus
# 3) cleaning the corpus (function cleanup)
# 4) generating word matrix, bigrams and trigrams
# 5) creating frequency dataframes

# libraries needed
library(ggplot2) 
library(stringi)
library(stringr)
library(caTools)
library(tm)
library(RWeka)


# this dir includes the 3 files used
home <- file.path("C:", "Users/Birgit/Desktop/Swift")
setwd(home)

# reading swearwords downloaded from http://www.bannedwordlist.com/lists/swearWords.txt
toberemoved <- readLines("./other/swearWords.txt", encoding="UTF-8")

# functions for cleaning corpus
removeGraphs <- function(x) {
    str_replace_all(x,"[^[:graph:]]", " ")  
}

cleanup <- function(x) {
    clean <- tm_map(x , removeNumbers)
    clean <- tm_map(clean, removeGraphs)
    clean <- tm_map(clean, iconv,  to="ASCII", sub=" ") 
    clean <- tm_map(clean, removePunctuation)
    clean <- tm_map(clean, tolower)
    clean <- tm_map(clean, removeWords, toberemoved)
    clean <- tm_map(clean , stripWhitespace)
    clean <- tm_map(clean, PlainTextDocument)
}

#reading files completely
twitter_US <- readLines("./OriginalUS/en_US.twitter.txt", encoding="UTF-8")
blogs_US <- readLines("./OriginalUS/en_US.blogs.txt",  encoding="UTF-8")
news_US <- readLines("./OriginalUS/en_US.news.txt",  encoding="UTF-8")

#getting samples of each of 2% of twitter, 3% blogs and 5% news
set.seed(1555) # for reproducibility
sample_twitter <- twitter_US[rbinom(length(twitter_US), 1, 0.02) == 1]
sample_blogs<- blogs_US[rbinom(length(blogs_US), 1, 0.03) == 1]
sample_news <- news_US[rbinom(length(news_US), 1, 0.05) == 1]

#10% for test
set.seed(1001) 
#twitter
sample = sample.split(sample_twitter, SplitRatio = .9)
twittertrain = subset(sample_twitter , sample == TRUE)
twittertest = subset(sample_twitter, sample == FALSE)
#blogs
sample = sample.split(sample_blogs, SplitRatio = .9)
blogstrain = subset(sample_blogs , sample == TRUE)
blogstest = subset(sample_blogs, sample == FALSE)
#news
sample = sample.split(sample_news, SplitRatio = .9)
newstrain = subset(sample_news , sample == TRUE)
newstest = subset(sample_news, sample == FALSE)

#10% of remaining train for dev 
#twitter
sample = sample.split(twittertrain, SplitRatio = .9)
twittertrain = subset(twittertrain , sample == TRUE)
twitterdev = subset(twittertrain, sample == FALSE)
#blogs
sample = sample.split(blogstrain, SplitRatio = .9)
blogstrain = subset(blogstrain , sample == TRUE)
blogsdev = subset(blogstrain, sample == FALSE)
#news
sample = sample.split(newstrain, SplitRatio = .9)
newstrain = subset(newstrain , sample == TRUE)
newsdev = subset(newstrain, sample == FALSE)

# saving test+dev data and clearing cache
writeLines(twittertest, "./test/twittertest.txt")
writeLines(blogstest, "./test/blogstest.txt")
writeLines(newstest, "./test/newstest.txt")

writeLines(sample_twitter, "./sample/twittersample.txt")
writeLines(sample_blogs, "./sample/blogssample.txt")
writeLines(sample_news, "./sample/newssample.txt")

writeLines(twitterdev, "./dev/twitterdev.txt")
writeLines(blogsdev, "./dev/blogsdev.txt")
writeLines(newsdev, "./dev/newsdev.txt")

writeLines(twittertrain, "./train/formodel/twittertrain.txt")
writeLines(blogstrain, "./train/formodel/blogstrain.txt")
writeLines(newstrain, "./train/formodel/newstrain.txt")

rm(twittertest, blogstest, newstest)
rm(twitterdev, blogsdev, newsdev)
rm(sample_twitter, sample_blogs, sample_news) 
rm(twittertrain, blogstrain, newstrain)
rm(twitter_US, blogs_US, news_US)

#creating train corpus
data <- VCorpus(DirSource("./train/formodel", encoding = "UTF-8"),
                                   readerControl = list(language = "en")) # 11.8 Mb
# cleaning train corpus
clean <- cleanup(data) # see function above

# saving corpus
writeCorpus(clean, filenames = c("./train/twittertraincl.txt.txt","./train/blogstraincl.txt.txt", "./train/newstraincl.txt.txt" ))


# unigram - changed to have 1 and 2 char words also included
word_matrix <- DocumentTermMatrix(clean, control=list(wordLengths=c(1,Inf)))
#creating word frequence matrix 
wordFreq <- colSums(as.matrix(word_matrix))
# checking frequency of single words
wf <- data.frame(word = names(wordFreq), freq = wordFreq)
wf <- wf[order(-wf[,2]),]
# saving file
save(wf, file="./train/wfdataframe.RData") # 69915 obs

# function for rowsum, dataframe and sorting
processing <- function(x){
    token <- rowSums(as.matrix(x))
    token <- data.frame(term = names(token), freq = token)
    token <- token[order(-token[,2]),] 
}

#bigram
options(mc.cores=1) 
BigramToken <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bitoken <- TermDocumentMatrix(clean, control = list(tokenize = BigramToken))
# make dataframe
bf <- processing(bitoken) #579684 obs
# saving data frame
save(bf, file="./train/bfdataframe.RData") 

# trigram 
TrigramToken <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tritoken <- TermDocumentMatrix(clean, control = list(tokenize = TrigramToken))
# make dataframe
tf <- processing(tritoken) # obs #1084159
# saving data frame
save(tf, file="./train/tfdataframe.RData") 

# clearing workspace
rm(list=ls())

# further processing is done in process.R