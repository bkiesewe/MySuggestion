# Birgit Kiesewetter, August 2016, devprocess.R
# This script sources predict1.R for the prediction functions
# the script also ready the dev sample txt and processes them to trigrams 
# and splits the last word of. With the unique 2 word phrases the next 3 words are predicted
# accuracy is calculated on the result.

# libraries needed
library(ggplot2) 
library(stringi)
library(stringr)
library(caTools)
library(tm)
library(RWeka)
library(plyr)
library(tidyr)


# this dir includes the 3 files used
home <- file.path("C:", "Users/Birgit/Desktop/Swift")
setwd(home)

# includes the final dataframes and the prediction function
source("C:/Users/Birgit/Desktop/Data/Capstone/predict1.R")


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



#creating dev corpus
devdata <- VCorpus(DirSource("./dev/", encoding = "UTF-8"),
                readerControl = list(language = "en")) 

# cleaning train corpus
devclean <- cleanup(devdata) # see function above


# function for rowsum, dataframe and sorting
processing <- function(x){
    token <- rowSums(as.matrix(x))
    token <- data.frame(term = names(token), freq = token)
    token <- token[order(-token[,2]),] 
}


#creating trigrams from dev data
options(mc.cores=1) 
TrigramDev <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tridev <- TermDocumentMatrix(devclean, control = list(tokenize = TrigramDev))

#creating dataframe
dev_df <- processing(tridev)
rownames(dev_df) <- c()

# separate the 3gram to single words
dev_df <- separate(dev_df, term, c("word1", "word2", "word") ,sep=" ")

#combine the first 2 again
dev_df <-  unite(dev_df, term, word1, word2, sep = " ")

# save 1
save(dev_df, file="./dev/dataframe/devdf1.RData") 

# need to do the same cosmetic done for the original dicts
# converting suggested words to proper concatinations including ' eg wasnt to wasn't

org <- readLines("./other/org.txt", encoding="UTF-8")
replace <- readLines("./other/replace.txt", encoding="UTF-8")
dev_df$word<- mapvalues(dev_df$word, from=org, to=replace)


# valid 2 letter words
valid <- "am	an	as	at	be	by	do	go	he	if	in	is	it	me	my	no	of	
oh	on	or	so	to	tv	up	us	we"																																													
valid <- unlist(strsplit(valid, "\t"))

dev_df <- dev_df[(dev_df$word %in% valid & nchar(dev_df$word) == 2) | (nchar(dev_df$word) > 2) |
                     (nchar(dev_df$word) == 1) ,]


# save 2
save(dev_df, file="./dev/dataframe/devdf2.RData") 


# adding emtpy columns for the 3 suggested words and the single words
dev_df$complete <- 0
dev_df$suggestion1 <- 0
dev_df$suggestion2 <- 0
dev_df$suggestion3 <- 0



######## run test
# predict into dev_df$complete column, this takes more than 2 hours on my PC
Sys.time()
dev_sub$complete <- lapply(dev_sub$term, prediction)
Sys.time()

save(dev_sub, file="./dev/dataframe/devdf3.RData") 

# splitt the suggestions into the 3 columns
for (i in 1:nrow(dev_sub)){
    dev_sub$suggestion1[i] <- dev_sub$complete[[i]][1]
    dev_sub$suggestion2[i] <- dev_sub$complete[[i]][2]
    dev_sub$suggestion3[i] <- dev_sub$complete[[i]][3]
}

save(dev_sub, file="./dev/dataframe/devdf4.RData") 

# True / False conversion
dev_sub$suggestion1 <- dev_sub$word==dev_sub$suggestion1
dev_sub$suggestion2 <- dev_sub$word==dev_sub$suggestion2
dev_sub$suggestion3 <- dev_sub$word==dev_sub$suggestion3

#results:
# total accuracy
sum(rowSums(dev_sub[, c(5,6,7)]))/nrow(dev_sub)
# top 1
sum(dev_sub$suggestion1)/nrow(dev_sub)
# top 2
sum(dev_sub$suggestion2)/nrow(dev_sub)
# top 3
sum(dev_sub$suggestion3)/nrow(dev_sub)


# > sum(rowSums(dev_sub[, c(5,6,7)]))/nrow(dev_sub)
# [1] 0.6642334
# > sum(dev_sub$suggestion1)/nrow(dev_sub)
# [1] 0.5042071
# > sum(dev_sub$suggestion2)/nrow(dev_sub)
# [1] 0.1058842
# > sum(dev_sub$suggestion3)/nrow(dev_sub)
# [1] 0.05414208
