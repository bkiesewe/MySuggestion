# Birgit Kiesewetter, August 2016, predict1.R
# This script includes the prediction code for the suggestion up 
# It is used to within script devprocess.R for cross validation 

cleanup <- function(x) {
    clean <- tm_map(x , removeNumbers)
#    clean <- tm_map(clean, removeGraphs)
    clean <- tm_map(clean, iconv,  to="ASCII", sub=" ") 
    clean <- tm_map(clean, removePunctuation)
    clean <- tm_map(clean, tolower)
#    clean <- tm_map(clean, removeWords, toberemoved)
    clean <- tm_map(clean , stripWhitespace)
    clean <- tm_map(clean, PlainTextDocument)
}


cname <- file.path("C:", "Users/Birgit/Desktop/Swift")   
setwd(cname)

load("train/final_3gram.RData") #698265
load("train/final_2gram.RData") #110606

# generating frame with most frequent words
final_w <- data.frame(From = "3 most common words", Phrase = c(rep('---', 3)), 
                      Word=c("the", "to", "end"),   stringsAsFactors = F)


library(tm)


# input of a phrase
input <- "I'm getting bored with my friends and family services division"

# prediction
# search on trigrams , if less than 3 are found backoff to bigram
# if still not 3 suggestions use frequent words 
# finally show the 3 suggested words
prediction <- function(inputtext)
{
    inputtext <- VCorpus(VectorSource(inputtext))
    inputtext <- cleanup(inputtext)
    inputtext <- sapply(inputtext, `[[`, "content")
    inputwords <- unlist(strsplit(inputtext, " "))
    
    suggestion <- data.frame()
    suggestion2 <- data.frame()
    
    if (length(inputwords) > 0) {
        # pulling the last 2 words from the input
        word2 <- inputwords[length(inputwords)]
        word1 <- inputwords[length(inputwords)-1]
        interm <- paste(word1, word2) 
        
        suggestion <- subset(final_tri, Phrase==interm)
        
        if (dim(suggestion)[1] < 3) {
            suggestion2 <- subset(final_bi, Phrase==word2)
            suggestion <- rbind(suggestion, suggestion2)}
        if (length(unique(suggestion$Word)) < 3) {
            suggestion <- rbind(suggestion, final_w)}
        suggestion <- unique(suggestion$Word)
        return(suggestion[1:3])
        
    } 
}

prediction(input)
