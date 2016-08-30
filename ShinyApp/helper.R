# Birgit Kiesewetter, August 2016, helper.R
# This script is loaded by server.R in the shiny app My next word suggestion
# includes required functions for suggestion app server 

# loading frames

load("data/final_3gram.RData")
load("data/final_2gram.RData")


# generating frame with most frequent words
final_w <- data.frame(From = "3 most common words", Phrase = c(rep('---', 3)), 
                      Word=c("the", "to", "end"),   stringsAsFactors = F)

# clean input string the same way as train corpus has been cleaned
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

# prediction
# search on trigrams, if less than 3 are found backoff to bigram
# if still not 3 suggestions use frequent words 
# return the raw data frame with the suggestions build up
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
        return(suggestion)
        
    } 
}

