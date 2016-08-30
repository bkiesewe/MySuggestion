# Birgit Kiesewetter, August 2016, cosmetic.R
# This script uses the saved dataframes from script get_clean_corpus.R, process.R
# script includes cosmetic processing and final preparation for shiny dictionaries.

# loading prepared data frames
cname <- file.path("C:", "Users/Birgit/Desktop/Swift")   
setwd(cname)
load("./train/trigram.RData") #720292 -> 698265
load("./train/bigram.RData") #115671 -> 110606


library(plyr)

# subsetting / removing garbage from the dictionaries
# long words and terms that do not make sense
final_tri <- final_tri[!nchar(final_tri$word) > 20 ,]
final_bi <- final_bi[!nchar(final_bi$word) > 20 ,]
final_bi <-final_bi[!nchar(final_bi$term) > 20,] 
final_tri <-final_tri[!nchar(final_tri$term) > 30,] 

# single char beside a and i
final_bi <- final_bi[!(nchar(final_bi$word) == 1 & !(final_bi$word %in% c("a", "i"))),] 
final_bi <- final_bi[!(nchar(final_bi$term) == 1 & !(final_bi$term %in% c("a", "i"))),] 

# only works on words not terms!
final_tri <- final_tri[!(nchar(final_tri$word) == 1 & !(final_tri$word %in% c("a", "i"))),] 

# single character beside a and i eg 'y commodity' within terms
final_tri <- final_tri[!(final_tri$term %in% (grep("^[^a,i] ", final_tri$term, value = T))),] 
# same at the end of the term
final_tri <- final_tri[!(final_tri$term %in% (grep(" [b-h]{1}$", final_tri$term, value = T))),] 
final_tri <- final_tri[!(final_tri$term %in% (grep(" [j-z]{1}$", final_tri$term, value = T))),]

# get all words where letters repeate at least 3 times 
final_tri <- final_tri[!(final_tri$word %in% 
                             (grep("(.)\\1{3}", final_tri$word, value = T, perl=TRUE))),] 
final_tri <- final_tri[!(final_tri$term %in%
                             (grep("(.)\\1{3}", final_tri$term, value = T, perl=TRUE))),]
final_bi <- final_bi[!(final_bi$word %in% 
                             (grep("(.)\\1{3}", final_bi$word, value = T, perl=TRUE))),]
final_bi <- final_bi[!(final_bi$term %in%
                             (grep("(.)\\1{3}", final_bi$term, value = T, perl=TRUE))),]

# remove left terms a a and i i that do not make sense
final_tri <- final_tri[!(final_tri$term %in% 
                             (grep("^a a$", final_tri$term, value = T))),] # 3000
final_tri <- final_tri[!(final_tri$term %in%
                             (grep("^i i$", final_tri$term, value = T))),]

# converting suggested words to proper concatinations including ' eg wasnt to wasn't

org <- readLines("./other/org.txt", encoding="UTF-8")
replace <- readLines("./other/replace.txt", encoding="UTF-8")


final_tri$word<- mapvalues(final_tri$word, from=org, to=replace)
final_bi$word <- mapvalues(final_bi$word, from=org, to=replace)

# removing 2 character words that are no real english words
# valid 2 character words
valid <- "am	im	an	as	at	be	by	do	go	he	if	in	is	it	me	my	no	of	oh	ok	
on	or	so	to	tv	up	us	we"																																													
valid <- unlist(strsplit(valid, "\t"))

final_tri <- final_tri[(final_tri$word %in% valid & nchar(final_tri$word) == 2) | 
                           (nchar(final_tri$word) > 2) | (nchar(final_tri$word) == 1) ,]
final_bi <- final_bi[(final_bi$word %in% valid & nchar(final_bi$word) == 2) |
                         (nchar(final_bi$word) > 2) | (nchar(final_bi$word) == 1) ,]
final_bi <- final_bi[(final_bi$term %in% valid & nchar(final_bi$term) == 2) |
                         (nchar(final_bi$term) > 2) | (nchar(final_bi$term) == 1) ,]

# saving 
save(final_tri, file="./train/cosmetic_trigram.RData")
save(final_bi, file="./train/cosmetic_bigram.RData")

# adding source for behind the scenes
final_bi <- final_bi[, 1:2]
final_bi$From <- "Bigram Dictionary"
final_tri <- final_tri[,1:2]
final_tri$From <- "Trigram Dictionary"
# removing unneeded columns like freq
final_tri$Phrase <- final_tri$term
final_tri$Word <- final_tri$word
final_tri <- final_tri[, c(3,4,5)]
final_bi$Phrase <- final_bi$term
final_bi$Word <- final_bi$word
final_bi <- final_bi[, c(3,4,5)]

# processing directory
save(final_tri, file="./train/final_3gram.RData") #698265
save(final_bi, file="./train/final_2gram.RData") #110606

# current shiny directory
save(final_tri, file="C:/Users/Birgit/Desktop/Swift/MySuggestion/data/final_3gram.RData")
save(final_bi, file="C:/Users/Birgit/Desktop/Swift/MySuggestion/data/final_2gram.RData")

#load("./train/final_3gram.RData")
#load("./train/final_2gram.RData")

#final_bi[final_bi$Phrase=="am",]
#final_bi[final_tri$Phrase=="am",]


# clearing workspace
rm(list=ls())