# Birgit Kiesewetter, August 2016, process.R
# This script uses the saved dataframes from script get_clean_corpus.R
# script includes further processing to generate the data frames / dictionaries


library(plyr)
library(tidyr)

cname <- file.path("C:", "Users/Birgit/Desktop/Swift")   
setwd(cname)

# generated through get_clean_corpus.R
load("./train/tfdataframe.RData") #1084159
load("./train/wfdataframe.RData") #69915
load("./train/bfdataframe.RData") #579684

# separate the 2gram to single words
bf_fin <- separate(bf, term, c("term", "word") ,sep=" ")
bf_fin <- bf_fin[order(bf[,1]),] # sorts the phrases on term

# separate the 3gram to single words
tf_fin <- separate(tf, term, c("word1", "word2", "word") ,sep=" ")
#combine the first 2 again
tf_fin <-  unite(tf_fin, term, word1, word2, sep = " ")
tf_fin <- tf_fin[order(tf[,1]),] # sorts the phrases on term

# sorting on terms + frequency of following word 
sorted_tf_fin <- tf_fin[order(tf_fin$term, tf_fin$freq,  decreasing = T) , ] 
sorted_bf_fin <- bf_fin[order(bf_fin$term, bf_fin$freq,  decreasing = T) , ] 

# merge tri and word frequence to get the frame sorted by phrase frequence and second on single word freq
mergedtf <- merge(sorted_tf_fin, wf , by.x="word",by.y="word", all=TRUE)
mergedbf <- merge(sorted_bf_fin, wf , by.x="word",by.y="word", all=TRUE)

sorted_mergedtf <- mergedtf[order(mergedtf$term, mergedtf$freq.x, mergedtf$freq.y,  decreasing = T) , ] 
sorted_mergedbf <- mergedbf[order(mergedbf$term, mergedbf$freq.x, mergedbf$freq.y,  decreasing = T) , ]
#head(sorted_mergedtf)


# reducing dataframes to only the needed obs
# removing all terms that exist more than 3 times and leave the first 3 based also sorted on word frequency
final_tri <- ddply(sorted_mergedtf, .(term), function(x) x[c(1,2,3), ] ) # pulling the first 3 occurances
final_tri <- final_tri[complete.cases(final_tri),] # removing incomplete cases as all the time 3 are pulled
final_bi <- ddply(sorted_mergedbf, .(term), function(x) x[c(1,2,3), ] ) # pulling the first 3 occurances
final_bi <- final_bi[complete.cases(final_bi),] # removing incomplete cases as all the time 3 are pulled

# saving for predicition model
save(final_tri, file="./train/trigram.RData")
save(final_bi, file="./train/bigram.RData")

# clearing workspace
rm(list=ls())





