# TA: Patrick Chester
# Course: Text as Data
# Date: 3/28/2017
# Recitation 9: Unsupervised Learning 1

# Clear Global Environment
rm(list = ls())

# Loading packages
install.packages("lsa")

library(quanteda)
library(quantedaData)
library(lsa)


# Let's look at a few more unsupervised approaches

## 1) Latent Semantic Analysis (LSA)

data("data_corpus_SOTU")


# Use the real example from class

SOTU_dfm<-dfm(data_corpus_SOTU[145:223,], stem = T, removePunct=T, remove = stopwords("english"))

SOTU_dfm@Dimnames$docs


# Create LSA weights using TDM
SOTU_tdm_lsa <- lsa(t(SOTU_dfm))


# Check to see what a good number of dimensions is

SOTU_tdm_lsa_svd<-svd(SOTU_tdm_lsa$tk)$d

dimcalc_share(share = 0.5)(SOTU_tdm_lsa_svd)

plot(SOTU_tdm_lsa_svd)


# By default, share is set to .5; let's try .9

dimcalc_share(share = 0.9)(SOTU_tdm_lsa_svd)


# Lecture example uses 5

lsa_fit<-lsa(t(SOTU_dfm), 5 )

myNewMatrix <- t(as.textmatrix(lsa_fit) )


# Compare features
SOTU_dfm@Dimnames$docs[9]

topfeatures(SOTU_dfm[9,])

sort(myNewMatrix[9,], decreasing=T)[1:10]



SOTU_dfm@Dimnames$docs[55]

topfeatures(SOTU_dfm[55,])

sort(myNewMatrix[55,], decreasing=T)[1:10]


SOTU_dfm@Dimnames$docs[72]

topfeatures(SOTU_dfm[72,])

sort(myNewMatrix[72,], decreasing=T)[1:10]

# Associate: a method to identify words that are most similar to other words using a LSA

lsa_fit<-lsa(t(SOTU_dfm), 3 )


myNewMatrix = as.textmatrix(lsa_fit) 


china<-associate(myNewMatrix, "china", "cosine", threshold = .7)

china[1:10]


oil<-associate(myNewMatrix, "oil", "cosine", threshold = .7)

oil[1:10]

america<-associate(myNewMatrix, "america", "cosine", threshold = .7)

america[1:10]

health<-associate(myNewMatrix, "health", "cosine", threshold = .7)

health[1:10]

# Any other word suggestions?


## 2) WORDFISH

# How is it different from other approaches we've used for scaling?


# Read in conservative and labour manifestos

setwd("E:/Documents/Word saves/NYU/NYU Classes/2016 - Spring/Text as Data/TAD Labs/Text_as_Data-master/cons_lab")

# Read in the files
files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))


# Name data
files<-unlist(files)
files<-gsub("./", "", files )
files<-gsub(".txt", "", files )

# Create metadata

year<-unlist(strsplit(files, "[^0-9]+"))

year<-year[year!=""]

party<-unlist(strsplit(files, "[^A-z]+"))

party<-party[party!="a" & party!="b"]

#create data frame
man_df<-data.frame(year = as.numeric(year),
                   party = party,
                   stringsAsFactors = TRUE)
man_df$text<-text


lab_con_dfm<-dfm(man_df$text, stem=T, remove = stopwords("english"), removePunct = T)


# fit wordfish

# Setting the anchor on parties

df_fit<-textmodel_wordfish(lab_con_dfm, c(1,24))


?textmodel

plot(year[1:23], df_fit@theta[1:23])

points(year[24:46], df_fit@theta[24:46], pch=8)
?plot

plot(as.factor(party), df_fit@theta)


# most important features--word fixed effects


words<-df_fit@psi
names(words) <- df_fit@features

sort(words)[1:50]

sort(words, decreasing=T)[1:50]

# Guitar plot

weights<-df_fit@beta

plot(weights, words)



