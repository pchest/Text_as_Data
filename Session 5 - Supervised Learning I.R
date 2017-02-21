# TA: Patrick Chester
# Course: Text as Data
# Date: 2/21/2017
# Recitation 5: Supervised Learning I

## 1 Setting up Quanteda 

# Clear Global Environment
rm(list = ls())

# Libraries
library(quanteda)
library(quantedaData)

# 2 Loading data: conservative manifestos
setwd("E:/Documents/Word saves/NYU/NYU Classes/2016 - Spring/Text as Data/TAD Labs/Text_as_Data-master/cons")

##read in the files
files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))


#name data
files<-unlist(files)
files<-gsub("./Con", "", files )
files<-gsub(".txt", "", files )

# Why might we get an error here?
man_df<-data.frame(year = as.numeric(files), text = text , stringsAsFactors = FALSE)

# But we can fix it with regular expressions
man_df<-data.frame(year = files, text = text , stringsAsFactors = FALSE)

grep("[a-z]|\\W$", man_df$year, value = T) # Searches for letters or symbols
man_df$year <- gsub("a$", ".2", man_df$year)
man_df$year <- gsub("b$", ".8", man_df$year)
man_df$year <- as.numeric(man_df$year)

# 3 Regular Expressions

# More regular expression examples

words <- c("Washington Post", "NYT", "Wall Street Journal", "Peer-2-Peer", "Red State")

# Exploring by character type
grep("\\w", words, value = T)  # Elements that have letters
grep("\\w{7}", words, value = T)  # Elements that have words that are at least 7 characters long
grep("\\d", words, value = T)  # Elements that contain numbers
grep("\\W", words, value = T)  # Elements that contain special characters (Including white space)

words2 <- c("voting", "votes", "devoted", "vote")

grep("^vot", words2) #Returns the location of a vector
grep("^vot", words2, value = T) #Returns the matched components of the vector
grepl("^vot", words2)  # Returns a logical vector indicating whether or not the component containes the expression

presidents <- c("Roosevelt-33", "Roosevelt-37", "Obama-2003")

# Replace patterns
gsub("(\\w)-(\\d{2})", "\\1-19\\2", presidents) # Parentheses can identify components that can later be referenced by \\1 - \\9
gsub("(\\w)-(\\d{2})$", "\\1-19\\2", presidents) # $ Identifies the end of a string


# 4 Selecting Features from DFM using Regular Expressions

# Using simple texts

testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with the newspaper from a a boy named Seamus, in his mouth."

print(dfm(testText, select="s$", valuetype = "regex"))  # keep only words ending in "s"

testTweets <- c("2 + 2 = 4 #1984",
                "I thought you said the park? Why are we at the vet? #QuestionsFromPets",
                "Holy freeway #flooding Batman! #californiastorms taking their toll.")

print(dfm(testTweets, select="^#", valuetype = "regex"))  # keep only hashtags


# Selecting features from a corpus

data(ie2010Corpus)

ieDfm <- dfm(ie2010Corpus, select=c("tax|budg|^auster"), 
             valuetype = "regex") # valuetype = "regex" ensures that the select input will be interpreted as a regular expression

View(ieDfm)


# 5 Dictionaries


mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
             "New York City has raised a taxes: an income tax and a sales tax.")

mydict <- list(tax=c("tax", "income", "capital", "gains", "inheritance"))

print(dfm(mytexts, dictionary=mydict))

mydfm<-dfm(mytexts, dictionary=mydict)

featnames(mydfm)

# Example: Laver Garry dictionary

setwd("E:/Documents/Word saves/NYU/NYU Classes/2016 - Spring/Text as Data/TAD Labs/Text_as_Data-master")

lgdict <- dictionary(file = "LaverGarry.cat", format = "wordstat")

head(dfm(man_df$text, dictionary=lgdict))

dic_dfm<-dfm(man_df$text, dictionary=lgdict)

featnames(dic_dfm)

# plot it

plot(man_df$year, 
     dic_dfm[,"CULTURE.SPORT"],
     xlab="Year", ylab="SPORTS", type="b", pch=19)

plot(man_df$year, 
     dic_dfm[,"VALUES.CONSERVATIVE"],
     xlab="Year", ylab="Conservative values", type="b", pch=19)


plot(man_df$year, 
     dic_dfm[,"INSTITUTIONS.CONSERVATIVE"] - dic_dfm[,"INSTITUTIONS.RADICAL"],
     xlab="Year", ylab="Net Conservative Institutions", type="b", pch=19)





# RID Dictionary--Regressive Imagery Dictionary

rid_dict <- dictionary(file = "RID.cat", format = "wordstat")

data("SOTUCorpus")

sotus <- texts(SOTUCorpus)

year<-(SOTUCorpus$documents$Date)

pres<-(SOTUCorpus$documents$President)


head(dfm(sotus, dictionary=rid_dict))

dic_dfm<-dfm(sotus, dictionary=rid_dict)

featnames(dic_dfm)

plot(year, 
     dic_dfm[,"PRIMARY.REGR_KNOL.NARCISSISM"],
     xlab="Year", ylab="Narcissism", type="b", pch=19)

plot(year, 
     dic_dfm[,"EMOTIONS.ANXIETY._"],
     xlab="Year", ylab="Anxiety", type="b", pch=19)

plot(year, 
     dic_dfm[,"EMOTIONS.AGGRESSION._"],
     xlab="Year", ylab="Aggression", type="b", pch=19)

plot(year, 
     dic_dfm[,"PRIMARY.ICARIAN_IM.FIRE"] + dic_dfm[,"PRIMARY.ICARIAN_IM.ASCEND"] +dic_dfm[,"PRIMARY.ICARIAN_IM.DESCENT"] +
       dic_dfm[,"PRIMARY.ICARIAN_IM.DEPTH"] + dic_dfm[,"PRIMARY.ICARIAN_IM.HEIGHT"] + dic_dfm[,"PRIMARY.ICARIAN_IM.WATER"],
     xlab="Year", ylab="Icarian-ness", type="b", pch=19)


# Code Credit: Some Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
# Avaliable on his website: www.kenbenoit.net
