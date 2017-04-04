# TA: Patrick Chester
# Course: Text as Data
# Date: 4/3/2017
# Recitation 10: Homework 2 Review

# Clearing global environment
rm(list = ls())

# Loading libraries
libs <- c("dplyr","jsonlite","stringr","foreach","rjson","quanteda","NLP","tm", "RTextTools")
lapply(libs, library, character.only = T)
rm(libs)

# Setting WD
setwd("E:/Documents/Word saves/NYU/NYU Classes/2017 - Spring/Text as Data - TA/Homeworks/Homework 2")

# Problem 1: Calculating naive Bayes by Hand
df <- data.frame(
lepen0 = c("immigration voter culture help neighbourhood"),
lepen1 = c("immigration women assimilate afraid win"),
lepen2 = c("culture voter economy president help"),
macron1 = c("voter women help reform education"),   
macron2 = c("union economy hope immigration neighbourhood"),  
macron3 = c("win union europe elect president"), 
lepen3 = c("economy voter immigration president culture"),  
macron4 = c("success german culture help french"))
dfm <- dfm(t(df))
mat <- as.matrix(dfm)
rownames(mat) <- names(df)

# 1A: Without smoothing
(4/7)*(1/20)*(1/20)*(1/20)*(2/20)*(1/20) # Macron
(3/7)*(2/15)*(2/15)*(2/15)*(1/15)*(0/15) # Lepen

# 1B: With smoothing
(4/7)*(2/40)*(2/40)*(2/40)*(3/40)*(2/40) # Macron
(3/7)*(3/35)*(3/35)*(3/35)*(2/35)*(1/35) # Lepen


## Problem 2: Sentitment Analysis of Amazon Reviews

# Loading data
samp <- read.csv("amazon_reviews.csv", stringsAsFactors = F)

# 2A: Generating variables
med<-median(samp$Score)

samp$positive<-as.numeric(samp$Score>med)
samp$anchor_positive<-as.numeric(samp$Score==5)
samp$anchor_negative<-as.numeric(samp$Score==1)

# Checking data
head(samp %>% select(anchor_positive, anchor_negative))
table(samp$anchor_positive)


# Read in positive and negatrive words

pos<-read.table("https://raw.githubusercontent.com/pchest/Text_as_Data/master/HW2data/positive-words.txt", stringsAsFactors = F)
neg<-read.table("https://raw.githubusercontent.com/pchest/Text_as_Data/master/HW2data/negative-words.txt", stringsAsFactors = F)

pos<-as.character(pos$V1)

neg<-as.character(neg$V1)


# 2B i: Variable creation

# Create dict
mydict <- dictionary(list(pos=pos, neg=neg))


# Pre-process
mydfm<-dfm(samp$Text, removePunct=TRUE, stem=F, dictionary=mydict)

# Calculate sentiment score
samp$Sent<-as.numeric(mydfm[,'pos'])-as.numeric(mydfm[,'neg'])

# Calculate binary vector
samp$sent_pos <- as.numeric(samp$Sent >= 0)

# 2B ii: Histogram and Positive Sentiment Score
hist(samp$Sent)

percent_positive <- mean(samp$sent_pos)*100

# Find number of positive sentiment reviews
pos_sent<-length(which(samp$sent_pos==TRUE))

# Confusion matrix
tab <- table(samp$positive, samp$sent_pos)

acc1<- sum(diag(tab))/sum(tab) # (TP + TN) / (TP + FP + TN + FN)
recall1<-tab[2,2]/sum(tab[2,]) # TP / (TP + FN)
precision1<-tab[2,2]/sum(tab[,2]) # TP / (TP + FP)

# What does that tell us about our sentiment model?


# 2B iii: Rank Sum
# generate ranks

samp$rank_scores<-rank(samp$Score, ties.method="average")  

samp$rank_sent<-rank(samp$Sent, ties.method="average")  

rank_gap_sen<-sum(abs(samp$rank_scores - samp$rank_sent))

# 2C Naive Bayes

# 2C i


NB_reviews<-samp

label_NB<-factor(NB_reviews$positive)

label_NB[1:(0.8*length(label_NB))]<-NA

true_label<-factor(NB_reviews$positive)

NBdfm<-dfm(NB_reviews$Text, stem=T,  removePunct=T, remove=stopwords("english"))

NBmodel <- textmodel_NB(NBdfm, label_NB,   smooth=1, priors="uniform") 

# Predict: Uniform Priors
NBpredict <- predict(NBmodel, newdata = NBdfm[is.na(label_NB),])
tab2 <- table(true_label[is.na(label_NB)], as.numeric(NBpredict$posterior.prob[,1] > NBpredict$posterior.prob[,2]))

acc2<- sum(diag(tab2))/sum(tab2) # (TP + TN) / (TP + FP + TN + FN)
recall2<-tab2[2,2]/sum(tab2[2,]) # TP / (TP + FN)
precision2<-tab2[2,2]/sum(tab2[,2]) # TP / (TP + FP)

# 2C ii: With docfreq

NBmodel <- textmodel_NB(NBdfm, label_NB,   smooth=1, priors="docfreq") 

# Predict: Docfreq Priors
NBpredict <- predict(NBmodel, newdata = NBdfm[is.na(label_NB),])
tab3 <- table(true_label[is.na(label_NB)], as.numeric(NBpredict$posterior.prob[,1] > NBpredict$posterior.prob[,2]))

acc3<- sum(diag(tab3))/sum(tab3) # (TP + TN) / (TP + FP + TN + FN)
recall3<-tab3[2,2]/sum(tab3[2,]) # TP / (TP + FN)
precision3<-tab3[2,2]/sum(tab3[,2]) # TP / (TP + FP)


# 2D: Wordscores!

# 2D i: 

anchors <- samp %>% filter(anchor_positive == T | anchor_negative == T)

# Lets try with and without smoothing

# Without smoothing

wsdfm1 <- dfm(anchors$Text, stem = T, remove = stopwords("english")) # Why do you think I left in punctuation?
wsdfm2 <- dfm(samp$Text, stem = T, remove = stopwords("english"))

wordscore1 <- textmodel_wordscores(wsdfm1, ifelse(anchors$anchor_positive == T, 1, -1))

hist(wordscore1@Sw)
sort(wordscore1@Sw, decreasing = T)[1:20]
sort(wordscore1@Sw, decreasing = F)[1:20]

# With smoothing
wordscore2 <- textmodel_wordscores(dfm(anchors$Text, stem = T, remove = stopwords("english")), ifelse(anchors$anchor_positive == T, 1, -1), smooth = 1)

hist(wordscore2@Sw)
sort(wordscore2@Sw, decreasing = T)[1:20]
sort(wordscore2@Sw, decreasing = F)[1:20]

wspredict <- predict(wordscore1, newdata = wsdfm2)

samp$rank_ws <- rank(wspredict@textscores$textscore_raw, ties.method = "average")

rank_gap_ws <- sum(abs(samp$rank_scores - samp$rank_ws))


# Which did better?

rank_gap_ws > rank_gap_sen


# Why might we use sentiment instead of word scores?


# 2E: SVM

# What advantage might SVM or Naive Bayes have over the dictionary approach or wordscores?

# Let's train the linear SVM model
SVM_reviews<-samp[1:1000,]

dtm <- create_matrix(SVM_reviews$Text, language="english", stemWords = FALSE, removePunctuation = FALSE, removeStopwords=TRUE)

break_accuracy_lin<-vector()

for(i in 1:9){
  training_break <- as.integer(0.1*i*nrow(SVM_reviews))
  container      <- create_container(dtm, true_label, trainSize=1:training_break,
                                     testSize=(training_break+1):nrow(SVM_reviews), virgin=FALSE)
  cv.svm <- cross_validate(container, nfold=5, algorithm = 'SVM', kernel = 'linear')
  break_accuracy_lin[i]<-cv.svm$meanAccuracy
}

plot(break_accuracy_lin)
mean(break_accuracy_lin)

# Let's train the model --with radial kernel
break_accuracy_rad<-vector()

for(i in 1:9){
  training_break <- as.integer(0.1*i*nrow(SVM_reviews))
  container      <- create_container(dtm, true_label, trainSize=1:training_break,
                                     testSize=(training_break+1):nrow(SVM_reviews), virgin=FALSE)
  cv.svm <- cross_validate(container, nfold=5, algorithm = 'SVM', kernel = 'radial')
  break_accuracy_rad[i]<-cv.svm$meanAccuracy
}

plot(break_accuracy_rad)
mean(break_accuracy_rad)

# Which SVM model did better?


## Problem 3

# 3A: Nationality

trust<-read.csv("https://raw.githubusercontent.com/pchest/Text_as_Data/master/HW2data/CF_rate_trustworthiness.csv",
                header = T)

reg1<-lm(rating ~ X_country, data=trust) # Testing for a relationship between rating and country of origin

summary(reg1)

# 3B: Demographics

trust$demog <- as.factor(gsub("*[0-9]", "", as.character(trust$image_name)))

reg2<-lm(rating ~ demog, data=trust) # Testing a for a relationship between rating and demographics

summary(reg2)

# 3C: Gender

trust$gender <- grepl("*woman$", trust$demog)

reg3<-lm(rating ~ gender, data=trust) # Testing a for a relationship between rating and gender

summary(reg3)

# What does this tell us about possible challenges associated with using human coders?

# Gold standard questions?