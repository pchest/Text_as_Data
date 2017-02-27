# TA: Patrick Chester
# Course: Text as Data
# Date: 2/21/2017
# Recitation 6: Supervised Learning II

# Load Quanteda

library(quanteda)
library(quantedaData)


## 1 Supervised Learning: Naive Bayes

# Example: Replication of 13.1 from IIR textbook

trainingset <- matrix(0,ncol=6,nrow=5)
trainingset[1,] <- c(1, 2, 0, 0, 0, 0)
trainingset[2,] <- c(0, 2, 0, 0, 1, 0)
trainingset[3,] <- c(0, 1, 0, 1, 0, 0)
trainingset[4,] <- c(0, 1, 1, 0, 0, 1)
trainingset[5,] <- c(0, 3, 1, 0, 0, 1)
colnames(trainingset) <- c("Beijing", "Chinese",  "Japan", "Macao", "Shanghai", "Tokyo")
rownames(trainingset) <- paste("d", 1:5, sep="")
trainingset <- as.dfm(trainingset)
trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered=TRUE)


# replicate IIR p261 prediction for test set (document 5)
nb.p261 <- textmodel_NB(x=trainingset, y=trainingclass, data=NULL,
                        smooth=1, prior="docfreq") # Smooth gives values of 1 for new words; NB wouldn't work very well


pr.p261 <- predict(nb.p261)
pr.p261


# 2 Classification using Word Scores


# Read in conservative and labour manifestos

setwd("E:/Documents/Word saves/NYU/NYU Classes/2016 - Spring/Text as Data/TAD Labs/Text_as_Data-master/cons_lab")

files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))

head(files)

# Name data

files<-unlist(files)
files<-gsub("./", "", files )
files<-gsub(".txt", "", files )

# Create metadata

year<-unlist(strsplit(files, "[^0-9]+"))

year<-year[year!=""]

party<-unlist(strsplit(files, "[^A-z]+"))

party<-party[party!="a" & party!="b"]

# Testing for equal length

all.equal(length(year),length(party))

# Create data frame

man_df<-data.frame(year = as.numeric(year),
                   party = party,
                   stringsAsFactors = TRUE)
man_df$text<-text

# Identifying test speech: Labor

test_speech<-man_df[46,]

# Setting training speeches: The remaining 45 Labor and Conservative speeches

training<-man_df[1:45,]

# Create DFMs

lab_con_dfm<-dfm(training$text)

test_dfm<-dfm(test_speech$text)

# Train Word Score model

ws_base<-textmodel(lab_con_dfm, 
                   y = 2*(as.numeric(training$party)-1)-1, # Y variable must be coded on a binary x in {-1,1} scale
                   model="wordscores")

# Look at strongest features

lab_features<-sort(ws_base@Sw, decreasing=TRUE)

lab_features[1:10]

con_features<-sort(ws_base@Sw, decreasing=FALSE)

con_features[1:10]

ws_base@Sw[c("drugs", "minorities", "unemployment")]

# Trying it again with smoothing

ws_smooth<-textmodel(lab_con_dfm, 
                     y = 2*(as.numeric(training$party)-1)-1, 
                     model="wordscores", smooth=1)

ws_smooth@Sw[c("drugs", "minorities", "unemployment")] # Smoothing is giving stronger priors, decreasing the impact of new information

plot(ws_base@Sw, ws_smooth@Sw, xlim=c(-1, 1),
     xlab="No Smooth", ylab="Smooth")



## predict that last speech
predict(ws_base, newdata = test_dfm,
        rescaling = "none", level = 0.95, verbose = TRUE) 


predict(ws_base, newdata = test_dfm,
        rescaling = "lbg", level = 0.95, verbose = TRUE)

## 3 Applying Naive Bayes and Word Scores to Amicus texts from Evans et al

# Loading data

require(quantedaData)

data(amicusCorpus)
summary(amicusCorpus)

amDfm <- dfm(amicusCorpus)


amNBmodel <- textmodel(amDfm, docvars(amicusCorpus, "trainclass"), model="NB", smooth=1) 
print(amNBmodel, 10)
amNBpredict <- predict(amNBmodel)

# "confusion matrix": NB
tab_NB <- table(amNBpredict$nb.predicted, docvars(amicusCorpus, "testclass"))

# Accuracy: NB
(tab_NB[1,1]+tab_NB[2,2])/sum(tab_NB)

reference <- c(1, 1, -1, -1, rep(NA, 98))
amWSmodel <- textmodel(amDfm, reference, model="wordscores", smooth=1)
plot(amWSmodel@Sw, c(1, -1) %*% amNBmodel$PcGw, xlab="Wordscore", ylab="Linear Posterior Class Pr. Diff")
(amWSpredict <- predict(amWSmodel))
amWSresults <- ifelse(amWSpredict@textscores[,1] > 0, "P", "R")

# "confusion matrix": WS
tab_WS <- table(amWSresults, docvars(amicusCorpus, "testclass"))

# Accuracy: WS
(tab_WS[1,1]+tab_WS[2,2])/sum(tab_WS)


# Plot differences between wordscore and NB class prediction

plot(jitter(amWSmodel@Sw,20), 
     jitter(amNBmodel$Pw@x,20),
     pch=19, cex=.6, main="(a) Word level", col="grey70",
     xlab="Wordscores", ylab="NB")

plot(jitter(amWSpredict@textscores[-c(1,2),1],20), 
     jitter(amNBpredict$posterior.prob[-c(1,2),1],20),
     pch=19, cex=.6, main="(b) Document level",
     col=ifelse(docvars(amicusCorpus, "testclass")=="AP", "blue", "red"),
     xlab="Wordscores", ylab="NB")
abline(v=0, lty="dashed", col="grey80")
abline(h=0, lty="dashed", col="grey80")

