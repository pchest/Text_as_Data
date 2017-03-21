# TA: Patrick Chester
# Course: Text as Data
# Date: 3/21/2017
# Recitation 8: Homework 1 Review

# Loading libraries

library(readtext)
library(quanteda)
library(quantedaData)
library(ggplot2)
library(dplyr)
library(lubridate)
library(foreach)
library(parallel)
library(doParallel)

TTR <- function(x){
  require(quanteda)
  ntype(x)/lengths(x)
}

## Code for Question 1:


data("data_corpus_inaugural")
b2001 <- data_corpus_inaugural$documents$texts[54]
o2009 <- data_corpus_inaugural$documents$texts[56]

speeches<-c(b2001, o2009)

summary(data_corpus_inaugural[c(54,56),])

## raw

# 1a - Calculate TTR

tok<-tokenize(speeches, removePunct = T) 

TTR(tok) # Bush is first, Obama is second


# 1b - Create DFM

s_dfm <- dfm(speeches, removePunct = T, tolower = F)


# 1c - Calculate Cosine Similarity

sim <- textstat_simil(s_dfm, margin = "documents",method = "cosine")

as.matrix(sim)


# 1di - Calculating TTR & Similarity w/ Stemming

## Processing
tok <- tokenize(speeches, removePunct = T) 

stems <- tokens_wordstem(tok)

## TTR
TTR(stems) # Bush is first, Obama is second

## Similarity
s_dfm <- dfm(speeches, removePunct = T, tolower = F, stem = T)

textstat_simil(s_dfm, margin = "documents", method = "cosine")

as.matrix(sim)


# 1dii - Calculating TTR & Similarity w/o Stopwords 

## Processing
tokens<-tokenize(speeches, removePunct=TRUE) 

no_stop<-tokens_remove(tokens, features = stopwords("english")) 

## TTR
TTR(no_stop)

## Similarity
s_dfm<-dfm(speeches, removePunct = T, tolower = F, remove = stopwords("english"))
sim <- similarity(s_dfm, margin = "documents", method = "cosine")

as.matrix(sim)

# 1diii - Calculating TTR & Similarity w/ all lowercase 

## Processing
tokens<-tokenize(speeches, removePunct=TRUE) 

lowers<-toLower(tokens)

## TTR
TTR(lowers)

## Similarity
s_dfm<-dfm(speeches, removePunct = T, tolower= T)
sim <- similarity(s_dfm, margin = "documents", method = "cosine")

as.matrix(sim)

# 1diV - TF-IDF

tfidf_dfm<-dfm_weight(dfm(speeches, removePunct = T), type = "tfidf")
sim <- similarity(tfidf_dfm, margin = "documents", method = "cosine")

as.matrix(sim)


# 1e - MLTD 


mltd_homebrew<-function(text, ttr = 0.72, returnvalues = F){
  require(quanteda)
  keeps<-list()
  dat<-tokenize(text,what = "word",removePunct = T) #Tokenizes the speeches
  dat<-toLower(dat) #Changes the text to lower case
  for(i in 1:length(text)){
    keeps[[i]]<-NA #Creates a vector for each speech
    temp<-list()
    type<-0
    t<-0
    for(k in 1:length(dat[[i]])){
      type<-ifelse(dat[[i]][k] %in% unlist(temp),yes = type+0, no = type+1) #Token type counter: Adds one if the new word doesn't match an old word
      temp[k]<-unlist(dat[[i]][k]) #Adds the word to a list of tokens
      if(type/length(unlist(temp))<ttr){
        t<-t+1 #Keeper counter: designed to increase as the TTR is reached
        keeps[[i]][t]<-length(unlist(temp)) #Stores a count of the number of words passed before the TTR was hit
        temp<-list() #Clears the token list
        type<-0 #Clears the type counter
      }
    }
  }
  ifelse(returnvalues == T,
         final<-list(data = keeps, 
                     means = lapply(keeps, function(x) mean(x))), #Returns lists of the means and the word counts
         final<-list(means = lapply(keeps, function(x) mean(x))) #Returns a list of the mean MLTD by document
  )
  return(final)
}

# Bush MLTD

mltd_homebrew(b2001,returnvalues = F)

# Obama MLTD

mltd_homebrew(o2009,returnvalues = F)


## Code for Question 2:

vecs<-data.frame(terms=c("whenever", "you", "find", "are", "on", "the", "side", "of", "majority", "it", "is", "time", "to", "pause", "and", "reflect",
                         "a", "jury", "consists", "twelve", "people", "who", "determine", "which", "client", "has", "better", "lawyer"),
                 one = c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 two = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

norm_vec <- function(x) sqrt(sum(x^2))

cos<-vecs$one %*% vecs$two / (norm_vec(vecs$one)*norm_vec(vecs$two))

euc_dist<-sqrt(sum(sapply(vecs$one-vecs$two, function(x) x^2)))

man_dist<-sum(sapply(vecs$one-vecs$two, function(x) abs(x)))


# Code for Questions 3 and 4:

####################
# Code from Arthur #
####################


## Code for Question 3: BASIC STYLOMETRICS

setwd("C:/Users/patro/Dropbox/Text_As_Data_Spring2017/data")

#some texts by Austen
austen <- corpus(readtext("austen_texts/*.txt", docvarsfrom=c("filenames")) )

#some texts by Dickens
dickens <- corpus(readtext("dickens_texts/*.txt",  docvarsfrom=c("filenames")) )

#a mystery text
mystery <- corpus(readtext("mystery/*.txt" ))

#let's look at some key function words
# and make the DTMs with those in mind
# (from Peng and Hengartner)

func.words <- c('the', 'may', 'it', 'was', 'which', 'not', 'be', 'upon')

#create dfm, and make sure colnames are in same order
all.together <- c(austen, dickens, mystery)
all.dfm <- dfm(all.together, select=func.words)

#let's get 'average'rates for each author

# Method 1
austen.rates <- colMeans(all.dfm[1:4,]/rowSums(all.dfm[1:4,]))
dickens.rates <- colMeans(all.dfm[5:8, ]/rowSums(all.dfm[5:8,]))
mystery.rates <- colMeans(all.dfm[9,]/rowSums(all.dfm[9,]))

# Method 2
test <- dfm_weight(all.dfm, "relFreq")
aggregate(test, by = list(sub("\\_.*","", docnames(test))), FUN = mean)


#then look at abs difference of mystery author from these two

cat("Austen difference\n")
print(round(abs(mystery.rates-austen.rates), d=3))
names(mystery.rates)

cat("\nDickens difference\n")
print(round(abs(mystery.rates-dickens.rates), d=3))


# Plotted results
df <- data.frame(Authors = rep(c("Austen", "Dickens"), each = 8), 
                 Words = rep(names(mystery.rates), 2), 
                 Diff = c(abs(mystery.rates-austen.rates), abs(mystery.rates-dickens.rates)))

ggplot(aes(x = as.factor(Words), y = Diff, fill = as.factor(Authors)), data = df) + 
  geom_bar(stat = "identity", position = position_dodge()) + theme_bw() + xlab("") + ylab("Difference with Mystery Text")

# Who looks like most plausible author?


## Code for question 4:


#Heap's Law

#      M = kT^b

# M = vocab size
# T = number of tokens
# k, b are constants

# If no mystery

austen_dick_dfm <- dfm(c(austen, dickens),tolower = T,removePunct = T)

austen_texts <- gsub("\n", "", austen$documents$texts)
dickens_texts <- gsub("\n", "", dickens$documents$texts)

tokens<-tokens(x = c(austen_texts,dickens_texts), what = "word",removePunct = T) 

Tee<-sum(lengths(tokens))

M<-nfeature(austen_dick_dfm)

k<-44

b<-log(M/k)/log(Tee)

# Zipf's law

setwd("E:/Documents/Word saves/NYU/NYU Classes/2017 - Spring/Text as Data - TA/Homeworks")

pdf("Q_4.pdf", 7, 5)

plot(log10(1:100), log10(topfeatures(austen_dick_dfm, 100)),
     xlab="log10(rank)", ylab="log10(frequency)", main="Top 100 Words")
# regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(austen_dick_dfm, 100)) ~ log10(1:100))
abline(regression, col="red")

dev.off()

confint(regression)




## FRE bootstrap: Question 6

# 6a
data("data_corpus_SOTU")

# Removing Cleveland
SOTU <- corpus_subset(data_corpus_SOTU, President != "Cleveland")

# Identifying Adams
conditions <- docvars(SOTU)[,"Date"] > as.Date("1801-01-01") & 
  docvars(SOTU)[,"President"] == "Adams"

# Fixing JQA's name
docvars(SOTU)[conditions,"FirstName"] <- "John Quincy"
docvars(SOTU)[,"Year"] <- year(docvars(SOTU)[,"Date"])

docvars(SOTU)[,"FirstLastName"] <- interaction(docvars(SOTU)[,"FirstName"], docvars(SOTU)[,"President"],sep = " ")
SOTU_sent <- corpus_reshape(SOTU, to = "sentences")

# Fancy parallel bootstrap function

results <- list()

# Sets aside cores of your processer for R to use
cl <- makeCluster(3)

registerDoParallel(cl)

# Loads libraries and data into Parallel environment
clusterEvalQ(cl,library(dplyr))

clusterEvalQ(cl,library(quanteda))

clusterExport(cl, "SOTU_sent")

# Bootstrap function
system.time(results <- foreach(i = 1:75, .combine = rbind) %dopar% {
  samp <- corpus_sample(SOTU_sent,2000)
  FRE <- textstat_readability(samp, measure = "Flesch")
  data.frame(President = docvars(samp)[,"FirstLastName"],
             Year = docvars(samp)[,"Year"],
             Flesch = FRE,
             Sim = i) %>%
    group_by(President) %>% 
    summarise(FRE = mean(Flesch), 
              Year = max(Year),
              Sim = mean(Sim))
})

# Frees up your computer's processor cores for other uses
stopCluster(cl)

# Standard error function
std <- function(x) sd(x)/sqrt(length(x))

# Calculating the mean and standard error of the FRE
results_summed <- results %>% 
  group_by(President) %>% 
  summarise(FRE_mean = mean(FRE), # Calculates the average FRE score
            FRE_sd = std(FRE), # Calculates Standard Error for the FRE score
            Year = max(Year)) %>% 
  arrange(Year) %>% # Orders data by Year
  mutate(
    FRE_025 = FRE_mean - 2 * FRE_sd, # Creates the low end of the 95% confidence interval
    FRE_975 = FRE_mean + 2*FRE_sd, # Creates the high end of the 95% confidence interval
    President = reorder(President, Year) # Sorts President names by the year that they were President
  )

# Estimating the FRE directly from the text
direct_results <- data.frame(
  President = docvars(SOTU_sent)[,"FirstLastName"],
  FRE = textstat_readability(SOTU_sent, measure = "Flesch"),
  Year = docvars(SOTU_sent)[,"Year"]
) %>% group_by(President) %>%
  summarise(FRE = mean(FRE), 
            Year = max(Year)) %>% 
  arrange(Year)


# Graphing estimated and base FRE scores by President with CI
results_summed$FRE_direct <- direct_results$FRE

limits <- aes(ymax = results_summed$FRE_975, ymin=results_summed$FRE_025)

pdf("presidents.pdf",width = 12, height = 7)

ggplot(aes(x = President), data = results_summed) +
  geom_point(aes(y = FRE_mean), size = 2, color = "blue") +
  geom_errorbar(limits, width=.5) + 
  geom_point(aes(y = FRE_direct), size = 1, color = "red") + 
  coord_flip() + 
  theme_bw() +
  ylab("FRE Mean") + 
  xlab("")

dev.off()

# 6b
results_summed <- mutate(results_summed,
  FRE_in = ifelse(FRE_direct > FRE_025 & FRE_direct < FRE_975, 1, 0) # Assigns 1 if score falls within 95% confidence interval, 0 otherwise
) 

# Percentage of base FRE scores contained within 95% confidence intervals
mean(results_summed$FRE_in)*100

# 6C
base_DC <- textstat_readability(SOTU_sent, measure = "Dale.Chall")
direct_DC <- data.frame(
  President = docvars(SOTU_sent)[,"FirstLastName"],
  DC = base_DC,
  Year = docvars(SOTU_sent)[,"Year"]
) %>% group_by(President) %>%
  summarise(DC = mean(DC), 
            Year = max(Year)) %>% 
  arrange(Year)

cor(direct_DC$DC, direct_results$FRE)

