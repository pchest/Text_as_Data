# TA: Patrick Chester
# Course: Text as Data
# Date: 4/3/2017
# Recitation 11: Unsupervised Learning III

rm(list = ls())

## 1 Running LDA


# Make sure you have the appropriate packages installed

install.packages("tidytext")
install.packages("topicmodels")
install.packages("ldatuning")
install.packages("stringi")

libs <- c("ldatuning","topicmodels","ggplot2","dplyr","rjson","quanteda","lubridate","parallel","doParallel","tidytext")
lapply(libs, library, character.only = T)
rm(libs)

# First, you need to go to my github and download the data

# Save the two folders to your desktop


setwd("E:/Documents/Data/HPC")

# Setting seed
set.seed(2017)

#blm_tweets <- read.csv("eg_samples.csv", stringsAsFactors = F) %>% sample_n(10000)
write.csv(blm_tweets, "blm_samp.csv")

blm_tweets <- read.csv("blm_samp.csv", stringsAsFactors = F)

## 1 Preprocessing


# Creates a more managable date vector
blm_tweets$date <- as.POSIXct(strptime(blm_tweets$created_at, "%a %b %d %T %z %Y",tz = "GMT"))
blm_tweets$date2 <- mdy(paste(month(blm_tweets$date), day(blm_tweets$date), year(blm_tweets$date), sep = "-"))

# Collapse tweets so we are looking at the total tweets at the day level
blm_tweets_sum <- blm_tweets %>% group_by(date2) %>% summarise(text = paste(text, collapse = " "))

# Remove non ASCII characters
blm_tweets_sum$text2 <- stringi::stri_trans_general(blm_tweets_sum$text, "latin-ascii")

# Removes solitary letters
blm_tweets_sum$text3 <- gsub(" [A-z] ", " ", blm_tweets_sum$text2)

# Create DFM
mat <-dfm(blm_tweets_sum$text3, stem=F, removePunct = T, tolower=T,removeTwitter = T, removeNumbers = T,
          remove = c(stopwords(kind="english"), "http","https","rt", "t.co"))

## 2 Selecting K

# Identify an appropriate number of topics (FYI, this function takes a while)
result <- FindTopicsNumber(
  mat,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = 3L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# What should you consider when choosing the number of topics you use in a topic model?

# What does robustness mean here?

# About 16-19 topics

## 3 Visualizing Word weights

# Set number of topics
k <-19

# Run the topic model (this may also take a while)
TM<-LDA(mat, k = k, method = "Gibbs",  control = list(seed = 2017)) # Keep in mind that in "control" you can set the LDA parameters

# Quickly extracts the word weights and transforms them into a data frame
blm_topics <- tidy(TM, matrix = "beta") 

# Generates a df of top terms
blm_top_terms <- blm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Creates a plot of the weights and terms by topic
blm_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

## Relevant topics: 
# 1  - Sandra Bland 
# 10 - Eric Garner
# 16 - Freddie Gray 

## 4 Visualizing topic trends over time

# Store the results of the distribution of topics over documents
doc_topics<-TM@gamma

# Store the results of words over topics
words_topics<-TM@beta

# Transpose the data so that the days are columns
doc_topics <- t(doc_topics)

# Arrange topics
max<-apply(doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2<-function(x){
  which(x == sort(x,partial=(k-1))[k-1])
  
}

max2<- apply(doc_topics, 2, which.max2)
max2<-sapply(max2, max)

# Coding police shooting events
victim <- c("Freddie Gray", "Sandra Bland")
shootings <- mdy(c("04/12/2015","7/13/2015"))

# Combine data
index<-seq(1:nrow())
top2<-data.frame(max = max, max2 = max2, 
                 index = index, date = ymd(blm_tweets_sum$date2))

# Plot
z<-ggplot(top2, aes(x=date, y=max, pch="First")) 

z + geom_point(aes(x=date, y=max2, pch="Second") ) +theme_bw() + 
  ylab("Topic Number") + ggtitle("BLM-Related Tweets from 2014 to 2016 over Topics") + geom_point() + xlab(NULL) + 
  geom_vline(xintercept=as.numeric(shootings[1]), color = "blue", linetype=4) + # Freddie Gray (Topic)
  geom_vline(xintercept=as.numeric(shootings[2]), color = "black", linetype=4)  + # Sandra Bland
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 

# Thanks guys!
