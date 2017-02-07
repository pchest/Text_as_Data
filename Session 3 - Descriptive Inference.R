# TA: Patrick Chester
# Course: Text as Data
# Date: 2/7/2017


## 1 Setting up Quanteda 

# Clear Global Environment
rm(list = ls())

# Libraries
library(quanteda)
library(quantedaData)


## 2 Demonstrate Heap's law

#     M = kT^b

# M = vocab size
# T = number of tokens
# k, b are constants

tokens <- tokenize(inaugCorpus, removePunct=TRUE) 
Tee <- lengths(tokens)
Tee <- sum(Tee)

mydfm <- dfm(inaugCorpus)

M <- nfeature(mydfm)

# Let's check using parameter values from MRS for a corpus with more than 100,000 tokens

k <- 44
b <- .49

k * (Tee)^b

# Let's think about why


## 3 Demonstrate Zipf's law

plot(log10(1:100), log10(topfeatures(mydfm, 100)),
     xlab="log10(rank)", ylab="log10(frequency)", main="Top 100 Words")

# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mydfm, 100)) ~ log10(1:100))

# Adds the fitted line from regression to the plot
abline(regression, col="red")

# Returns the 95% confidence intervals for the regression coefficients
confint(regression)

# Provides R-squared, F-test, and cofficient estimates from regression
summary(regression)


## 4 Stopwords: do they matter?

mydfm <- dfm(inaugCorpus, remove=stopwords("english"))

plot(log10(1:100), log10(topfeatures(mydfm, 100)),
     xlab="log10(rank)", ylab="log10(frequency)", main="Top 100 Words")

# Regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mydfm, 100)) ~ log10(1:100))
abline(regression, col="red")
confint(regression)


## 5 Key Words In Context (KWIC) is a good way to summarize info about a topic

kwic(inaugCorpus, "terror", 3)

help(kwic)

# Suggested terms?


## 6 Measuring similarity

# This helps illustrate the value of the vector representation

# Cosine similarity--take the dot product of two vectors

# x * y = |x||y|cos
# cos = x*y/|x||y|

x <- c(1,2,3)
y <- c(1,2,3)
 
# Define the norm function

norm_vec <- function(x) sqrt(sum(x^2))

x %*% y / (norm_vec(x)*norm_vec(y))

# What if they're different

a <- c(1,2,3)
b <- c(1,2,4000)

a %*% b / (norm_vec(a)*norm_vec(b))

# Let's do it with texts

last_speech_text <- inaugCorpus[ndoc(inaugCorpus)]
first_speech_text <- inaugCorpus[1]

# Make a dfm of these two

inaug_dfm <- dfm(c(last_speech_text, first_speech_text), ignoredFeatures = stopwords("english"), stem = TRUE)

# Calculate similarity

tmp <- similarity(inaug_dfm, margin = "documents")

as.matrix(tmp)

# Let's see how stopwords/stemming affect this

inaug_dfm <- dfm(c(last_speech_text, first_speech_text))

# Calculate similarity

tmp <- similarity(inaug_dfm, margin = "documents")

as.matrix(tmp)

# Make a dfm of a bunch

inaug_dfm <- dfm(corpus_subset(inaugCorpus , Year > 1980), remove = stopwords("english"), stem = TRUE)

# Calculate similarity

tmp <- similarity(inaug_dfm, margin = "documents")

as.matrix(tmp)

# Specific comparisons

similarity(inaug_dfm, "2009-Obama", n = 5, margin = "documents")
