#install required packages
install.packages("tm")
install.packages("SnowballC")
install.packages("syuzhet")
install.packages("tidyverse")
install.packages("googlesheets4")

#load required packages
library(tm)
library(SnowballC)
library(syuzhet)
library(tidyverse)
library(googlesheets4)

#Reading in data

#load in csv file 
data = read_sheet("https://docs.google.com/spreadsheets/d/1QSYjGLoLkpMPeYvp56fuZegqib6t7IVgEnW_P1bVHCc/edit?usp=share_link")
#check structure of csv file
str(data)

#Creating corpus

#convert review column of dataframe to character vector
corpus = iconv(data$Review)
#create corpus from character vector above
corpus = Corpus(VectorSource(corpus))
#inspect first five rows
inspect(corpus[1:5])

#Cleaning corpus data

#convert the text to lower case
cleaned_corpus = tm_map(corpus, tolower)
#inspect first five rows
inspect(cleaned_corpus[1:5])
#remove punctuation
cleaned_corpus = tm_map(corpus, removePunctuation)
#inspect first five rows
inspect(cleaned_corpus[1:5])
#remove numbers
cleaned_corpus = tm_map(corpus, removeNumbers)
#inspect first five rows
inspect(cleaned_corpus[1:5])
#remove common english stopwords (the, is, at, on, etc.)
cleaned_corpus = tm_map(corpus, removeWords, stopwords('english'))
#inspect first five rows
inspect(cleaned_corpus[1:5])
#remove extra whitespaces
cleaned_corpus = tm_map(corpus, stripWhitespace)
#inspect first five rows
inspect(cleaned_corpus[1:5])
#reduce words to their root form (such as couldn't, wouldn't, shouldn't to just n't)
cleaned_corpus = tm_map(corpus, stemDocument)
#inspect first five rows
inspect(cleaned_corpus[1:5])

#Creating term document matrix(tdm)

#build term document matrix (frequency of words)
tdm = TermDocumentMatrix(cleaned_corpus)
#convert above list to matrix
tdm_m = as.matrix(tdm)
#show frequency of ten terms across 20 documents (reviews)
tdm_m[1:10, 1:20]
#sort by frequency of words in descending order
tdm_s = sort(rowSums(tdm_m), decreasing = TRUE)
#convert numeric vector to dataframe
tdm_d = data.frame(word = names(tdm_s), freq = tdm_s)
#show top 5 most frequent words
head(tdm_d, 5)

#Sentiment Analysis

#convert review column of dataframe to character vector
text = iconv(data$Review)

#Generating sentiment scores

#generate sentiment scores (positive, neutral, negative) using syuzhet method (scales vary per method)
syuzhet_vector = get_sentiment(text, method = "syuzhet")
#see first row of vector
head(syuzhet_vector)
#see summary statistics of vector
summary(syuzhet_vector)

#generate sentiment scores (positive, neutral, negative) using bing method
bing_vector = get_sentiment(text, method="bing")
#see first row of vector
head(bing_vector)
#see summary statistics of vector
summary(bing_vector)

#generate sentiment scores (positive, neutral, negative) using afinn method
afinn_vector = get_sentiment(text, method="afinn")
#see first row of vector
head(afinn_vector)
#see summary statistics of vector
summary(afinn_vector)

#compare first row of each vector (sign function creates common scale)
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

#Emotion Classification

#returns dataframe in which each row represents a review, while the columns represent eight emotions along with positive and negative sentiment
nrc = get_nrc_sentiment(text)
#view first 10 rows
head(nrc, 10)

#transpose dataframe above
nrct = data.frame(t(nrc))

#compute column sums for each row based on grouping (summing all occurrences of anger for example)
nrcs = data.frame(rowSums(nrct))

#make row names equal sentiment names
nrcs = cbind("sentiment" = rownames(nrcs), nrcs)
#make row names null 
rownames(nrcs) = NULL
#change column names
names(nrcs)[1] = "sentiment"
names(nrcs)[2] = "frequency"
#show first 5 rows
head(nrcs)
#create percent of total column
nrcs = nrcs %>% 
  mutate(percent = frequency/sum(frequency))

#create indexed version of dataframe above (removing the positive and negative rows)
nrcs2 = nrcs[1:8,]
#show new dataframe
head(nrcs2, 8)
#change column name
colnames(nrcs2)[1] = "emotion"


