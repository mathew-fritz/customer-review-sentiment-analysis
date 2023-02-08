#install required packages
install.packages("tm")
install.packages("SnowballC")
install.packages("ggthemes")
install.packages("wordcloud2")
install.packages("syuzhet")
install.packages("tidyverse")

#load required packages
library(tm)
library(SnowballC)
library(ggplot2)
library(ggthemes)
library(wordcloud2)
library(syuzhet)
library(tidyverse)

#Reading in data

#load in csv file 
data = read.csv("C:\\Users\\Mathew\\Documents\\Sentiment Analysis\\Data\\tripadvisor.csv", header = TRUE)
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

#Data visualization based on tdm

#plot of top 5 most frequent words in reviews
ggplot(tdm_d[1:5,], aes(x = reorder(word, -freq), y = freq, fill = word,
                        label = freq)) +
  geom_bar(stat = "identity") +
  ggtitle("Amount of Times Words Appear in Reviews") +
  xlab("Words") + 
  ylab("Frequency") +
  geom_text(vjust = -0.2,
            color="black",
            size = 5,
            fontface="bold",
  )+
  scale_fill_discrete(breaks=c("hotel", "room", "not", "stay", "great")) +
  theme_minimal()

#wordcloud showing top 100 most popular words (seed is set to reproduce results)
set.seed(1234)
wordcloud2(data = tdm_d[1:100,])

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

#plot showing percentage of positive and negative reviews
nrcs[9:10,] %>% 
ggplot(aes(x = reorder(sentiment, -frequency), y = percent, 
           fill = sentiment, label = scales::percent(percent))) +
  geom_bar(stat = "identity") +
  ggtitle("Percentage of Positive and Negative Reviews") +
  xlab("Sentiments") + 
  ylab("Frequency") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,0.3)) +
  geom_text(nudge_y= .007,
            color="black",
            size = 5,
            fontface="bold",
  )+
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal()

#create indexed version of dataframe above (removing the positive and negative rows)
nrcs2 = nrcs[1:8,]
#show new dataframe
head(nrcs2, 8)
#change column name
colnames(nrcs2)[1] = "emotion"

#Data visualization of emotion classification

#plot the frequency of emotions as a percent based on total (emotion column reordered based on descending frequency)
nrcs2 %>% 
mutate(emotion = fct_reorder(.f = emotion, .x = frequency)) %>% 
ggplot(aes(x = emotion, y = percent, fill = emotion, label = scales::percent(percent))) +
  geom_bar(stat = "identity") +
  ggtitle("Most Popular Emotions") +
  xlab("Emotion") + 
  ylab("Frequency") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(nudge_y= .007,
            color="black",
            size = 5,
            fontface="bold",
            )+
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip()
