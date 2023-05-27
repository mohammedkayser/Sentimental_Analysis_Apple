library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text)
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab='count',
        main='SENTIMENTAL ANALYSIS FOR APPLE TWEETS')

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Perform sentiment analysis
tweets <- iconv(apple$text)
s <- get_nrc_sentiment(tweets)

# View the sentiment analysis results for the first 5 tweets
head(s)

# Sentiment polarity: Positive, negative, or neutral sentiment for each tweet
sentiment_polarity <- ifelse(s$positive > s$negative, "Positive",
                             ifelse(s$positive < s$negative, "Negative", "Neutral"))

# Add sentiment polarity to the data frame
apple$sentiment <- sentiment_polarity

# View the sentiment polarity for the first 5 tweets
head(apple$sentiment)

# Sentiment distribution: Visual representation of the sentiment distribution using bar charts
sentiment_counts <- table(apple$sentiment)
barplot(sentiment_counts,
        main = "Sentiment Distribution",
        xlab = "Sentiment",
        ylab = "Count",
        col = rainbow(length(sentiment_counts)))

# Word cloud: Visualization of the most frequent words in the tweets
library(wordcloud)
wordcloud(tweets,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

text <- apple$text

# Create a list of words from the text
words <- strsplit(text, " ")

# Count the number of times each word appears
word_counts <- table(unlist(words))

# Print the top 10 most common words
head(word_counts, 10)

ggplot(data.frame(word = names(word_counts), count = word_counts), aes(x = word)) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Most Common Words in Tweets about Apple")

wordcloud(words, max.words = 100)
