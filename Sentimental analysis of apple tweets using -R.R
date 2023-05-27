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
library(wordcloud)

# Function to perform sentiment analysis on tweets
perform_sentiment_analysis <- function(data) {
  # Convert tweets to UTF-8 encoding
  tweets <- iconv(data$text)
  
  # Perform sentiment analysis
  sentiment_scores <- get_nrc_sentiment(tweets)
  
  # Return the sentiment scores
  return(sentiment_scores)
}

# Load the Apple dataset
apple <- read.csv("your_dataset.csv")

# Perform sentiment analysis
sentiment_scores <- perform_sentiment_analysis(apple)

# Display the sentiment scores
head(sentiment_scores)

# Example: Get sentiment score for a specific tweet
# Example: Get sentiment score for a specific tweet
tweet <- apple$text[4]
score <- get_nrc_sentiment(tweet)
cat("Sentiment score for the tweet:\n")
print(score)
cat("\n")

# Create a data frame with tweets
tweets_df <- data.frame(text = tweets)

# Remove rows with missing values
tweets_df <- tweets_df[complete.cases(tweets_df), ]

# Install the data.table package if not already installed
if (!require(data.table)) {
  install.packages("data.table")
}

# Load the data.table library
library(data.table)
tweets_dt <- as.data.table(tweets_df)
str(tweets_df)
colnames(tweets_df)
str(tweets_df)


clean_text <- gsub("RT\\s|@[\\w_]+|https?[^\\s]+", "", tweets_df)
clean_text <- gsub("[^[:alnum:]\\s]", "", clean_text)

tweets_text <- as.character(clean_text)
tweets_text <- trimws(tweets_text)

word_freq <- table(tweets_text)
print(word_freq

wordcloud(names(word_freq), freq = word_freq, random.order = FALSE)
sorted_word_freq <- sort(word_freq, decreasing = TRUE)

# Select the top 20 most frequent words
top_words <- names(sorted_word_freq)[1:20]
top_freq <- sorted_word_freq[1:20]

# Create a bar plot of the top word frequencies
barplot(top_freq, names.arg = top_words, horiz = TRUE, 
        main = "Top 20 Word Frequencies",
        xlab = "Frequency", ylab = "Words")




# Sentiment over time: Analysis of sentiment trends over a specified time period
apple$created <- as.POSIXct(apple$created, format = "%d-%m-%Y %H:%M")
sentiment_over_time <- aggregate(sentiment_scores, by = list(date = as.Date(apple$created)), FUN = sum)
sentiment_over_time$date <- as.Date(sentiment_over_time$date)
ggplot(sentiment_over_time, aes(x = date, y = joy, group = 1)) +
  geom_line() +
  labs(x = "Date", y = "Joy Sentiment Score", title = "Sentiment Over Time (Joy)")



# Sentiment polarity: Positive, negative, or neutral sentiment for each tweet
sentiment_polarity <- ifelse(sentiment_scores$Positive > sentiment_scores$Negative, "Positive",
                             ifelse(sentiment_scores$Negative > sentiment_scores$Positive, "Negative", "Neutral"))
sentiment_polarity <- data.frame(apple, Sentiment_Polarity = sentiment_polarity)
head(sentiment_polarity)

# Additional analysis and visualizations can be added here
