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
