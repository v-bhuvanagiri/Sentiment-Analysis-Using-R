## IMPORTING NECESSARY LIBRARIES
# Install packages if not already installed
required_packages <- c("tm", "twitteR", "ggplot2", "syuzhet", "tidytext", "wordcloud", "dplyr", "RColorBrewer")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load the libraries
library(tm)
library(twitteR)
library(ggplot2)
library(syuzhet)
library(tidytext)
library(wordcloud)
library(dplyr)
library(RColorBrewer)


## IMPORTING THE TWEETS
# Use forward slashes or double backslashes for Windows file paths
tweet.df <- read.csv("C:/SNU/R/Sentiment Analysis Using R/Human Rights Violation - tweets.csv")

# View the first few tweet texts
head(tweet.df$Text)

# Extract the text column for processing
tweet.df2 <- tweet.df$Text


## CLEANING THE TWEETS

# Remove URLs
tweet.df2 <- gsub('http\\S+\\s*', '', tweet.df2)
tweet.df2 <- gsub('https:\\S+\\s*', '', tweet.df2)
tweet.df2 <- gsub('https\\S+\\s*', '', tweet.df2)

# Remove RT, Hashtags, Mentions, controls, punctuations, numbers, and extra spaces
tweet.df2 <- gsub('\\b+RT', '', tweet.df2)
tweet.df2 <- gsub('#\\S+', '', tweet.df2)
tweet.df2 <- gsub('@\\S+', '', tweet.df2)
tweet.df2 <- gsub('[[:cntrl:]]', '', tweet.df2)
tweet.df2 <- gsub("\\d", '', tweet.df2)
tweet.df2 <- gsub('[[:punct:]]', '', tweet.df2)
tweet.df2 <- gsub("^[[:space:]]*", "", tweet.df2)
tweet.df2 <- gsub("[[:space:]]*$", "", tweet.df2)
tweet.df2 <- gsub(' +', ' ', tweet.df2)
tweet.df2 <- gsub('[^[:graph:]]', ' ', tweet.df2)


## CALCULATING SENTIMENTS/EMOTIONS

# Convert cleaned tweets to a vector
word.df <- as.vector(tweet.df2)

# Get the sentiment/emotion scores using NRC lexicon
emotion.df <- get_nrc_sentiment(word.df)

# Combine cleaned tweets with their sentiment scores
emotion.df2 <- cbind(tweet.df2, emotion.df)

# View the first few entries
head(word.df)
head(emotion.df2)


## FINDING THE MOST POSITIVE & NEGATIVE TWEET

# Calculate sentiment value for each tweet
tweet.sent.value <- get_sentiment(word.df)

# Separate tweets into positive, negative, and neutral
positive.tweets <- word.df[tweet.sent.value > 0]
negative.tweets <- word.df[tweet.sent.value < 0]
neutral.tweets <- word.df[tweet.sent.value == 0]

# Find the most positive tweet
most.positive <- word.df[tweet.sent.value == max(tweet.sent.value)]
print("Most Positive Tweet:")
print(most.positive)

# Find the most negative tweet
most.negative <- word.df[tweet.sent.value == min(tweet.sent.value)]
print("Most Negative Tweet:")
print(most.negative)


## CREATING PIE CHART OF CALCULATED SENTIMENTS

# Count the number of positive, neutral, and negative tweets
Tweet.Positive <- length(positive.tweets)
Tweet.Neutral <- length(neutral.tweets)
Tweet.Negative <- length(negative.tweets)

# Combine the counts for plotting
Tweet.Sentiments <- c(Tweet.Positive, Tweet.Neutral, Tweet.Negative)

# Labels for the pie chart
Tweet.labels <- c("Positive", "Neutral", "Negative")

# Create and save the pie chart
png("sentiment_pie_chart.png", width = 800, height = 600)
pie(Tweet.Sentiments, labels = Tweet.labels, main = "Sentiment Analysis On Tweets",
    col = rainbow(length(Tweet.Sentiments)))
legend('topright', Tweet.labels, cex = 0.8, fill = rainbow(length(Tweet.labels)))
dev.off()  # Close the PNG device to save the file


## TERM DOCUMENT MATRIX

# Create a text corpus from the cleaned tweets
tweet_corpus <- Corpus(VectorSource(word.df))

# Create a term-document matrix
tweet.tdm <- TermDocumentMatrix(tweet_corpus,
                                control = list(removePunctuation = TRUE, wordLengths = c(5, 15),
                                               stopwords = c('comment', 'conspiracy', 'theory', 
                                                             'theories', 'google', stopwords("english")),
                                               removeNumbers = TRUE, tolower = TRUE))

# Convert the term-document matrix to a matrix
tweet.tdm.matrix <- as.matrix(tweet.tdm)

# Calculate word frequencies
tweet.word_freqs <- sort(rowSums(tweet.tdm.matrix), decreasing = TRUE)

# Create a data frame with words and their frequencies
tweet.dm <- data.frame(word = names(tweet.word_freqs), freq = tweet.word_freqs)


## PREPARING THE WORD CLOUD TO EXTRACT INSIGHTS

# Save the word cloud as a PNG file
png("wordcloud.png", width = 800, height = 600)
wordcloud(tweet.dm$word, tweet.dm$freq, 
          min.freq = 3, max.words = 50, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))
dev.off()  # Close the PNG device to save the file

# Plot histogram of sentiment scores
ggplot(data.frame(Sentiment = tweet.sent.value), aes(x = Sentiment)) +
  geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7, color = "black") +
  ggtitle("Distribution of Sentiment Scores") +
  xlab("Sentiment Score") +
  ylab("Frequency")



# Calculate sentiment scores
tweet.df$sentiment <- get_sentiment(tweet.df$Text)

# Generate the sentiment distribution plot
sentiment_plot <- ggplot(tweet.df, aes(x = sentiment, fill = label)) + 
  geom_histogram(binwidth = 0.5, position = "dodge") +
  ggtitle("Sentiment Distribution by Label") +
  xlab("Sentiment Score") + 
  ylab("Frequency") +
  theme_minimal()  # Optional: adds a clean theme to the plot

# Display the plot
print(sentiment_plot)

# Save the plot as a PNG file
ggsave("sentiment_distribution_by_label.png", plot = sentiment_plot, 
       width = 10, height = 6, dpi = 300)







