# Amazon Reviews sentiment analysis in R.
library(tm)
library(wordcloud)
library(syuzhet)

review.data <- read.csv(file.choose(),header = T)
review.data
str(review.data)

# Creating a corpus document to store review data.

corpus <- iconv(review.data$text)
corpus <- Corpus(VectorSource(corpus))

corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus,removeWords,stopwords="english")
corpus <- tm_map(corpus,stripWhitespace)

inspect(corpus[1:3])

final_rev <- corpus

# Creating a term document matrix.

tdm <- TermDocumentMatrix(final_rev)
tdm <- as.matrix(tdm)
tdm

# Plotting the widely used terms in the corpus.

word.count <- rowSums(tdm)
word.count <- subset(word.count,word.count > 5)
barplot(word.count,las=2,col=rainbow(7))

# Removing unecessary terms.

corpus <- tm_map(corpus,removeWords,c("and","the"))

# Re-running from line 21


# Creating a word cloud for better perception.

word.count <- sort(rowSums(tdm),decreasing = T)
set.seed(1234)
wordcloud(words = names(word.count), freq = word.count,
          max.words = 100,random.order = T,min.freq = 5,rot.per=0.35,
          scale = c(3,0.3),
          colors = brewer.pal(8, "Dark2"))

# Extracting the sentiments from each review

sentiment.data <- iconv(review.data$text)
sentiment <- get_nrc_sentiment(sentiment.data)
sentiment


sentiment$positive

count(sentiment$positive)
count(sentiment$negative)

score = sentiment$positive - sentiment$negative
score

sentiment$score <- score

final_scores <- colSums(sentiment)
final_scores

# Plotting sentiments

barplot(final_scores,las=2,col=rainbow(7))

count(sentiment$score)

# x freq
# 1 -1    2
# 2  0   15
# 3  1    9
# 4  2    3
# 5  3    2
# 6  4    1
# 7  9    1

