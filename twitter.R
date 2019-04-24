Author: GathingirahWilfredWakanya

library(twitteR)
library(httr)
library(streamR)

#Set directory
setwd("C:/Users/waka-the-fisi/Desktop/twitter")

#Accessing Twitter API
consumer_key <- 'D2SoRvRrWwWo87ysS8hq1zpPk'
consumer_secret <- 'vVkxMgu4t3Hk0ZHnhl7ACJpRXMQyVogBP8vPaGgQReWUG3yfsp'
access_token <- '2561943617-FRypUMwnXYs1ILF8A5QZDVcMyB4Xnf4npaKrmKV'
access_secret <- 'v5ZS9W4PKdRVD8yAZOhH9wC3zTwQNB6iYpm8VOgfTdTiI'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Extract tweets
clinton_tweets <- userTimeline("HillaryClinton", n = 2000, since = "2016-01-09", languageEl("english", which = "en"))
tweetsc.df <- twListToDF(clinton_tweets)
dim(tweets)

#Save Rdata
write.csv(tweets, file = 'C:/Users/waka-the-fisi/Desktop/twitter/tweets.csv', row.names = F)
head(tweets)

#Get the text
text <- readLines(file.choose())
library(slam) #for sparse arrays and matrices
library(NLP) #to understand human language as it is spoken
library(tm) #for text mining
library(SnowballC) #for text stemming
library(wordcloud) #word-cloud generartor
library(RColorBrewer) #for color palettes 

#Load data as corpus
tweets <- iconv(tweets$text, to = "utf-8")
tweets <- Corpus(VectorSource(tweets))
inspect(tweets)

#Text transformation
toSpace <- content_transformer(function (x, pattern) gsub(pattern, "", x))
tweets <- tm_map(tweets, toSpace, "/")
tweets <- tm_map(tweets, toSpace, "@")
tweets <- tm_map(tweets, toSpace,"\\|")

#CLeaning the text
#Convert the text to lower case
tweets <- tm_map(tweets, content_transformer(tolower))

#Remove numbers
tweets <- tm_map(tweets, removeNumbers)

#Remove english common stopwords
tweets <- tm_map(tweets, removeWords, stopwords("english"))

#Remove punctuations
tweets <- tm_map(tweets, removePunctuation)

#Eliminate white spaces
tweets <- tm_map(tweets, stripWhitespace)
 
#Text stemming (reduce words to unify across documents)
tweets <- tm_map(tweets, stemDocument)


#Build a term-document matrix
dtm <- TermDocumentMatrix(tweets)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

#Generate wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, 
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

#Generate barplot
enc2utf8("tweets")
library(syuzhet) #Derive Plot Arcs from Text
library(dplyr) #Grammar of Data Manipulation
library(tm) #for text mining
library(wordcloud) #word-cloud generartor
library(RColorBrewer) #color palettes

#Read file
tweets <- read.csv(file.choose(), header = T)
tweets <- iconv(tweets$text, to = 'utf-8')

s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Tweets')
