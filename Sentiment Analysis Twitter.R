library(twitteR)
library(ROAuth)
library(rtweet)
setup_twitter_oauth('5Fwc2mCwnkDzld8dph4ZgvQmt','xwSGiuEtjbt1STZ789pVByzwnzrLGXrJt04KnZSVm7hvAfKIgK','1103898362141528064-P8HLymRzNN3Kv9md5FKhybyeQODWiX','7E4A2M7mCEFZpowqlePQHH5ybzb3maphb3MHddn6kXpjS')


auth_setup_default()

auth_has_default()
crypto <- get_trends("reducecryptotax")
crypto
crypto<-search_tweets("#reducecryptotax")
crypto

getwd()
setwd("C:/Users/Hp/Documents")

# Read file
Cryptdata <- read.csv(file.choose("tax.csv"), header = T)
Cryptdata
str(Cryptdata)
# Build corpus
library(tm)
library(corpus)
corpus <- iconv(Cryptdata$text, to = "UTF-8")
corpus
#corpus <- iconv(apple$text, to = "UTF-8-mac")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)
