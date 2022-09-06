# Anastasiia Andreeva
# Big Data for Pubic Policy
# 01.05.2022
# data collection code


setwd("/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project")

# install.packages("SnowballC")
library(wordcloud)
library(SnowballC)
library(tm)
library(RJSONIO)
library(twitteR)
library(ROAuth)
library(streamR)
library(bit)
library(ggplot2)
library(grid)
library(maps)

### establishing Twitter connection: ----
accessToken = '1361741111040040962-MoQF9ZYg6c6e9Y8gjsSvGiUWUS5JmU'
accessSecret = '3DuOQxmb4SR20KREf6WRiKJyw1CqiBtLQQfffmzxb0ghx'
consumerKey <- 'Gk2nl4ksqLY74dABa1QrBLXSp'
consumerSecret <- '5WdHJKxRlQRdDlrQDVYLbyR2NrDCZ3IAvZY8y2oGnEMnGmL17d'

options(httr_oauth_cache=TRUE)
setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret,
                    access_token=accessToken, access_secret=accessSecret)

### extracting the tweets & saving them as csvs ----
tweets.gnd <- searchTwitter("Green New Deal", n = 10000, lang="en", 
                            since="2018-1-1")
tweets.gnd.df <- twListToDF(tweets.gnd)
write.csv(tweets.gnd.df,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/Tweets_GND-1.csv", 
          row.names = TRUE)

tweets.ib <- searchTwitter("Infrastructure Bill", n = 10000, lang="en", 
                           since="2018-1-1")
tweets.ib.df <- twListToDF(tweets.ib)
write.csv(tweets.ib.df,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/Tweets_IB-1.csv", 
          row.names = TRUE)

tweets.cc <- searchTwitter("Climate change", n = 10000, lang="en", 
                           since="2018-1-1")
tweets.cc.df <- twListToDF(tweets.cc)
write.csv(tweets.cc.df,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/Tweets_СC-1.csv", 
          row.names = TRUE)

hashtags.gnd <- searchTwitter("#GreenNewDeal", n = 18000, lang="en", 
                            since="2018-1-1")
hashtags.gnd.df <- twListToDF(hashtags.gnd)
write.csv(hashtags.gnd.df,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/Hashtags_GND.csv", 
          row.names = TRUE)

hashtags.ib <- searchTwitter("#InfrastructureBill", n = 18000, lang="en", 
                              since="2018-1-1")
hashtags.ib.df <- twListToDF(hashtags.ib)
write.csv(hashtags.ib.df,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/Hashtags_IB.csv", 
          row.names = TRUE)

hashtags.bb <- searchTwitter("#BuildBackBetter", n = 18000, lang="en", 
                             since="2018-1-1")
hashtags.bb.df <- twListToDF(hashtags.bb)
write.csv(hashtags.bb.df,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/Hashtags_BB.csv", 
          row.names = TRUE)

hashtags.cc <- searchTwitter("#ClimateChange", n = 10000, lang="en", 
                              since="2018-1-1")
hashtags.cc.df <- twListToDF(hashtags.cc)
write.csv(hashtags.cc.df,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/Hashtags_CC-1.csv", 
          row.names = TRUE)

### making word clouds: ----
gnd_text <- sapply(GND_alltweets, function(x) x$getText())
ib_text <- sapply(tweets.ib, function(x) x$getText())
cc_text <- sapply(tweets.cc, function(x) x$getText)

# Create a corpus from the collection of text files
gnd_text_corpus <- Corpus(VectorSource(gnd_text))
ib_text_corpus <- Corpus(VectorSource(ib_text))
cc_text_corpus <- Corpus(VectorSource(cc_text))

# Data cleaning
# 1. Remove punctuation
gnd_text_corpus <- tm_map(gnd_text_corpus, removePunctuation)
ib_text_corpus <- tm_map(ib_text_corpus, removePunctuation)
cc_text_corpus <- tm_map(cc_text_corpus, removePunctuation)
# 2. Transforming to lowercase
gnd_text_corpus <- tm_map(gnd_text_corpus, content_transformer(tolower))
ib_text_corpus <- tm_map(ib_text_corpus, content_transformer(tolower))
cc_text_corpus <- tm_map(cc_text_corpus, content_transformer(tolower))
# 3. Removing stopwords (common words)
gnd_text_corpus <- tm_map(gnd_text_corpus, function(x)removeWords(x,stopwords()))
ib_text_corpus <- tm_map(ib_text_corpus, function(x)removeWords(x,stopwords()))
cc_text_corpus <- tm_map(cc_text_corpus, function(x)removeWords(x,stopwords()))

# 5. Build a term-document matrix
# Document matrix is a table containing the frequency of the words. 
# Column names are words and row names are documents.
gnd_2 <- TermDocumentMatrix(gnd_text_corpus)
gnd_2 <- as.matrix(gnd_2)
gnd_2 <- sort(rowSums(gnd_2),decreasing=TRUE)

ib_2 <- TermDocumentMatrix(ib_text_corpus)
ib_2 <- as.matrix(ib_2)
ib_2 <- sort(rowSums(ib_2),decreasing=TRUE)

cc_2 <- TermDocumentMatrix(cc_text_corpus)
cc_2 <- as.matrix(cc_2)
cc_2 <- sort(rowSums(cc_2),decreasing=TRUE)

#Converting words to dataframe
gnd_2 <- data.frame(word = names(gnd_2),freq=gnd_2)
ib_2 <- data.frame(word = names(ib_2),freq=ib_2)
cc_2 <- data.frame(word = names(cc_2),freq=cc_2)
#The frequency table of words
head(gnd_2, 20)
head(ib_2, 20)
head(cc_2, 20)

# PLOT WORD FREQUENCIES
barplot(gnd_2[1:20,]$freq, las = 2, names.arg = gnd_2[1:20,]$word,
        col ="yellow", main ="Most frequent words",
        ylab = "Word frequencies")
barplot(ib_2[1:20,]$freq, las = 2, names.arg = ib_2[1:20,]$word,
        col ="darkolivegreen", main ="Most frequent words",
        ylab = "Word frequencies")
barplot(cc_2[1:20,]$freq, las = 2, names.arg = cc_2[1:20,]$word,
        col ="turquoise", main ="Most frequent words",
        ylab = "Word frequencies")

# GENERATE THE WORD CLOUD
set.seed(1234)
wordcloud(gnd_text_corpus,min.freq=1,max.words=80,scale=c(2.2,1), 
          colors=brewer.pal(8, "Dark2"), random.color=T, random.order=F)
wordcloud(ib_text_corpus,min.freq=1,max.words=80,scale=c(2.2,1), 
          colors=brewer.pal(8, "Dark2"), random.color=T, random.order=F)
wordcloud(cc_text_corpus,min.freq=1,max.words=80,scale=c(2.2,1), 
          colors=brewer.pal(8, "Dark2"), random.color=T, random.order=F)


# ALTERNATIVE METHOD FOR CC ##
library(tm)            # Text mining cleaning
library(stringr)
install.packages("qdapRegex")
library(qdapRegex)     # Removing URLs 
install.packages("wordcloud2")
library(wordcloud2)
# Clean the data
text_cc <- str_c(tweets.cc.df$text, collapse = "")
# continue cleaning the text
text_cc <- 
  text_cc %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))                   # Final cleanup of other small changes

textCorpus_cc <- 
  Corpus(VectorSource(text_cc)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus_cc <- sort(rowSums(textCorpus_cc), decreasing=TRUE)
textCorpus_cc <- data.frame(word = names(textCorpus_cc), freq=textCorpus_cc, row.names = NULL)
wordcloud_cc <- wordcloud2(data = textCorpus_cc, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud_cc

### creating the maps ----
# i need to extract specifically geolocated tweets with rtweet, bc 
# the ones that i scraped with twitteR returns 0
library(rtweet)
# whatever name you assigned to your created app
appname <- "nottodaycl1matechange"
## api key (example below is not a real key)
key <- "Gk2nl4ksqLY74dABa1QrBLXSp"
## api secret (example below is not a real key)
secret <- "5WdHJKxRlQRdDlrQDVYLbyR2NrDCZ3IAvZY8y2oGnEMnGmL17d"
access_token <- "1361741111040040962-MoQF9ZYg6c6e9Y8gjsSvGiUWUS5JmU"
access_secret <- "3DuOQxmb4SR20KREf6WRiKJyw1CqiBtLQQfffmzxb0ghx"
# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)
twts.gnd.df <- search_tweets("Green New Deal", n = 10000, lookup_coords("usa"),
                              type = "recent",
                             retryonratelimit = TRUE,
                               include_rts = TRUE,
                               parse = TRUE)
twts.ib.df <- search_tweets("Infrastructure bill", n = 10000, lookup_coords("usa"),
                              type = "recent",
                              include_rts = TRUE,
                              parse = TRUE)
hshtg.gnd.df <- search_tweets("#GreenNewDeal", n = 3000, lookup_coords("usa"),
                                type = "recent",
                                include_rts = TRUE,
                                parse = TRUE)
hshtg.ib.df <- search_tweets("#InfrastructureBill", n = 3000, lookup_coords("usa"),
                               type = "recent",
                               include_rts = TRUE,
                               parse = TRUE)
twts.cc.df <- search_tweets("climate change", n = 3000, lookup_coords("usa"),
                              type = "recent",
                              include_rts = TRUE,
                              parse = TRUE)
hshtg.cc.df <- search_tweets("#ClimateChange", n = 3000, lookup_coords("usa"),
                               type = "recent",
                               include_rts = TRUE,
                               parse = TRUE)
# keeping only geolocated tweets with precise long/lat information
geo_tweets.gnd <- lat_lng(twts.gnd.df)
geo_tweets.ib <- lat_lng(twts.ib.df)
geo_hashtag.gnd <- lat_lng(hshtg.gnd.df)
geo_hashtag.ib <- lat_lng(hshtg.ib.df)
geo_tweets.cc <- lat_lng(twts.cc.df)
geo_hashtag.cc <- lat_lng(hshtg.cc.df)

geo_tweets.gnd <- geo_tweets.gnd[!is.na(geo_tweets.gnd$lng),]
geo_tweets.ib <- geo_tweets.ib[!is.na(geo_tweets.ib$lng),]
geo_hashtag.gnd <- geo_hashtag.gnd[!is.na(geo_hashtag.gnd$lng),]
geo_hashtag.ib <- geo_hashtag.ib[!is.na(geo_hashtag.ib$lng),]
geo_tweets.cc <- geo_tweets.cc[!is.na(geo_tweets.cc$lng),]
geo_hashtag.cc <- geo_hashtag.cc[!is.na(geo_hashtag.cc$lng),]

# binding tweets + hashtags
map.gnd <- rbind(geo_tweets.gnd, geo_hashtag.gnd)
map.ib <- rbind(geo_tweets.ib, geo_hashtag.ib)
map.cc <- rbind(geo_tweets.cc, geo_hashtag.cc)


## Now we create a data frame with the map data 
map.data <- map_data("state")
# And finally we use ggplot2 to draw the map:
# 1) map base
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "grey90", 
                            color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  # 2) limits for x and y axis
  scale_x_continuous(limits=c(-125,-66)) + scale_y_continuous(limits=c(25,50)) +
  # 3) adding the dot for each tweet
  geom_point(data = map.gnd, 
             aes(x = lng, y = lat), size = 1, alpha = 1/5, color = "darkslategray") +
  # 4) removing unnecessary graph elements
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) 

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "grey90", 
                            color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  scale_x_continuous(limits=c(-125,-66)) + scale_y_continuous(limits=c(25,50)) +
  geom_point(data = map.ib, 
             aes(x = lng, y = lat), size = 1, alpha = 1/5, color = "navyblue") +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) 

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "grey90", 
                            color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  scale_x_continuous(limits=c(-125,-66)) + scale_y_continuous(limits=c(25,50)) +
  geom_point(data = map.cc, 
             aes(x = lng, y = lat), size = 1, alpha = 1/5, color = "darkgreen") +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) 

### sentiment analysis ----
# loading lexicon of positive and negative words (from Neal Caren)

lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]

# a look at a random sample of positive and negative words
sample(pos.words, 10)
sample(neg.words, 10)

# function to clean the text
#install.packages("tm")
library(tm)

clean_tweets <- function(text){
  # loading required packages
  lapply(c("tm", "SnowballC", "stringr"), require, c=T, q=T)
  # avoid encoding issues by dropping non-unicode characters
  utf8text <- iconv(text, to='UTF-8', sub = "byte")
  # remove punctuation and convert to lower case
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  # spliting in words
  words <- str_split(words, " ")
  return(words)
}

# now we clean the text
tweets.gnd.df <- twListToDF(tweets.gnd)
tweets.gnd.df$text[1]
tweets.gnd.df$text[7]

tweets.ib.df <- twListToDF(tweets.ib)
tweets.ib.df$text[1]
tweets.ib.df$text[7]

tweets.cc.df <- twListToDF(tweets.cc)
tweets.cc.df$text[1]
tweets.cc.df$text[7]

text <- clean_tweets(tweets.gnd.df$text)
text[[1]]
text[[7]]

text.ib <- clean_tweets(tweets.ib.df$text)
text.ib[[1]]
text.ib[[7]]

text.cc <- clean_tweets(tweets.cc.df$text)
text.cc[[1]]
text.cc[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}


# this is how we would apply it
classify(text[[1]], pos.words, neg.words)
classify(text[[7]], pos.words, neg.words)

classify(text.ib[[1]], pos.words, neg.words)
classify(text.ib[[7]], pos.words, neg.words)

classify(text.cc[[1]], pos.words, neg.words)
classify(text.cc[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(text, pos.words, neg.words)
classifier(text.ib, pos.words, neg.words)
classifier(text.cc, pos.words, neg.words)


