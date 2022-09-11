library(tm)
library(tidytext)
library('quanteda')
library(ROCR)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(SnowballC)
library(RJSONIO)
library(twitteR)
library(ROAuth)
library(streamR)
library(bit)
library(ggplot2)
library(grid)
library(maps)
setwd("/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project")
library(readr)
gnd <- read_csv("gnd.csv")
ib <- read_csv("ib.csv")
cc <- read_csv("cc.csv")

#########################
## Sentiment analysis ##
########################
# loading lexicon of positive and negative words (from Neal Caren)
lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]
# function to clean the text
clean_tweets <- function(text){
  lapply(c("tm", "SnowballC", "stringr"), require, c=T, q=T)
  utf8text <- iconv(text, to='UTF-8', sub = "byte")
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  words <- str_split(words, " ")
  return(words)
}
# clean the text
text.gnd <- clean_tweets(gnd$text)
text.ib <- clean_tweets(ib$text)
# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

classify(text.ib[[78]], pos.words, neg.words)

classifier <- function(text, pos.words, neg.words){
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}
# applying classifier function
classifier(text.gnd, pos.words, neg.words)
classifier(text.ib, pos.words, neg.words)



###############
## Quanteda ##
##############

library(ggplot2)
library(quanteda)
library(quanteda.textstats)
library("quanteda.textplots")
library(tm)
## creating corpus
corpus.gnd = VCorpus(VectorSource(gnd$text))
corpus.gnd[[110]]$content
stopwords("english")
corpus.gnd = tm_map(corpus.gnd, removeWords, stopwords("english"))
corpus.gnd = tm_map(corpus.gnd, content_transformer(tolower))
corpus.gnd = tm_map(corpus.gnd, removePunctuation)
corpus.gnd = tm_map(corpus.gnd, removeWords, "rt")
corpus.gnd = tm_map(corpus.gnd, removeWords, "RT")
corpus.gnd = tm_map(corpus.gnd, removeWords, "the")
corpus.gnd = tm_map(corpus.gnd, removeWords, "this")
corpus.gnd = tm_map(corpus.gnd, removeWords, "like")
#remove exotic encoding charactersation
corpus.gnd <- tm_map(corpus.gnd, function(x) iconv(x, "latin1", "ASCII", sub=""))
#make sure the data type is correct
corpus.gnd <- tm_map(corpus.gnd, PlainTextDocument)
# Look at first tweet after processing
corpus.gnd[[129]]$content
## DTM and sparsity 
dtm.gnd = DocumentTermMatrix(corpus.gnd)
dtm.gnd
dtm.gnd = removeSparseTerms(dtm.gnd, 0.97)
dtm.gnd

## creating corpus
corpus.ib = VCorpus(VectorSource(ib$text))
corpus.ib = tm_map(corpus.ib, removeWords, stopwords("english"))
corpus.ib = tm_map(corpus.ib, content_transformer(tolower))
corpus.ib = tm_map(corpus.ib, removePunctuation)
corpus.ib = tm_map(corpus.ib, removeWords, "rt")
corpus.ib = tm_map(corpus.ib, removeWords, "RT")
corpus.ib = tm_map(corpus.ib, removeWords, "the")
corpus.ib = tm_map(corpus.ib, removeWords, "this")
corpus.ib = tm_map(corpus.ib, removeWords, "like")
#remove exotic encoding charactersation
corpus.ib <- tm_map(corpus.ib, function(x) iconv(x, "latin1", "ASCII", sub=""))
#make sure the data type is correct
corpus.ib <- tm_map(corpus.ib, PlainTextDocument)
# Look at first tweet after processing
corpus.ib[[117]]$content
## DTM and sparsity 
dtm.ib = DocumentTermMatrix(corpus.ib)
dtm.ib
dtm.ib = removeSparseTerms(dtm.ib, 0.97)
dtm.ib

# converting document term matrix to document feature matrix
gnd.dfm <- quanteda::as.dfm(dtm.gnd)
ib.dfm <- quanteda::as.dfm(dtm.ib)

# top features
topfeatures(gnd.dfm, 20)
topfeatures(ib.dfm, 20)

# wordcloud
set.seed(100)
textplot_wordcloud(gnd.dfm, min_count = 80, random_order = FALSE,
                   rotation = 0,
                   dfm_trim(gnd.dfm, min_termfreq = 200),
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

textplot_wordcloud(ib.dfm, min_count = 80, random_order = FALSE,
                   rotation = 0,
                   dfm_trim(ib.dfm, min_termfreq = 200),
                   color = RColorBrewer::brewer.pal(8,"Set1"))



#####################
## Topic Modeling ##
####################
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
raw.sum=apply(dtm.gnd,1,FUN=sum)
dtm.gnd=dtm.gnd[raw.sum!=0,]
gnd_lda <- LDA(dtm.gnd, k = 3, control = list(seed = 1234))
gnd_lda
gnd_topics <- tidy(gnd_lda, matrix = "beta")
gnd_topics
#let's visualize the top terms in each topic.

#first retrieve the top 10 terms by topic
gnd_top_terms <- gnd_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#then plot them
gnd_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

### THE MOST DISCRIMINATIVE
beta_spread.gnd <- gnd_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  #also filter for words that are relatively common
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread.gnd

beta_spread.gnd %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

raw.sum=apply(dtm.ib,1,FUN=sum)
dtm.ib=dtm.ib[raw.sum!=0,]
ib_lda <- LDA(dtm.ib, k = 3, control = list(seed = 1234))
ib_lda
ib_topics <- tidy(ib_lda, matrix = "beta")
ib_topics
#let's visualize the top terms in each topic.

#first retrieve the top 10 terms by topic
ib_top_terms <- ib_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#then plot them
ib_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

### THE MOST DISCRIMINATIVE
beta_spread.ib <- ib_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  #also filter for words that are relatively common
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread.ib

beta_spread.ib %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()



####################
## Random Forest ##
###################
# Create data frame
clean.gnd = as.data.frame(as.matrix(dtm.gnd))
clean.ib = as.data.frame(as.matrix(dtm.ib))
# Add in the outcome variable
# 1 - Green New Deal
# 0 - Infrastructure Bill
clean.gnd$type <- 1
clean.ib$type <- 0
library(plyr)
clean.all <- rbind.fill(clean.gnd, clean.ib)
#let's train an algorithm on a training set, then test it on unseen data.
# Split the data
set.seed(1)
spl = sample.split(clean.all$type, 0.7)
train = subset(clean.all, spl == TRUE)
test = subset(clean.all, spl == FALSE)
# Build a CART decision tree classifier model
allCART = rpart(type~., data=train, method="class")
prp(allCART)
# Make predictions on the test set
pred = predict(allCART, newdata=test,type="prob")[,2]
# Look at the confusion matrix and compute accuracy
table(test$type, pred >= 0.5)
(7279+3938)/(7279+3938+436+5109)
# 67% accuracy
# Baseline model accuracy: what if we always guessed that the tweet was irrelevant?
table(test$type)
7715/(7715+9047)
# 46%
# Baseline model accuracy: or if we read all the emails?
9047/(7715+9047)
# 53.9%

# We can also evaluate a binary classifier using the receiver operating characteristic
# or ROC curve. It evaluates the trade off between false positives and false negatives
# as our models prediction threshold varies. 
predROCR = prediction(pred, test$type)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC: area under the curve of the ROC. This is a single metric that is widely
#used to measure binary classifier performance
performance(predROCR, "auc")@y.values
# .75

# Build a random forest classifier model
#first process the data for the randomForest package
train$type<-as.factor(train$type)
test$type<-as.factor(test$type)
names(train) <- make.names(names(train))
names(test) <- make.names(names(test))
tweetforest = randomForest(type~., mtry=28, data=train,ntrees=100, na.action=na.roughfix)
# Make predictions on the test set
forestpred = predict(tweetforest, newdata=test)
table(test$type, forestpred)
# 0% accuracy...

#need to cast the predictions to continuous values for ROC analysis
forestpred = predict(tweetforest, newdata=test,type = "prob")[,2]
# ROC curve
forestpred <- as.list(na.omit(forestpred))
predROCR = prediction(forestpred,test$type)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
# .75




# ---- not included in the paper ----:
#logistic regression? - doesn't seem to work - one dimension? - CONSULTATION
logisticmodel <- glm(type ~.,family=binomial(link='logit'),data=train)
summary(logisticmodel)
# Make predictions on the test set
logisticpred = predict(logisticmodel, newdata=test,type = "response")

# Compute accuracy
table(test$type, logisticpred >= 0.5)

predROCR = prediction(logisticpred, test$type)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values







