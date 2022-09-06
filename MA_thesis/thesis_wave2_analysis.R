setwd("/Users/pirozhok/Desktop/OneDrive - Central European University/MATHESIS/data")
library(haven)
data <- read.csv("wave2.csv")
library(tm)
library(tidytext)
library(quanteda)
library(ROCR)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(SnowballC)
library(RJSONIO)
library(ROAuth)
library(bit)
library(ggplot2)
library(grid)
library(dplyr)
# sample breakdown
dim(data)
names(data)
print(data$climate_treatment)
summary(data$climate_treatment)
table(data$climate_treatment)
data$age <- as.numeric(data$age)
print(data$gender)
library(summarytools)
freq(data$gender)
ctable(x = data$gender, y = data$climate_treatment, prop = "c")
ctable(x = data$pid1, y = data$climate_treatment, prop = "c")
summary(data$age)
data$mc_1[data$mc_1 == "Strongly agree"] <- 1
data$mc_1[data$mc_1 == "Somewhat agree"] <- 2
data$mc_1[data$mc_1 == "Neutral"] <- 3
data$mc_1[data$mc_1 == "Somewhat disagree"] <- 4
data$mc_1[data$mc_1 == "Strongly disagree"] <- 5
summary(data$mc_1)

data$mc_2[data$mc_2 == "Strongly agree"] <- 1
data$mc_2[data$mc_2 == "Somewhat agree"] <- 2
data$mc_2[data$mc_2 == "Neutral"] <- 3
data$mc_2[data$mc_2 == "Somewhat disagree"] <- 4
data$mc_2[data$mc_2 == "Strongly disagree"] <- 5
summary(data$mc_2)

data$mc_3[data$mc_3 == "Strongly agree"] <- 1
data$mc_3[data$mc_3 == "Somewhat agree"] <- 2
data$mc_3[data$mc_3 == "Neutral"] <- 3
data$mc_3[data$mc_3 == "Somewhat disagree"] <- 4
data$mc_3[data$mc_3 == "Strongly disagree"] <- 5
summary(data$mc_3)

# two way anovas
summary(climate_glo$age)
data$climate_treatment <- ordered(data$climate_treatment,
                         levels = c("local", "national", "global"))
levels(data$climate_treatment)
data$renewable_cces <- as.factor(data$renewable_cces)
data$air_jobs_cces <- as.factor(data$air_jobs_cces)
data$carbon_cces <- as.factor(data$carbon_cces)
data$paris_cces <- as.factor(data$paris_cces)
data$responsibility <- as.factor(data$responsibility)
cor <- cbind(data$renewable_cces, data$air_jobs_cces, data$carbon_cces, 
             data$paris_cces, data$paris_cces)
colnames(cor) <- c("renewable", "air", "carbon", "paris", "resp")
cor <- as.data.frame(cor)
cor(cor, use = "pairwise.complete.obs")
data$mc_1 <- as.factor(data$mc_1)
data$mc_2 <- as.factor(data$mc_2)
data$mc_3 <- as.factor(data$mc_3)
library(ggcorrplot)
ggcorrplot(cor(cor))
group_by(data, climate_treatment) %>%
  summarise(
    count = n(),
    mean = mean(renewable_cces, na.rm = TRUE),
    sd = sd(renewable_cces, na.rm = TRUE)
  )
library("ggpubr")
ggboxplot(data, x = "climate_treatment", y = "renewable_cces", 
          color = "climate_treatment", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("local", "national", "global"),
          ylab = "Each state use a minimum amount of renewables", xlab = "Treatment")

library(tidyr)
data$renewable_cces <- as.vector(data$renewable_cces)
data$treatment <- 0
data$treatment[data$climate_treatment == "local"] <- 1
data$treatment[data$climate_treatment == "national"] <- 2
data$treatment[data$climate_treatment == "global"] <- 3

data$dems <- 0
data$dems[data$pid1 == 2] <- 1
data$reps <- 0
data$reps[data$pid1 == 1] <- 1
data$female <- 0
data$female[data$gender == 2 ] <- 1

data$resp_int <- 0
data$resp_int[data$responsibility == 1] <- 1
data$resp_govt <- 0
data$resp_govt[data$responsibility == 2] <- 1
data$resp_state <- 0
data$resp_state[data$responsibility == 3] <- 1

no.na <- data[complete.cases(data$renewable_cces),]
no.na$carbon_cces[is.na(no.na$carbon_cces)] <- 2

# creating index from DVs
library(dplyr)
library(datapasta)
library(tidyverse)
library(magrittr)
library(purrr)
no.na <- no.na %>%
  rowwise() %>%
  mutate(clim_att = lift_vd(mean)(renewable_cces,
                                  air_jobs_cces,
                                  carbon_cces,
                                  paris_cces))

no.na <- no.na %>%
  rowwise() %>%
  mutate(clim_att = mean(c(renewable_cces,
                         air_jobs_cces,
                         carbon_cces,
                         paris_cces)))
                          
climate_loc <- subset(no.na, subset=(climate_treatment == "local"))
climate_nat <- subset(no.na, subset=(climate_treatment == "national"))
climate_glo <- subset(no.na, subset=(climate_treatment == "global"))

no.na$treatment <- as.vector(no.na$treatment)
no.na$treatment <- as.numeric(no.na$treatment)
no.na$renewable_cces <- as.vector(no.na$renewable_cces)
no.na$renewable_cces <- as.numeric(no.na$renewable_cces)
no.na$air_jobs_cces <- as.numeric(no.na$air_jobs_cces)
no.na$carbon_cces <- as.numeric(no.na$carbon_cces)
no.na$paris_cces <- as.numeric(no.na$paris_cces)
ggline(no.na, x = "treatment", y = "renewable_cces", 
       add = c("mean_se", "jitter"), 
       order = c("local", "national", "global"),
       ylab = "Each state use a minimum amount of renewables", xlab = "Treatment")
library("gplots")
par(mfrow=c(2,2))
plotmeans(renewable_cces ~ treatment, data = no.na, frame = FALSE,
          xlab = "Treatment", ylab = "Renewables")
plotmeans(air_jobs_cces ~ treatment, data = no.na, frame = FALSE,
          xlab = "Treatment", ylab = "Clean air vs. jobs")
plotmeans(carbon_cces ~ treatment, data = no.na, frame = FALSE,
          xlab = "Treatment", ylab = "Carbon regularions")
plotmeans(paris_cces ~ treatment, data = no.na, frame = FALSE,
          xlab = "Treatment", ylab = "Re-join Paris Agreement")
par(mfrow=c(1,1))
# Compute the analysis of variance
aov_mc1 <- aov(mc_1 ~ factor(treatment), data = no.na)
aov_mc2 <- aov(mc_2 ~ factor(treatment), data = no.na)
aov_mc3 <- aov(mc_3 ~ factor(treatment), data = no.na)
# Summary of the analysis
summary(aov_mc1)
summary(aov_mc2)
summary(aov_mc3)
TukeyHSD(aov_mc3)

# Compute the analysis of variance
res.aov_ren <- aov(renewable_cces ~ factor(treatment), data = no.na)
res.aov_air <- aov(air_jobs_cces ~ factor(treatment), data = no.na)
res.aov_car <- aov(carbon_cces ~ factor(treatment), data = no.na)
res.aov_par <- aov(paris_cces ~ factor(treatment), data = no.na)
res.aov_res <- aov(responsibility ~ factor(treatment), data = no.na)
# Summary of the analysis
summary(res.aov_ren)
summary(res.aov_air)
summary(res.aov_car)
summary(res.aov_par)
summary(res.aov_res)


# Regressions - playing around
no.na$responsibility <- as.numeric(no.na$responsibility)
cor(no.na$responsibility, no.na$air_jobs_cces, use ="pairwise.complete.obs")
model <- lm(responsibility ~ air_jobs_cces + age + gender + pid1, data = climate_loc)
summary(model)

general_gl <- lm(mc_1 ~ clim_att + responsibility + age + female + dems, data = no.na)
summary(general_gl)
general_na <- lm(mc_2 ~ clim_att + responsibility + age + female + dems, data = no.na)
summary(general_na)
general_lo <- lm(mc_3 ~ clim_att + responsibility + age + female + dems, data = no.na)
summary(general_lo)

install.packages("pscl")
library(pscl)
# THE regressions
glo_int <- glm(resp_int ~ age + female + dems + clim_att + mc_1, data = climate_glo, family = "binomial")
summary(glo_int)
pR2(glo_int)
glo_govt <- glm(resp_govt ~ age + female + dems + clim_att + mc_1, data = climate_glo, family = "binomial")
summary(glo_govt)
pR2(glo_govt)
glo_state <- glm(resp_state ~ age + female + dems + clim_att + mc_1, data = climate_glo, family = "binomial")
summary(glo_state)
pR2(glo_state)

nat_int <- glm(resp_int ~ age + female + dems + clim_att + mc_2, data = climate_nat, family = "binomial")
summary(nat_int)
pR2(nat_int)
nat_govt <- glm(resp_govt ~ age + female + dems + clim_att + mc_2, data = climate_nat, family = "binomial")
summary(nat_govt)
pR2(nat_govt)
nat_state <- glm(resp_state ~ age + female + dems + clim_att + mc_2, data = climate_nat, family = "binomial")
summary(nat_state)
pR2(nat_state)

loc_int <- glm(resp_int ~ age + female + dems + clim_att + mc_3, data = climate_loc, family = "binomial")
summary(loc_int)
pR2(loc_int)
loc_govt <- glm(resp_govt ~ age + female + dems + clim_att + mc_3, data = climate_loc, family = "binomial")
summary(loc_govt)
pR2(loc_govt)
loc_state <- glm(resp_state ~ age + female + dems + clim_att + mc_3, data = climate_loc, family = "binomial")
summary(loc_state)
pR2(loc_state)
library(stargazer)
stargazer(glo_int, glo_govt, glo_state, nat_int, nat_govt, nat_state, loc_int, loc_govt,
          loc_state, type = "text", digits = 2, out = "regs.html",
          covariate.labels = c("Age", "Female", "Democrat", "Policy Preference", 
                               "CC-global issue", "CC-national issue", "CC-local issue"))


# ROBUSTNESS REGRESSIONS
glo_intR <- glm(resp_int ~ age + female + dems + clim_att + mc_2, data = climate_glo, family = "binomial")
summary(glo_intR)
pR2(glo_intR)
glo_govtR <- glm(resp_govt ~ age + female + dems + clim_att + mc_2, data = climate_glo, family = "binomial")
summary(glo_govtR)
pR2(glo_govtR)
glo_stateR <- glm(resp_state ~ age + female + dems + clim_att + mc_2, data = climate_glo, family = "binomial")
summary(glo_stateR)
pR2(glo_stateR)

nat_intR <- glm(resp_int ~ age + female + dems + clim_att + mc_3, data = climate_nat, family = "binomial")
summary(nat_intR)
pR2(nat_intR)
nat_govtR <- glm(resp_govt ~ age + female + dems + clim_att + mc_3, data = climate_nat, family = "binomial")
summary(nat_govtR)
pR2(nat_govtR)
nat_stateR <- glm(resp_state ~ age + female + dems + clim_att + mc_3, data = climate_nat, family = "binomial")
summary(nat_stateR)
pR2(nat_stateR)

loc_intR <- glm(resp_int ~ age + female + dems + clim_att + mc_1, data = climate_loc, family = "binomial")
summary(loc_intR)
pR2(loc_intR)
loc_govtR <- glm(resp_govt ~ age + female + dems + clim_att + mc_1, data = climate_loc, family = "binomial")
summary(loc_govtR)
pR2(loc_govtR)
loc_stateR <- glm(resp_state ~ age + female + dems + clim_att + mc_1, data = climate_loc, family = "binomial")
summary(loc_stateR)
pR2(loc_stateR)
stargazer(nat_intR, nat_govtR, nat_stateR, loc_intR, loc_govtR, loc_stateR, glo_intR, 
          glo_govtR,
          glo_stateR, type = "text", digits = 2, out = "ROB_regs.html",
          covariate.labels = c("Age", "Female", "Democrat", "Policy Preference", 
                               "CC-national issue", "CC-local issue", "CC-global issue"))


# General regression
general_int <- glm(resp_int ~ age + female + dems + clim_att, data = no.na, family = "binomial")
summary(general_int)
pR2(general_int)
general_govt <- glm(resp_govt ~ age + female + dems + clim_att, data = no.na, family = "binomial")
summary(general_govt)
pR2(general_govt)
general_state <- glm(resp_state ~ age + female + dems + clim_att, data = no.na, family = "binomial")
summary(general_state)
pR2(general_state)
stargazer(general_int, general_govt, general_state, type = "text", digits = 2, 
          out = "gen_regs.html",
          covariate.labels = c("Age", "Female", "Democrat", "Policy Preference"))



#######################
# QUANT TEXT ANALYSIS #
#######################
# creating corpus 
corpus = VCorpus(VectorSource(no.na$clim_openend))
corpus[[139]]$content
# Pre-process data
#lowercase
corpus = tm_map(corpus, content_transformer(tolower))
#remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, "climate")
corpus = tm_map(corpus, removeWords, "change")
#remove exotic encoding characters
corpus<- tm_map(corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))
#make sure the data type is correct
corpus <- tm_map(corpus, PlainTextDocument)
# Look at first email after processing
corpus[[229]]$content
## DTM and sparsity 
dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, 0.97)
dtm

corp <- corpus(no.na, text_field = "clim_openend")

options(width = 200)
corpus <- corpus(VectorSource(corpus))
corp <- tokens(corp)
kwic(corp, "don't")
kwic(corp, "climate")

# 5. Build a term-document matrix
# Document matrix is a table containing the frequency of the words. 
# Column names are words and row names are documents.
tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm),decreasing=TRUE)
#Converting words to dataframe
tdm <- data.frame(word = names(tdm),freq=tdm)
#The frequency table of words
head(tdm, 20)
# PLOT WORD FREQUENCIES
barplot(tdm[1:20,]$freq, las = 2, names.arg = tdm[1:20,]$word,
        col ="green", main ="Most frequent words",
        ylab = "Word frequencies")
# GENERATE THE WORD CLOUD
library(wordcloud)
set.seed(1234)
wordcloud(corpus,min.freq=1,max.words=80,scale=c(2.2,1), 
          colors=brewer.pal(8, "Dark2"), random.color=T, random.order=F)


### sentiment analysis ----
# loading lexicon of positive and negative words (from Neal Caren)
lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]
# function to clean the text
#install.packages("tm")
library(tm)
clean <- function(text){
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
text <- clean(data$clim_openend)
text[[1]]
text[[32]]

# a function to classify individual answers
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

classifier <- function(text, pos.words, neg.words){
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "answers:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}
# applying classifier function
classifier(text, pos.words, neg.words)



# ANALYSIS OF LOCAL TREATMENT
# creating corpus 
corpus_loc = VCorpus(VectorSource(climate_loc$clim_openend))
corpus_loc[[104]]$content
# Pre-process data
#lowercase
corpus_loc = tm_map(corpus_loc, content_transformer(tolower))
#remove punctuation
corpus_loc = tm_map(corpus_loc, removePunctuation)
corpus_loc = tm_map(corpus_loc, removeWords, stopwords("english"))
#remove exotic encoding characters
corpus_loc<- tm_map(corpus_loc, function(x) iconv(x, "latin1", "ASCII", sub=""))
#make sure the data type is correct
corpus_loc <- tm_map(corpus_loc, PlainTextDocument)
# Look at first email after processing
corpus_loc[[317]]$content
## DTM and sparsity 
dtm_loc = DocumentTermMatrix(corpus_loc)
dtm_loc
dtm_loc = removeSparseTerms(dtm_loc, 0.97)
dtm_loc
# 5. Build a term-document matrix
# Document matrix is a table containing the frequency of the words. 
# Column names are words and row names are documents.
tdm_loc <- TermDocumentMatrix(corpus_loc)
tdm_loc <- as.matrix(tdm_loc)
tdm_loc <- sort(rowSums(tdm_loc),decreasing=TRUE)
#Converting words to dataframe
tdm_loc <- data.frame(word = names(tdm_loc),freq=tdm_loc)
#The frequency table of words
head(tdm_loc, 10)
# PLOT WORD FREQUENCIES
barplot(tdm_loc[1:10,]$freq, las = 2, names.arg = tdm_loc[1:10,]$word,
        col ="green", main ="Most frequent words (Local prompt)",
        ylab = "Word frequencies")
# GENERATE THE WORD CLOUD
library(wordcloud)
set.seed(1234)
wordcloud(corpus_loc,min.freq=1,max.words=50,scale=c(2.2,1), 
          colors=brewer.pal(8, "Dark2"), random.color=F, random.order=F)
### sentiment analysis ----
text_loc <- clean(climate_loc$clim_openend)
text_loc[[1]]
text_loc[[32]]
# applying classifier function
classifier(text_loc, pos.words, neg.words)



# ANALYSIS OF NATIONAL TREATMENT
# creating corpus 
corpus_nat = VCorpus(VectorSource(climate_nat$clim_openend))
corpus_nat[[408]]$content
# Pre-process data
#lowercase
corpus_nat = tm_map(corpus_nat, content_transformer(tolower))
#remove punctuation
corpus_nat = tm_map(corpus_nat, removePunctuation)
corpus_nat = tm_map(corpus_nat, removeWords, stopwords("english"))
#remove exotic encoding characters
corpus_nat<- tm_map(corpus_nat, function(x) iconv(x, "latin1", "ASCII", sub=""))
#make sure the data type is correct
corpus_nat <- tm_map(corpus_nat, PlainTextDocument)
# Look at first email after processing
corpus_nat[[319]]$content
## DTM and sparsity 
dtm_nat = DocumentTermMatrix(corpus_nat)
dtm_nat
dtm_nat = removeSparseTerms(dtm_nat, 0.97)
dtm_nat
# 5. Build a term-document matrix
# Document matrix is a table containing the frequency of the words. 
# Column names are words and row names are documents.
tdm_nat <- TermDocumentMatrix(corpus_nat)
tdm_nat <- as.matrix(tdm_nat)
tdm_nat <- sort(rowSums(tdm_nat),decreasing=TRUE)
#Converting words to dataframe
tdm_nat <- data.frame(word = names(tdm_nat),freq=tdm_nat)
#The frequency table of words
head(tdm_nat, 10)
# PLOT WORD FREQUENCIES
barplot(tdm_nat[1:10,]$freq, las = 2, names.arg = tdm_nat[1:10,]$word,
        col ="darkgreen", main ="Most frequent words (National prompt)",
        ylab = "Word frequencies")
# GENERATE THE WORD CLOUD
library(wordcloud)
set.seed(1234)
wordcloud(corpus_nat,min.freq=1,max.words=50,scale=c(2.2,1), 
          colors=brewer.pal(8, "Spectral"), random.color=F, random.order=F)
### sentiment analysis ----
text_nat <- clean(climate_nat$clim_openend)
text_nat[[1]]
text_nat[[32]]
# applying classifier function
classifier(text_nat, pos.words, neg.words)





# ANALYSIS OF GLOBAL TREATMENT
# creating corpus 
corpus_glo = VCorpus(VectorSource(climate_glo$clim_openend))
corpus_glo[[403]]$content
# Pre-process data
#lowercase
corpus_glo = tm_map(corpus_glo, content_transformer(tolower))
#remove punctuation
corpus_glo = tm_map(corpus_glo, removePunctuation)
corpus_glo = tm_map(corpus_glo, removeWords, stopwords("english"))
#remove exotic encoding characters
corpus_glo<- tm_map(corpus_glo, function(x) iconv(x, "latin1", "ASCII", sub=""))
#make sure the data type is correct
corpus_glo <- tm_map(corpus_glo, PlainTextDocument)
# Look at first email after processing
corpus_glo[[317]]$content
## DTM and sparsity 
dtm_glo = DocumentTermMatrix(corpus_glo)
dtm_glo
dtm_glo = removeSparseTerms(dtm_glo, 0.97)
dtm_glo
# 5. Build a term-document matrix
# Document matrix is a table containing the frequency of the words. 
# Column names are words and row names are documents.
tdm_glo <- TermDocumentMatrix(corpus_glo)
tdm_glo <- as.matrix(tdm_glo)
tdm_glo <- sort(rowSums(tdm_glo),decreasing=TRUE)
#Converting words to dataframe
tdm_glo <- data.frame(word = names(tdm_glo),freq=tdm_glo)
#The frequency table of words
head(tdm_glo, 10)
# PLOT WORD FREQUENCIES
barplot(tdm_glo[1:10,]$freq, las = 2, names.arg = tdm_glo[1:10,]$word,
        col ="darkslategray", main ="Most frequent words (Global prompt)",
        ylab = "Word frequencies")
# GENERATE THE WORD CLOUD
library(wordcloud)
set.seed(1234)
wordcloud(corpus_glo,min.freq=1,max.words=50,scale=c(1,1.5), 
          colors=brewer.pal(8, "Paired"), random.color=F, random.order=F)
### sentiment analysis ----
text_glo <- clean(climate_glo$clim_openend)
text_glo[[1]]
text_glo[[32]]
# applying classifier function
classifier(text_glo, pos.words, neg.words)







#####################
## Topic Modeling ##
####################
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(quanteda)
raw.sum=apply(dtm,1,FUN=sum)
dtm=dtm[raw.sum!=0,]
lda <- LDA(dtm, k = 10, control = list(seed = 1234))
lda
topics <- tidy(lda, matrix = "beta")
topics
#let's visualize the top terms in each topic.
#first retrieve the top 10 terms by topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#then plot them
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
### THE MOST DISCRIMINATIVE
beta_spread <- topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  #also filter for words that are relatively common
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001| topic4 > .001| topic5 > .001| topic6 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

beta_spread %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()



raw.sum=apply(dtm_loc,1,FUN=sum)
dtm_loc=dtm_loc[raw.sum!=0,]
lda_loc <- LDA(dtm_loc, k = 2, control = list(seed = 1234))
lda_loc
topics_loc <- tidy(lda_loc, matrix = "beta")
topics_loc
#let's visualize the top terms in each topic.
#first retrieve the top 10 terms by topic
top_terms_loc <- topics_loc %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#then plot them
top_terms_loc %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
### THE MOST DISCRIMINATIVE
beta_spread_loc <- topics_loc %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  #also filter for words that are relatively common
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic1))

beta_spread_loc

beta_spread_loc %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()


raw.sum=apply(dtm_glo,1,FUN=sum)
dtm_glo=dtm_glo[raw.sum!=0,]
lda_glo <- LDA(dtm_glo, k = 2, control = list(seed = 1234))
lda_glo
topics_glo <- tidy(lda_glo, matrix = "beta")
topics_glo
#let's visualize the top terms in each topic.
#first retrieve the top 10 terms by topic
top_terms_glo <- topics_glo %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#then plot them
top_terms_glo %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
### THE MOST DISCRIMINATIVE
beta_spread_glo <- topics_glo %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  #also filter for words that are relatively common
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread_glo

beta_spread_glo %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

raw.sum=apply(dtm_nat,1,FUN=sum)
dtm_nat=dtm_nat[raw.sum!=0,]
lda_nat <- LDA(dtm_nat, k = 2, control = list(seed = 1234))
lda_nat
topics_nat <- tidy(lda_nat, matrix = "beta")
topics_nat
#let's visualize the top terms in each topic.
#first retrieve the top 10 terms by topic
top_terms_nat <- topics_nat %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#then plot them
top_terms_nat %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
### THE MOST DISCRIMINATIVE
beta_spread_nat <- topics_nat %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  #also filter for words that are relatively common
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread_nat

beta_spread_nat %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()





no.na$clim_att <- round(no.na$clim_att, digits = 0)

##################
# RANDOM FOREST #
#################
# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))
# Add in the outcome variable
labeledTerms$clim_att = no.na$clim_att
###analysing the processed texts###
#let's train an algorithm on a training set, then test it on unseen data.
# Split the data
set.seed(1)
spl = sample.split(labeledTerms$clim_att, 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)
# Build a CART decision tree classifier model
treatCART = rpart(clim_att~., data=train, method="class")
prp(treatCART)
# Make predictions on the test set
pred = predict(treatCART, newdata=test,type="prob")[,2]
table(test$clim_att, pred >= 0.5)
(241+22)/(241+22+97+12)
# 70% accuracy

# Baseline model accuracy: what if we always guessed that the document was irrelevant?
table(test$clim_att)
253/(253+119)
# 68% accuracy

# Baseline model accuracy: or if we read all the answers?
119/(253+119)
# 31% accuracy

# We can also evaluate a binary classifier using the receiver operating characteristic
# or ROC curve. It evaluates the trade off between false positives and false negatives
# as our models prediction threshold varies. 
predROCR = prediction(pred, test$clim_att)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC: area under the curve of the ROC. This is a single metric that is widely
#used to measure binary classifier performance
performance(predROCR, "auc")@y.values
# 0.92

# Build a random forest classifier model, takes about 1 minute
#first process the data for the randomForest package
train$clim_att<-as.factor(train$clim_att)
test$clim_att<-as.factor(test$clim_att)
names(train) <- make.names(names(train))
names(test) <- make.names(names(test))
climattforest = randomForest(clim_att~., mtry=28, data=train,ntrees=100)

# Make predictions on the test set
forestpred = predict(climattforest, newdata=test)
# Compute accuracy
table(test$clim_att, forestpred)
(235+93)/(235+26+18+83)
# up to 90% accuracy !!!

#need to cast the predictions to continuous values for ROC analysis
forestpred = predict(climattforest, newdata=test,type = "prob")[,2]
# ROC curve
predROCR = prediction(forestpred,test$clim_att)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
# 92% !!!
#how about logistic regression?
logisticmodel <- glm(clim_att ~.,family=binomial(link='logit'),data=train)
summary(logisticmodel)
# Make predictions on the test set
logisticpred = predict(logisticmodel, newdata=test,type = "response")
# Compute accuracy
table(test$clim_att, logisticpred >= 0.5)
(228+99)/(228+20+25+99)
# .87 - logistic regression seems to be doing quite good
# ROC curve
predROCR = prediction(logisticpred, test$clim_att)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
# 0.93

#######
# STM #
#######
# Note: This script takes several hours to run if all models are fitted.
# Note: Existing models may also be loaded in the script.
# You may want to skip to the existing models to shorten runtime.
library("stm")
library("topicmodels")
library("slam")

no.na$partyid[no.na$pid1 == 1] <- "Republican"
no.na$partyid[no.na$pid1 == 2] <- "Democrat"
no.na$partyid[no.na$pid1 == 3] <- "Independent"
no.na$partyid[no.na$pid1 == 3] <- "Smth else"

############
# Preprocessing
############
set.seed(23456)
processed <- textProcessor(documents = no.na$clim_openend, metadata = no.na)
out <- prepDocuments(documents = processed$documents, 
                     vocab = processed$vocab,
                     meta = processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

###############
# Fitting the example model
###############

out <- prepDocuments(documents = processed$documents, 
                     vocab = processed$vocab,
                     meta = processed$meta, lower.thresh = 15)
shortdoc <- substr(out$meta$documents, 1, 200)
poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                       K = 20, prevalence =~ resp_govt + s(clim_att), 
                       max.em.its = 75,
                       data = out$meta, init.type = "Spectral")

#################
# Model selection
#################

poliblogSelect <- selectModel(out$documents, out$vocab, K = 20,
                              prevalence =~ resp_govt + s(clim_att), max.em.its = 75,
                              data = out$meta, runs = 20, seed = 8458159)

pdf("stmVignette-009.pdf")
plotModels(poliblogSelect)
dev.off()

selectedmodel <- poliblogSelect$runout[[1]]

###############
# Searching Topic Numbers
###############
storage <- searchK(out$documents, out$vocab, K = c(10, 20),
                   prevalence =~ clim_att + s(resp_govt), data = meta)
t <- storage$out[[1]]
t <- storage$out[[2]]

##############################
# Describing the poliblogPrevFit model
#############################

labelTopics(poliblogPrevFit, c(1, 3, 7))

thoughts3 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                          n = 1, topics = 2)$docs[[1]]
thoughts20 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                           n = 2, topics = 18)$docs[[1]]

pdf("stmVignette-015.pdf")
par(mfrow = c(2, 1), mar = c(.5, .5, 1, .5))
plotQuote(thoughts3, width = 40, main = "Topic 6")
plotQuote(thoughts20, width = 40, main = "Topic 18")
dev.off()

# INTERESTING!! :
meta$clim_att <- as.factor(meta$clim_att)
prep <- estimateEffect(1:20 ~ clim_att + s(resp_state), poliblogPrevFit,
                       meta = out$meta, uncertainty = "Global")
summary(prep, topics = 5)

pdf("stmVignette-017.pdf")
plot(poliblogPrevFit, type = "summary", xlim = c(0, .3))
dev.off()

## cool graph
pdf("stmVignette-018.pdf")
plot(prep, covariate = "resp_state", topics = c(16, 14, 3, 13, 9, 4),
     model = poliblogPrevFit, method = "difference",
     cov.value1 = "0", cov.value2 = "1",
     xlab = "Other level of governance ... Local responsibility",
     main = "Effect of Assignment of Local Responsibility",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c("Gets hotter", "Increased droughts/floods", "Surely there is change", "Notice change in the weather", "More extreme weather", "Ice caps are melting"))
dev.off()


##############################
# Using the content covariate
#############################

poliblogContent <- stm(out$documents, out$vocab, K = 20,
                       prevalence =~ clim_att + s(reps), content =~ reps,
                       max.em.its = 75, data = out$meta, init.type = "Spectral")
pdf("stmVignette-021.pdf")
plot(poliblogContent, type = "perspectives", topics = 10)
dev.off()

pdf("stmVignette-022.pdf")
plot(poliblogPrevFit, type = "perspectives", topics = c(3, 13))
dev.off()

##############################
# Using Interactions
#############################

poliblogInteraction <- stm(out$documents, out$vocab, K = 20,
                           prevalence =~ rating * day, max.em.its = 75,
                           data = out$meta, init.type = "Spectral")
prep <- estimateEffect(c(14) ~ rating * day, poliblogInteraction,
                       metadata = out$meta, uncertainty = "None")
pdf("stmVignette-024.pdf")
plot(prep, covariate = "day", model = poliblogInteraction,
     method = "continuous", xlab = "Days", moderator = "rating",
     moderator.value = "Liberal", linecol = "blue", ylim = c(0, .12),
     printlegend = FALSE)
plot(prep, covariate = "day", model = poliblogInteraction,
     method = "continuous", xlab = "Days", moderator = "rating",
     moderator.value = "Conservative", linecol = "red", add = TRUE,
     printlegend = FALSE)
legend(0, .06, c("Liberal", "Conservative"),
       lwd = 2, col = c("blue", "red"))
dev.off()
save(out, poliblogContent, poliblogInteraction, poliblogPrevFit,
     poliblogSelect, shortdoc, file = "results.rda")

############################
# Plotting clouds and correlations
############################

pdf("stmVignette-025.pdf")
cloud(poliblogPrevFit, topic = 13, scale = c(2, .25))
dev.off()
mod.out.corr <- topicCorr(poliblogPrevFit)
pdf("stmVignette-027.pdf")
plot(mod.out.corr)
dev.off()

############################
# Diagnostics
############################

pdf("stmVignette-028.pdf")
plot(poliblogPrevFit$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")
dev.off()

# See v91i02-table1.R for replication of Table 1







