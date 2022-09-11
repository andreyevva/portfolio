####################
# Getting the data #
####################
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

# putting the data together
gnd <- rbind(Tweets_GND, Tweets_GND_1, Hashtags_GND, Hashtags_GND_1)
write.csv(gnd,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/gnd.csv", 
          row.names = TRUE)
ib <- rbind(Tweets_IB, Tweets_IB_1, Hashtags_IB, Hashtags_BB)
write.csv(ib,"/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/project/ib.csv", 
          row.names = TRUE)
library(readr)
gnd <- read_csv("gnd.csv")
ib <- read_csv("ib.csv")