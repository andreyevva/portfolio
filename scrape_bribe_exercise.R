# ipadabribe.com allows users to report the instances where they paid a bribe
# this is an exercise in scraping the data on amounts of the bribes from that website
# the exercise was completed within Big Data for Public Policy class
# taught by Mihaly Fazekas, Winter semester 2022
# Central European University

# scraping ipaidabribe website
install.packages("httr")
install.packages("rvest")
library(httr)
library(rvest, warn.conflicts=FALSE)

setwd("/Users/pirozhok/Desktop/OneDrive - Central European University/WINTER2022/BIG DATA 4 PP/4")

options(scipen = 999) # to avoid scientific notation

url <- 'http://ipaidabribe.com/reports/paid'
bribes <- read_html(url) # reading the HTML code

amounts <- html_nodes(bribes, ".paid-amount span") # identify the CSS selector
amounts # content of CSS selector
html_text(amounts)

#text cleaning
amounts <- html_text(amounts)
(amounts <- gsub("Paid INR | |\r|\n|,", "", amounts)) # remove text, white space, and commas
(amounts <- as.numeric(amounts)) # convert to numeric

#Let's do another one: transactions during which the bribe occurred
transaction <- html_nodes(bribes, ".transaction a")
transaction
(transaction <- html_text(transaction))

#And one more: the department that is responsible for these transactions
dept <- html_nodes(bribes, ".name a")
(dept <- html_text(dept))

#now collect an additional attribute: title url
namelink <- html_nodes(bribes, ".heading-3 a") %>% html_attr("href")
namelink

htmllink <- html_nodes(bribes, ".transaction a") %>% html_attr("href")
htmllink
#strsplit()

#bits of information from another line
location <- html_nodes(bribes, ".location")
(location <- html_text(location))

date <- html_nodes(bribes, ".date")
(date <- html_text(date))

# getting data on multiple pages
# we write a function that takes a url, and returns all relevant data for that url
scrape_bribe <- function(url){
  bribes <- read_html(url)
  # variables that we're interested in
  amounts <- html_text(html_nodes(bribes, ".paid-amount span"))
  amounts <- as.numeric(gsub("Paid INR | |\r|\n|,", "", amounts))
  transaction <- html_text(html_nodes(bribes, ".transaction a"))
  dept <- html_text(html_nodes(bribes, ".name a"))
  # putting together into a data frame
  df <- data.frame(
    amounts = amounts,
    transaction = transaction,
    dept = dept,
    stringsAsFactors=F)
  return(df)
}

#then we loop over a list of links. How we get these? By observing
#the pattern of pagination in the url
bribes2 <- list()
bribes2[[1]] <- scrape_bribe(url)
str(bribes2)
base_url <- "http://ipaidabribe.com/reports/paid?page="
#let's get the first 10 pages
pages <- seq(0, 5, by=1)

for (i in 2:length(pages)){
  message(i, '/', length(pages))
  url <- paste(base_url, pages[i], sep="")
  bribes2[[i]] <- scrape_bribe(url)
  Sys.sleep(2)
}

#combining the results into a dataframe with rbind
bribes_processed <- do.call(rbind, bribes2)
head(bribes_processed)
str(bribes_processed)

#######collect data from the full reports

titlelink <- html_nodes(bribes, ".heading-3 a") %>% html_attr("href")
titlelink

url<-read_html("http://ipaidabribe.com/reports/paid/buy-15-scale-crawler-online-rb-innovations")

rep_text <- url %>% 
  html_nodes(".body-copy-lg") %>%
  html_text()
table(rep_text)
rep_text <- gsub("\r\n    ", "", rep_text)
rep_text <- trimws(rep_text, which = "both")
table(rep_text)


#loop over urls collected above

url <- 'http://ipaidabribe.com/reports/paid'

titlelink <- url %>% 
  read_html() %>%
  html_nodes(".heading-3 a") %>%
  html_attr("href")

df <- data.frame()

for (i in 1:length(titlelink)){
  
  link=titlelink[i]
  
  print(link)
  
  text <- link %>% 
    read_html() %>%
    html_nodes(".body-copy-lg") 
  
  text <- gsub('<p class="body-copy-lg">\r\n                    ', "", text)
  
  a <- data.frame(link,text)
  df <- rbind(df,a)
}
View(df)


#analysis
tab <- table(bribes_processed$transaction) # frequency table
tab <- sort(tab, decreasing=TRUE)	# sorting the table from most to least common
head(tab)
#What was the average bribe payment?
summary(bribes_processed$amount)
#how does the distribution of payments look like?
hist(bribes_processed$amount)
#And what was the average payment for each department?
agg <- aggregate(bribes_processed$amount, by=list(dept=bribes_processed$dept), FUN=mean)
agg[order(agg$x, decreasing = TRUE),] # ordering from highest to lowest

