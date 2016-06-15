######################################################
### This is some R code that scrapes TWITTER to get data to use for IBM Watson ENRICHMENT in other Blogs
### The Watson services here http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/services-catalog.html
### Thanks to https://github.com/geoffjentry/twitteR
### Support: http://thinktostart.wordpress.com/2013/05/22/twitter-authentification-with-r/
### there is also an IBM Watson ASK - Appliation Starter Kit as another tool set
######################################################

# install.packages(c("devtools", "rjson", "bit64", "httr"))
# library(devtools)
# install_github("geoffjentry/twitteR")
# http://www.r-bloggers.com/randomly-sample-twitter-followers-in-r/

## TWITTER
library(twitteR)
library(RJSONIO)
library(stringr)
library(tm)
library(plyr)
library(ROAuth)  # if you get this error - need this lib Error: object 'OAuthFactory' not found
library(RCurl)

## WATSON
library(RCurl) # General Network Client Interface for R
library(rjson) # JSON for R
library(jsonlite) # JSON parser
library(XML) # XML parser
library(httr) # Tools for URLs and HTTP
library(stringr)
library(data.table) # data shaping
library(reshape2) # data shaping
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(png) # for the presenting of images

######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Project_Birdsong")
getwd()
source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
Sys.setlocale(locale="C") # error: input string 1 is invalid in this locale
options(warn=-1) # careful - turns off warnings  


### SET UP THE ACCESS
reqURL <- "https://api.twitter.com/oauth/request_token/"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api"  ## Tone Analyzer (also can do as part of alchemy combined call)

## CHECK WE GOT THESE FROM KEYS.R
consumer_key # check we got it 
consumer_secret # check we got it 
access_token # check we got it 
access_secret # check we got it 
username_password_TON # check we got it

## http://www.r-bloggers.com/setting-up-the-twitter-r-package-for-text-analytics/
## https://apps.twitter.com/app/
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)  
me <- getUser("ryan77anderson")




############  FUNCTION DECLARATIONS - UTILITY FUNCTIONS - CLEAN DATA

try.tolower = function(x)
{ y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y) }

## Clean up junk from text 
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}



#### DATA ! - GO GET SOME TWEETS !

tweets = searchTwitter("#brexit", 1000, lang="en")
length(tweets) #how many? 1000?  Sometimes you get less!
head(tweets)

tweets = laply(tweets, function(t) t$getText())
tweets = clean.text(tweets)
head(tweets)

###############################  ALCHEMY - COMBINED CALL - RETREIVE AND STORE INFO
alchemy_url <- "http://gateway-a.watsonplatform.net/calls/text/"
api_feature <- "TextGetCombinedData"
api_key #check we got this from keys.R
output_mode <- "json"

# BASIC TEST 
# text <- URLencode("I like chocolate ice cream and red boots and Molson Beer")
# query <- paste(alchemy_url,api_feature,"?extract=keyword,entity,concept&apikey=",api_key,"&text=",text,"&outputMode=",output_mode, sep="")
# response <- POST(query)
# content(response)
# response


### BIG FILE!
#text <- read.csv("data.csv") # old method - csv input - we'll use tweets we just pulled
text <- tweets
Encoding(text) <- "UTF=8"
head(text)

len <- length(text) # how tall is our data frame?
# Set up data frame to store
catchers_mitt <- as.data.frame(matrix(ncol=14, nrow=len))
setnames(catchers_mitt,c("index","docSentiment","taxonomy","keyword1","keyword2","keyword3","entity1","entity2","entity3","concept1","concept2","concept3","language","tweet"))


for (i in 1:len){
  url_tweet <- URLencode(text[i])
  query <- paste(alchemy_url,api_feature,"?extract=keyword,doc-sentiment,entity,taxonomy,concept&apikey=",api_key,"&text=",url_tweet,"&outputMode=",output_mode, sep="")
  query
  response <- POST(query)
  response
  reply <- content(response)
  reply
  
  # store data in data frame
  catchers_mitt$index[i] <- i
  
  info_docSentiment = do.call("rbind", lapply(reply$docSentiment, "[[", 1))
  if(!is.null(info_docSentiment)){ ifelse(info_docSentiment=="neutral",catchers_mitt$docSentiment[i] <- 0,
                                          catchers_mitt$docSentiment[i] <- round(as.numeric(info_docSentiment[2]),3)) 
  }
  
  info_taxonomy = do.call("rbind", lapply(reply$taxonomy, "[[", 1))
  if(!is.null(info_taxonomy)){catchers_mitt$taxonomy[i] <-info_taxonomy[1]} 
  if(  (!is.null(info_taxonomy)) && (info_taxonomy[1]=='no') )
  {catchers_mitt$taxonomy[i] <- lapply(reply$taxonomy, "[[", 2)[1]} # EXCEPTION HANDLES - SOMETIMES RETURN STRUCTURE DIFFERS
  
  info_keywords = do.call("rbind", lapply(reply$keywords, "[[", 1))
  if(!is.null(info_keywords)){ 
    catchers_mitt$keyword1[i] <- info_keywords[1]
    catchers_mitt$keyword2[i] <- info_keywords[2]
    catchers_mitt$keyword3[i] <- info_keywords[3] }
  
  info_entities = do.call("rbind", lapply(reply$entities, "[[", 4))
  if(!is.null(info_entities)){ 
    catchers_mitt$entity1[i] <- info_entities[1]
    catchers_mitt$entity2[i] <- info_entities[2]
    catchers_mitt$entity3[i] <- info_entities[3] }
  
  info_concepts = do.call("rbind", lapply(reply$concepts, "[[", 1))
  if(!is.null(info_concepts)){
    catchers_mitt$concept1[i] <- info_concepts[1]
    catchers_mitt$concept2[i] <- info_concepts[2]
    catchers_mitt$concept3[i] <- info_concepts[3] }
  
  catchers_mitt$language[i] <- reply$language
  catchers_mitt$tweet[i] <- text[i]  # add the actual tweet (Clean version) to end
  
  print(catchers_mitt[i,])
}


catchers_mitt <- data.frame(lapply(catchers_mitt, as.character), stringsAsFactors=FALSE)
dim(catchers_mitt)
write.csv(catchers_mitt,"data_alchemy_enriched.csv")


### TONE 
#getURL("https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-02-11&text=hello",userpwd = username_password_TON ) 


### Function to process output from API and table
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data <- data[-c(1), ] # remove dud first row
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),3)
  setnames(data,c("trait","signal"))
  return(data)
}


### FUNCTION to post data to Tone Analyzer and return results
process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19",
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )
  
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  abc <- tidyResponse(response_text)
  return(abc)
}



###  OK!  LET'S DO MORE STUFF - TONE ANALYSIS


# query <- "Coding is fun!"
# analysis <- process_data_to_tone(query)
# analysis

# Set up data frame to store
len <- length(text) # how tall is our data frame?
catchers_mitt2 <- as.data.frame(matrix(ncol=15, nrow=len))
setnames(catchers_mitt2,c("index","anger","disgust","fear","joy","sadness","analytical","confident","tentative",
                          "openness","conscientiousness","extraversion","agreeableness","neuroticism","tweet"))
head(catchers_mitt2)


for (i in 1:len){
  
  # tweet <- text$status[i]
  tweet <- iconv(text[i], "ISO_8859-2", "UTF-8")
  # tweet <- gsub('http.* *', '', tweet) # kill URLs which cause trouble
  tweet <- gsub('"', '', tweet) # kill things that cause trouble
  tweet <- gsub('\\\\', '', tweet) # kill things that cause trouble
  # tweet <- gsub('00', '', tweet) # kill things that cause trouble <U+008B><U+0170>
  tweet <- gsub('#', ' ', tweet) # kill things that cause trouble HASHTAGS!
  url_tweet <- URLencode(tweet)
  analysis <- process_data_to_tone(url_tweet) # get info from API Call
  catchers_mitt2[i,2:14] <- analysis$signal[1:13] ## pushes the 13 elements in these colunmns
  catchers_mitt2[i,1] <- i
  catchers_mitt2[i,15] <- tweet
  print(catchers_mitt2[i,])
}


head(catchers_mitt2,10)

write.csv(catchers_mitt2,"data_tone_enriched.csv")


# plotty mc plotface
m <- rbind(c(1,2,3), c(4,5,6))
layout(m)
layout.show(max(m))
hist(catchers_mitt2$anger,breaks=100)
hist(catchers_mitt2$joy,breaks=100)
hist(catchers_mitt2$fear,breaks=100)
hist(catchers_mitt2$disgust,breaks=100)
hist(catchers_mitt2$sadness,breaks=100)

plot(catchers_mitt2[2:6],main="Tone Tweets \n June 2016")

 
# This code and the postings on this site are my own and don't necessarily represent IBM's position, strategies or opinions. Anyone is free to use, copy, distribute, modify or sell the source code here on GitHub and other materials directly linked from dreamtolearn.com and is provided "as is" without warranties.  # I am not responsible for any harm or damage caused to your computer, software or anything else caused by the material. (So handle with care :)

