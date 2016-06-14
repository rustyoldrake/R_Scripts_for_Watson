######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services 
### Focus: IBM Watson - Alchemy Language - Batch Processing 
### Watson Services: http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/services-catalog.html
###
### This code takes a bunch of TWEETS or DATA and then runs Watson/Alchemy on it, and then stores the ENRICHED data
###
######################################################

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
library(tau)

## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
Sys.setlocale(locale="C") # error: input string 1 is invalid in this locale

######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/")
getwd()
source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

###############################  ALCHEMY - COMBINED CALL - RETREIVE AND STORE INFO

## TESTING - Response on Basic Sentiment? - FROM TEXT (provided - should be faster)
alchemy_url <- "http://gateway-a.watsonplatform.net/calls/text/"
api_feature <- "TextGetCombinedData"
api_key <- "YOURKEYHERE" # 
output_mode <- "json"

# BASIC TEST 
# text <- URLencode("I like chocolate ice cream and red boots and Molson Beer")
# query <- paste(alchemy_url,api_feature,"?extract=keyword,entity,concept&apikey=",api_key,"&text=",text,"&outputMode=",output_mode, sep="")
# query
# response <- POST(query)
# content(response)
# response


### BIG FILE!
text <- read.csv("YOUR_data.csv")
text$status <- paste(text$status) # kill the levels
Encoding(text$status) <- "UTF=8"
text$status[9999] # big file
head(text)

# Set up data frame to store
catchers_mitt <- as.data.frame(matrix(ncol=14, nrow=100000))
setnames(catchers_mitt,c("index","docSentiment","taxonomy","keyword1","keyword2","keyword3","entity1","entity2","entity3","concept1","concept2","concept3","language","tweet"))


count <- 100000
for (i in 1:count){
  tweet <- text$status[i]
  tweet <- iconv(tweet, "ISO_8859-2", "UTF-8") #bad things happen when no utf8
  tweet <- gsub('http.* *', '', tweet) # kill URLs which cause trouble
  tweet <- gsub('"', '', tweet) # kill things that cause trouble
  tweet <- gsub('\\\\', '', tweet) # kill things that cause trouble
  tweet <- gsub('00', '', tweet) # kill things that cause trouble <U+008B><U+0170>
  tweet <- gsub('#', ' ', tweet) # kill things that cause trouble HASHTAGS!
  tweet
  url_tweet <- URLencode(tweet)
  
  ## THIS IS BIG CALL - YOU CAN CALL "TONE-EMOTION TOO IF YOU WANT)
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
  
  catchers_mitt$tweet[i] <- tweet

  print(catchers_mitt[i,])
}
 

catchers_mitt <- data.frame(lapply(catchers_mitt, as.character), stringsAsFactors=FALSE)
#catchers_mitt <- catchers_mitt[-c(20014:100000), ]  # peel NA rows if not filled
dim(catchers_mitt)
write.csv(catchers_mitt,"news_data_enriched_100000.csv")

## peek
table_tax <- table(catchers_mitt$taxonomy)
head(table_tax)
table_tax <- sort(table_tax,decreasing = TRUE)
table_tax
head(table_tax)

plot(table_tax, main="Tweets - Taxonomy - Distribution")
plot(table_tax[1:20], main="Tweets - Taxonomy - Distribution")



 


### TONE (can include next time in alchemy combined call) - breaks a little - so handle with care
### TONE (can include next time in alchemy combined call) - breaks a little - so handle with care


# base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer-beta/api"
## USE GA VERSION NOW!
base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api"


### GET - Basic Test 
# getURL("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11&text=hello",userpwd = username_password_TON ) 
getURL("https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19&text=hello",userpwd = username_password_TON ) 


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
  data <- data[c("X2", "X1")]  # this sometimes throws error
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
  #print(response_text)
  abc <- NULL
  if(length(grep("reached the request rate limit",response_text))>0) print("error") else (abc <- tidyResponse(response_text))
  return(abc)
}



###  OK!  LET'S DO MORE STUFF (breaks sometimes)  Error in `[.data.frame`(data, c("X2", "X1")) :

query <- "Coding is fun!"
analysis <- process_data_to_tone(query)
analysis


#                 trait signal
# 1              anger  0.039
# 2            disgust  0.075
# 3               fear  0.090
# 4                joy  0.852
# 5            sadness  0.053
# 6         analytical  0.000
# 7          confident  0.000
# 8          tentative  0.000
# 9           openness  0.132
# 10 conscientiousness  0.571
# 11      extraversion  0.805
# 12     agreeableness  0.465
# 13   emotional_range  0.401

# Set up data frame to store
catchers_mitt2 <- as.data.frame(matrix(ncol=15, nrow=20000))
setnames(catchers_mitt2,c("index","anger","disgust","fear","joy","sadness","analytical","confident","tentative",
                         "openness","conscientiousness","extraversion","agreeableness","neuroticism","tweet"))
head(catchers_mitt2)


options(warn=-1) # careful - turns off warnings so it can keep trucking

#i <- 11

count <- 1000
for (i in 1:count){
  
  tweet <- text$status[i]
  tweet <- iconv(tweet, "ISO_8859-2", "UTF-8")
  tweet <- gsub('http.* *', '', tweet) # kill URLs which cause trouble
  tweet <- gsub('"', '', tweet) # kill things that cause trouble
  tweet <- gsub('\\\\', '', tweet) # kill things that cause trouble
  tweet <- gsub('00', '', tweet) # kill things that cause trouble <U+008B><U+0170>
  tweet <- gsub('#', ' ', tweet) # kill things that cause trouble HASHTAGS!
  tweet
  url_tweet <- URLencode(tweet)
  analysis <- process_data_to_tone(url_tweet) # get info from API Call
  catchers_mitt2[i,2:14] <- analysis$signal[1:13] ## pushes the 13 elements in these colunmns
  catchers_mitt2[i,1] <- i
  catchers_mitt2[i,15] <- tweet
  print(catchers_mitt2[i,])
  
}


head(catchers_mitt2,10)

write.csv(catchers_mitt2,"news_data_tone_1000.csv")


# plotty mc plotface
m <- rbind(c(1,2,3), c(4,5,6))
layout(m)
layout.show(max(m))
hist(catchers_mitt2$anger,breaks=100)
hist(catchers_mitt2$joy,breaks=100)
hist(catchers_mitt2$fear,breaks=100)
hist(catchers_mitt2$disgust,breaks=100)
hist(catchers_mitt2$sadness,breaks=100)

plot(catchers_mitt2[2:6],main="Tone Test 1k News Org Tweets \n June 2016")




