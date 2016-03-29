
###################################################### 
### IBM Watson - Alchemy API - Calls Remaining check and latency check
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(stringr)

### Simple test - no trasaction - simply asking how m
test_count <- 20 # number of tests to run 
time <- matrix(0, ncol = 1, nrow = test_count)
for (i in 1:test_count){
ptm <- proc.time()
GET(url=paste("http://gateway-a.watsonplatform.net/calls/info/GetAPIKeyInfo?apikey=<<GET_YOUR_OWN_KEY>>&outputMode=json"))
temp <- proc.time() - ptm
time[i] <- temp[3]
}

time
summary(time)

# Response looks like this:
# Date: 2016-03-29 00:21
# Status: 200
# Content-Type: text/plain; charset=utf-8
# Size: 105 B
# {
# "status": "OK",
# "consumedDailyTransactions": "0",
# "dailyTransactionLimit": "100000"

###############################

## TESTING - Response on Basic Sentiment? - FROM URL - SO WILL TAKE LONGER BECAUSE OF URL RETRIEVE STEP
alchemy_url <- "http://gateway-a.watsonplatform.net/calls/url/"
api_feature <- "URLGetTextSentiment"
the_url <- "https://dreamtolearn.com/node/8OVIWY2C5RLZZSYD9OHCKM8AI/80"
api_key <- "<<GET_YOUR_OWN_KEY>>"  # alchemy API secret key 
output_mode <- "json"
query <- paste(alchemy_url,api_feature,"?url=",the_url,"&apikey=",api_key,"&outputMode=",output_mode, sep="")
query
POST(query)


time <- matrix(0, ncol = 1, nrow = test_count)
for (i in 1:test_count){
  ptm <- proc.time()
  response <- POST(query)
  temp <- proc.time() - ptm
  time[i] <- temp[3]
}

time
summary(time) # for me 
# Min.   :0.1850  
# 1st Qu.:0.1965  
# Median :0.2075  
# Mean   :0.3887  
# 3rd Qu.:0.6520  
# Max.   :0.8390 


##########################  MAIN TEST!

## TESTING - Response on Basic Sentiment? - FROM TEXT (provided - should be faster)
alchemy_url <- "http://gateway-a.watsonplatform.net/calls/text/"
api_feature <- "TextGetRankedKeywords"
api_key <- "<<GET_YOUR_OWN_KEY>>"  # alchemy API secret key 
output_mode <- "json"
text <- URLencode("I like pink ice cream and red boots")
query <- paste(alchemy_url,api_feature,"?text=",text,"&apikey=",api_key,"&outputMode=",output_mode, sep="")
query
POST(query)

test_count <- 1000
time <- matrix(0, ncol = 1, nrow = test_count)
for (i in 1:test_count){
  ptm <- proc.time()
  response <- POST(query)
  temp <- proc.time() - ptm
  time[i] <- temp[3]
}

time
summary(time) # for me - on my residential grade WIFI in the hills of East Bay - 96ms to 250ms
# Min.   :0.09600  
# 1st Qu.:0.09875  
# Median :0.10150  
# Mean   :0.13710  
# 3rd Qu.:0.19075  
# Max.   :0.25800 

## LETS PLOT:
plot(time[,1],main="Response Time - AlchemyAPI Text Keywords \n from SFO Residential Location with WIFI")
hist(time[,1],breaks=100,main="Response Time - AlchemyAPI Text Keywords \n from SFO Residential Location with WIFI")


########## what if we do XML (default, rather than JSON?) - seems pretty similar (removed test)
