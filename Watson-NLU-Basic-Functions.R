######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services - R. Anderson
### Focus: NLU - Natural Language Understanding
### https://www.ibm.com/watson/developercloud/natural-language-understanding.html
### Code is experimental / Please use with caution!
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)
library(stringr)
library(splitstackshape)


## Before you begin you will need (1) An IBM Bluemix demo account (2) BLuemix account https://console.ng.bluemix.net/
## and (3) Credentials to NLU Service /service and confirm you're able to CURL service with
## curl from https://www.ibm.com/watson/developercloud/natural-language-understanding/api/v1/


# PROBLEM > If you get this > Error in function (type, msg, asError = TRUE)  :  SSL certificate problem: self signed certificate in certificate chain 
# SOLUTION then you need to do this > YOU MUST KEEP THIS IN FOR below to work To begin: this line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),httpauth=AUTH_BASIC)) # NOTE - the "httpauth=AUTH_BASIC" piece gets rid of the "Error: UNAUTHORIZED" message 


######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Service - NLU")  # i'm on a mac
getwd()

source("keys.r") ## KEYS has acutal username:password for each IBM service. 

url_NLU="https://gateway.watsonplatform.net/natural-language-understanding/api"
username_NLU # check we got it from KEYs.R file in same directory - looks like this - "e63f524d-9999-9999-9999-e6b1d4e99b87"
password_NLU # check we got it from KEYs.R file in same directory - looks like this - "ABCD0EggCXYZ"
version="?version=2017-02-27"

## WORKS ## curl -G -u "6b4f8dae-xxxx-4344-xxxx-e8cac7572bf0":"21xxxxbxxxDS" -d "version=2017-02-27" -d "url=www.ibm.com" -d "features=keywords,entities" -d "entities.emotion=true" -d "entities.sentiment=true" -d "keywords.emotion=true" -d "keywords.sentiment=true" "https://gateway.watsonplatform.net/natural-language-understanding/api/v1/analyze"

#WORKS
POST(url="https://gateway.watsonplatform.net/natural-language-understanding/api/v1/analyze?version=2017-02-27&url=www.ibm.com&features=keywords,entities",
     authenticate(username_NLU,password_NLU),
     add_headers("Content-Type"="application/json")
)

#WORKS - PASS URL
response <- POST(url=paste(
              url_NLU,
              "/v1/analyze",
              version,
              "&url=www.ibm.com",
              "&features=keywords,entities",
              "&entities.emotion=true",
              "&entities.sentiment=true",
              "&keywords.emotion=true",
              "&keywords.sentiment=true",
              sep=""),
        authenticate(username_NLU,password_NLU),
        add_headers("Content-Type"="application/json")
     )

response
content(response)


#WORKS - PASS TEXT (url encoded)
raw_text <- "IBM is an American multinational technology company headquartered in Armonk, New York, United States, with operations in over 170 countries"
raw_text <- "I love ice cream and unicorns! I love Vancouver"
text <- URLencode(raw_text)

response <- POST(url=paste(
  url_NLU,
  "/v1/analyze",
  version,
  "&text=",text,
  "&features=keywords,entities",
  "&entities.emotion=true",
  "&entities.sentiment=true",
  "&keywords.emotion=true",
  "&keywords.sentiment=true",
  sep=""),
  authenticate(username_NLU,password_NLU),
  add_headers("Content-Type"="application/json")
)

response
response$headers
response$headers$date


## pull out response 
signal <- content(response)

signal$language #"en"
signal$keywords # [[8]]$emotion$joy [1] 0.041067
signal$entities # [[4]]$disambiguation$dbpedia_resource [1] "http://dbpedia.org/resource/United_States"

keywords <- signal$keywords
length(keywords)
for(i in 1:length(keywords))
{
  #print("hi")
  #print(keywords[[i]]$text)
  print(paste(keywords[[i]]$text," sentiment:",keywords[[i]]$sentiment$score))
}


# [1] "ice cream  sentiment: 0.599941"
# [1] "unicorns  sentiment: 0.599941"

######

entities <- signal$entities
for(i in 1:length(entities))
{
  #print("hi")
  print(entities[[i]]$text)
}


entities[[1]]$text
entities[[1]]$type
entities[[1]]$sentiment$score

# > entities[[1]]$text
# [1] "Vancouver"
# > entities[[1]]$type
# [1] "Location"
# > entities[[1]]$sentiment$score
# [1] 0.732329



####

### not complete.....  hope this helps though!



