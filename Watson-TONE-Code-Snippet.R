######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services -
### Focus: IBM Watson Tone Analyzer is a service that helps people comprehend, consume, and revise the language tone of their writing for more efficient communications 
### https://tone-analyzer-demo.mybluemix.net/
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/tone-analyzer/
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/tone-analyzer-apis.html
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)


######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Service - Tone Analyzer")
getwd()
source("keys.R")

base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer-beta/api"

### GET - Basic Test 
getURL("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11&text=hello",userpwd = username_password_TON ) 

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
  data
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),2)
  setnames(data,c("trait","signal"))
  return(data)
}


### POST - Basic Test
process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11",
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )

  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  abc <- tidyResponse(response_text)
  return(abc)
}
process_data_to_tone("happy and weird")
  
###  OK!  LET'S DO STUFF
###  OK!  LET'S DO STUFF
###  OK!  LET'S DO STUFF


query <- "I really love to eat stew in the winter because it is delicious - of this I am sure!"
analysis <- process_data_to_tone(query)
analysis
#                 trait signal
# 1              anger   0.13
# 2            disgust   0.11
# 3               fear   0.08
# 4                joy   0.44
# 5            sadness   0.13
# 6         analytical   0.31
# 7          confident   0.57
# 8          tentative   0.00
# 9           openness   0.12
# 10 conscientiousness   0.35
# 11      extraversion   0.87
# 12     agreeableness   0.80
# 13       neuroticism   0.88



### ROUGH PLOTS FOR NOW - MORE LATER
par(mfrow=c(2, 2))

barplot(analysis$signal, main="Tone Analysis \n Emotion, Language and Social-Big-5", 
        xlab="", ylab="Tone Signal Amplitude", 
        names.arg=analysis$trait,
        border="red",las=3) 

slices <- analysis$signal[1:5]
lbls <- analysis$trait[1:5]
pie(slices, labels = lbls, main="Tone Analysis of Text \n EMOTION")

slices <- analysis$signal[6:8]
lbls <- analysis$trait[6:8]
pie(slices, labels = lbls, main="Tone Analysis of Text \n LANGUAGE")

slices <- analysis$signal[9:13]
lbls <- analysis$trait[9:13]
pie(slices, labels = lbls, main="Tone Analysis of Text \n SOCIAL-BIG-5")

### end of rough plot



### POST - Basic Test
response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11",
                 authenticate(username_TON,password_TON),
                 add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                 body="happy but uncertain" )

response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
response_text
abc <- tidyResponse(response_text)
print(abc)

#                 trait  signal
# 1              anger   0.05
# 2            disgust   0.03
# 3               fear   0.02
# 4                joy   0.18
# 5            sadness   0.06
# 6         analytical   0.99
# 7          confident   0.00
# 8          tentative   0.99
# 9           openness   0.03
# 10 conscientiousness   0.00
# 11      extraversion   0.91
# 12     agreeableness   0.43
# 13       neuroticism   1.00

##########

