# How to get an IAM token by using a Watson service API key
# You can access IBM Watson™ service APIs by using the API keys that were generated
# in the service instance credentials. The API key is used to generate an IAM access token.
# You also use this process if you are developing an application that needs to work with 
# other IBM Cloud services.

## WATSON
library(RCurl) # General Network Client Interface for R
library(rjson) # JSON for R
library(jsonlite) # JSON parser
library(XML) # XML parser
library(httr) # Tools for URLs and HTTP

######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Service - Tone Analyzer")
getwd()

#source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
Sys.setlocale(locale="C") # error: input string 1 is invalid in this locale
options(warn=-1) # careful - turns off warnings  

### NEW TEST - IAM - Tone Analyzer : Tone Analyzer-Sydney-Koala
 
### RYAN TEST CURL (run this from terminal)
curl -v -X POST -u "apikey:su#################hbee" -H "Content-Type: text/plain" -H "Content-Language: en" -H "Accept-Language: en" -H "Accept: application/json" -H "Cache-Control: no-cache" -d 'Hello my name is richie. How are you.' "https://gateway-syd.watsonplatform.net/tone-analyzer/api/v3/tone?version=2018-05-19"



# {
#   "apikey": "##################-yhbee",
#   "iam_apikey_description": "Auto generated apikey during resource-key operation for Instance - crn:v1:bluemix:public:tone-analyzer:au-syd:a/e86e9##############-a3c5ae35dafc::",
#   "iam_apikey_name": "auto-generated-apikey-3##############7d6b4aeb99",
#   "iam_role_crn": "crn:v1:bluemix:public:iam::::serviceRole:Manager",
#   "iam_serviceid_crn": "crn:v1:bluemix:public:iam-identity::a/e86##########f25f92::serviceid:ServiceId-860f#############21e3f9",
#   "url": "https://gateway-syd.watsonplatform.net/tone-analyzer/api"
# }



process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway-syd.watsonplatform.net/tone-analyzer/api/v3/tone?version=2018-05-19",
                   authenticate("apikey",apikey),
                   add_headers("Content-Type"="text/plain",
                               "Content-Language" = "en", 
                               "Accept-Language" = "en", 
                               "Accept" = "application/json",
                               "Cache-Control" = "no-cache", 
                               "charset"="UTF-8"), 
                   body=text )
  
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  return(response_text)
}

"{\"document_tone\":{\"tones\":[{\"score\":0.98243,\"tone_id\":\"joy\",\"tone_name\":\"Joy\"}]}}"


##
latency_log <- data.frame(
  index = 0, 
  response_time = 0, 
  stringsAsFactors = FALSE
)
index <- 1

while(TRUE){
  start_time <- Sys.time()
  result <- process_data_to_tone("I am so happy and life is wonderful")
  time_to_complete = Sys.time() - start_time - 
  print(time_to_complete)
  latency_log[index,]$index <- index
  latency_log[index,]$response_time <- time_to_complete
  index = index + 1
  ##print(result)
  }


plot(latency_log, main = "Tone API Response Times")




###


### OLD METHOD -  - USername:Password
# BURNER CREDS - MAY 25 2018 - delete later # Sydney  - testing IBM - BUrner Credentials Tone Analyzer-Sydney-Kangaroo
# username_TON = "6f####################972"
# password_TON = "###########"
# 
# 
# process_data_to_tone <- function(text)
# {
#   response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19",
#                    authenticate(username_TON,password_TON),
#                    add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
#                    body=text )
#   
#   response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
#   return(response_text)
# }
# process_data_to_tone("I am so happy and life is wonderful")
# {\"score\":0.98243,\"tone_id\":\"joy\",\"tone_name\":\"Joy\"}


