###################################################### 
#  The IBM Watson™ Discovery Service is a cognitive search and content analytics engine that you can add to applications to 
#  identify patterns, trends and actionable insights to drive better decision-making. Securely unify structured and 
#  unstructured data with pre-enriched content, and use a simplified query language to eliminate the need for manual filtering of results.
#  https://www.ibm.com/watson/developercloud/discovery/api/v1/curl.html?curl#authentication RRAA
#  https://gateway.watsonplatform.net/discovery/api
######################################################

library(RCurl) 
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(dplyr)
library(stringr)

######### Housekeeping And Authentication 
getwd()
setwd("/Users/ryan/Documents/Service - Discovery News")

# KEYS - proper & safe way
# source("keys.r") ## KEYS has acutal username:password for each IBM service. 

# KEYS - hacky quick way (Service Name: Discovery-Gemstone)
username_DISCO <- "555-GET-YOUR-CREDENTIALS-IBM-CLOUD-555"
password_DISCO <- "555444333222"
#username_password_DISCO <- paste(username_DISCO,password_DISCO,sep="")
url_DISCO <- "https://gateway.watsonplatform.net/discovery/api"
version_DISCO <- "?version=2018-03-05"


## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
Sys.setlocale(locale="C") # error: input string 1 is invalid in this locale
options(warn=-1) # careful - turns off warnings


# GETTING STARTED
# https://console.bluemix.net/docs/services/discovery/getting-started.html#getting-started-with-the-api
# we're going to use the old authentication method - not IAM for now


# 1 ############ CREATE CREATE  CREATE  ############# CREATE CREATE  CREATE - WORKS (as CURL)
# curl -X POST -u "9bb5555555555555555aec":"555555555555" -H "Content-Type: application/json" -d '{ "name":"ryans-second-environment", "description":"exploring ryan environments"}' "https://gateway.watsonplatform.net/discovery/api/v1/environments?version=2018-03-05"
#   "environment_id" : "b6154398-5555-5555-55555-7becd9c7af55", #   "name" : "ryans-first-environment",
#   "description" : "exploring ryan environments", #   "status" : "active", 
#   etc..

# Works!
response <- POST(url=URLencode(paste(url_DISCO,"/v1/environments",version_DISCO,sep="")),
                 authenticate(username_DISCO,password_DISCO),
                 add_headers("Content-Type"="application/json"),
                 body = '{ "name":  "newname6",
                 "description": "description6"
                 }',
                 encode = "json"
)
response



# 2 ###### LIST / CHECK / VERIFY API IS AWAKE AND GET THE ENVIRONMENT_ID
# CHECK EXISTS/OK - AS CURL (OK)
# curl -u "9bbd9cc6-5555-5555-55555-f83a013d0aec":"jkWel2ZuTCDy" "https://gateway.watsonplatform.net/discovery/api/v1/environments/78b9e17c-5555-5555-5555-313982c0d593?version=2018-03-05"
# GET WORKS HERE TOO (OK)

GET(paste(url_DISCO,"/v1/environments/",version_DISCO,sep=""),
    authenticate("9bbd9cc6-5555-5555-55555-f83a013d0aec","55555555555")
)

# GET WORKS HERE TOO # "LIST" What's available after creation using "GET" 
response <- GET(url=URLencode(paste(url_DISCO,"/v1/environments",version_DISCO,sep="")),
                 authenticate(username_DISCO,password_DISCO),
                 add_headers("Content-Type"="application/json"))

response
response_text <- content(response)
response_text
#response_text$environments[[2]]$environment_id  ## NO NO NO - here's the problem - use [[1]] for default SYSTEM news id  - not 2 - which is empty

response_text$environments[[1]]$environment_id  


# ENVIRONMENT ID - once we have it - store - [[2]] is your own [[1]] is the 'ready to go enriched news'
environment_ID_DISCO <- response_text$environments[[1]]$environment_id
environment_ID_DISCO # check we have it


# 3a ##### CREATE COLLECTION ID
# To find your default configuration_id, use the GET /v1/environments/{environment_id}/configurations method.
# Replace {username}, {password}, and {environment_id} with your information:


# GET WORKS HERE TOO # "LIST" What's available after creation using "GET" 
response <- GET(url=URLencode(paste(url_DISCO,"/v1/environments/",
                                    environment_ID_DISCO,
                                    "/configurations",
                                    version_DISCO,
                                    sep="")),
                authenticate(username_DISCO,password_DISCO),
                add_headers("Content-Type"="application/json")
                )
response
#  Status: 200
#  "configuration_id": "947f143d-5555-5555-5555-55555df9ba",

response_text <- content(response)  # empty if using SYSTEM
response_text
# response_text$configurations[[1]]$configuration_id
configuration_ID_DISCO <- response_text$configurations[[1]]$configuration_id
configuration_ID_DISCO # check we have it


# 3b Use the POST /v1/environments/{environment_id}/collections method to create a collection called my-first-collection.
# Replace {username}, {password}, {environment_id} and {configuration_id} with your information:
# curl -X POST -u "{username}":"{password}" -H "Content-Type: application/json" 
# -d '{"name": "my-first-collection", "description": "exploring collections", "configuration_id":"{configuration_id}" ,"language": "en_us"}' 
# https://gateway.watsonplatform.net/discovery/api/v1/environments/{environment_id}/collections?version=2017-11-07

# TO FIX - MANUAL CONFIG ID
response <- POST(url=URLencode(paste(url_DISCO,"/v1/environments/",
                                    environment_ID_DISCO,
                                    "/collections",
                                    version_DISCO,
                                    sep="")),
                authenticate(username_DISCO,password_DISCO),
                add_headers("Content-Type"="application/json"),
                  body = '{ "name":  "my-seventh-collection",
                            "description": "exploringcollections6",
                             "configuration_id":  "47e7d65c-5555-5555-5555-94984451af1e",  
                             "language":  "en_us"

                          }',
                  encode = "json"
                )

## CAREFUL - configuration_ID is hard wired
response

response_text <- content(response)
response_text
response_text$collection_id

collection_ID_DISCO <- response_text$collection_id
collection_ID_DISCO # check we have it

### 3c - INVENTORY ALL THE STUFF WE HAVE NOW
environment_ID_DISCO
configuration_ID_DISCO
collection_ID_DISCO
version_DISCO

# curl -u "{username}":"{password}" https://gateway.watsonplatform.net/discovery/api/v1/environments/{environment_id}/collections/{collection_id}?version=2017-11-07
# curl -u "9bbd9cc6-5555-5555-5555-f83a013d0aec":"55555555" "https://gateway.watsonplatform.net/discovery/api/v1/environments/858ba150-5555-5555-5555-b532477a3fb0/fields?collection_ids=dc6601ef-5555-5555-5555-519621bb2bf3&version=2018-03-05"
# above comes up EMPTY because not any UNIQUE fields

# 4 - LEt's try to get some data (?!?!) ###### USE AND TRY ENVIRONMENT
# https://console.bluemix.net/docs/services/discovery/watson-discovery-news.html#watson-discovery-news
# IBM Watson™ Discovery News is included with Discovery. Watson Discovery News is an indexed dataset that is pre-enriched with the following cognitive insights: Keyword Extraction, Entity Extraction, Semantic Role Extraction, Sentiment Analysis, Relation Extraction, and Category Classification
# https://console.bluemix.net/docs/services/discovery/using.html#querying-news



# WORKS IN BROWSER (sort of) - GENERIC SYSTEM for NEWS ENRICHED  - LIST
#https://gateway.watsonplatform.net/discovery/api/v1/environments/system/collections/?query=&version=2018-03-05

# WORKS WELL IN BROWSER
#https://gateway.watsonplatform.net/discovery/api/v1/environments/system/collections/news-en/query?version=2018-03-05&count=3&return=text

#query <- "&query=enriched_text.entities.text:IBM'"
# query <- "&query=&count=15&filter=entities.text:IBM&return=text"
collections/news-en/query?version=2018-03-05&count=3&filter=entities.text:IBM&return=text

count = 6
#topic = "twilio"
topic = "IBM"
query <- paste("query", version_DISCO, "&count=",count,"&filter=text:",topic,"&return=text",sep="")
query

response <- GET(url=URLencode(paste(url_DISCO,"/v1/environments/",environment_ID_DISCO,
                                    "/collections/news-en/",
                                    query,
                                    sep="")),
                authenticate(username_DISCO,password_DISCO),
                add_headers("Content-Type"="application/json"))

response
response_text <- content(response)

#length(response_text$results)

# Loop and print all of them
for (i in 1:length(response_text$results)){
  #print(i)
  print(paste("[", i,"]", response_text$results[[i]]$text))
}

# SAMPLE OF THE RESPONSE
# [1] "[ 1 ] Innovate How To Prepare Your Organization For Innovation It's always better to prepare than adapt. By the time you realize you need to adapt, it might very well be too late. @Digitaltonto CREDIT: Getty Images In 1919, Mahatma Gandhi initiated a campaign of civil disobedience, including the sale of banned"
# [1] "[ 2 ] Data Wahrehousing Google Cloud DataFlow ETL Project Need a freelancer to build an ETL process using Google Cloud DataFlow (Apache Beam) project. The Pipeline would begin with an ERP Source residing in IBM DB2 or MSFT SQL Server and would end in a Data Warehouse in Google BigQuery. It would"
# [1] "[ 3 ] 0 Where do data scientists come from? They don't all have PhDs, and hail from a wide variety of fields of study, levels of education, and prior jobs. That's the conclusion of Chris Lindner, a product scientist at Indeed, who recently looked at piles of resumes that his firm processes."
# [1] "[ 4 ] Actionable Conclusions (1-10): Brokers Predicted 9.1% To 21.5% Net Gains For Ten Dow Dogs By September, 2019 Three of ten top dividend-yielding Dow dogs were verified as being among the top ten gainers for the coming year based on analyst 1-year target prices. (They are tinted gray in the chart"
# [1] "[ 5 ] Share On Facebook IBM Lotus Notes and Microsoft Outlook are two of the most popular email client programs, and both are installed on a large scale in businesses worldwide. Because they are so popular, it makes it likely that information will be imported from one program to the other. But"
# [1] "[ 6 ] Advertisement Topics Track topics on Twitter Track topics that are important to you Printed From BioPortfolio.com Homeland Security and Emergency Management Market by End Use Law Enforcement Intelligence Gathering, Cyber Security, Critical Infrastructure Security, Risk Emergency Services, Border Security, CBRNE, System, and Region Global Forecast to 2023 [Report U 10:36"



# 5 ##### DELETE ENVIRONMENT##### DELETE ENVIRONMENT##### DELETE ENVIRONMENT  (if you built your own)
# (Curl works) # curl -X DELETE "https://gateway.watsonplatform.net/discovery/api/v1/environments/78b9e17c-5555-55555-55555-313982c0d593?version=2018-03-05" -u "9bbd9cc6-55555-55555-55555-f83a013d0aec":"5555555"
# "status" : "deleted"

response <- DELETE(url=URLencode(paste(url_DISCO,"/v1/environments/",environment_ID_DISCO,version_DISCO,sep="")),
                 authenticate(username_DISCO,password_DISCO),
                 add_headers("Content-Type"="application/json")
)
response # should say status DELETED if works



