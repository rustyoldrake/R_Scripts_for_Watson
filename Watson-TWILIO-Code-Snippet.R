######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services -
### Focus: IBM Watson Tone Analyzer is a service that helps people comprehend, consume, and revise the language tone of their writing for more efficient communications 
### Focus: TWILIO - https://www.twilio.com/ - apps to communicate with the world
### https://github.com/rustyoldrake/R_Scripts_for_Watson - Ryan Anderson  - this is my test code.  Use at your own peril!  Representations do not necessarily represent the views of my employer
######################################################

library(RCurl) # the big one
library(rjson)
library(jsonlite)
library(XML)
library(httr)
library(rjson)

## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Service - Tone Analyzer")
getwd()
source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

## check we got FOUR ITEMS: authentication and phone numbers (From keys.r and from signing up for bluemix and twilio)
AccountSID_TWILIO # confirm SID is here from Keys. R file (you need to sign up to Twilio and get keys)
AuthToken_TWILIO # Token
NUMBER_FROM # check it's looking ok
NUMBER_TO # check it's looking ok

######## STEP #1 - Confirm Credentails work and API is responding happily
getURL("https://api.twilio.com/2010-04-01")  # working? - get's a response - generic/public
httpGET("https://api.twilio.com/2010-04-01")  # working?
GET("https://api.twilio.com/2010-04-01")  # working? yes - 200 response

ABC_authenticate_twilio <- paste("https://",AccountSID_TWILIO,":",AuthToken_TWILIO,"@api.twilio.com/2010-04-01/Accounts",sep="")
authenticate_response <- getURL(ABC_authenticate_twilio)
print(authenticate_response)  ## if this work, you should get a long string that contains something like "<FriendlyName>ryan###@gmail.com's Account

getURL(paste("https://api.twilio.com/2010-04-01/Accounts/",AccountSID_TWILIO,"/Messages.JSON",sep=""),userpwd = paste(AccountSID_TWILIO,":",AuthToken_TWILIO,sep=""))

GET(ABC_authenticate_twilio) # this work too? 200 yes?

######## STEP #2 - Let's send an SMS Text
ABC_post_twilio <- paste("https://",AccountSID_TWILIO,":",AuthToken_TWILIO,"@api.twilio.com/2010-04-01/Accounts/",AccountSID_TWILIO,"/Messages.XML",sep="")
ABC_post_twilio # look good?

########## THIS FUNCTION Receives query/message and also emotion and pushes out to SMS/MMS via twilio

send_SMS_results <- function(message,emotion)
{
  switch(emotion,
         "joy" = image_link <- "https://dreamtolearn.com/internal/doc-asset/251BS61FR6H557318H0CS0YLS/joy.jpg",
         "sadness" = image_link <- "https://dreamtolearn.com/internal/doc-asset/251BS61FR6H557318H0CS0YLS/sad.jpg",
         "anger" = image_link <- "https://dreamtolearn.com/internal/doc-asset/251BS61FR6H557318H0CS0YLS/anger.jpg",
         "disgust" = image_link <- "https://dreamtolearn.com/internal/doc-asset/251BS61FR6H557318H0CS0YLS/disgust.jpg",
         "fear" = image_link <- "https://dreamtolearn.com/internal/doc-asset/251BS61FR6H557318H0CS0YLS/fear.jpg"
        )  
POST(ABC_post_twilio, 
     body = list(
       From = NUMBER_FROM, 
       To = NUMBER_TO,
       Body = paste("MESSAGE ANALYZED: ",message,sep=""),
       MediaUrl = image_link
      ))
}

