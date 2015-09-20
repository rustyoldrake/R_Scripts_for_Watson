###################################################### 
### Experimental Code.  Experimental R Interface for IBM Watson Services - R Programming Language
### Focus: DIALOG - R Programming Language Interface
### Adapting the DIALOG APIs https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/ to RCURL and HTTR
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/dialog/
######################################################
### Before you begin you will need (1) An IBM Bluemix demo account (2) A dialog App and (3) Credentials to that Service and confirm you're able to CURL service with
### REFERENCE - https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/dialog/api/v1/#get-dialogs
 

####  WARNING - THIS IS A CODE SNIPPET TO ILLUSTRATE SYNTAX FOR "POST" TO API - IS IS NOT COMPLETE CODE 
####  WARNING - THIS IS A CODE SNIPPET TO ILLUSTRATE SYNTAX FOR "POST" TO API - IS IS NOT COMPLETE CODE 


setwd("/Users/ryan/Documents/Project Daisy")
library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)



###### FUNCTION - CONVERSE - Used to obtain a response from the system for a submitted input message. Also used to start new conversations if conv id blank
###### Note the use of "LIST" and NESTING THE LIST "SEGUE" into the BODY for POST  
watson.dialog.converse <- function(dialog_id, conv_id, cl_id, text_input) 
{
  segue <- list(
    conversation_id = conv_id,
    client_id = cl_id,
    input = text_input
  )
  return
  ( POST(url=paste(base_url,"/dialogs/",dialog_id,"/conversation",sep=""), 
         authenticate(username,password),
         body = segue, encode = "form")
  )}
### end of function 



#####  A short test - note 'dialog-beta' in URL, some docs and cases this should just be 'dialog'
base_url = "https://gateway.watsonplatform.net/dialog-beta/api/v1" 

username = "9a85e-NEED-TO-GET-YOUR-OWN-23666f4b8"  # Bluemix Portal for these credentials after you stand up Dialog Service
password = "555_YOUR_PW_555"  # Bluemix Portal for these credentials after you stand up Dialog Service
username_password = paste(username,":",password,sep="")

dialog_id="5555555-YOUR-DIALOG-555555555"  # you'll need to get this from another function that uploads XML to Dialog API - see main code body for this

#conv_id <- "13232" # for this example, assumes you've already started the conversation and had this info returned
conv_id <- "" # blank spawns new conversation, populated continues prior one

#cl_id <- "13336" # for this example, assumes you've already started the conversation and had this info returned
cl_id <- "" # blank spawns new conversation, populated continues prior one

text_input <- "large pizza"
#text_input <- "ham"
#text_input <- "delivery"

response <-  watson.dialog.converse(dialog_id,conv_id,cl_id,text_input)  # first time through this should be blank (spawn a new one)

response  #201 = good

content(response, "text") 
###  NEW: "{\"conversation_id\":13711,\"client_id\":13776,\"input\": \"delivery\",\"confidence\":0,\"response\":[\"Hi, I'm Watson! I can help you order a pizza, what size would you like?\"]}"
### "{\"conversation_id\":13232,\"client_id\":13336,\"input\": \"delivery\",\"confidence\":1.0,\"response\":[\"Ok, I have one Large canadian bacon pizza for delivery. Is that correct?\"]}"



Copyright 2015 Ryan Anderson 
Licensed under the Apache License, Version 2.0 (the "License"); 
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
