###################################################### 
### Experimental Code. R Interface for IBM Watson Services -
### Focus: DIALOG - R Programming Language Interface
### Adapting the DIALOG APIs https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/ to RCURL and HTTR
### http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/dialog/
######################################################
### Before you begin you will need (1) An IBM Bluemix demo account (2) A dialog App and (3) Credentials to that Service and confirm you're able to CURL service with
### DOCS: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/dialog-apis.html
### REFERENCE - https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/dialog/api/v1/#get-dialogs

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(dplyr)
library(stringr)
setwd("/Users/ryan/Documents/Project Daisy")

#"DIALOG ORANGE SERVICE credentials": { UNBOUND
base_url = "https://gateway.watsonplatform.net/dialog-beta/api/v1" # no "/" on end 
username = "55555555-5555-5555-55555-55555555555"
password = "5555555555"
username_password = paste(username,":",password,sep="")
dialog_id=""

####### FUNCTION - New LIST APPS
watson.dialog.listapps <- function() 
{ data <- getURL(paste(base_url,"/dialogs",sep=""),userpwd = username_password )
  data <- as.data.frame(strsplit(as.character(data),"name"))
  data <- data[-c(1), ] # remove dud first row
  data <- data.frame(matrix(data)) # can tidy this method later - IF YOU HAVE MORE THAN ONE DIALOG - YOU"LL GET MULTIPLE HERE
  data <- strsplit(as.character(data$matrix.data),"dialog_id")
  data <- t(data.frame(data)) # can tidy this method later
  rownames(data) <- NULL
  # tidy up (there is a better way later)
  data[,1] <- str_replace_all(data[,1], "[[:punct:]]", "") 
  data[,2] <- gsub("-"," ",data[,2]) 
  data[,2] <- str_replace_all(data[,2], "[[:punct:]]", "") 
  data[,2] <- gsub(" ","-",data[,2]) 
  data <- data.frame(data)
  setnames(data,c("dialog_name","dialog_id"))
  return(data)
  }

####### FUNCTION - New Dialog - UPLOAD XML FILE
watson.dialog.uploadfile <- function(file,name) 
  { return(POST(url=paste(base_url,"/dialogs",sep=""),
              authenticate(username,password),
              body = list(file = upload_file(file),
                          name = name
              ) ))  }


####### FUNCTION - CONVERSE - Used to obtain a response from the system for a submitted input message. Also used to start new conversations
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

####### FUNCTION - DELTETE *ONE* DIALOG ID's - WORKING
watson.dialog.deletedialogID <- function(kill_dialogID) 
{ return( DELETE(url=paste(base_url,"/dialogs/",kill_dialogID,sep=""),userpwd = username_password)
)  }
### end of function declaration 

######## FUNCTION TO  DELETE ALL DIALOGS (ALL!) > delete /dialogs > Used to close an entire all Dialog applications associated with the service instance.
watson.dialog.deletedialogALL <- function() {
  return( DELETE(url="https://gateway.watsonplatform.net/dialog-beta/api/v1/dialogs",userpwd = username_password) )}
### end of function declaration 

################################    END OF FUNCTION DECLARATIONS #########
################################    END OF FUNCTION DECLARATIONS #########

################################    START THE PIZZA PARTY  #########

####### What Dialog ID's already exist?
watson.dialog.listapps()

###### TEST FUNCTION OK ACTION: Create a new Dialog!  (200 = Good outcome) - 
##### Where to get files and background?  here: http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/dialog/

##### https://github.com/watson-developer-cloud/dialog-tool/blob/master/dialogs/pizza_sample.xml  # not all XML created equal - careful
thefile <- "pizza_sample_gold.xml"   #you'll need a local, 'good' Dialog XML file, in the right directory,
thename <- "Pizza_Exercise_Gold3"
response <-  watson.dialog.uploadfile(thefile,thename)  # Pushes XML to dialog API - should return NEW dialog_id if OK
response #201?  if so, that's good
content(response, "text") # "{\"dialog_id\": 
#### end of test

watson.dialog.listapps() # do you see what i see?

### OK - let's extract the dialog ID so we can start a conversation  
data <- watson.dialog.listapps() # returns FULL LIST of them, could be up to 10
dialog_id <- tail(data$dialog_id,1) ### LETS TAKE THE LATEST LAST ONE IF MORE THAN ONE DIALOG_ID
dialog_id <- paste(dialog_id) # gets rid of levels
dialog_id

## ACTION - Let's start a conversation!  LET'S BEGIN
conv_id <- "" # needs to be blank to trigger new
cl_id <- "" # needs to be blank to trigger new
text_input <- "*" # "Hi" # doesnt really matter what our first input is 
response <-  watson.dialog.converse(dialog_id,conv_id,cl_id,text_input)  # first time through this should be blank (spawn a new one)
response
content(response, "text")
## OK NOW WE"RE COOKING (Pizza!)
## response\":[\"Hi, I'm Watson! I can help you order a pizza, what size would you like?\"]}"

### EXTRACT INFO FOR CONVERSATION ANCHOR - EXTRACT the conv Id and client id to CONTINUE - EXTRACT INFORMATION TO KEY OFF OF - Segue
data <- content(response,"text")
data <- as.data.frame(strsplit(as.character(data),'\"'))
setnames(data,c("V1"))
data$V1 <- str_replace_all(data$V1, "[[:punct:]]", "") 
data <- data[c(3,5,14), ] # cherry pick what we want - conv_id, client_id, and reponse
data <- data.frame(matrix(data)) # can tidy this method later
setnames(data,c("V1"))
conv_id <- toString(data$V1[1])
cl_id <- toString(data$V1[2])
watson_response <-toString(data$V1[3])
dialog_id # should still have this from opening round
###### OK - we've extracted key information - can continue dialog!
print(cat("This is your CONVERSATION ANCHOR:", "\n", "dialog_id = ", paste(dialog_id), "\n",
          "conversation_id = ", conv_id, "\n", "client_id = ", cl_id ,"\n", "watson_response = ",watson_response,"\n"))

#  watson_response =  Hi Im Watson I can help you order a pizza what size would you like 

## ACTION - Let's CONTINUE the conversation!
text_input <- "medeium" # spelling error intentional- dialog has some spelling skills
response <-  watson.dialog.converse(dialog_id,conv_id,cl_id,text_input)
#response
content(response, "text")
#"response\":[\"\",\"What toppings are you in the mood for? (Limit 4)\"]}"

text_input <- "ham and cheese and pineapple and pepperoni"
response <-  watson.dialog.converse(dialog_id,conv_id,cl_id,text_input)
content(response, "text")

#response\":[\"\",\"Do you prefer pickup or delivery?\"]}"
text_input <- "delivery please!"
response <-  watson.dialog.converse(dialog_id,conv_id,cl_id,text_input)
content(response, "text")


#response\":[\"Ok, I have one Medium canadian bacon, Cheese, Pineapple & Pepperoni pizza for delivery. Is that correct?\"]}"
text_input <- "yes - thank you"
response <-  watson.dialog.converse(dialog_id,conv_id,cl_id,text_input)
content(response, "text")

#"response\":[\"Sounds good! Your pizza will be ready in 20 minutes. Do you want to order another?\"]}"
text_input <- "no thank you"
response <-  watson.dialog.converse(dialog_id,conv_id,cl_id,text_input)
content(response, "text")
#"response\":[\"Okay, thank you for your order! Just let me know if you want to place another order.\"]}"

############# DANGER ZONE
############# DANGER ZONE

## EXECUTE FUNCTION TO DELETES ONE DIALOG_ID - Just one - CAREFUL
watson.dialog.listapps() # find one to kill
dialog_id 
watson.dialog.deletedialogID(dialog_id)
watson.dialog.listapps() # did we kill it? should be gone now
## working 9/15/2015

########## EXECUTE BE --  DANGER ---  DANGER ---   
watson.dialog.listapps() # we who are about to be deleted, salute you
watson.dialog.deletedialogALL()  ## CARFUL  - this is NUCLEAR Option - kills ALL! DANGER 200 Status = the job is done
watson.dialog.listapps() # did we kill it? should be gone now

#EOF

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

