######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services - R. Anderson
### Focus: Natural Language Classifier - R Programming Language Interface
### Adapting the NLC APIs https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/ to RCURL
### Code is experimental, not official release and has not been QA'd / Please use with caution!
### All harry potter characters and concepts are property of JKR / HP publisher
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

## Before you begin you will need (1) An IBM Bluemix demo account (2) An NLC App and Service up and running (see documents http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/nl-classifier/
## and (3) Credentials to that Service and confirm you're able to CURL service with
## curl -u "<username>":"<password>" "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers"

######### Housekeeping And Authentication 
setwd("/Users/ryan/Documents/Project Daisy")
base_url = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
username = "abc123-####-####-YOUR-CREDS-HERE" #### BE VERY CAREFUL TO understand "Instantiating Credentials" from bound service vs "Service Credentials"
password = "123456789ABC"  # you need your own ## if you are having authentication issues , may need the other creds.
username_password = paste(username,":",password)

## Next - let's create all the functions (but not actually execute them just yet)

###### FUNCTION: LIST ALL CLASSIFIERS AND RETURN NEAT LIST
watson.nlc.listallclassifiers <- function(){ 
    data <- getURL(base_url,userpwd = username_password )
    data <- as.data.frame(strsplit(as.character(data),"classifier_id"))
    data <- data[-c(1), ] # remove dud first row
    data <- data.frame(matrix(data))
    colnames(data) <- "V1"
    data$V1 <- gsub("[{}]","", data$V1)
    data$V1 <- gsub("]","", data$V1)
    data$V1 <- gsub("\"","", data$V1)
    data$V1 <- gsub("name:","", data$V1)
    data$V1 <- gsub(":","", data$V1)
    data <- cSplit(data, 'V1', sep=",", type.convert=FALSE)
    data[,c(2,4)] <- NULL
    data <- as.data.table(data)
    setnames(data,c("classifier","name","date_created"))
    data <- data[order(date_created),] 
return(data)
}


###### FUNCTION CREATE NEW CLASSIFIER - post /v1/classifiers - Creates a classifier with CSV data ## URL below no "/" after base url
watson.nlc.createnewclassifier <- function(file,classifiername) {
  return(POST(url="https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers",
         authenticate(username,password),
         body = list(training_data = upload_file(file),
                     training_metadata = paste("{\"language\":\"en\",\"name\":",classifiername,"}",sep="") 
         )))}
###### end of function


###### FUNCTION - CHECK CLASSIFIER STATUS
watson.nlc.checkclassifierstatus <- function(classifier_id) {
  return(
    getURL(paste(base_url,classifier_id,sep=""),userpwd = username_password)
  )
}
### end of function


###### FUNCTION - DELETE CLASSIFIER - Receives name of Classifier to Kill; May not be able to do this until training complete
watson.nlc.deleteclassifier <- function(kill_classifier) {
  return(DELETE(paste(base_url,kill_classifier,sep=""),
                userpwd = username_password)) }
### end of function


###### FUNCTION: ACCEPT QUERY & RETURN RESULT: CLASSIFIER and % FROM TEXT INPUT AND PROCESS TO LOOK GOOD
watson.nlc.processtextreturnclass <- function(classifier_id,query_text){
    query_text <- URLencode(query_text)
    data <- getURL(paste(base_url,classifier_id,"/classify","?text=", query_text,sep=""),userpwd = username_password)
    data <- as.data.frame(strsplit(as.character(data),"class_name"))
    data <- data[-c(1), ] # remove dud first row
    data <- gsub("[{}]","", data)
    data <- gsub("confidence","", data)
    data <- data.frame(matrix(data))
    setnames(data,("V1"))
    data$V1 <- gsub("\"","", data$V1)
    data$V1 <- gsub(":","", data$V1)
    data$V1 <- gsub("]","", data$V1)
    data <- cSplit(data, 'V1', sep=",", type.convert=FALSE)
    setnames(data,c("class","confidence"))
  return(data) }
### end of function


######################################################### END OF FUNCTION DECLARATIONS

######## OK - let's do stuff!

###### ACTION: Create a new CLassifier!  (200 = Good outcome) - 
thefile <- "sortinghat.csv"   #you'll need a local CSV file, in the right directory,  that is 'ground truth' to train the NLC.  
thename <- "\"sortinghat104\""   # what you want to call it - can be anything you like
watson.nlc.createnewclassifier(thefile,thename)  # calls function, passes file and name from above, starts the magic. might take 2 to 20+ minutes depending on complexity


###### ACTION: Retrieve list of classifiers (NEAT VERSION) - oldest to newest
watson.nlc.listallclassifiers()
# ALternate: messy version # getURL(base_url,userpwd = username_password ) #not formatted, see below for formatting


###### ACTION: CHECK CLASSIFIER STATUS (pick the one from bottom of list above, that's newest one you just made)
status <- "950DCB-nlc-###" 
watson.nlc.checkclassifierstatus(status)
## if new will say "not yet ready to accept classify requests" - once done in a few mintues will say
## "The classifier instance is now available and is ready to take classifier requests" - then you can submit query below


##################################################################
##### ACTION: LET'S GO!  SUBMIT TEXT AND CLASSIFY, RETURN CLASS / %
classifier = "950DCB-nlc-###" 

query = "I am brave and strong"
watson.nlc.processtextreturnclass(classifier,query)

query = "I am mean and nasty and love snakes"
watson.nlc.processtextreturnclass(classifier,query)

query = "I am clever and brainy"
watson.nlc.processtextreturnclass(classifier,query)

query = "I am a loyal friend"
watson.nlc.processtextreturnclass(classifier,query)


##################################################################


##### ACTION: EXECUTE FUNCTION  TO KILL (!!!) DELETE (!!!) CLASSIFIER - WARNING
kill <- "950DCB-nlc-###"
watson.nlc.deleteclassifier(kill)

## More NLC API DOCS here: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/natural-language-classifier/api/v1/#authentication


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
