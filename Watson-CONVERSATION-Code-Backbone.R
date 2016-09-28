######################################################
### IBM Watson - Code Snippet ---  CONVERSATION CODE Backbone
### Experimental Code. R Interface for IBM Watson Services
### DOCS: http://www.ibm.com/watson/developercloud/doc/conversation/
### Before you begin you will need (1) An IBM Bluemix demo account (2) CONVERSATION Service tile created on bluemix with API credentials and 
###  (3) Create a basic conversation with Tooling provided in bluemix - see https://dreamtolearn.com/ryan/r_journey_to_watson/39 for examples
#####################################################

#library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(splitstackshape) # for the text to colummns 
library(reshape2)
#library(rjson)

######### Housekeeping And Authentication
setwd("/Users/ryan/Documents/Service - Conversation") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. 

url_CONV="https://gateway.watsonplatform.net/conversation/api/v1"
version="?version=2016-07-11"
username_CONV # check we got it from KEYs.R file in same directory - looks like this - "e63f524d-9999-9999-9999-e6b1d4e99b87"
password_CONV # check we got it from KEYs.R file in same directory - looks like this - "ABCD0EggCXYZ"
workspace_CONV # check we got it from KEYs.R file - looks like this - "2ded4293-9999-9999-9999-4c8b1289be81" <- **** YOU NEED TO PULL THIS FROM BROWSER URL WHEN YOU TEST CONVO IN TOOLING - I THINK ONLY PLACE TO GET RIGHT NO

###########  FUNCTION DECLARATIONS ################

## FUNCTION DECLARE - INITIATE CONVERSATION WITH SERVICE
conversation.init <- function()  
    {
    raw_response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                 authenticate(username_CONV,password_CONV),
                 add_headers("Content-Type"="application/json"),
                 body = '{ "input": { "text":""},
                           "system":{ "dialog_stack":["root"]},
                           "dialog_turn_counter":1,
                           "dialog_request_counter":1
                         }',
                 encode = "json"
                 )
                return(content(raw_response, "text", encoding = "UTF-8"))
}


## FUNCTION DECLARE - GO DEEPER INTO CONVERSATION
conversation.chat <- function(dialog,conversation_id,dialog_stack_node)  #### LEVEL 1 ENGAGE
{
  the_body <-  paste('{"input":{"text":"',
                     dialog,
                     '"},"context":{"conversation_id":"',
                     conversation_id,
                     '","system":{"dialog_stack":["',
                     dialog_stack_node,
                     '"],',
                     '"dialog_turn_counter":1,"dialog_request_counter":1}}}',
                     sep="")
  
  raw_response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                       authenticate(username_CONV,password_CONV),
                       add_headers("Content-Type"="application/json"),
                       body = the_body
  )
  return(content(raw_response, "text", encoding = "UTF-8"))
}


## FUNCTION DECLARE - ORGANIZE RESPONSES
response.process <- function(data)  
{
    data <- response
    data <- as.data.frame(strsplit(as.character(data),","))
    data[,1]  <- gsub("\"","",data[,1] ) 
    data[,1]  <- gsub("\\[", "", data[,1] ) 
    data[,1]  <- gsub("\\]", "", data[,1] ) 
    data[,1]  <- gsub("\\{", "", data[,1] ) 
    data[,1]  <- gsub("\\}", "", data[,1] ) # yes, paint by numbers, but clear what we're hacking :)
    data[,1]  <- gsub("\\\\n", "", data[,1] )  #data$V1_2  <- gsub("\\\\n", "", data$V1_2)  ## removes NEWLINES 
    data <- data.frame(data)
    data <- setNames(data,c("V1"))
    data <- cSplit(data, "V1", sep=":")
    data <- setNames(data,c("element","X1","X2"))
    return(data)
}
########### END OF FUNCTION DECLARE



########### MAIN CODE
# ** START HERE ** INITIALIZATION AND GET HOOKS 
response <- conversation.init()
temp <- response.process(response)
text <- paste(temp$X1[5])  # system's response text
conv_id <- paste(temp$X2[7]) # CONVERSATION ID - our main 'hook' into this thread- should not change
dial_stack <- paste(temp$X2[8]) # DIALOG STACK - our first stepping stone / node
#text

# ** CONTINUE CONVERSATION
repeat{
  chat <- readline(prompt="Enter response: ")
  response <- conversation.chat(chat,conv_id,dial_stack)
  print(response)
  temp <- response.process(response)
  text <- paste(temp$X1[6])  # system's response text
  print(text)
  conv_id <- paste(temp$X2[8]) # CONVERSATION ID this should not change if all going well. but just in case
  dial_stack <- paste(temp$X2[9])  # DIALOG STACK - NEXT NODE this is next node - need to hold onto this
  if(conv_id=='NA'){
    print("end")
    break
    }
}
