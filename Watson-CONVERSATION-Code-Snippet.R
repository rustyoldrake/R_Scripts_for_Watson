######################################################
### IBM Watson - Code Snippet --- VERY EARLY CODE - API"S NOT FULLY AVAILALBLE YET
### Experimental Code. R Interface for IBM Watson Services
### DOCS: http://www.ibm.com/watson/developercloud/doc/conversation/
### Before you begin you will need (1) An IBM Bluemix demo account (2) CONVERSATION Service tile created on bluemix and (3) Credentials for service
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)

######### Housekeeping And Authentication
setwd("/Users/ryan/Documents/Service - Conversation") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. 

## Base URLs for IBM Watson APIs
base_url_CONV <- "https://gateway.watsonplatform.net/conversation/api/v1"
workspace_CONV  # will look something like "2ded4293-XXXX-4b24-b080-XXXXXXXXXX"

#### AS OF JULY 2016 - RIGHT AFTER LAUNCH - YOU NEED TO USE GUI / BLUEMIX TO LAUNCH UI/UX TOOLING FIRST 
# http://www.ibm.com/watson/developercloud/doc/conversation/convo_getstart.shtml

paste(base_url_CONV,"/workspaces/",workspace_CONV,"/message?version=2016-07-11",sep="")
# "https://gateway.watsonplatform.net/conversation/api/v1/workspaces/2ded4293-XXXX-4b24-b080-XXXXXXXXXX/message?version=2016-07-11"

#TEST 200
POST(url=paste(base_url_CONV,"/workspaces/",workspace_CONV,"/message?version=2016-07-11",sep=""),authenticate(username_CONV,password_CONV))


## OK - Lets put the response somewhere and take a look:
response <- POST(url=paste(base_url_CONV,"/workspaces/",workspace_CONV,"/message?version=2016-07-11",sep=""),
                 authenticate(username_CONV,password_CONV))

reply <- content(response)
reply$output$text[1]
# [1] "Welcome to the Tasty Helper!  \nWhat are you looking for?" 
# (THIS IS WHAT I CREATED WITH THE UX I LAUNCHED FROM TOOLING IN BLUEMIX
     

# HERE IS WHAT COMES BACK
# content(response)
#     
# $context
# $context$conversation_id
# [1] "c7a11109-672d-XXXX-a3f4-XXXXXXXXXXX"
# 
# $context$system
# $context$system$dialog_stack
# $context$system$dialog_stack[[1]]
# [1] "node_6_1468893593485"
# 
# 
# $context$system$dialog_turn_counter
# [1] 1
# 
# $context$system$dialog_request_counter
# [1] 1
# 
# 
# 
# $intents
# list()
# 
# $entities
# list()
# 
# $input
# named list()
# 
# $output
# $output$log_messages
# list()
# 
# $output$text
# $output$text[[1]]
# [1] "Welcome to the Tasty Helper!  \nWhat are you looking for?"
# 
# 
# $output$nodes_visited
# $output$nodes_visited[[1]]
# [1] "node_6_1468893593485"



### CODE INCOMPLETE - AWAITING MORE API FUNCTIONALITY 
### CODE INCOMPLETE - AWAITING MORE API FUNCTIONALITY 

# once Python / CURL examples of interacting from code - can engage more....
# https://watson-api-explorer.mybluemix.net/apis/conversation-v1#!/message/post_v1_workspaces_workspace_id_message
# http://www.ibm.com/watson/developercloud/conversation/api/v1/?node#send_input



## OK - Lets put the response somewhere and take a look:
response <- POST(url=paste(base_url_CONV,"/workspaces/",workspace_CONV,"/message?version=2016-07-11",sep=""),
                 authenticate(username_CONV,password_CONV),
                 add_headers("input"="text"), 
                 body="I like coffee" )
                 
content(response) #gives a 200, but not really useful yet.
