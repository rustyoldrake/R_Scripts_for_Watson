######################################################
### IBM Watson - Code Snippet ---  CONVERSATION CODE SNIPPET2
### Experimental Code. R Interface for IBM Watson Services
### DOCS: http://www.ibm.com/watson/developercloud/doc/conversation/
### Before you begin you will need (1) An IBM Bluemix demo account (2) CONVERSATION Service tile created on bluemix with API credentials and 
###  (3) Create a basic conversation with Tooling provided in bluemix - see https://dreamtolearn.com/ryan/r_journey_to_watson/39 for examples
######################################################
 
library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(rjson)
#library(XML)

######### Housekeeping And Authentication
setwd("/Users/ryan/Documents/Service - Conversation") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. 

url_CONV="https://gateway.watsonplatform.net/conversation/api/v1"
version="?version=2016-07-11"
username_CONV # check we got it from KEYs.R file in same directory - looks like this - "e63f524d-9999-9999-9999-e6b1d4e99b87"
password_CONV # check we got it from KEYs.R file in same directory - looks like this - "ABCD0EggCXYZ"
workspace_CONV # check we got it from KEYs.R file - looks like this - "2ded4293-9999-9999-9999-4c8b1289be81" <- **** YOU NEED TO PULL THIS FROM BROWSER URL WHEN YOU TEST CONVO IN TOOLING - I THINK ONLY PLACE TO GET RIGHT NO

getwd()


### CAREFUL - THIS CODE IS IN PROGRESS - JUST TO GET SYNTAX RIGHT - NO FUNCTIONS OR OPTIMIZATION YET


## LEVEL 0 - INITIATE - FIRST CONTACT - blank utterance works to start thigns off (But 'coffee' will also hit NLC and return confidence)
response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                 authenticate(username_CONV,password_CONV),
                 add_headers("Content-Type"="application/json"),
                 body = '{ "input": { "text":""},
                           "system":{ "dialog_stack":["root"]},
                           "dialog_turn_counter":1,
                           "dialog_request_counter":1
                         }',
                 encode = "json"
                 )
response
response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
response_text


## RESPONSE [1] "{\"intents\":[],\"entities\":[],\"input\":{\"text\":\"\"},\"output\":{\"log_messages\":[],
# \"text\":[\"Welcome to the Tasty Helper!  \\nWhat are you looking for?\"],\"nodes_visited\":[\"node_6_1468893593485\"]},
# \"context\":{\"conversation_id\":\"1a598ffd-2b01-4330-923a-291a0cecc5f9\",
# \"system\":{\"dialog_stack\":[\"node_6_1468893593485\"],\"dialog_turn_counter\":1,\"dialog_request_counter\":1}}}"

## (!!) NOW WE NEED TO COPY/PASTE THE RETURNED NODE and ALSO THE CONVERSATION ID INTO SUBSEQUENT DIALOG - TO FOLLOW THREAD:

#### LEVEL 1 ENGAGE

response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                 authenticate(username_CONV,password_CONV),
                 add_headers("Content-Type"="application/json"),
                 body = '{"input":{"text":"coffee"},
                          "context":{"conversation_id":"1a598ffd-2b01-4330-923a-291a0cecc5f9",
                          "system":{"dialog_stack":["node_6_1468893593485"],
                          "dialog_turn_counter":1,"dialog_request_counter":1}}}'
)
response
response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
response_text

# RESPONSE [1] "{\"intents\":[{\"intent\":\"coffee\",\"confidence\":1}],\"entities\":[],\"input\":{\"text\":\"coffee\"},
# \"output\":{\"log_messages\":[],\"text\":[\"Coffee makes me very productive (level 1)\"],
# \"nodes_visited\":[\"node_12_1468894103975\"]},\"context\":{\"conversation_id\":\"1a598ffd-2b01-4330-923a-291a0cecc5f9\",
# \"system\":{\"dialog_stack\":[\"node_12_1468894103975\"],\"dialog_turn_counter\":2,\"dialog_request_counter\":2}}}"


#### LEVEL 2 CONTINUE CONV - take note how we need to continue to propogate CONV_ID and ALSO UPDATE DIALOG_STACK NODE to new level (!)

response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                 authenticate(username_CONV,password_CONV),
                 add_headers("Content-Type"="application/json"),
                 body = '{"input":{"text":"coffee"},
                          "context":{"conversation_id":"1a598ffd-2b01-4330-923a-291a0cecc5f9",
                          "system":{"dialog_stack":["node_12_1468894103975"],
                          "dialog_turn_counter":1,"dialog_request_counter":1}}}'
                  )
response
response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
response_text


# RESULTS _ [1] "{\"intents\":[{\"intent\":\"coffee\",\"confidence\":1}],\"entities\":[],\"input\":{\"text\":\"coffee\"},
#\"output\":{\"log_messages\":[],\"text\":[\"wow - two in a row - you must LOVE caffeine (level 2)\"],
#\"nodes_visited\":[\"node_6_1474949437344\"]},\"context\":{\"conversation_id\":\"1a598ffd-2b01-4330-923a-291a0cecc5f9\",
#\"system\":{\"dialog_stack\":[\"node_6_1474949437344\"],\"dialog_turn_counter\":2,\"dialog_request_counter\":2}}}"


##### LEVEL 3 DEEP INTO THE DIALOG TREE (and resets to beginning)

response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                 authenticate(username_CONV,password_CONV),
                 add_headers("Content-Type"="application/json"),
                 body = '{"input":{"text":"coffee"},
                          "context":{"conversation_id":"d8c4f16c-1165-4486-9f71-085e0578a587",
                          "system":{"dialog_stack":["node_6_1474949437344"],
                          "dialog_turn_counter":1,"dialog_request_counter":1}}}'
                   )
response
response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
response_text

### RESPONSE - [1] "{\"intents\":[{\"intent\":\"coffee\",\"confidence\":1}],\"entities\":[],\"input\":{\"text\":\"coffee\"},
#\"output\":{\"log_messages\":[],\"text\":[\"WOWOWOW Coffee (level 3)\",
#\"Welcome to the Tasty Helper!  \\nWhat are you looking for?\"],
#\"nodes_visited\":[\"node_7_1474949480138\",\"node_6_1468893593485\"]},
#\"context\":{\"conversation_id\":\"d8c4f16c-1165-4486-9f71-085e0578a587\",
#\"system\":{\"dialog_stack\":[\"node_6_1468893593485\"],\"dialog_turn_counter\":2,\"dialog_request_counter\":2}}}"
## BACK TO START OF LOOP / CYCLE (BY DESIGN)
