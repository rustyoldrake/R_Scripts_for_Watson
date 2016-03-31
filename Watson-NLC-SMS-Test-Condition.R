######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services -
### Focus: Natural Language Classifier - R Programming Language Interface
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(audio) 
library(data.table)
library(dplyr)
library(reshape2)
library(Rtts)
library(splitstackshape)
library(seewave) # need to play wav back?
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML)
library(png)

######### Housekeeping And Authentication 

setwd("/Users/ryan/Documents/Service - Natural Language Classifier (NLC)") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below

username_password_NLC  # check you got this from NLC file
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
getURL(base_url_NLC,userpwd = username_password_NLC )  # non essential , but checks if working /authenticated

###### FUNCTION CREATE NEW CLASSIFIER - post /v1/classifiers - Creates a classifier with CSV data ## URL below no "/" after base url
watson.nlc.createnewclassifier <- function(file,classifiername) {
  return(POST(url="https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers",
         authenticate(username_NLC,password_NLC),
         body = list(training_data = upload_file(file),
                     training_metadata = paste("{\"language\":\"en\",\"name\":",classifiername,"}",sep="") 
         )))}
###### end of function

###### FUNCTION - CHECK CLASSIFIER STATUS
watson.nlc.checkclassifierstatus <- function(classifier_id) {
  return(
    getURL(paste(base_url_NLC,classifier_id,sep=""),userpwd = username_password_NLC)
  )
}
### end of function


###### FUNCTION - DELETE CLASSIFIER - Receives name of Classifier to Kill; May not be able to do this until training complete
watson.nlc.deleteclassifier <- function(kill_classifier) {
  DELETE(url=(paste(base_url_NLC,kill_classifier,sep="")),authenticate(username_NLC,password_NLC))
}
 
### end of function

###### FUNCTION: ACCEPT QUERY & RETURN RESULT: CLASSIFIER and % FROM TEXT INPUT AND PROCESS TO LOOK GOOD
watson.nlc.processtextreturnclass <- function(classifier_id,query_text){
    query_text <- URLencode(query_text)
    data <- getURL(paste(base_url_NLC,classifier_id,"/classify","?text=", query_text,sep=""),userpwd = username_password_NLC)
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
 
###### FUNCTION: LIST ALL CLASSIFIERS AND RETURN NEAT LIST
watson.nlc.listallclassifiers <- function(){ 
  data <- getURL(base_url_NLC,userpwd = username_password_NLC )
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

##### ACTION: EXECUTE FUNCTION  TO KILL (!!!) DELETE (!!!) CLASSIFIER - WARNING
watson.nlc.listallclassifiers()  # inventory - what do we want to delete - classifier id
# kill <- "563C46x19-nlc-XXX"
# watson.nlc.deleteclassifier(kill)
# watson.nlc.listallclassifiers()  # check it's gone


######################################################### END OF FUNCTION DECLARATIONS


######################################################### OK LETS DO STUFF


###### ACTION: Create a new CLassifier!  (200 = Good outcome) - 
thename <- "\"SMS-NLC-Opt-Out-Test-V1\""   
thefile <- "SMS-NLC-Opt-Out-Test-V1.csv" # 
# watson.nlc.createnewclassifier(thefile,thename)  # calls function, passes file and name from above, starts the magic. might take 2 to 20+ minutes depending on complexityclassifier_id" : "563C46x19-nlc-377",
# "classifier_id" : "beb9bfx46-nlc-XXX",
# "name" : "SMS-NLC-Opt-Out-Test-V1",
# "language" : "en",
# "created" : "2016-03-30T01:34:30.297Z",
# if new will say "not yet ready to accept classify requests" - once done in a few mintues will say
# "The classifier instance is now available and is ready to take classifier requests" - then you can submit query below

###### ACTION: Retrieve list of classifiers (NEAT VERSION) - oldest to newest
watson.nlc.listallclassifiers()  # not happy response if no classifiers (Blank) if blank, use below

#getURL(base_url_NLC,userpwd = username_password_NLC ) #not formatted, see below for formatting

classifierA <- "beb9bfx46-nlc-XXX" #       name  SMS-NLC-Opt-Out-Test-V1 
watson.nlc.checkclassifierstatus(classifierA)
query <- "I'm going to take Sally to the store for an ice cream cone"
watson.nlc.processtextreturnclass(classifierA,query)
 
query <- "please remove me from your distro"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "REMOVE - do not text"
watson.nlc.processtextreturnclass(classifierA,query)



#=========
### Simple test - no trasaction - simply asking how m
SMS_test <- matrix(0, ncol = 4, nrow = 20)  # Later just import from CSV. Col 1 input; col2 results
SMS_test[1] <- "hey - will be late for dinner"
SMS_test[2] <- "where are you?"
SMS_test[3] <- "did you walk dog?"
SMS_test[4] <- "where r u?"
SMS_test[5] <- "lol! "
SMS_test[6] <- "see you soon"
SMS_test[7] <- "unsubscribe"
SMS_test[8] <- "where is Jason?"
SMS_test[9] <- "remove me from SMS distro please"
SMS_test[10] <- "pick up some gluten free beer"
SMS_test[11] <- "your uber is about to arrive"
SMS_test[12] <- "can you call me back?"
SMS_test[13] <- "remove"
SMS_test[14] <- "STOP texting me!"
SMS_test[15] <- "dog ate homework"
SMS_test[16] <- "IBM watson amazing!"
SMS_test[17] <- "sorry"
SMS_test[18] <- "just arrived"
SMS_test[19] <- "this is a palendrome - sit on a potato pan otis"
SMS_test[20] <- "the quick brown fox jumped over the lazy cat"

dim(SMS_test)[1]

### Lets' test this baby - and time it!

for (i in 1:dim(SMS_test)[1]){
  ptm <- proc.time() # start timer for this i
  print(SMS_test[i])
  query <- SMS_test[i]
  response <- watson.nlc.processtextreturnclass(classifierA,query)
  elapsed <- proc.time() - ptm
  SMS_test[i,2] <- elapsed[3]  # store time to complete for this i
  SMS_test[i,3] <- response$class[1]
  SMS_test[i,4] <- response$confidence[1]
}

SMS_test <- data.frame(SMS_test)
setnames(SMS_test,c("SMS","response_time","class","confidence"))
SMS_test
SMS_test$response_time <- as.matrix(SMS_test$response_time)  # the long way around - but works
SMS_test$response_time <- as.numeric(SMS_test$response_time) # the long way around - but works
summary(SMS_test$response_time)
plot(SMS_test$response_time, main="NLC Response Time Test - SMS 20 samples \n Varied inputs - 10k ground truth")

write.csv(SMS_test,"SMS_test_results_V1.csv")



#### 100 sample test - same query each time
query <- "there once was a small dog that ran over my foot when it was nearing the barn.  I was surprised!"
SMS_test <- matrix(0, ncol = 2, nrow = 100)  #
for (i in 1:dim(SMS_test)[1]){
  ptm <- proc.time() # start timer for this i
  watson.nlc.processtextreturnclass(classifierB,query)
  elapsed <- proc.time() - ptm
  SMS_test[i,2] <- elapsed[3]  # store time to complete for this i
}
SMS_test <- data.frame(SMS_test)
setnames(SMS_test,c("SMS","response_time"))
SMS_test
SMS_test$response_time <- as.matrix(SMS_test$response_time)  # the long way around - but works
SMS_test$response_time <- as.numeric(SMS_test$response_time) # the long way around - but works
summary(SMS_test$response_time)
plot(SMS_test$response_time, main="NLC Response Time Test - 100 samples \n Identical input LONG UTTERANCE")

# SHORT UTTERANCE
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5560  0.6722  0.7535  0.8535  0.8970  1.8920 
# 
# LONG UTTERANCE
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5790  0.7082  0.8240  0.9149  0.9715  3.3150 

