
##### PROTOYPE: What WINE should we pair with your Chef Watson meal?

###################################################### 
### IBM Watson - SPEECH TO TEXT > NLC > TEXT TO SPEECH - Code Snippet 
### Experimental Code. R Interface for IBM Watson Services 
### Ryan Anderson, Solutions Architect
### Focus: STT>NLC>TTS - R Programming Language Interface
### DOCS: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/speech-to-text/  http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/nl-classifier/  http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/text-to-speech.html
### Before you begin you will need (1) An IBM Bluemix demo account (2) STT/NLC and TTS Services stood up (3) Credentials to each Service 
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

######### Housekeeping And Authentication 

# Set working Directory
setwd("/Users/ryan/Documents/Project_Grapevine")
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below

username_password_STT = paste(username_STT,":",password_STT,sep="")
username_password_NLC = paste(username_NLC,":",password_NLC,sep="")
username_password_TTS = paste(username_TTS,":",password_TTS,sep="")

# Exaple of the old ways....
#username_ABC <-"xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxx" # get your own for STT service! :)
#password_ABC <- "abcdefghijk"   # get your own creds
#username_password_ABC = paste(username_ABC,":",password_ABC,sep="")

## Base URLs
base_url_STT <- "https://stream.watsonplatform.net/speech-to-text/api"
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
base_url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"

# For audio Sampling later
sample_count <- 64000  ## 64k fine for short - 128k for longer
sample_count <- 128000  ## 64k fine for short - 128k for longer
sample_rate <- 16000 ## at 16khz - is ~5 seconds recording time
classifier_name <- "c7fa49x23-nlc-10030" # YOU WILL NEED TO PUT YOUR OWN IN HERE - RETURNED FROM TRAINING ABOVE!    

########## FUNCTION DECLARATIONS  #### FUNCTION DECLARATIONS ###########

### STT FUNCTION to test connectivity and return models available
watson.STT.getmodels <- function()
{return(GET(url=paste(base_url_STT,"/v1/models",sep=""),
            authenticate(username_STT,password_STT)))} 
### FOR BEST RESULTS - USE USB HEADSET and ensure in MAc > Sys Preferences >Sound it's selected

## STT FUNCTION - Record!  
watson.STT.record <- function(samp_count,samp_rate)
{
  # record 8000 samples at 8000Hz (1 sec), mono (1 channel)
  # record 64k samples at 16kHz (4 sec), mono (1 channel), stereo = 2
  a <- record(samp_count, samp_rate, 2)
  wait(a) # wait for the recording to finish
  x <- a$data # get the result
  x[1:10] # show first ten samples
  close(a); rm(a) # you can close the instance at this point
  # amplify and crop the signal
  audio <- x * 2
  audio[audio < -1] <- -1
  audio[audio > 1] <- 1
  return(audio)
}


## STT AUDIO FUNCTION - Simple audio out test Function
watson.STT.tonetest <- function()
{
  wait(play(sin(1:10000/32)))
  wait(play(sin(1:10000/16)))
  wait(play(sin(1:10000/8)))
  wait(play(sin(1:10000/4)))
  wait(play(sin(1:10000/2)))
  wait(play(sin(1:10000/1)))
}
watson.STT.tonetest()


#### STT FUNCTION TO TIDY UP the STT response - just export the TRANSCRIPT ONLY
stt_transcript_only <- function(raw) 
{
  data <- as.data.frame(strsplit(as.character(raw),"\\n"))
  data <- data[c(7), ] # for now, grab just what we want
  data <- paste(data) # kill levels, - fyi this nukes confidence % info (may want later)
  data <- gsub("  ","",data) # remove excessive whitespace  0 cannot use ALL [[punct]] here
  data <- gsub("\\\\","",data) # remove punct we dont like
  data <- gsub("\"","",data) # remove punct we dont like
  data <- gsub("transcript","",data) # remove excessive whitespace
  data <- gsub(":","",data) # remove excessive whitespace - later: Improve this tidy step. 
  return(data) 
}


###### STT FUNCTION - ANalyze AUDIO WAV file with IBM Watson Speech to Text service - SESSIONLESS
watson.speech_to_text.recognize <- function(audio_file)
{ return(POST(url=paste(base_url_STT,"/v1/recognize",sep=""),
              authenticate(username_STT,password_STT),
              add_headers("Content-Type"="audio/wav"),
              body = (file = upload_file(audio_file))  
))} #works # hope this helps you with syntax!
## this is SESSIONLESS MODE - https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/#!/speech-to-text/recognizeSessionless


##### STT FUNCTION FOR SPEECH TO TEXT - RETURNS TRANSCRIPT - SESSIONLESS
watson.speech_to_text.sessionless <- function(file_name)
{ 
  wait(play(sin(1:5000/4)))  # recording START tone
  print("RECORDING ------------------ (beep) ")
  the_audio <- watson.STT.record(sample_count,sample_rate)
  wait(play(sin(1:5000/2)))  # recording STOP tone
  print("Recording COMPLETE --------- (beep) ")
  print("Saving WAV File")
  save.wave(the_audio,file_name) 
  print("Calling IBM Watson Speech To Text API")
  response <- watson.speech_to_text.recognize(file_name)
  wait(play(sin(1:2500/16)))
  wait(play(sin(1:2500/8)))
  return(stt_transcript_only(content(response,"text")))
} 

### NLC 101
getURL(base_url_NLC,userpwd = username_password_NLC ) 


###### NLC FUNCTION: LIST ALL CLASSIFIERS AND RETURN NEAT LIST
watson.NLC.listallclassifiers <- function(){ 
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

###### NLC FUNCTION: ACCEPT QUERY & RETURN RESULT: CLASSIFIER and % FROM TEXT INPUT AND PROCESS TO LOOK GOOD
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


###### NLC FUNCTION CREATE NEW CLASSIFIER - post /v1/classifiers - Creates a classifier with CSV data ## URL below no "/" after base url
watson.nlc.createnewclassifier <- function(file,classifiername) {
  return(POST(url="https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers",
              authenticate(username_NLC,password_NLC),
              body = list(training_data = upload_file(file),
                          training_metadata = paste("{\"language\":\"en\",\"name\":",classifiername,"}",sep="") 
              )))}

###### NLC FUNCTION - CHECK CLASSIFIER STATUS
watson.nlc.checkclassifierstatus <- function(classifier_id) {
  return(
    getURL(paste(base_url_NLC,classifier_id,sep=""),userpwd = username_password_NLC)
  )
}

###### NLC FUNCTION - DELETE CLASSIFIER - Receives name of Classifier to Kill; May not be able to do this until training complete
watson.nlc.deleteclassifier <- function(kill_classifier) {
  DELETE(url=(paste(base_url,kill_classifier,sep="")),authenticate(username_nlc,password_nlc))
}

###### NLC FUNCTION: ACCEPT QUERY & RETURN RESULT: CLASSIFIER and % FROM TEXT INPUT AND PROCESS TO LOOK GOOD
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

###### NLC FUNCTION: LIST ALL CLASSIFIERS AND RETURN NEAT LIST
watson.NLC.listallclassifiers <- function(){ 
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

##### NLC ACTION: EXECUTE FUNCTION  TO KILL (!!!) DELETE (!!!) CLASSIFIER - WARNING
# watson.NLC.listallclassifiers()  # inventory - what do we want to delete - classifier id
# kill <- "563C46x19-nlc-382"
# watson.nlc.deleteclassifier(kill)
# watson.NLC.listallclassifiers()  # check it's gone
## More NLC API DOCS here: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/natural-language-classifier/api/v1/#authentication


####### TTS Function to list voices
watson.TTS.listvoices <- function()
{
  voices <- GET(url=paste("https://stream.watsonplatform.net/text-to-speech/api/v1/voices"),authenticate(username_TTS,password_TTS))
  data <- content(voices,"text")
  data <- as.data.frame(strsplit(as.character(data),"name"))
  data <- data[-c(1:2), ] # remove dud first row
  data <- strsplit(as.character(data),",")
  data <- data.frame(matrix(data))
  colnames(data) <- "V1"
  data <- cSplit(data, 'V1', sep="\"", type.convert=FALSE)
  data <- data.frame(data$V1_04)
  data[,1]  <- gsub("\\\\","",data[,1] )
  return(data)
}
watson.TTS.listvoices()

########  TTS FUNCTION --- TEXT TO SPEECH
watson.TTS.execute <- function(url1,text1,voice1,filename1)
{
  the_audio = CFILE(filename1, mode="wb") 
  curlPerform(url = paste(url1,"?text=",text1,"&voice=",voice1,sep=""),
              userpwd = username_password_TTS,
              httpheader=c(accept="audio/wav"),
              writedata = the_audio@ref)
  close(the_audio)
  system(paste("open",filename1,"-a vlc"))
}


##### FULL PACKAGE FUNCTION - PULLING IT ALL TOGETHER
##### TAkes an AUDIO Sample -> Speech to Text -> NLC (Trained for wine, simple version) -> Get back wine recommend -> TTS AUdio recommend
sensemaking_full_process <- function(input_file)
{
  transcript <- watson.speech_to_text.sessionless(input_file)
  print(transcript)
  flavour <- watson.nlc.processtextreturnclass(classifier_name,transcript)
  print(flavour)
  wine_intent <- head(flavour$class,1)
  wine_intent <- gsub("_"," ", wine_intent) # this replaces any underscores in the class name with a space (so we dont read out "underscore" in speech)
  wine_intent <- paste("Terrific!  I think a ",
                       wine_intent,
                       " would go nicely with your meal. Tell me, would you like some suggestions for bottles of ", 
                       wine_intent,"?",sep="")
  print(wine_intent)
  
  the_url <- paste(base_url_TTS,
                   "?text=",
                   URLencode(wine_intent),"&voice=en-GB_KateVoice", 
                   sep="")
  print(the_url)
  
  ## here we receive the audio back 
  # use Kate because British people sound smart :-)
  the_audio = CFILE("wine_intent.wav", mode="wb") 
  curlPerform(url = the_url, 
              userpwd = username_password_TTS,
              httpheader=c(accept="audio/wav"),
              writedata = the_audio@ref)
  close(the_audio)
  system("open wine_intent.wav -a vlc")  # Now - Let's listen 
  
}


##### END OF FUNCTION DECLARATIONS



#######################    OK - LETS DO STUFF   ##################################

###### ACTION: Create a new CLassifier!  (200 = Good outcome) - 
thename <- "\"NLC-WINE1\""   
thefile <- "watson-wine-gt.csv"  
watson.nlc.createnewclassifier(thefile,thename)  # lets BUILD NLC classifier 
# calls function, passes file and name from above, starts the magic. might take 2 to 20+ minutes depending on complexityclassifier_id" : "563C46x19-nlc-377",

#"classifier_id" : "c7fa49x23-nlc-xxxxx",
#"name" : "NLC-WINE1",
#"language" : "en",
#"created" : "2016-02-25T08:40:35.160Z",
# "url" : "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/c7fa49x23-nlc-xxxxx",

# if new will say "not yet ready to accept classify requests" - once done in a few mintues will say
# "The classifier instance is now available and is ready to take classifier requests" - then you can submit query below


###### ACTION: Retrieve list of classifiers (NEAT VERSION) - oldest to newest
watson.NLC.listallclassifiers()  # not happy response if no classifiers (Blank) if blank, use below
#getURL(base_url,userpwd = username_password ) #not formatted, see below for formatting

classifier_name <- "c7fa49x23-nlc-xxxxxx" # YOU WILL NEED TO PUT YOUR OWN IN HERE - RETURNED FROM TRAINING ABOVE!    
watson.nlc.checkclassifierstatus(classifier_name) # Is it Ready Yet?

query <- "I'd like something to drink with my greasy burger and fries"
watson.nlc.processtextreturnclass(classifier_name,query)

query <- "Happy birthday to me!"
watson.nlc.processtextreturnclass(classifier_name,query)

query <- "big T-bone steak and garlic bread"
watson.nlc.processtextreturnclass(classifier_name,query)


#### Text to Speech Test
voices <- watson.TTS.listvoices()
voices
#1        ja-JP_EmiVoice
#2    en-US_AllisonVoice  # voices[2,]  #  "en-US_AllisonVoice"
#3      fr-FR_ReneeVoice
#4  it-IT_FrancescaVoice
#5      es-ES_LauraVoice
#6     de-DE_BirgitVoice
#7    es-ES_EnriqueVoice
#8     de-DE_DieterVoice
#9       en-US_LisaVoice
#10      en-GB_KateVoice
#11   en-US_MichaelVoice
#12     es-US_SofiaVoice

watson.STT.tonetest()  ## beeps? lucky you!

watson.STT.getmodels() # returns list of 10+ voice models ##### works

### RECORD AUDIO - GET A TRANSCRIPT!
watson.speech_to_text.sessionless("test_file1.wav") # the_audio.wav is boutput

#### SENSEMAKING - TALK - SPeech to Text > NLC > INTENT > Text to Speech Confirm

##### EXECUTE THE FULL PACKAGE - When it beeps - tell it what kind of wine you'd like
##### EXECUTE THE FULL PACKAGE - When it beeps - tell it what kind of wine you'd like
#sample_count <- 64000  ## 64k fine for short (~ 3 seconds)
sample_count <- 128000  ## 64k fine for short - 128k for longer sample duration (6 seconds)
sensemaking_full_process("wine_intent.wav")



# this is not super fast (~3 seconds) but good POC and good forprotytypes  - ENJOY!




########## END OF CODE

########## LICENSE Licensed under the Apache License, Version 2.0 (the "License");  you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.

