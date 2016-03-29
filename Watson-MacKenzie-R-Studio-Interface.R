
###################################################### 
### IBM Watson - SPEECH TO TEXT > NLC > TEXT TO SPEECH - Code Snippet 
### MacKenzie Speech to Text Voice Recognition Interface for "R" and R-Studio
### Experimental Code. R Interface for IBM Watson Services 
### Focus: STT>NLC>TTS - R Programming Language Interface
### DOCS: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/speech-to-text/
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
library(png)

######### Housekeeping And Authentication 

setwd("/Users/ryan/Documents/Project_Mackenzie") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below

# username_password_STT = paste(username_STT,":",password_STT,sep="")
# username_password_NLC = paste(username_NLC,":",password_NLC,sep="")
# username_password_TTS = paste(username_TTS,":",password_TTS,sep="")

## Base URLs for IBM Watson APIs
base_url_STT <- "https://stream.watsonplatform.net/speech-to-text/api"
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
base_url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"

getURL(base_url_NLC,userpwd = username_password_NLC )  # non essential , but checks if working /authenticated

# For audio Sampling later
sample_count <- 32000  ## 0k samples
sample_rate <- 16000 ## at 16khz - is ~5 seconds recording time
classifier_name <- "668877x36-nlc-192" # YOU WILL NEED TO PUT YOUR OWN IN HERE - RETURNED FROM TRAINING ABOVE!    


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
  wait(play(sin(1:2500/2)))  # recording STOP tone
  print("Recording COMPLETE --------- (beep) ")
  print("Saving WAV File")
  save.wave(the_audio,file_name) 
  print("Calling IBM Watson Speech To Text API")
  response <- watson.speech_to_text.recognize(file_name)
  wait(play(sin(1:2500/16)))
  wait(play(sin(1:2500/8)))
  return(stt_transcript_only(content(response,"text")))
} 


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
    getURL(paste(base_url_NLC,classifier_id,sep=""),userpwd = username_password_NLC) )}


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
  return(data) }


watson.NLC.listallclassifiers()  # check it's gone
## More NLC API DOCS here: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/natural-language-classifier/api/v1/#authentication
#1: c7fa49x23-nlc-9960     name  NLC-coffee created  2016-02-24T201354.851Z
#2: c7fa49x23-nlc-10030      name  NLC-WINE1 created  2016-02-25T084035.160Z
#3: 668877x36-nlc-51 name  NLC-mackenzie1 created  2016-03-12T002209.560Z


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
  return(data) }

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

watson.TTS.execute(base_url_TTS,"test",)


##### FULL PACKAGE FUNCTION - PULLING IT ALL TOGETHER
##### TAkes an AUDIO Sample -> Speech to Text -> NLC (Trained for COffee, simple version) -> Get back coffee recommend -> TTS AUdio recommend
sensemaking_full_process <- function(input_file,input_classifier)
{
  transcript <- watson.speech_to_text.sessionless(input_file)
  print(transcript)
  top_class <- watson.nlc.processtextreturnclass(input_classifier,transcript)
  print(top_class)
  top_class <- head(top_class$class,1)
  user_intent <- paste("Thank you! Let's process ",top_class,sep="")
  print(user_intent)
  the_url <- paste(base_url_TTS,"?text=",URLencode(user_intent),"&voice=",voice,sep="")
  print(the_url)
  the_audio = CFILE("audiophile.wav", mode="wb")  ## here we receive the audio back
  curlPerform(url = the_url,userpwd = username_password_TTS,
              httpheader=c(accept="audio/wav"),
              writedata = the_audio@ref)
  close(the_audio)
  system("open audiophile.wav -a vlc")  # Now - Let's listen 
  return(top_class)
}

###### NLC FUNCTION - DELETE CLASSIFIER - Receives name of Classifier to Kill; May not be able to do this until training complete
watson.nlc.deleteclassifier <- function(kill_classifier) {
  DELETE(url=(paste(base_url_NLC,kill_classifier,sep="")),authenticate(username_NLC,password_NLC)) }

##### NLC ACTION: EXECUTE FUNCTION  TO KILL (!!!) DELETE (!!!) CLASSIFIER - WARNING
# watson.NLC.listallclassifiers()  # inventory - what do we want to delete - classifier id
# kill <- "668877x36-nlc-51"
# watson.nlc.deleteclassifier(kill)

listen_for_wake_up_word <- function()
{
  repeat{
    ## LOOPY-TICK
    x <- a$data # get the result
    close(a); rm(a) # you can close the instance at this point # amplify and crop the signal
    audio <- x * 2 ; audio[audio < -1] <- -1 ; audio[audio > 1] <- 1 # bitshifting
    save.wave(audio,file_name) 
    
    ## WHILE WE ARE WAITNG FOR STT TO COMPLETE START RECORDING NEXT BLOCK (INTERLEAVE)
    b <- record(sample_count_fast, sample_rate_fast, 2)  # CAN WE START RECORDING SECOND THREAD?
    
    response <- watson.speech_to_text.recognize(file_name)  # THIS IS SLOW API - Processing "A" here
    transcript_a <- (stt_transcript_only(content(response,"text")))
    print(transcript_a) ## THIS MEANS "A" is done!  Can re-use filename.WAV now
    wait(b) # wait for the B recording to finish - unless it finished while waiting for API call
    
    nlc_response <- watson.nlc.processtextreturnclass(classifier_name,transcript_a)
    print(head(nlc_response,3))
    if((as.numeric(nlc_response$confidence[1])  > threshold) && (nlc_response$class[1] == "mackenzie-wake-up")){loop=FALSE}  # break loop if our new result is WAKE UP WORD
    if(loop == FALSE){break}
     
    ## LOOPY-TOCK
    x <- b$data # get the result
    close(b);  rm(b) # you can close the instance at this point # amplify and crop the signal
    audio <- x * 2 ; audio[audio < -1] <- -1 ; audio[audio > 1] <- 1 # bitshifting
    save.wave(audio,file_name) 
    
    ## WHILE WE ARE WAITNG FOR STT TO COMPLETE START RECORDING NEXT BLOCK (INTERLEAVE)
    a <- record(sample_count_fast, sample_rate_fast, 2)  # 
    
    response <- watson.speech_to_text.recognize(file_name) # THIS IS SLOW API - Processing "B" here
    transcript_b <- (stt_transcript_only(content(response,"text")))
    print(transcript_b)
    wait(a) # OK - A is probably done before the API return, but just in case , need to wait for A if not done already (probably yes, with API)
    nlc_response <- watson.nlc.processtextreturnclass(classifier_name,transcript_b)
    print(head(nlc_response,3))
    if((as.numeric(nlc_response$confidence[1])  > threshold) && (nlc_response$class[1] == "mackenzie-wake-up")){loop=FALSE}  # break loop if our new result is WAKE UP WORD
    if(loop == FALSE){break}
  }# and loop (if loop = TRUE)
}

# function to react to user's command after wake up,a nd after classifier interpret's intent
react_to_user_command <- function(user_command)
{
  if(user_command == "alexa-engage"){
    wait(3)
    the_url <- paste(base_url_TTS,"?text=",URLencode("Hi Ah-lex-ah. This is MacKenzie. Ah-lex-ah, please tell me a joke"),"&voice=",voice,sep="")
    the_audio = CFILE("alexajoke.wav", mode="wb")  ## here we receive the audio back
    curlPerform(url = the_url,userpwd = username_password_TTS,httpheader=c(accept="audio/wav"),writedata = the_audio@ref)
    close(the_audio)
    system("open alexajoke.wav -a vlc")  # Now - Let's listen 
  }
  if(user_command == "change-voice"){
    if(v==4){v<<-1}else{v<<-v+1} # increase v by 1 - unless it's at 4 - in which case reset
    #voice <- list_of_voices[v]
    voice <<- "en-US_MichaelVoice"
    # now utter in new voice
    the_url <- paste(base_url_TTS,"?text=",URLencode("My voice has been changed."),"&voice=",voice,sep="")
    the_audio = CFILE("newvoice.wav", mode="wb")  ## here we receive the audio back
    curlPerform(url = the_url,userpwd = username_password_TTS,httpheader=c(accept="audio/wav"),writedata = the_audio@ref)
    close(the_audio)
    system("open newvoice.wav -a vlc")  # Now - Let's listen     
    
    }
  if(user_command == "profanity"){print("now there, please mind your manners!")}
  if(user_command == "r-command-data-load-table"){newtable <- iris}
  if(user_command == "r-command-install-new-package"){install.packages("kohonan")}
  if(user_command == "r-command-load-library"){library("plyr")}
  if(user_command == "r-command-plot-image"){
    dev.off()
    plot(iris)}
  
}


##### END OF FUNCTION DECLARATIONS
##### END OF FUNCTION DECLARATIONS
##### END OF FUNCTION DECLARATIONS

 

########  WAKE-UP WORD:  MACKENZIE -- SHE'S STANDING BY AND LISTENING FOR KEYWORD "MacKenzie" - 
########  WAKE-UP WORD:  MACKENZIE -- SHE'S STANDING BY AND LISTENING FOR KEYWORD "MacKenzie" - 
########  WAKE-UP WORD:  MACKENZIE -- SHE'S STANDING BY AND LISTENING FOR KEYWORD "MacKenzie" - 
########  WAKE-UP WORD:  MACKENZIE -- SHE'S STANDING BY AND LISTENING FOR KEYWORD "MacKenzie" - 

file_name <- "audio.wav"
sample_count_fast <- 40000  ## 
sample_rate_fast <- 16000 ## 
threshold <- 0.90

# show logo to start
img <- readPNG("ibm_watson.png")
grid::grid.raster(img)



grid::grid.raster(img)
# To begin - Load our voice options
list_of_voices <- watson.TTS.listvoices()
setnames(list_of_voices,c("V1"))
list_of_voices <- list_of_voices[grep("en-", list_of_voices$V1),]  ## subset from the list where English "en-"
list_of_voices
v <- 2 # set voice counter at 3 to begin - english kate - as english people sound smart :)
voice <- list_of_voices[v]
## MAIN LOOP - Comes back here after voice command executed
repeat{
loop = TRUE # reset for the wake up word listen
a <- record(sample_count_fast, sample_rate_fast, 2)  # ## PRIME PUMP - need to wait - just the first time - to have audio ready to process
wait(a) # FIRST TIME THROUGH - PRIME PUMP - JUST SIT AND wait for the recording to finish
##### THIS IS THE MAIN LISTEN AND WAIT LOOP!
wait(play(sin(1:2500/8)))  # recording STOP tone
listen_for_wake_up_word()
# OK CHAMP - MACKENZIE Wake up word Heard!  What now?
wait(play(sin(1:2500/2)))  # recording STOP tone
print("out of the loop!  good job MacKenzie")
spoken_text <- paste("Greetings!  I am MacKenzie - your interface to are Studio. How can I help you?",sep="")
print(spoken_text)
the_url <- paste(base_url_TTS,"?text=",URLencode(spoken_text),"&voice=",voice, sep="")
the_audio = CFILE("spoken_audio.wav", mode="wb")
curlPerform(url = the_url, userpwd = username_password_TTS, httpheader=c(accept="audio/wav"), writedata = the_audio@ref)
close(the_audio)
system("open spoken_audio.wav -a vlc")  # Now - Let's listen
wait(7)
##### Now you have her attention - ask her something!  # mackenzie #2 is 6688a2x37-nlc-153
user_command <- sensemaking_full_process("desire_intent.wav","668877x36-nlc-192")
print(user_command)
react_to_user_command(user_command)
}
# To begin - Load our voice options
list_of_voices <- watson.TTS.listvoices()





# end of file


#######################   UTILITY -  OK - LETS DO STUFF   ##################################

###### ACTION: Create a new CLassifier!  (200 = Good outcome) - 
thefile <- "mackenzie3.csv"  
nlc_name <- "\"NLC-mackenzie3\"" 
watson.nlc.createnewclassifier(thefile,nlc_name)  
# calls function, passes file and name from above, starts the magic. might take 2 to 20+ minutes depending on complexityclassifier_id" : 
# if new will say "not yet ready to accept classify requests" - once done in a few mintues will say - "The classifier instance is now available and is ready to take classifier requests" - then you can submit query below

###### ACTION: Retrieve list of classifiers (NEAT VERSION) - oldest to newest
watson.NLC.listallclassifiers()  # not happy response if no classifiers (Blank) if blank, use below
#classifier_name <- "668877x36-nlc-192" # MacKenzie # #"name" : "NLC-mackenzie1",
#classifier_name <- "6688a2x37-nlc-153"# Upgrade! NLC-mackenzie2 # defined elsewhere
watson.nlc.checkclassifierstatus(classifier_name)

query <- "   mackenzie blue hot dog"
watson.nlc.processtextreturnclass(classifier_name,query)

#######

#### Text to Speech Test
watson.STT.getmodels() # returns list of 10+ voice models for Speech INPUT ##### works
