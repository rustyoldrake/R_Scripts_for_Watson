###################################################### 
### IBM Watson - SPEECH TO TEXT + TONE for Bootstrapper
### DOCS: https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/doc/speech-to-text/
### Before you begin you will need (1) An IBM Bluemix demo account (2) A Speech to Text App and (3) Credentials to that Service 
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(audio)
library(stringr)  # string splitting

# get keys
source("keys.r") ## KEYS has acutal username:password for each IBM service. 

## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
Sys.setlocale(locale="C") # error: input string 1 is invalid in this locale
options(warn=-1) # careful - turns off warnings


# Speech-To-Text-Orange credentials" "RED": {
url <- "https://stream.watsonplatform.net/speech-to-text/api"
username_STT 
password_STT 
username_password_STT

setwd("/Users/ryan/Documents/Service - Speech to Text (STT)")
getwd()


### FUNCTION to test connectivity and return models available
watson.speech_to_text.getmodels <- function()
{return(GET(url=paste(url,"/v1/models",sep=""),
            authenticate(username_STT,password_STT)))}

### FOR BEST RESULTS - USE USB HEADSET and ensure in MAc > Sys Preferences >Sound it's selected

## FUNCTION - Record!  
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


# command line call works: curl -u $USERNAME:$PASSWORD -H "content-type: audio/wav" --data-binary @"ryan_rasp_pi1.wav" "https://stream.watsonplatform.net/speech-to-text/api/v1/recognize"


#### FUNCTION TO TIDY UP the STT response - just export the TRANSCRIPT ONLY
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


###### FUNCTION - ANalyze AUDIO WAV file with IBM Watson Speech to Text service - SESSIONLESS
watson.speech_to_text.recognize <- function(audio_file)
{ return(POST(url=paste(url,"/v1/recognize",sep=""),
              authenticate(username_STT,password_STT),
              add_headers("Content-Type"="audio/wav"),
              body = (file = upload_file(audio_file))  
))} #works # hope this helps you with syntax!
## this is SESSIONLESS MODE - https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/#!/speech-to-text/recognizeSessionless

######### OK - let's do stuff

watson.speech_to_text.getmodels() # returns list of 10+ models ##### works


######## Let's record some audio
sample_count <- 64000  ## 0k samples
sample_rate <- 16000 ## at 16khz - is ~5 seconds recording time


##### BASIC FUNCTION FOR SPEECH TO TEXT - RETURNS TRANSCRIPT - SESSIONLESS
watson.speech_to_text.sessionless <- function()
{ 
  wait(play(sin(1:5000/4)))  # recording START tone
  print("RECORDING ------------------ (beep) ")
  the_audio <- watson.STT.record(sample_count,sample_rate)
  wait(play(sin(1:5000/2)))  # recording STOP tone
  print("Recording COMPLETE --------- (beep) ")
  print("Saving WAV File")
  save.wave(the_audio,"the_audio.wav") 
  print("Calling IBM Watson Speech To Text API")
  response <- watson.speech_to_text.recognize("the_audio.wav")
  #wait(play(sin(1:2500/16)))
  #wait(play(sin(1:2500/8)))
  return(stt_transcript_only(content(response,"text")))
} 

# TONE ZONE
tidyResponse <- function(data)
{
  data
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data
  data <- data[-c(1), ] # remove dud first row
  data
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data
  
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',')))
  data
  data <- data[,-c(3:6), ] # remove unneeded columns 
  
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),2)
  setNames(data,c("signal","trait"))
  return(data)
}


# TONE ZONE
### POST - Basic Test
process_data_to_tone <- function(text)
{
  response <- POST(url=base_url_TON,
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  response_text
  abc <- tidyResponse(response_text)
  return(abc)
}

base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19"
username_password_TON


############# 

## LET'S GO!
w = as.integer(1)
table = matrix(nrow=0,ncol=2) # if you go over number of rows, will fail
table
colnames(table) <- c("utterance","class")

while(w>0){
  
  print(w) 
  temp = watson.speech_to_text.sessionless()   # (1) Capture Audio and transrcipt 
  print(temp)
  
  table <- rbind(table,paste(temp))  # (2) Put transcript in the growing matrix
  
  # (3) tone enrich in columnn 2 -take dominant signal
  query <- URLencode(temp)
  process_data_to_tone(query)
  
  analysis <- process_data_to_tone(query) # pull TONE from TONE generic API
  analysis <- analysis[1:5,] # just grab these 5 for now
  analysis
  colnames(analysis) <- c("signal","trait")
  analysis <- analysis[rev(order(analysis$signal)),] # reorder big one on top
  analysis
  if(analysis$signal[1]>.50){
    table[w,2] <- paste(analysis$trait[1])
  } else {table[w,2] <- c("neutral")} # crude way to clear second column
  
  # (4) Print updated table
  print(table) 
  w = w+1
}


