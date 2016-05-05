######################################################
### Boaty McBoatFace -  Chatting with Cognitive Toys
### Experimental Code.  Experimental R Interface for IBM Watson Services -
### PART 1 - We have a speech interface that determines user intent from IBM Watson Speech to Text and NLC services
### PART 2 - We engage PYTHON scripts (serial made easier) from R Studio
### PART 3 - Python Calls Arduino and Modifies Boaty McBoatface
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
library(rPython)

closeAllConnections()

setwd("/Users/ryan/Documents/Project_Boaty_McBoatFace") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below

boaty_mcboatface_test() # test function from other side

## Base URLs for IBM Watson APIs
base_url_STT <- "https://stream.watsonplatform.net/speech-to-text/api"
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
base_url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"

getURL(base_url_NLC,userpwd = username_password_NLC )  # non essential , but checks if working /authenticated

# For audio Sampling later
sample_count <- 40000  ## 0k samples
sample_rate <- 16000 ## at 16khz - is ~5 seconds recording time

########## FUNCTION DECLARATIONS  #### FUNCTION DECLARATIONS ###########
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
  wait(play(sin(1:200/4)))  # recording START tone
  print("RECORDING ------------------ (beep) ")
  the_audio <- watson.STT.record(sample_count,sample_rate)
  wait(play(sin(1:200/2)))  # recording STOP tone
  print("Recording COMPLETE --------- (beep) ")
  #print("Saving WAV File")
  save.wave(the_audio,file_name) 
  print("Calling IBM Watson Speech To Text API")
  response <- watson.speech_to_text.recognize(file_name)
  return(stt_transcript_only(content(response,"text")))
} 

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

## Function boaty speaks
boaty_speaks <- function(voice_transcript)
{
  voice <- "en-US_MichaelVoice"
  the_url <- paste(base_url_TTS,"?text=",URLencode(voice_transcript),"&voice=",voice,sep="")
  the_audio = CFILE("toy_talks.wav", mode="wb")  ## here we receive the audio back
  curlPerform(url = the_url,userpwd = username_password_TTS,httpheader=c(accept="audio/wav"),writedata = the_audio@ref)
  close(the_audio)
  system("open toy_talks.wav -a vlc")  # Now - Let's listen 
  wait(5) ## this gives ECHO some time to use the WiFI and not fight for bandwidth
}


###### PYTHON TEST - ## thanks http://www.r-bloggers.com/calling-python-from-r-with-rpython/
boaty_mcboatface_test <- function(){
  python.load("initialize.py", get.exception = TRUE)
  Sys.sleep(1)
  python.exec("ser.write(' ')")
  Sys.sleep(1)
  python.exec("ser.write('l')")
  Sys.sleep(1)
  python.exec("ser.write('r')")
  Sys.sleep(1)
  python.exec("ser.write('c')")
  Sys.sleep(1)
  python.exec("ser.write(' ')")
}

## test
#boaty_mcboatface_test()

###### 
closeAllConnections()
file_name <- "file_name"
#watson.speech_to_text.sessionless("test.wav")

img <- readPNG("ibm_watson.png")
grid::grid.raster(img)

repeat
{
    response <- watson.speech_to_text.sessionless("test.wav")
    #print(response)
    if(grepl("reset", response)){python.exec("ser.write(' ')")}
    if(grepl("left", response)){python.exec("ser.write('l')")}
    if(grepl("right", response)){python.exec("ser.write('r')")}
    if(grepl("cross", response)){python.exec("ser.write('c')")}
    if(grepl("start", response)){python.exec("ser.write('a')")}
    if(grepl("go", response)){python.exec("ser.write('a')")}
    if(grepl("stop", response)){python.exec("ser.write('z')")}
    if(grepl("off", response)){python.exec("ser.write('z')")}
    if(grepl("red", response)){python.exec("ser.write('2')")}
    if(grepl("dark", response)){python.exec("ser.write('0')")}
    if(grepl("black", response)){python.exec("ser.write('0')")}
    if(grepl("talk", response)){boaty_speaks("Hello, My name is Boaty McBoatFace! Let's be friends.  OK?")}
    
}

## See also https://www.youtube.com/watch?v=DjESWNOfUUs  and https://www.youtube.com/watch?v=l2Q0S33RC80



