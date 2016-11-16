######################################################
### Test Bench for Augmented Reality / VR Glasses
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
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML)
library(png)
library(rPython)

#closeAllConnections()

setwd("/Users/ryan/Documents/Project_AR_Command_Control") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below


## Base URLs for IBM Watson APIs
base_url_STT <- "https://stream.watsonplatform.net/speech-to-text/api"
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
base_url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"
base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api"

### GET - Basic Test Tone
getURL("https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19&text=hello",userpwd = username_password_TON ) 

getURL(base_url_NLC,userpwd = username_password_NLC )  # non essential , but checks if working /authenticated

# For audio Sampling later
sample_count <- 60000  ## 0k samples
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

## Function Robot speaks
headset_speaks <- function(voice_transcript)
{
  voice <- "en-US_MichaelVoice"
  the_url <- paste(base_url_TTS,"?text=",URLencode(voice_transcript),"&voice=",voice,sep="")
  the_audio = CFILE("toy_talks.wav", mode="wb")  ## here we receive the audio back
  curlPerform(url = the_url,userpwd = username_password_TTS,httpheader=c(accept="audio/wav"),writedata = the_audio@ref)
  close(the_audio)
  system("open toy_talks.wav -a vlc")  # Now - Let's listen 
  wait(6) ## this gives ECHO some time to use the WiFI and not fight for bandwidth
}

###### 
closeAllConnections()
file_name <- "file_name"
img <- readPNG("ibm_watson.png")
grid::grid.raster(img)

python.load("initialize.py", get.exception = TRUE)





### Function to process output from API and table
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data <- data[-c(1), ] # remove dud first row
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),2)
  setnames(data,c("trait","signal"))
  return(data)
}


### FUNCTION to post data to Tone Analyzer and return results
process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19",
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )
  
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  abc <- tidyResponse(response_text)
  return(abc)
}


process_data_to_tone("I am certainly going to make this project a success") # not perfect, but good enough for now!
query <- "I am super scared and nervous about bees"
process_data_to_tone(query)



### FUNCTION to recieve results of analysis, pull out 5 traits, and show image and plot
display_results <- function(results){
  results <- data.frame(results[1:5,]) # just the gang :)
  results <- results[with(results, order(-signal)),] # reorder
  results <- data.frame(results,stringsAsFactors=FALSE) # stringsAsFactors=FALSE keeps junky factors out
  results
  
  main_trait <- paste(results$trait[1]) # paste kills levels 
  main_signal <- results$signal[1]
  signal_threshold <- 0.75 # this is level that we present 
  
  ## Switch Statement
  if(main_signal>signal_threshold)
  { 
    
    switch(main_trait,
           "joy" = image <- python.exec("ser.write('y')"),
           "sadness" = image <- python.exec("ser.write('b')"),
           "anger" = image <- python.exec("ser.write('r')"),
           "disgust" = image <- python.exec("ser.write('g')"),
           "fear" = image <- python.exec("ser.write('f')")
    )
    
  } #else{python.exec("ser.write('0')")}
  
  ### this from twilio_comms file 
  
  barplot(results$signal, main="Tone Analysis", 
          xlab="", ylab=query, 
          names.arg=results$trait,
          border="red",las=3, srt=45) 
}





### OK - let's throw a few softballs - one for each inside out character (my kids love the movie!)

query <- "Coding is fun!"
analysis <- process_data_to_tone(query)
display_results(analysis)

query <- "Dont pick your nose - it is gross"
analysis <- process_data_to_tone(query)
display_results(analysis)


repeat
{
  response <- watson.speech_to_text.sessionless("test.wav")
  print(response)

  # first we do vanilla Grep - because if we have high amplitude tone AFTER, it should overide - reason? SCARED ended up as scaRED (red) before - overwrote
  if(grepl("all", response)){python.exec("ser.write('a')")}
  if(grepl("off", response)){python.exec("ser.write('0')")}
  if(grepl("reset", response)){python.exec("ser.write('0')")}
  if(grepl("clear", response)){python.exec("ser.write('c')")}
  if(grepl("red", response)){python.exec("ser.write('r')")}
  if(grepl("green", response)){python.exec("ser.write('g')")}
  if(grepl("blue", response)){python.exec("ser.write('b')")}
  if(grepl("yellow", response)){python.exec("ser.write('y')")}
  if(grepl("power", response)){python.exec("ser.write('x')")}
  if(grepl("talk", response)){headset_speaks("Hello, My name is Jarvis! Let's be friends.  OK?")}
  
  # now we see if there is a high amplitude emotion to override the grep (if not it sends)
  analysis <- process_data_to_tone(response)
  display_results(analysis)
  
}


# repeat{
#   headset_speaks("Hello, My name is Blackie. I am a cat")
#   python.exec("ser.write('x')")
# }

